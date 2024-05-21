
# This is the function that is called when the endpoint
# is invoked
select_items <- function(Records) {

  # user_id,
  # num_items_review = 3L,
  # num_items_new = 3L,
  # approach_name = c("new_and_review_randomly_chosen_approaches", names(new_item_approaches), names(review_item_approaches)),
  # fallback_item_bank = c("singpause_phrase", "singpause_item"),
  # only_use_items_from_fallback_item_banks = TRUE
  #

  # tictoc::tic() # Remember to not deploy this!


  records <- rjson::fromJSON(Records$body)

  logging::loginfo('records', records)

  logging::loginfo('records[[1]]', records[[1]])
  # logging::loginfo('records[[1]][[1]]', records[[1]][[1]])
  # logging::loginfo('records[[1]][[2]]', records[[1]][[2]])
  logging::loginfo('records[[1]][1]', records[[1]][1])
  logging::loginfo('records[[1]][2]', records[[1]][2])


  dynamodb <- paws::dynamodb()

  dynamo_response <- store_job(dynamodb, job_id = 999, name = "sebtest", message = "hi", status = "PENDING")

  approach_name <- match.arg(approach_name)


  logging::loginfo("Inside select_items function")
  logging::loginfo("user_id = %s", user_id)
  logging::loginfo("num_items_review = %s", num_items_review)
  logging::loginfo("num_items_new = %s", num_items_new)
  logging::loginfo("approach_name = %s", approach_name)
  logging::loginfo("fallback_item_bank = %s", fallback_item_bank)
  logging::loginfo("Taking approach: %s", approach_name)

  response <- tryCatch({

    logging::loginfo("Compiling user trials")

    user_trials <- compile_item_trials(db_con,
                                       user_id = user_id,
                                       join_item_banks_on = TRUE,
                                       filter_item_banks = if(only_use_items_from_fallback_item_banks) fallback_item_bank else NULL,
                                       add_trial_scores = TRUE)

    logging::loginfo("Got user trials")


    if(approach_name == "new_and_review_randomly_chosen_approaches") {

      review_items_ids <- get_items(type = "review", approach_name = "choose_approach_randomly", user_trials, fallback_item_bank, num_items_review, user_id)
      new_items_ids <- get_items(type = "new", approach_name = "choose_approach_randomly", user_trials, fallback_item_bank,  num_items_new, user_id)


    } else {
      type <- if(approach_name %in% names(new_item_approaches)) "new" else if (approach_name %in% names(review_item_approaches)) "review" else stop("Approach not known")
      item_ids <- get_items(type, approach_name, user_trials, fallback_item_bank, num_items, user_id)
      if(type == "new") {
        new_items_ids <- item_ids
      } else {
        review_items_ids <- item_ids
      }
    }


    # Append selected items to DynamoDB
    update_job(dynamodb, job_id = 999, message = rjson::toJSON(list(review_items_ids = review_items_ids,
                                                      new_items_ids = new_items_ids)), status = "FINISHED")

    list(status = 200,
         message = paste0("You have successfully selected new items for ", user_id, "!"),
         review_items_ids = review_items_ids,
         new_items_ids = new_items_ids,
         no_items_review = length(review_items_ids),
         no_items_new = length(new_items_ids))


  }, error = function(err) {

    logging::logerror(err)

    list(
      status = 400,
      message = "Something went wrong!",
      no_items_new = NA,
      no_items_review = NA
    )

  })

  #tictoc::toc() # Remember to not deploy this!

  return(response)




}



get_items <- function(type = c("new", "review"),
                      approach_name = NULL,
                      user_trials,
                      fallback_item_bank_names,
                      num_items,
                      user_id) {

  logging::loginfo("get_items..")
  logging::loginfo("type: %s", type)

  # Get the fallback item_bank

  item_banks_table <- dplyr::tbl(db_con, "item_banks")

  fallback_item_bank_ids <- as.integer(item_bank_name_to_id(item_banks_table, fallback_item_bank_names))

  logging::loginfo("fallback_item_bank_id: %s", fallback_item_bank_ids)

  fallback_item_banks <- purrr::map2(fallback_item_bank_names, fallback_item_bank_ids, function(fallback_item_bank_name, fallback_item_bank_id) {
    dplyr::tbl(db_con, paste0("item_bank_", fallback_item_bank_name)) %>%
      dplyr::slice_sample(n = 10000) %>%  # We don't want to collect too much information in
      dplyr::collect() %>%
      dplyr::mutate(item_bank_id = !! fallback_item_bank_id)
  })

  shared_cols <- mutual_column_names(fallback_item_banks)

  fallback_item_banks <- fallback_item_banks %>%
    purrr::map(function(df) {
      df %>% dplyr::select(dplyr::all_of(shared_cols))
    })

  # Use reduce to perform inner joins on the list of tables
  grand_fallback_item_bank <- dplyr::bind_rows(fallback_item_banks)

  if(type == "new") {
    tbl_name <- "new_items"
    approach_fun <- new_item_approaches[[approach_name]]
    previously_practiced_items <- user_trials %>% dplyr::pull(item_id) %>% unique()
    sampling_df <- grand_fallback_item_bank %>% dplyr::filter(!item_id %in% !! previously_practiced_items)
  } else if(type == "review") {
    tbl_name <- "review_items"
    approach_fun <- review_item_approaches[[approach_name]]
    sampling_df <- user_trials
  } else {
    stop("Not a valid type")
  }

  primary_key_col <- paste0(tbl_name, "_", "id")


  num_unique_items <- user_trials %>%
    dplyr::pull(item_id) %>%
    unique() %>%
    length()

  if(type == "review" && num_unique_items < num_items) {

    logging::loginfo("Not enough trials for user yet, apply random selection")

    item_ids_df <- item_sel_random_item_selection(grand_fallback_item_bank, num_items)

    approach_name <- "random_item_selection"

  } else {
    # Apply the approach

    if(approach_name == "choose_approach_randomly") {
      # Update the name with the selected approach
      item_ids_df <- approach_fun(sampling_df, num_items, user_id, type)
      approach_name <- item_ids_df$random_approach_chosen
      item_ids_df <- item_ids_df$result
    } else {
      item_ids_df <- approach_fun(sampling_df, num_items, user_id, type)
    }
  }


  if(nrow(item_ids_df) > num_items) {
    item_ids_df <- item_ids_df %>%
      dplyr::slice_min(ranking, n = num_items)
  }

  df_to_append <- item_ids_df %>%
    dplyr::mutate(prediction_method = approach_name,
                  user_id = user_id,
                  prediction_time = Sys.time(),
                  active = 1L) %>%
    dplyr::select(prediction_method,
                  user_id,
                  prediction_time,
                  item_id,
                  item_bank_id,
                  ranking,
                  prediction_statistic,
                  active)

  # Append prediction information to SQL DB
  db_append_to_table(db_con, tbl_name, df_to_append, primary_key_col = primary_key_col)

  return(item_ids_df)

}



# New item approaches


item_sel_choose_approach_randomly <- function(user_trials, num_items, user_id = NULL, type = c("review", "new")) {

  type <- match.arg(type)

  if(type == "review") {
    approaches <- review_item_approaches
  } else if(type == "new") {
    approaches <- new_item_approaches
  } else {
    stop("Approach unknown")
  }

  # Randomly select an approach

  # Pop-off the random selection approach which has already been chosen
  approaches$choose_approach_randomly <- NULL

  logging::loginfo("Randomly selecting from %s approaches", length(approaches))

  random_approach_name <- sample(names(approaches), 1)

  logging::loginfo("Taking randomly selected approach: %s", random_approach_name)


  approach_fun <- approaches[[random_approach_name]]

  list(result = approach_fun(user_trials, num_items, user_id),
       random_approach_chosen = random_approach_name)

}

item_sel_random_item_selection <- function(item_bank, num_items, user_id = NULL, type = NULL) {
  # Even though we don't use user_id, we need to leave it in for standardization
  item_bank %>%
    dplyr::select(item_id, item_bank_id) %>%
    dplyr::collect() %>%
    unique() %>%
    dplyr::slice_sample(n = num_items) %>%
    dplyr::mutate(ranking = dplyr::row_number(),
                  prediction_statistic = NA)
}


# Add here sample_melody_in_key and sample_from_item_bank (i.e, stratified sampling?)

item_sel_irt_difficulty <- function(item_bank, num_items, user_id, type = NULL, rhythmic = TRUE) {

  if(is(item_bank, "item_bank")) {
    item_bank <- tibble::as_tibble(item_bank)
  }

  if(rhythmic) {
    difficulty_col_name <- as.name('rhythmic_difficulty')
  } else {
    difficulty_col_name <- as.name('arrhythmic_difficulty')
  }

  user_ability_value <- get_latest_user_ability_estimate(user_id)

  item_bank %>%
    dplyr::mutate(diff_from_difficulty = abs(user_ability_value - !! difficulty_col_name) ) %>%
    dplyr::slice_min(diff_from_difficulty, n = num_items, with_ties = FALSE) %>%
    dplyr::arrange(diff_from_difficulty) %>%
    dplyr::mutate(ranking = dplyr::row_number()) %>%
    dplyr::rename(prediction_statistic = diff_from_difficulty) %>%
    dplyr::collect()

}

# db_con <- musicassessr_con()
# item_sel_irt_difficulty(Berkowitz::ngram_item_bank, num_items = 10, user_id = 2L)

item_sel_collaborative_filtering_item_based <- function(items) {

  SAA_benchmark_data <- SAA_benchmark_data %>%
    dplyr::mutate(mel_p_id = paste(abs_melody, p_id, sep="-")) %>%
    # This gets rid of duplicates and ensures that if there
    # are entries for a user both 'opti3' and not 'opti3'
    # a survey, the 'opti3' entry is retained
    dplyr::arrange(desc(opti3)) %>%
    dplyr::distinct(mel_p_id, .keep_all=T) %>%
    dplyr::select(-mel_p_id) %>%
    tidyr::pivot_wider(names_from=abs_melody, values_from=opti3, values_fill = NA)
    # Transform into binarized matrix
    user_id <- dat$p_id
    dat$p_id <- NULL
    mel_mat <- as.matrix(dat)
    rownames(mel_mat) <- user_id
    r <- as(mel_mat,"realRatingMatrix")
    r_b <- binarize(r, minRating=1)
    # Build the hybrid predictor
    logging::loginfo("Training recommenders")
    popular_recommender <- Recommender(data=r_b, method="POPULAR")
    my_recommender <- popular_recommender
    # if the user isn't in dat, we recommend based on popularity,
    # in which case any index will do (but the recommender stil requires one)
    user_idx <- 1
    if (p_id %in% user_id) {
      # If the user has attempted at least one survey before, use
      # a mixture of user-based any hybrid recommendations
      user_recommender <- Recommender(data = r_b, method="UBCF", param=list(nn=50))
      hybrid <- HybridRecommender(user_recommender, popular_recommender,
                                  weights=c(0.50, 0.50))
      # get the index of the p_id
      user_idx <- match(p_id, user_id )
      my_recommender <- hybrid
    }

  logging::loginfo("Generating recommendations")
  output <- predict(object = my_recommender, newdata = user_idx, n = n, data = r_b)
  recs <- unlist(output@items)
  itemIds <- colnames(mel_mat)
  recommended_items <- itemIds[recs]
  logging::loginfo("Recommendations generated, returning to client")
  recommended_items


}


# t <- recommend_n_melodies(p_id = sample(unique(dat$p_id), size = 1),
#                           n = 10)



item_sel_xgboost <- function() {

}

new_item_approaches <- list("choose_approach_randomly" = item_sel_choose_approach_randomly,
                            "random_item_selection" = item_sel_random_item_selection,
                            "irt_difficulty" = item_sel_irt_difficulty)



# Auxilliary

get_latest_user_ability_estimate <- function(user_id, rhythmic = TRUE, last_attempt = TRUE) {

  scores <- dplyr::tbl(db_con, "scores_session") %>%
    dplyr::filter(user_id == !! user_id,
                  grepl("ability_estimate", measure)) %>%
    { if(!rhythmic) dplyr::filter(., grepl("arrhythmic", measure)) else dplyr::filter(., grepl("_rhythmic", measure)) } %>%
    { if(last_attempt) dplyr::filter(., grepl("last_attempt", measure)) else dplyr::filter(., grepl("first_attempt", measure)) } %>%
    dplyr::slice_max(session_id) %>%
    dplyr::pull(score)

}







# Review item approaches


item_sel_rev_random_item_selection <- function(review_items, num_items, user_id = NULL, score_name = "opti3", type = NULL) {
  review_items %>%
    sort_review_scores(score_name) %>%
    dplyr::slice_sample(n = num_items) %>%
    dplyr::mutate(ranking = dplyr::row_number(),
                  prediction_statistic = NA)
}

item_sel_rev_min_score <- function(review_items, num_items, user_id = NULL, score_name = "opti3", type = NULL) {

  selection <- review_items %>%
    sort_review_scores(score_name) %>%
    dplyr::slice_min(score, n = num_items) %>%
    dplyr::select(item_id, item_bank_id, score) %>%
    dplyr::arrange(score) %>%
    dplyr::mutate(ranking = dplyr::row_number() ) %>%
    dplyr::rename(prediction_statistic = score)

}

item_sel_rev_max_score <- function(review_items, num_items, user_id = NULL, score_name = "opti3", type = NULL) {
  selection <- review_items %>%
    sort_review_scores(score_name) %>%
    dplyr::slice_max(score, n = num_items) %>%
    dplyr::select(item_id, item_bank_id, score) %>%
    dplyr::arrange(dplyr::desc(score)) %>%
    dplyr::mutate(ranking = dplyr::row_number()) %>%
    dplyr::rename(prediction_statistic = score)

}

item_sel_rev_lowest_difficulty <- function(review_items, num_items, user_id = NULL, score_name = "opti3", type = NULL) {
  selection <- review_items %>%
    sort_review_scores(score_name) %>%
    dplyr::slice_min(rhythmic_difficulty, n = num_items) %>%
    dplyr::select(item_id, item_bank_id, rhythmic_difficulty) %>%
    dplyr::arrange(rhythmic_difficulty) %>%
    dplyr::mutate(ranking = dplyr::row_number()) %>%
    dplyr::rename(prediction_statistic = rhythmic_difficulty)

}

item_sel_rev_highest_difficulty <- function(review_items, num_items, user_id = NULL, score_name = "opti3", type = NULL) {
  selection <- review_items %>%
    sort_review_scores(score_name) %>%
    dplyr::slice_max(rhythmic_difficulty, n = num_items) %>%
    dplyr::select(item_id, item_bank_id, rhythmic_difficulty) %>%
    dplyr::arrange(dplyr::desc(rhythmic_difficulty)) %>%
    dplyr::mutate(ranking = dplyr::row_number()) %>%
    dplyr::rename(prediction_statistic = rhythmic_difficulty)

}

sort_review_scores <- function(review_items, score_name) {
  review_items %>%
    dplyr::filter(measure == !! score_name,
                  "{score_name}" < 1) %>% # Reviews should be (at least now) based on non-mastered items
    dplyr::group_by(item_id) %>%
    dplyr::slice_max(trial_time_started) %>%
    dplyr::ungroup()
}

review_item_approaches <- list("choose_approach_randomly" = item_sel_choose_approach_randomly,
                               "random_item_selection" = item_sel_rev_random_item_selection,
                               "min_score" = item_sel_rev_min_score,
                               "max_score" = item_sel_rev_max_score,
                               "lowest_difficulty" = item_sel_rev_lowest_difficulty,
                               "highest_difficulty" = item_sel_rev_highest_difficulty)



# db_con <- musicassessr_con(local = TRUE)
# db_con <- dbConnect(duckdb::duckdb(), "musicassessr.duckdb")

# t <- select_items(user_id = 58L)
# t <- select_items(user_id = 2L)



# Initialize the DynamoDB client
# dynamodb <- paws::dynamodb()
# t <- store_job(dynamodb, job_id = 999, name = "sebtest", message = "hi", status = "PENDING")


store_job <- function(dynamodb, job_id, name, message = "", status = "PENDING") {

  created_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

  response <- dynamodb$put_item(
    TableName = 'jobs',
    Item = list(
      jobId = list(S = job_id),
      name = list(S = name),
      message = list(S = message),
      status = list(S = status),
      createdAt = list(S = created_at)
    )
  )

  return(response)
}



# Function to update the job in DynamoDB
update_job <- function(dynamodb, job_id, message, status) {
  response <- dynamodb$update_item(
    TableName = 'jobs',
    Key = list(
      jobId = list(S = job_id)
    ),
    UpdateExpression = 'SET message = :msg, #status = :sts',
    ExpressionAttributeValues = list(
      ':msg' = list(S = message),
      ':sts' = list(S = status)
    ),
    ExpressionAttributeNames = list(
      '#status' = 'status'
    )
  )

  return(response)
}


# t <- rjson::fromJSON('{
#   "Records": [
#     {
#       "messageId": "059f36b4-87a3-44ab-83d2-661975830a7d",
#       "receiptHandle": "AQEBwJnKyrHigUMZj6rYigCgxlaS3SLy0a...",
#       "body": "{\"jobId\":\"36f93907-ba3a-41d6-90e5-b4da4c558f06\",\"user_id\":55}",
#       "attributes": {
#         "ApproximateReceiveCount": "1",
#         "SentTimestamp": "1545082649183",
#         "SenderId": "AIDAIENQZJOLO23YVJ4VO",
#         "ApproximateFirstReceiveTimestamp": "1545082649185"
#       },
#       "messageAttributes": {},
#       "md5OfBody": "e4e68fb7bd0e697a0ae8f1bb342846b3",
#       "eventSource": "aws:sqs",
#       "eventSourceARN": "arn:aws:sqs:us-east-2:123456789012:my-queue",
#       "awsRegion": "us-east-2"
#     }
#   ]
# }')
