
#
# library(tidyverse)
# load_all()
#

# db_con <- musicassessr_con()
# t <- select_items(96L)
#
# t$new_items
# t$review_items


# tt <- tbl(db_con, "item_bank_Berkowitz_songbird") %>%
#   filter(item_id == "Berkowitz_ngram_407289") %>%
#   collect()

# tt <- tbl(db_con, "review_items") %>% collect()

# db_disconnect(db_con)


#' Get job status API for select items lambda
#'
#' @param job_id
#' @param filename
#'
#' @return
#' @export
#'
#' @examples
get_job_status_api <- function(job_id = NULL, filename = NULL) {

  # Define the request body as a list
  request_body <- list(
    job_id = job_id,
    filename = filename
  )

  endpoint_wrapper(function_name = "get-job-status",
                   request_body = request_body,
                   endpoint_url = paste0(Sys.getenv("ENDPOINT_URL"), 'v2/') )
}


# This is the function that is called when the endpoint
# is invoked
select_items <- function(user_id) {

  logging::loginfo("Inside select_items function")

  logging::loginfo("Test log4!")

  logging::loginfo("Test log 2!")


  response <- tryCatch({

    # Instantiate vars
    num_items_review <- 3L
    num_items_new <- 3L
    approach_name <- "new_and_review_randomly_chosen_approaches"
    fallback_item_bank <- "Berkowitz_songbird"
    only_use_items_from_fallback_item_banks <- TRUE


    logging::loginfo("user_id %s", user_id)
    logging::loginfo("num_items_review = %s", num_items_review)
    logging::loginfo("num_items_new = %s", num_items_new)
    logging::loginfo("approach_name = %s", approach_name)
    logging::loginfo("fallback_item_bank = %s", fallback_item_bank)
    logging::loginfo("Taking approach: %s", approach_name)

    # Compile user trials

    logging::loginfo("Compiling user trials")

    user_trials <- compile_item_trials(db_con,
                                       user_id = user_id,
                                       join_item_banks_on = TRUE,
                                       filter_item_banks = if(only_use_items_from_fallback_item_banks) fallback_item_bank else NULL,
                                       add_trial_scores = TRUE)

    logging::loginfo("Got user trials")


    if(approach_name == "new_and_review_randomly_chosen_approaches") {

      review_items_df <- get_items(type = "review", approach_name = "choose_approach_randomly", user_trials, fallback_item_bank, num_items_review, user_id)
      new_items_df <- get_items(type = "new", approach_name = "choose_approach_randomly", user_trials, fallback_item_bank,  num_items_new, user_id)


    } else {
      type <- if(approach_name %in% names(new_item_approaches)) "new" else if (approach_name %in% names(review_item_approaches)) "review" else stop("Approach not known")
      items_df <- get_items(type, approach_name, user_trials, fallback_item_bank, num_items, user_id)
      if(type == "new") {
        new_items_df <- items_df
      } else {
        review_items_df <- items_df
      }
    }

    # # Append selected items to DynamoDB
    # update_job(dynamodb, job_id = job_id, message = jsonlite::toJSON(list(review_items = review_items_df,
    #                                                                    new_items = new_items_df)), status = "FINISHED")

    list(status = 200,
         message = paste0("You have successfully selected new items for ", user_id, "!"),
         new_items = new_items_df,
         review_items = review_items_df
         )


  }, error = function(err) {

    logging::logerror(err)

    list(
      status = 400,
      message = "Something went wrong!"
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
  logging::loginfo("num_items %s", num_items)

  # Get the fallback item_bank

  item_banks_table <- item_banks_table_static

  fallback_item_bank_ids <- as.integer(item_bank_name_to_id(item_banks_table, fallback_item_bank_names))

  logging::loginfo("fallback_item_bank_id: %s", fallback_item_bank_ids)

  logging::loginfo("Getting fallback item banks...")

  fallback_item_banks <- purrr::map2(fallback_item_bank_names, fallback_item_bank_ids, function(fallback_item_bank_name, fallback_item_bank_id) {
    logging::loginfo(fallback_item_bank_name)
    dplyr::tbl(db_con, paste0("item_bank_", fallback_item_bank_name)) %>%
      dplyr::slice_sample(n = 1000) %>%  # We don't want to collect too much information in
      dplyr::collect() %>%
      dplyr::mutate(item_bank_id = !! fallback_item_bank_id)
  })

  logging::loginfo("... got fallback item banks.")

  shared_cols <- mutual_column_names(fallback_item_banks)

  fallback_item_banks <- fallback_item_banks %>%
    purrr::map(function(df) {
      df %>% dplyr::select(dplyr::all_of(shared_cols))
    })

  # Use reduce to perform inner joins on the list of tables
  grand_fallback_item_bank <- dplyr::bind_rows(fallback_item_banks) %>%
    unique() # In case duplicates due to partition DBs

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

    # This we use for storing the decision in the DB
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

  # Make sure there are not too many items (e.g., from ties or something earlier on)
  if(nrow(item_ids_df) > num_items) {
    item_ids_df <- item_ids_df %>%
      dplyr::slice_min(ranking, n = num_items)
  }


  # Store meta data about the prediction
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
  selected_items_ids <- db_append_to_table(db_con, tbl_name, df_to_append, primary_key_col = primary_key_col)

  selected_rows <- dplyr::tbl(db_con, tbl_name)

  if(type == "review") {
    selected_rows <- selected_rows %>%
      dplyr::filter(review_items_id %in% !! selected_items_ids)
  } else {
    selected_rows <- selected_rows %>%
      dplyr::filter(new_items_id %in% !! selected_items_ids)
  }

  selected_rows <- selected_rows %>%
    dplyr::collect()

  # Return the full item DF for the test
  if(type == "review" && num_unique_items < num_items) {
    items_df <- selected_rows %>%
      dplyr::left_join(grand_fallback_item_bank, by = "item_id")

  } else {

    items_df <- selected_rows %>%
      dplyr::select(-item_bank_id) %>%
      dplyr::left_join(grand_fallback_item_bank, by = "item_id")
  }

  # N.B, we use charactor vector so we can user dplyr::any_of below
  vars_to_select <- c("item_id", "stimulus_abs_melody", "stimulus_durations",
                      "abs_melody", "durations", "item_bank_id", "onset", "melody")

  if(type == "review") {
    vars_to_select <- c(vars_to_select, "review_items_id")
  } else {
    vars_to_select <- c(vars_to_select, "new_items_id")
  }

  items_df <- items_df %>%
    dplyr::select(dplyr::any_of(vars_to_select))

  return(items_df)

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
    dplyr::distinct(mel_p_id, .keep_all = TRUE) %>%
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




# Initialize the DynamoDB client
# dynamodb <- paws::dynamodb()
# t <- store_job(dynamodb, job_id = 999, name = "sebtest", message = "hi", status = "PENDING")

get_job_message <- function(dynamodb, job_id) {

  # Retrieve the item from DynamoDB
  response <- dynamodb$get_item(
    TableName = "jobs",
    Key = list(
      jobId = list(S = job_id)
    )
  )

  # Extract the message from the response
  if (!is.null(response$Item)) {
    message <- response$Item$message$S
    return(message)
  } else {
    logging::logerror("Item not found.")
  }
}

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

# t <- get_job_status_api("cae7bf1e-72b2-4ebc-bf7f-970cd792f86b")
