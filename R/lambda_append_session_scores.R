


#' Compute session scores and end session API
#'
#' @param test_id
#' @param session_id
#' @param user_id
#' @param psychTestR_session_id
#' @param session_complete
#' @param user_info
#'
#' @return
#' @export
#'
#' @examples
compute_session_scores_and_end_session_api <- function(test_id = NA,
                                                       session_id,
                                                       user_id,
                                                       psychTestR_session_id = NA,
                                                       session_complete = c("0", "1"),
                                                       user_info = NA) {

  session_complete <- match.arg(session_complete)

  if(is.null(user_info)) {
    user_info <- NA
  }

  if(is.null(psychTestR_session_id)) {
    psychTestR_session_id <- NA
  }

  session_complete <- match.arg(session_complete)

  # Define the request body as a list
  request_body <- list(test_id = test_id,
                       session_id = session_id,
                       user_id = user_id,
                       psychTestR_session_id = psychTestR_session_id,
                       session_complete = session_complete,
                       user_info = user_info)

  print(request_body)

  endpoint_wrapper(function_name = "compute-session-scores-and-end-session",
                   request_body = request_body)

}



# db_con <- musicassessr_con(pool = F)

# t <- compute_session_scores_and_end_session(test_id = 1L,
#                                             session_id = 2086,
#                                             user_id = 93L,
#                                             psychTestR_session_id = NA,
#                                             session_complete = "1")



# t <- compute_session_scores_and_end_session(test_id = 1L, session_id = 2973, user_id = 111)

# This is the function that is called when the endpoint
# is invoked
compute_session_scores_and_end_session <- function(test_id = NA,
                                                   session_id,
                                                   user_id,
                                                   psychTestR_session_id = NA,
                                                   session_complete = c("0", "1"),
                                                   user_info = NA) {

  logging::loginfo("Inside compute_session_scores_and_end_session...")


  session_complete <- match.arg(session_complete)
  session_complete <- as.integer(session_complete)

  test_id <- as.integer(test_id)
  session_id <- as.integer(session_id)

  if(length(test_id) == 0) {
    test_id <- NA
  }

  if(length(psychTestR_session_id) == 0) {
    psychTestR_session_id <- NA
  }

  if(length(user_info) == 0) {
    user_info <- NA
  }

  logging::loginfo("test_id = %s", test_id)

  logging::loginfo("session_id = %s", session_id)

  logging::loginfo("user_id = %s", user_id)

  logging::loginfo("psychTestR_session_id = %s", psychTestR_session_id)

  logging::loginfo("session_complete = %s", session_complete)

  logging::loginfo("user_info = %s", user_info)

  logging::loginfo("class(user_info) = %s", class(user_info))

  if(!is.character(user_info)) {
    logging::loginfo("user_info is not character, coerce to character")
    user_info <- user_info %>% jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE) %>%
      as.character()
    logging::loginfo("class(user_info) = %s", class(user_info))
  }


  complete_time <- Sys.time()


  response <- tryCatch({

    # Update sessions table with time finished

    session_df <- get_table(db_con, 'sessions', collect = FALSE)

    logging::loginfo("Storing complete time as %s", complete_time)

    new_dat <- tibble::tibble(session_id = session_id,
                              session_time_completed = complete_time,
                              psychTestR_session_id = psychTestR_session_id,
                              session_complete = session_complete,
                              user_info = user_info)

    update <- dbplyr::copy_inline(db_con, new_dat)

    dplyr::rows_update(session_df, update, in_place = TRUE, by = "session_id", unmatched = "ignore")


    scores_session_id <- session_scores_helper(db_con, test_id, session_id, user_id)

    # Return response

    list(
      status = 200,
      message = "You have successfully added the final session scores!",
      session_id = session_id,
      scores_session_id = scores_session_id
    )

  }, error = function(err){

    logging::logerror(err)

    list(
      status = 400,
      message = "Something went wrong!",
      session_id = session_id,
      scores_session_id = NA
    )

  })


  return(response)

}


session_scores_helper <- function(db_con,
                                  test_id,
                                  session_id,
                                  user_id,
                                  update = FALSE) {

  # Get trials

  trial_table <- compile_item_trials(db_con,
                                     test_id,
                                     session_id, # Here we give a session ID, because we only want to assess trials in this session
                                     user_id,
                                     join_item_banks_on = TRUE,
                                     trial_filter_fun = function(df) {
                                       df %>%
                                         dplyr::filter(melody_block_paradigm != "long_note",
                                                       ! grepl("CUSTOM_ITEM", item_id) )
                                     })

  # Long note trials too

  long_note_trials <- compile_item_trials(db_con,
                                          test_id,
                                          session_id, # Here we give a session ID, because we only want to assess trials in this session
                                          user_id,
                                          join_item_banks_on = FALSE,
                                          score_to_use = NULL,
                                          add_trial_scores = TRUE,
                                          trial_filter_fun = function(df) {
                                            df %>% dplyr::filter(melody_block_paradigm == "long_note")
                                          })

  if(nrow(long_note_trials) > 0L) {

    long_note_scores <- long_note_trials %>%
      dplyr::select(long_note_accuracy:long_note_freq_min)

    long_note_pca_scores <- long_note_scores %>%
      dplyr::select(long_note_accuracy, long_note_dtw_distance, long_note_autocorrelation_mean,
                    long_note_run_test, long_note_no_cpts, long_note_beginning_of_second_cpt) %>%
      musicassessr::get_long_note_pcas()

  } else {
    long_note_pca_scores <- NA
  }



  logging::loginfo("Number of trials found: %s", nrow(trial_table))

  if(get_nrows(trial_table) > 0L) {

    scores_trial <- get_table(db_con, "scores_trial", collect = FALSE) %>%
      dplyr::filter(trial_id %in% !! trial_table$trial_id) %>%
      dplyr::filter(!is.na(score)) %>%
      dplyr::select(-scores_trial_id) %>%
      # Handle duplicates... but this shouldn't really happen
      dplyr::group_by(trial_id, measure) %>%
      dplyr::slice_max(score) %>%
      dplyr::ungroup() %>%
      dplyr::collect() %>%
      unique() %>%
      tidyr::pivot_wider(names_from = "measure", values_from = "score")

    # Join on scores
    trial_table <- trial_table %>%
      { if("ngrukkon" %in% names(trial_table)) dplyr::rename(., ngrukkon_between_melody_and_parent_melody = ngrukkon) else . } %>%
      { if(!"log_freq" %in% names(trial_table)) dplyr::mutate(., log_freq = NA) else . } %>%
      dplyr::left_join(scores_trial, by = "trial_id")


    # First attempt
    first_attempt_trial_table <- trial_table %>%
      dplyr::group_by(melody) %>%
      dplyr::slice_min(attempt, with_ties = FALSE) %>%
      dplyr::ungroup()

    # Last attempt
    last_attempt_trial_table <- trial_table %>%
      dplyr::group_by(melody) %>%
      dplyr::slice_max(attempt, with_ties = FALSE) %>% #
      dplyr::ungroup()


    # Produce session-level scores

    ## Arrhythmic

    logging::loginfo("Getting arrhythmic scores..")

    ### Mean opti3
    mean_opti3_arrhythmic <- trial_table %>%
      dplyr::filter(!rhythmic) %>%
      dplyr::summarise(mean_opti3 = mean(opti3, na.rm = TRUE)) %>%
      dplyr::pull(mean_opti3)

    logging::loginfo("mean_opti3_arrhythmic %s", mean_opti3_arrhythmic)

    #### First attempt
    mean_opti3_arrhythmic_first_attempt <- first_attempt_trial_table %>%
      dplyr::filter(!rhythmic) %>%
      dplyr::summarise(mean_opti3 = mean(opti3, na.rm = TRUE)) %>%
      dplyr::pull(mean_opti3)

    logging::loginfo("mean_opti3_arrhythmic_first_attempt %s", mean_opti3_arrhythmic_first_attempt)

    #### Last attempt
    mean_opti3_arrhythmic_last_attempt <- last_attempt_trial_table %>%
      dplyr::filter(!rhythmic) %>%
      dplyr::summarise(mean_opti3 = mean(opti3, na.rm = TRUE)) %>%
      dplyr::pull(mean_opti3)

    logging::loginfo("mean_opti3_arrhythmic_last_attempt %s", mean_opti3_arrhythmic_last_attempt)

    logging::loginfo("Load lme4 namespace")

    # lme4 namespace needed for predict method
    loadNamespace("lme4")

    logging::loginfo("Get ability estimates..")


    ### Ability estimate
    #### First attempt
    ability_estimate_arrhythmic_first_attempt <-
      first_attempt_trial_table %>%
      dplyr::filter(!rhythmic) %>%
      dplyr::select(N, step.cont.loc.var, log_freq, i.entropy, opti3, tonalness) %>%
      dplyr::mutate(tmp_scores = opti3) %>%
      psychTestRCATME::predict_based_on_mixed_effects_arrhythmic_model(Berkowitz::lm2.2,  new_data = .)

    logging::loginfo("ability_estimate_arrhythmic_first_attempt %s", ability_estimate_arrhythmic_first_attempt)

    #### Last attempt
    ability_estimate_arrhythmic_last_attempt <-
      last_attempt_trial_table %>%
      dplyr::filter(!rhythmic) %>%
      dplyr::select(N, step.cont.loc.var, tonalness, log_freq, opti3) %>%
      dplyr::mutate(tmp_scores = opti3) %>%
      psychTestRCATME::predict_based_on_mixed_effects_arrhythmic_model(Berkowitz::lm2.2,  new_data = . )

    logging::loginfo("ability_estimate_arrhythmic_last_attempt %s", ability_estimate_arrhythmic_last_attempt)


    ## Rhythmic
    logging::loginfo("Getting rhythmic scores..")
    ### Mean opti3
    mean_opti3_rhythmic <- trial_table %>%
      dplyr::filter(rhythmic) %>%
      dplyr::summarise(mean_opti3 = mean(opti3, na.rm = TRUE)) %>%
      dplyr::pull(mean_opti3)

    logging::loginfo("mean_opti3_rhythmic %s", mean_opti3_rhythmic)

    #### First attempt
    mean_opti3_rhythmic_first_attempt <- first_attempt_trial_table %>%
      dplyr::filter(rhythmic) %>%
      dplyr::summarise(mean_opti3 = mean(opti3, na.rm = TRUE)) %>%
      dplyr::pull(mean_opti3)

    logging::loginfo("mean_opti3_rhythmic_first_attempt %s", mean_opti3_rhythmic_first_attempt)

    #### Last attempt
    mean_opti3_rhythmic_last_attempt <- last_attempt_trial_table %>%
      dplyr::filter(rhythmic) %>%
      dplyr::summarise(mean_opti3 = mean(opti3, na.rm = TRUE)) %>%
      dplyr::pull(mean_opti3)

    logging::loginfo("mean_opti3_rhythmic_last_attempt %s", mean_opti3_rhythmic_last_attempt)

    ### Ability estimate
    #### First attempt
    ability_estimate_rhythmic_first_attempt <-
      first_attempt_trial_table %>%
      dplyr::filter(rhythmic) %>%
      dplyr::select(N, step.cont.loc.var, log_freq, d.entropy, i.entropy, opti3) %>%
      dplyr::mutate(tmp_scores = opti3) %>%
      psychTestRCATME::predict_based_on_mixed_effects_rhythmic_model(Berkowitz::lm3.2,  new_data = .)

    logging::loginfo("ability_estimate_rhythmic_first_attempt %s", ability_estimate_rhythmic_first_attempt)

    #### Last attempt
    ability_estimate_rhythmic_last_attempt <-
      last_attempt_trial_table %>%
      dplyr::filter(rhythmic) %>%
      dplyr::select(N, step.cont.loc.var, log_freq, d.entropy, i.entropy, opti3) %>%
      dplyr::mutate(tmp_scores = opti3) %>%
      psychTestRCATME::predict_based_on_mixed_effects_rhythmic_model(Berkowitz::lm3.2,  new_data = . )

    logging::loginfo("ability_estimate_rhythmic_last_attempt %s", ability_estimate_rhythmic_last_attempt)


    scores <- tibble::tibble(
      # Arrhythmic
      mean_opti3_arrhythmic = mean_opti3_arrhythmic,
      mean_opti3_arrhythmic_first_attempt = mean_opti3_arrhythmic_first_attempt,
      mean_opti3_arrhythmic_last_attempt = mean_opti3_arrhythmic_last_attempt,
      ability_estimate_arrhythmic_first_attempt = ability_estimate_arrhythmic_first_attempt,
      ability_estimate_arrhythmic_last_attempt = ability_estimate_arrhythmic_last_attempt,
      # Rhythmic
      mean_opti3_rhythmic = mean_opti3_rhythmic,
      mean_opti3_rhythmic_first_attempt = mean_opti3_rhythmic_first_attempt,
      mean_opti3_rhythmic_last_attempt = mean_opti3_rhythmic_last_attempt,
      ability_estimate_rhythmic_first_attempt = ability_estimate_rhythmic_first_attempt,
      ability_estimate_rhythmic_last_attempt = ability_estimate_rhythmic_last_attempt)

    if(is.data.frame(long_note_pca_scores)) {
      scores <- cbind(scores, long_note_pca_scores)
    }

    scores <- scores %>%
      scores_to_long_format() %>%
      dplyr::mutate(session_id = !! session_id)


    if(update) {

      update <- dbplyr::copy_inline(db_con, scores)

      # Work out which ids will be updated before hand
      scores_session_id <- dplyr::left_join(update,
                                             dplyr::tbl(db_con, "scores_session"),
                                             by = c("session_id", "measure") ) %>%
        dplyr::pull(scores_session_id)

      dplyr::rows_update(dplyr::tbl(db_con, "scores_session"), update, in_place = TRUE, by = c("session_id", "measure"), unmatched = "ignore")

    } else {
      # Append scores
      scores_session_id <- db_append_scores_session(db_con, scores)
    }


  } else {
    scores_session_id <- NA
  }

  return(scores_session_id)
}




#' Store session-level scores in scores_session
#'
#' @param db_con
#' @param session_scores_df Should contain session_id, measure and score columns.
#'
#' @return
#' @export
#'
#' @examples
db_append_scores_session <- function(db_con, session_scores_df) {

  stopifnot(
    all(c('session_id', 'measure', 'score') %in% names(session_scores_df))
  )

  scores_session_id <- db_append_to_table(db_con, table = "scores_session", data = session_scores_df, primary_key_col = "scores_session_id")

  return(scores_session_id)
}


# t <- compute_session_scores_and_end_session(test_id = 2L, session_id = 594L, user_id = 2L)


