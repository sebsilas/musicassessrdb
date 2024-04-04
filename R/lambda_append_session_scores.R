


# Test
# t <- compute_session_scores_and_end_session_api(test_id = 2L,
#                                                 session_id = 584L,
#                                                 user_id = 2L)


# compute_session_scores_and_end_session(test_id = 2L, session_id = 584L, user_id = 2L)

compute_session_scores_and_end_session_api <- function(test_id,
                                                       session_id,
                                                       user_id) {

  # Define the request body as a list
  request_body <- list(test_id = test_id,
                       session_id = session_id,
                       user_id = user_id)

  endpoint_wrapper(function_name = "compute-session-scores-and-end-session",
                   request_body = request_body)

}




# This is the function that is called when the endpoint
# is invoked
compute_session_scores_and_end_session <- function(test_id,
                                                   session_id,
                                                   user_id) {

  logging::loginfo("Inside compute_session_scores_and_end_session...")

  test_id <- as.integer(test_id)
  session_id <- as.integer(session_id)

  logging::loginfo("test_id = %s", test_id)

  logging::loginfo("session_id = %s", session_id)

  logging::loginfo("user_id = %s", user_id)

  complete_time <- Sys.time()


  response <- tryCatch({
    # Get trials

    trial_table <- compile_item_trials(db_con, test_id, session_id, user_id, join_item_banks_on = TRUE) # Here we give a session ID, because we only want to assess trials in this session

    scores_trial <- get_table(db_con, "scores_trial") %>%
      dplyr::filter(!is.na(score)) %>%
      dplyr::select(-scores_trial_id) %>%
      # Handle duplicates... but this shouldn't really happen
      dplyr::group_by(trial_id, measure) %>%
      dplyr::slice_max(score) %>%
      dplyr::ungroup() %>%
      unique() %>%
      tidyr::pivot_wider(names_from = "measure", values_from = "score")

    # Join on scores
    trial_table <- trial_table %>%
      dplyr::rename(ngrukkon_between_melody_and_parent_melody = ngrukkon) %>%
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

    ### Ability estimate
    #### First attempt
    ability_estimate_arrhythmic_first_attempt <-
      first_attempt_trial_table %>%
      dplyr::filter(!rhythmic) %>%
      dplyr::select(N, step.cont.loc.var, log_freq, i.entropy, opti3) %>%
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


  scores <- scores %>%
    scores_to_long_format() %>%
    dplyr::mutate(session_id = !! session_id)

    # Append scores
    scores_session_id <- db_append_scores_session(db_con, scores)

    # Update sessions table with time finished

    session_df <- get_table(db_con, 'sessions', collect = FALSE)

    logging::loginfo("Storing complete time as %s", complete_time)

    update <- dbplyr::copy_inline(db_con, data.frame(session_id = session_id, time_completed = complete_time))

    dplyr::rows_update(session_df, update, in_place = TRUE, by = "session_id", unmatched = "ignore")

    # Predict items for next test time
    predict_new_items(user_id)


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


# compute_session_scores_and_end_session(test_id = 2L, session_id = 594L, user_id = 2L)


