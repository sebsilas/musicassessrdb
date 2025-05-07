

recompute_trial_scores_posthoc <- function(db_con = NULL,
                                           trial_id,
                                           db_name = "melody_dev",
                                           score_to_use = "opti3") {

  if(is.null(db_con)) {

    db_con <- musicassessr_con(db_name = db_name, pool = FALSE)
    connected_locally <- TRUE

  } else {
    connected_locally <- FALSE
  }

  # Get stimulus data
  trial <- dplyr::tbl(db_con, "trials") %>%
    dplyr::filter(trial_id == !! trial_id) %>%
    dplyr::left_join(dplyr::tbl(db_con, "sessions"), by = "session_id") %>%
    dplyr::collect()

  user_id <- trial$user_id
  test_id <- trial$test_id
  instrument <- trial$instrument
  item_id <- trial$item_id
  trial_time_completed <- trial$trial_time_completed

  stimuli <- trial %>% dplyr::pull(stimulus_abs_melody) %>% itembankr::str_mel_to_vector()
  stimuli_durations <- trial %>% dplyr::pull(stimulus_durations) %>% itembankr::str_mel_to_vector()

  # Get mel prod data

  res <- dplyr::tbl(db_con, "melodic_production") %>%
    dplyr::filter(trial_id == !! trial_id) %>%
    dplyr::collect()

  user_notes <- res$note

  # Store pYIN in DB
  scores <- musicassessr::score_melodic_production(user_melody_freq = res$freq,
                                                   user_melody_input = user_notes,
                                                   user_duration_input = res$dur,
                                                   user_onset_input = res$onset,
                                                   stimuli = stimuli,
                                                   stimuli_durations = stimuli_durations,
                                                   as_tb = FALSE)

  trial_scores <- scores %>%
    scores_to_long_format() %>%
    dplyr::mutate(trial_id = trial_id)

  logging::loginfo("trial_scores: %s", trial_scores)

  logging::loginfo("Append to scores_trial")

  study_history_stats <- get_study_history_stats(db_con,
                                                 user_id = user_id,
                                                 test_id = test_id,
                                                 inst = instrument,
                                                 item_id = item_id,
                                                 measure = score_to_use,
                                                 current_trial_scores = trial_scores,
                                                 current_trial_time_completed = trial_time_completed)

  study_history_stats <- study_history_stats %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) %>%
    tidyr::pivot_longer(dplyr::everything(),
                        names_to = "measure",
                        values_to = "score") %>%
    dplyr::mutate(trial_id = trial_id)


  logging::loginfo("study_history_stats: %s", study_history_stats)

  trial_scores <- rbind(trial_scores, study_history_stats)


  trial_scores_update <- dbplyr::copy_inline(db_con, trial_scores)

  dplyr::rows_update(tbl(db_con, "scores_trial"),
                     trial_scores_update,
                     in_place = TRUE,
                     by = c("trial_id", "measure"),
                     unmatched = "ignore")



  logging::loginfo("...appended.")

  if(connected_locally) {
    db_disconnect(db_con)
  }


  return(trial_scores)

}

# t <- recompute_trial_scores_posthoc(trial_id = 2669)


recompute_session_scores_posthoc <- function(db_con = NULL,
                                             session_id,
                                             db_name = "melody_dev") {

  if(is.null(db_con)) {

    db_con <- musicassessr_con(db_name = db_name, pool = FALSE)
    connected_locally <- TRUE

  } else {
    connected_locally <- FALSE
  }

  test_ids <- get_test_ids_in_session(db_name = "melody_prod", session_id = session_id)

  user_id <- dplyr::tbl(db_con, "sessions") %>%
    dplyr::filter(session_id == !! session_id) %>%
    dplyr::pull(user_id)

  logging::loginfo("user_id: %s", user_id)

  scores_session_id <- purrr::map(test_ids, function(test_id) {
    session_scores_helper(db_con, test_id, session_id, user_id, update = TRUE)
  }) %>% unlist()


}

# t <- recompute_session_scores_posthoc(db_con, session_id = session_ids[5])

get_test_ids_in_session <- function(db_con = NULL,
                                    db_name = "melody_dev",
                                    session_id) {

  if(is.null(db_con)) {

    db_con <- musicassessr_con(db_name = db_name, pool = FALSE)
    connected_locally <- TRUE

  } else {
    connected_locally <- FALSE
  }

  dplyr::tbl(db_con, "trials") %>%
    dplyr::left_join(dplyr::tbl(db_con, "sessions"), by = "session_id") %>%
    dplyr::filter(session_id == !! session_id) %>%
    dplyr::pull(test_id) %>%
    unique()
}

# t <- get_test_ids_in_session(db_name = "melody_prod", session_id = 3375)


