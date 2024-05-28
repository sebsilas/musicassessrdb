
# t <- add_trial_and_compute_trial_scores("Records")

# debug(itembankr::produce_extra_melodic_features)

# This is the function that is called when the endpoint
# is invoked
add_trial_and_compute_trial_scores <- function(Records) {

  logging::loginfo("Inside add_trial_and_compute_trial_scores function")

  processed_file <- rjson::fromJSON(Records$body)[[1]][[1]][[9]][[4]][[1]][[1]]
  logging::loginfo("processed_file = %s", processed_file)

  # Return response

  response <- tryCatch({

    metadata <- get_metadata(processed_file)

    logging::loginfo("metadata...")
    logging::loginfo(metadata)

    stimuli <- itembankr::str_mel_to_vector(metadata$stimuli)
    stimuli_durations <- itembankr::str_mel_to_vector(metadata$stimuli_durations)
    midi_vs_audio <- metadata$midi_vs_audio
    test_id <- as.integer(metadata$test_id)
    item_id <- metadata$item_id
    user_id <- metadata$user_id
    instrument <- metadata$instrument
    trial_time_completed <- lubridate::as_datetime(metadata$trial_time_completed)
    score_to_use <- "opti3"

    # Append trial info
    trial_id <- db_append_trials(
      db_con,
      audio_file = stringr::str_replace(processed_file, ".csv", ".wav"),
      trial_time_started = lubridate::as_datetime(metadata$trial_time_started),
      trial_time_completed = trial_time_completed,
      instrument = instrument,
      attempt = as.integer(metadata$attempt),
      item_id = item_id,
      display_modality = stringr::str_replace(metadata$display_modality, "-", "_"),
      phase = metadata$phase,
      rhythmic = as.logical(metadata$rhythmic),
      session_id = as.integer(metadata$session_id),
      test_id = test_id,
      stimulus_abs_melody = if(test_id == 3L) NULL else metadata$stimuli, # RTT (test_id == 3), doesn't have a melody
      stimulus_durations = metadata$stimuli_durations,
      review_items_id = if(length(metadata$review_items_id) == 0) NA else as.integer(metadata$review_items_id),
      new_items_id = if(length(metadata$new_items_id) == 0) NA else as.integer(metadata$review_items_id)
    )

    logging::loginfo("Got trial_id: %s", trial_id)


    # Get pYIN (or onset) res


    if(midi_vs_audio == "audio") {

      res <- readFromS3(filename = processed_file, bucket = Sys.getenv("DESTINATION_BUCKET"))

      logging::loginfo("res: %s", res)


      if(test_id == 3) { # i.e., the RTT

        logging::loginfo("Score rhythm production...")

        logging::loginfo("names(res)")
        logging::loginfo(names(res))

        logging::loginfo("res")
        logging::loginfo(res)

        scores <- get_rhythm_scores(res, stimuli_durations)

        melodic_production_ids <- NA

      } else {

        logging::loginfo("Score melodic production...")

        # TODO.. pyin_pitch_track
        #pyin_pitch_track <- "blah"

        res <- res %>%
          dplyr::mutate(freq = as.numeric(freq),
                        dur = as.numeric(dur),
                        onset = as.numeric(onset),
                        note = round(hrep::freq_to_midi(freq)))

        user_notes <- res$note

        logging::loginfo("Scoring melodic production...")
        logging::loginfo("res$freq %s", res$freq)
        logging::loginfo("user_notes %s", user_notes)
        logging::loginfo("res$dur %s", res$dur)
        logging::loginfo("res$onset %s", res$onset)
        logging::loginfo("stimuli %s", stimuli)
        logging::loginfo("stimuli_durations %s", stimuli_durations)

        # Store pYIN in DB
        scores <- musicassessr::score_melodic_production(user_melody_freq = res$freq,
                                                         user_melody_input = user_notes,
                                                         user_duration_input = res$dur,
                                                         user_onset_input = res$onset,
                                                         stimuli = stimuli,
                                                         stimuli_durations = stimuli_durations,
                                                         as_tb = FALSE)

        correct_boolean <- scores$correct_boolean
        correct_boolean_octaves_allowed <- scores$correct_boolean_octaves_allowed

        logging::loginfo("length(correct_boolean) %s", length(correct_boolean))
        logging::loginfo("correct_boolean: %s", correct_boolean)
        logging::loginfo("correct_boolean_octaves_allowed: %s", correct_boolean_octaves_allowed)

        logging::loginfo("Append melodic production...")

        # Add melodic production
        melodic_production_ids <- db_append_melodic_production(db_con, trial_id, res, correct_boolean, correct_boolean_octaves_allowed)

        logging::loginfo("...appended.")


      }


    } else if(midi_vs_audio == "midi") {
      logging::logerror("No MIDI method implemented here yet.")
    } else {
      melodic_production_ids <- NA
      logging::logerror("This is not a method.")
    }

    logging::loginfo("...scored.")
    logging::loginfo("scores: %s", scores)

    # Grab trial scores, which are actually numeric scores and don't have NA-like values
    trial_scores <- scores %>%
      scores_to_long_format() %>%
      dplyr::mutate(trial_id = trial_id)

    logging::loginfo("trial_scores: %s", trial_scores)

    logging::loginfo("Append to scores_trial")

    # Compute a few "change in score/review" vars

    last_score <- get_latest_score(db_con,
                                   user_id = user_id,
                                   test_id = test_id,
                                   inst = instrument,
                                   item_id = item_id,
                                   measure = score_to_use)

    logging::loginfo("last_score: %s", last_score)

    current_score <- trial_scores %>%
      dplyr::filter(measure == !! score_to_use) %>%
      dplyr::pull(score)

    logging::loginfo("current_score: %s", current_score)

    last_score_value <- last_score$score
    logging::loginfo("last_score_value: %s", last_score_value)

    learned_in_current_session <- if(is.na(last_score_value) && dplyr::near(current_score, 1)) 1L else if(last_score_value < 1 && dplyr::near(current_score, 1)) 1L else 0L

    logging::loginfo("learned_in_current_session: %s", learned_in_current_session)

    change_in_score_from_last_session <- current_score - last_score_value

    additional_scores <- tibble::tibble(
    learned_in_current_session = learned_in_current_session
    ) %>% dplyr::mutate(
      change_in_score_from_last_session = change_in_score_from_last_session,
      increase_since_last_session = dplyr::case_when(change_in_score_from_last_session > 0 ~ 1L, TRUE ~ 0L),
      time_since_last_item_studied = lubridate::as_datetime(trial_time_completed) - lubridate::as_datetime(last_score$trial_time_completed)
    ) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) %>%
      tidyr::pivot_longer(dplyr::everything(),
                          names_to = "measure",
                          values_to = "score") %>%
      dplyr::mutate(trial_id = trial_id)


    logging::loginfo("additional_scores: %s", additional_scores)

    print(names(trial_scores))

    print(names(additional_scores))

    trial_scores <- rbind(trial_scores, additional_scores)


    scores_trial_ids <- db_append_scores_trial(db_con,
                                               trial_id,
                                               measure = trial_scores$measure,
                                               score = trial_scores$score)
    logging::loginfo("...appended.")

    # logging::loginfo("scores_trial_id: %s", scores_trial_id)

    list(
      status = 200,
      message = "You have successfully added a trial and scores for the trial!",
      trial_id = trial_id,
      scores_trial_ids = scores_trial_ids,
      melodic_production_ids = melodic_production_ids
    )

  }, error = function(err) {

    logging::logerror(err)

    list(
      status = 400,
      message = "Something went wrong!",
      experiment_condition_id = NA
    )

  })

  return(response)

}


#' Store melodic production data in melodic_production table
#'
#' @param db_con
#' @param trial_id
#' @param pyin_res
#' @param correct_boolean
#' @param correct_boolean_octaves_allowed
#'
#' @return
#' @export
#'
#' @examples
db_append_melodic_production <- function(db_con, trial_id, pyin_res, correct_boolean, correct_boolean_octaves_allowed) {

  logging::loginfo("db_append_melodic_production")
  logging::loginfo("trial_id %s", trial_id)
  logging::loginfo("pyin_res, %s", pyin_res)
  logging::loginfo("correct_boolean %s", correct_boolean)
  logging::loginfo("correct_boolean_octaves_allowed %s", correct_boolean_octaves_allowed)


  melodic_production_df <- pyin_res %>%
    dplyr::mutate(trial_id = trial_id,
                  correct = correct_boolean,
                  correct_octaves_allowed = correct_boolean_octaves_allowed)

  melodic_production_id <- db_append_to_table(db_con, table = "melodic_production", data = melodic_production_df, primary_key_col = "melodic_production_id")

  return(melodic_production_id)

}



#' Append trial-level scores to the scores_trial table
#'
#' @param db_con
#' @param trial_id
#' @param measure
#' @param score
#'
#' @return
#' @export
#'
#' @examples
db_append_scores_trial <- function(db_con,
                                   trial_id,
                                   measure,
                                   score) {

  logging::loginfo("db_append_scores_trial")
  logging::loginfo("trial_id: %s", trial_id)
  logging::loginfo("measure: %s", measure)
  logging::loginfo("score: %s", score)

  stopifnot(
    length(trial_id) == 1
  )

  scores_df <- tibble::tibble(
    measure = measure,
    score = score,
    trial_id = trial_id
  )

  scores_trial_id <- db_append_to_table(db_con, table = "scores_trial", data = scores_df, primary_key_col = "scores_trial_id")

  return(scores_trial_id)

}




#' Append a trial to the trials table
#'
#' @param db_con
#' @param audio_file
#' @param trial_time_started
#' @param trial_time_completed
#' @param instrument
#' @param attempt
#' @param item_id
#' @param display_modality
#' @param phase
#' @param rhythmic
#' @param session_id
#' @param test_id
#' @param stimulus_abs_melody
#' @param stimulus_durations
#' @param review_items_id
#' @param new_item_id
#'
#' @return
#' @export
#'
#' @examples
db_append_trials <- function(db_con,
                             audio_file,
                             trial_time_started,
                             trial_time_completed,
                             instrument,
                             attempt,
                             item_id,
                             display_modality,
                             phase,
                             rhythmic,
                             session_id,
                             test_id,
                             stimulus_abs_melody,
                             stimulus_durations,
                             review_items_id = NULL,
                             new_items_id = NULL) {

  stopifnot(
    is.scalar.character(audio_file),
    lubridate::is.POSIXct(trial_time_started),
    lubridate::is.POSIXct(trial_time_completed),
    is.scalar.character(instrument),
    is.integer(attempt),
    is.scalar.character(item_id),
    is.scalar.character(display_modality),
    phase %in% c("learn", "test", "review", "example"),
    is.scalar.logical(rhythmic),
    is.integer(session_id),
    is.integer(test_id),
    is.null.or(stimulus_abs_melody, is.scalar.character), # RTT doesn't have a melody
    is.scalar.character(stimulus_durations),
    is.integer(review_items_id) || is.na(review_items_id) || is.null(review_items_id),
    is.integer(new_items_id) || is.na(new_items_id) || is.null(new_items_id)
  )


  trial_df <- tibble::tibble(audio_file = audio_file,
                             trial_time_started = trial_time_started,
                             trial_time_completed = trial_time_completed,
                             instrument = instrument,
                             attempt = attempt,
                             item_id = item_id,
                             display_modality = display_modality,
                             phase = phase,
                             rhythmic = rhythmic,
                             session_id = session_id,
                             test_id = test_id,
                             stimulus_abs_melody = stimulus_abs_melody,
                             stimulus_durations = stimulus_durations,
                             review_items_id = review_items_id,
                             new_items_id = new_items_id)

  trial_id <- db_append_to_table(db_con, table = "trials", data = trial_df, primary_key_col = "trial_id")

  return(trial_id)
}




readFromS3 <- function(filename, bucket) {
  # https://medium.com/@som028/how-to-read-and-write-data-from-and-to-s3-bucket-using-r-3fed7e686844

  logging::loginfo("readFromS3 function")

  return(aws.s3::s3read_using(FUN=readr::read_csv,
                              col_names = c("onset", "dur", "freq"),
                              bucket = bucket,
                              object = filename))
}




get_metadata <- function(file, bucket = Sys.getenv("DESTINATION_BUCKET")) {

  metadata <- aws.s3::head_object(object = file,
                                  bucket = Sys.getenv("DESTINATION_BUCKET")) %>%
  attributes() %>%
  tibble::as_tibble() %>%
  dplyr::rename_with(~ .x %>% stringr::str_remove("x-amz-meta-") %>% stringr::str_remove("x-amz-") %>% stringr::str_replace_all("-", "_")
                    ) %>%
  dplyr::select(midi_vs_audio, session_id, rhythmic, display_modality,
         trial_time_completed, phase, test_id, attempt,
          item_id, stimuli, stimuli_durations, instrument,
          trial_time_started, onset)

}


get_rhythm_scores <- function(onset_res, stimuli_durations) {

  last_dur <- 0.5
  user_onsets <- if(is.scalar.na.or.null(onset_res)) NA else onset_res$onset
  user_durations <- if(is.scalar.na.or.null(onset_res)) NA else c(diff(user_onsets), last_dur)

  logging::loginfo("user_onsets: %s", paste0(round(user_onsets, 2), collapse = " | "))

  scores <- musicassessr::score_rhythm_production(stimuli_durations, user_durations)

}
# t <- get_metadata("f04af2f95a4e7749610e3482b60c1d9de3da31e23d5ae06b159aaf15b88d915d.arrhythmic_melody_1_attempt_1.12-12-2023--15-25--35.csv")

# t <- add_trial_and_compute_trial_scores("f04af2f95a4e7749610e3482b60c1d9de3da31e23d5ae06b159aaf15b88d915d.arrhythmic_melody_1_attempt_1.12-12-2023--15-25--35.csv")



# onset_res <- readFromS3(filename = "qwdqwe23421.rhythm_call_and_response.record_audio_page.9-3-2024--11-49--29.csv",
#                         bucket = Sys.getenv("DESTINATION_BUCKET"))


# pyin::test_pyin() %>% dplyr::select(onset:note) %>% db_append_melodic_production()

