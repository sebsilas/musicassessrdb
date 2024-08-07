
# This is the function that is called when the endpoint
# is invoked
add_trial_and_compute_trial_scores <- function(Records) {

  logging::loginfo("Inside add_trial_and_compute_trial_scores function")

  processed_file <- utils::URLdecode(rjson::fromJSON(Records$body)[[1]][[1]][[9]][[4]][[1]][[1]])
  # URLdecode parses the name properly e.g., umlauts

  logging::loginfo("processed_file = %s", processed_file)

  # Return response

  response <- tryCatch({

    metadata <- get_metadata(processed_file)

    logging::loginfo("metadata...")
    logging::loginfo(metadata)

    stimuli <- itembankr::str_mel_to_vector(metadata$stimuli)
    stimuli_durations <- itembankr::str_mel_to_vector(metadata$stimuli_durations)
    test_id <- as.integer(metadata$test_id)
    item_id <- metadata$item_id
    user_id <- metadata$user_id
    instrument <- metadata$instrument
    trial_paradigm <- metadata$trial_paradigm
    trial_time_completed <- lubridate::as_datetime(metadata$trial_time_completed)
    score_to_use <- "opti3"
    audio_file <- stringr::str_replace(processed_file, ".csv", ".wav")

    logging::loginfo("trial_paradigm: %s", trial_paradigm)


    # Get pYIN (or rhythm onset) results

    res <- readFromS3(filename = processed_file, bucket = Sys.getenv("DESTINATION_BUCKET")) %>%
      dplyr::mutate(freq = as.numeric(freq),
                    dur = as.numeric(dur),
                    onset = as.numeric(onset),
                    note = round(hrep::freq_to_midi(freq)))

    logging::loginfo("res: %s", res)

    # Return quick feedback, if need be
    feedback <- metadata$feedback
    logging::loginfo("feedback: %s", feedback)


    if(feedback) {

      feedback_type <- metadata$feedback_type

      logging::loginfo("feedback_type: %s", feedback_type)

      if(feedback_type == "opti3") {
        result <- musicassessr::get_opti3(stimuli, stimuli_durations, length(stimuli), res)
      } else if(feedback_type == "produced_note") {
        result <- round(mean(hrep::freq_to_midi(res$freq), na.rm = TRUE))
      } else {
        stop("feedback_type not recognised")
      }

      logging::loginfo("result: %s", result)

      job_id <- digest::digest(audio_file, algo = "md5", serialize = FALSE)

      logging::loginfo("job_id to grab: %s", job_id)

      dynamodb <- paws::dynamodb()

      # Append selected items to DynamoDB
      update_job(dynamodb, job_id = job_id, message = rjson::toJSON(list(opti3 = result)), status = "FINISHED")

    }


    # Append trial info
    trial_id <- db_append_trials(
      db_con,
      audio_file = audio_file,
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
      new_items_id = if(length(metadata$new_items_id) == 0) NA else as.integer(metadata$new_items_id),
      trial_type = 'audio',
      trial_paradigm = trial_paradigm
    )

    logging::loginfo("Got trial_id: %s", trial_id)


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

      user_notes <- res$note

      logging::loginfo("Scoring melodic production...")

      logging::loginfo("res$freq %s", res$freq)
      logging::loginfo("user_notes %s", user_notes)
      logging::loginfo("res$dur %s", res$dur)
      logging::loginfo("res$onset %s", res$onset)
      logging::loginfo("stimuli %s", stimuli)
      logging::loginfo("stimuli_durations %s", stimuli_durations)

      logging::loginfo("is.numeric(res$freq) %s", is.numeric(res$freq))
      logging::loginfo("is.numeric(user_notes) %s", is.numeric(user_notes))
      logging::loginfo("is.numeric(res$dur) %s", is.numeric(res$dur))
      logging::loginfo("is.numeric(res$onset) %s", is.numeric(res$onset))
      logging::loginfo("is.numeric(stimuli) %s", is.numeric(stimuli))
      logging::loginfo("is.numeric(stimuli_durations) %s", is.numeric(stimuli_durations))

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

    logging::loginfo("...scored.")
    logging::loginfo("scores: %s", scores)

    # Grab trial scores, which are actually numeric scores and don't have NA-like values
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

    print(names(trial_scores))

    print(names(study_history_stats))

    trial_scores <- rbind(trial_scores, study_history_stats)



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
      trial_id = trial_id,
      scores_trial_ids = NA,
      melodic_production_ids = NA
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
#' @param trial_type
#' @param trial_paradigm
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
                             new_items_id = NULL,
                             trial_type = c("audio", "midi"),
                             trial_paradigm = c("call_and_response", "simultaneous_recall")) {

  trial_type <- match.arg(trial_type)
  trial_paradigm <- match.arg(trial_paradigm)

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
    is.integer(new_items_id) || is.na(new_items_id) || is.null(new_items_id),
    trial_type %in% c("audio", "midi"),
    trial_paradigm %in% c("call_and_response", "simultaneous_recall")
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
                             new_items_id = new_items_id,
                             trial_type = trial_type,
                             trial_paradigm = trial_paradigm)

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
  dplyr::select(session_id, rhythmic, display_modality,
         trial_time_completed, phase, test_id, attempt,
          item_id, stimuli, stimuli_durations, instrument,
          trial_time_started, onset, feedback, feedback_type, trial_paradigm)

}


get_rhythm_scores <- function(onset_res, stimuli_durations) {

  last_dur <- 0.5
  user_onsets <- if(is.scalar.na.or.null(onset_res)) NA else onset_res$onset
  user_durations <- if(is.scalar.na.or.null(onset_res)) NA else c(diff(user_onsets), last_dur)

  logging::loginfo("user_onsets: %s", paste0(round(user_onsets, 2), collapse = " | "))

  scores <- musicassessr::score_rhythm_production(stimuli_durations, user_durations)

}

