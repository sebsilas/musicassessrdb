
# This is the function that is called when the endpoint
# is invoked
add_trial_and_compute_trial_scores <- function(Records) {

  logging::loginfo("Inside add_trial_and_compute_trial_scores function")

  logging::loginfo("class(db_con): %s", class(db_con))
  logging::loginfo("DBI::dbIsValid(db_con): %s", DBI::dbIsValid(db_con))

  if(!DBI::dbIsValid(db_con)) {
    logging::loginfo("Try create a new db_con...")
    db_con <- musicassessr_con()
    logging::loginfo("DBI::dbIsValid(db_con): %s", DBI::dbIsValid(db_con))
  }

  logging::loginfo('jsonlite::fromJSON(Records$body) %s', jsonlite::fromJSON(Records$body))
  logging::loginfo('jsonlite::fromJSON(Records$body)[[1]] %s', jsonlite::fromJSON(Records$body)[[1]])
  logging::loginfo('jsonlite::fromJSON(Records$body)[[1]][[9]] %s', jsonlite::fromJSON(Records$body)[[1]][[9]])
  logging::loginfo('jsonlite::fromJSON(Records$body)[[1]][[9]][[4]] %s', jsonlite::fromJSON(Records$body)[[1]][[9]][[4]])
  logging::loginfo('jsonlite::fromJSON(Records$body)[[1]][[9]][[4]][[1]] %s', jsonlite::fromJSON(Records$body)[[1]][[9]][[4]][[1]])


  processed_file <- utils::URLdecode(jsonlite::fromJSON(Records$body)[[1]][[9]][[4]][[1]])
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

    logging::loginfo("user_id: %s", user_id)

    instrument <- metadata$instrument
    trial_paradigm <- metadata$trial_paradigm
    trial_time_completed <- lubridate::as_datetime(metadata$trial_time_completed)
    score_to_use <- "opti3"
    audio_file <- stringr::str_replace(processed_file, ".csv", ".wav")
    attempt <- as.integer(metadata$attempt)
    additional <- metadata$additional
    session_id <- as.integer(metadata$session_id)
    melody_block_paradigm <- if(length(metadata$melody_block_paradigm) == 0) "" else metadata$melody_block_paradigm
    page_label <- if(length(metadata$page_label) == 0) "" else metadata$page_label
    module <- if(length(metadata$module) == 0) "" else metadata$module
    stim_length <- length(stimuli)

    logging::loginfo("page_label: %s", page_label)
    logging::loginfo("module: %s", module)
    logging::loginfo("session_id %s", session_id)
    logging::loginfo("additional %s", additional)
    logging::loginfo("trial_paradigm: %s", trial_paradigm)
    logging::loginfo("melody_block_paradigm: %s", melody_block_paradigm)
    logging::loginfo("stim_length: %s", stim_length)


    # Get pYIN (or rhythm onset) results
    if(metadata$pyin_type == "smoothedpitchtrack") {
      res <- readFromS3(filename = processed_file,
                        col_names = c("onset", "freq"),
                        bucket = Sys.getenv("DESTINATION_BUCKET")) %>%
        dplyr::mutate(dur = c(diff(onset), 0.01)) # This doesn't matter, we don't use it
    } else {
      res <- readFromS3(filename = processed_file,
                        bucket = Sys.getenv("DESTINATION_BUCKET"))
    }

    onset <- metadata$onset %>%
      as.logical()

    logging::loginfo("onset: %s", onset)

    if(length(onset) == 0) {
      onset <- FALSE
    }

    logging::loginfo("onset again: %s", onset)

    if(onset) {

      res <- res %>%
        dplyr::mutate(onset = as.numeric(onset),
                      dur = c(diff(onset), 0.5),
                      freq = NA,
                      note = NA) %>%
        dplyr::relocate(freq, dur, onset, note)

    } else {
      res <- res %>%
        dplyr::mutate(freq = as.numeric(freq),
                      dur = as.numeric(dur),
                      onset = as.numeric(onset),
                      note = round(hrep::freq_to_midi(freq))) %>%
        itembankr::produce_extra_melodic_features()
    }


    logging::loginfo("res: %s", res)

    # Return quick feedback, if need be
    feedback <- metadata$feedback
    feedback_type <- metadata$feedback_type

    logging::loginfo("feedback: %s", feedback)
    logging::loginfo("feedback_type: %s", feedback_type)
    logging::loginfo("stimuli: %s", stimuli)
    logging::loginfo("stimuli_durations: %s", stimuli_durations)
    logging::loginfo("stim_length: %s", stim_length)
    logging::loginfo("res: %s", res)
    logging::loginfo("audio_file: %s", audio_file)


    logging::loginfo("class(db_con): %s", class(db_con))
    logging::loginfo("DBI::dbIsValid(db_con): %s", DBI::dbIsValid(db_con))

    # Handle immediate feedback
    handle_quick_feedback(feedback, feedback_type, stimuli, stimuli_durations, stim_length, res, audio_file, attempt)

    logging::loginfo("handle_quick_feedback complete")
    logging::loginfo("attempt: %s", attempt)

    logging::loginfo("class(db_con): %s", class(db_con))
    logging::loginfo("DBI::dbIsValid(db_con): %s", DBI::dbIsValid(db_con))

    if(grepl("CUSTOM_ITEM", item_id) && ! grepl("NO_SCORING", item_id)) {

      logging::loginfo("Custom item...")

      # If item_id is custom, see if the N-gram is already contained in the original item bank
      # Otherwise, create a new item for it

      logging::loginfo("additional: %s", additional)

      additional_obj <- additional %>%
        jsonlite::fromJSON()

      logging::loginfo("additional_obj: %s", additional_obj)

      original_item_id <- additional_obj$original_item_id

      logging::loginfo("original_item_id: %s", original_item_id)

      if(original_item_id == "NONE") {
        original_item_bank <- "NONE"
      } else {
        original_item_bank <- extract_item_bank_name_from_item_id(db_con, original_item_id)
      }

      logging::loginfo("original_item_bank: %s", original_item_bank)

      add_custom_melody_to_db(db_con,
                              stimuli,
                              stimuli_durations,
                              original_item_bank,
                              original_item_id)
    }

    logging::loginfo("class(db_con): %s", class(db_con))
    logging::loginfo("DBI::dbIsValid(db_con): %s", DBI::dbIsValid(db_con))

    # Append trial info
    trial_id <- db_append_trials(
      db_con,
      audio_file = audio_file,
      trial_time_started = lubridate::as_datetime(metadata$trial_time_started),
      trial_time_completed = trial_time_completed,
      instrument = instrument,
      attempt = attempt,
      item_id = item_id,
      display_modality = stringr::str_replace(metadata$display_modality, "-", "_"),
      phase = metadata$phase,
      rhythmic = as.logical(metadata$rhythmic),
      session_id = session_id,
      test_id = test_id,
      # Note, leave them as metadata$stimuli and metadata$stimuli rather than the assigned vars (stimuli, stimuli_durations)
      # Because we want them to stay strings here
      stimulus_abs_melody = if(test_id == 3L) NULL else metadata$stimuli, # RTT (test_id == 3), doesn't have a melody
      stimulus_durations = metadata$stimuli_durations,
      review_items_id = if(length(metadata$review_items_id) == 0) NA else as.integer(metadata$review_items_id),
      new_items_id = if(length(metadata$new_items_id) == 0) NA else as.integer(metadata$new_items_id),
      trial_type = 'audio',
      trial_paradigm = trial_paradigm,
      additional = if(length(additional) == 0) NA else if(!is.scalar.character(additional)) jsonlite::toJSON(additional, auto_unbox = TRUE) else additional,
      melody_block_paradigm = melody_block_paradigm,
      page_label = page_label,
      module = if(length(module) == 0) "NA" else module
    )

    logging::loginfo("Got trial_id: %s", trial_id)

    if(item_id == "CUSTOM_ITEM_NO_SCORING") {

      logging::loginfo("Append melodic production...")

      correct_boolean <- rep(NA, nrow(res))
      correct_boolean_octaves_allowed <- rep(NA, nrow(res))
      # Add melodic production
      melodic_production_ids <- db_append_melodic_production(db_con, trial_id, res, correct_boolean, correct_boolean_octaves_allowed)

      logging::loginfo("...appended.")

    } else if(trial_paradigm == "setup_sing_range_note") {

      # Return early, we don't need to do any scoring for this

      ret <- list(
        status = 200,
        message = "You have successfully added a trial and scores for the trial!",
        trial_id = trial_id,
        scores_trial_ids = NA,
        melodic_production_ids = NA
      )

      return(ret)

    }

    if(test_id == 3) { # i.e., the RTT

      logging::loginfo("Score rhythm production...")

      logging::loginfo("names(res)")
      logging::loginfo(names(res))

      logging::loginfo("res")
      logging::loginfo(res)

      scores <- get_rhythm_scores(res, stimuli_durations)

      melodic_production_ids <- NA

    } else if(melody_block_paradigm == "long_note") {

      logging::loginfo("Score long note production...")
      logging::loginfo("stimuli: %s", stimuli)
      logging::loginfo("res$freq: %s", res$freq)

      scores <- musicassessr::long_note_pitch_metrics(stimuli, res$freq)

      logging::loginfo("scores: %s", scores)

      logging::loginfo("res$note: %s", res$note)

      sung_midis <- res$note
      correct_boolean <- sung_midis == stimuli
      stimuli_diff_octaves <- stimuli + (-4:4 * 12)
      correct_boolean_octaves_allowed <- sung_midis %in% stimuli_diff_octaves

      melodic_production_ids <- db_append_melodic_production(db_con, trial_id, res, correct_boolean, correct_boolean_octaves_allowed)

    } else {

      logging::loginfo("Score melodic production...")

      # TODO.. pyin_pitch_track
      #pyin_pitch_track <- "blah"

      user_notes <- res$note

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

    trial_scores <- rbind(trial_scores, study_history_stats)



    scores_trial_ids <- db_append_scores_trial(db_con,
                                               trial_id,
                                               measure = trial_scores$measure,
                                               score = trial_scores$score)
    logging::loginfo("...appended.")

    if(DBI::dbIsValid(db_con)) {
      musicassessrdb::db_disconnect(db_con)
    }

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
    dplyr::select(onset, dur, freq, note) %>%
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
#' @param additional
#' @param melody_block_paradigm
#' @param page_label
#' @param module
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
                             trial_paradigm = c("call_and_response",
                                                "simultaneous_recall",
                                                "long_note_call_and_response",
                                                "long_note_simultaneous_recall",
                                                "setup_sing_range_note",
                                                "free_recall"),
                             additional = NA,
                             melody_block_paradigm,
                             page_label,
                             module = "") {

  trial_type <- match.arg(trial_type)
  trial_paradigm <- match.arg(trial_paradigm)

  stopifnot(
    is.scalar.character(audio_file),
    lubridate::is.POSIXct(trial_time_started),
    lubridate::is.POSIXct(trial_time_completed),
    is.scalar.character(instrument),
    is.integer(attempt),
    is.scalar.character(item_id) || is.na(item_id),
    is.scalar.character(display_modality),
    phase %in% c("learn", "test", "review", "example", "setup"),
    is.scalar.logical(rhythmic),
    is.integer(session_id),
    is.integer(test_id),
    is.null.or(stimulus_abs_melody, is.scalar.character), # RTT doesn't have a melody
    is.scalar.character(stimulus_durations),
    is.integer(review_items_id) || is.na(review_items_id) || is.null(review_items_id),
    is.integer(new_items_id) || is.na(new_items_id) || is.null(new_items_id),
    trial_type %in% c("audio", "midi"),
    trial_paradigm %in% c("call_and_response", "simultaneous_recall",
                          "long_note_call_and_response", "long_note_simultaneous_recall",
                          "setup_sing_range_note", "free_recall"),
    is.scalar.na(additional) || is.scalar.character(additional),
    is.scalar.character(melody_block_paradigm),
    is.scalar.character(page_label),
    is.null.or(module, is.scalar.character)
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
                             trial_paradigm = trial_paradigm,
                             additional = additional,
                             melody_block_paradigm = melody_block_paradigm,
                             page_label = page_label,
                             module = module)

  trial_id <- db_append_to_table(db_con, table = "trials", data = trial_df, primary_key_col = "trial_id")

  return(trial_id)
}



readFromS3 <- function(filename,
                       bucket,
                       col_names = c("onset", "dur", "freq")) {
  # https://medium.com/@som028/how-to-read-and-write-data-from-and-to-s3-bucket-using-r-3fed7e686844

  logging::loginfo("readFromS3 function")

  return(aws.s3::s3read_using(FUN=readr::read_csv,
                              col_names = col_names,
                              bucket = bucket,
                              object = filename))
}




get_metadata <- function(file,
                         bucket = Sys.getenv("DESTINATION_BUCKET")) {

  metadata <- aws.s3::head_object(object = file,
                                  bucket = Sys.getenv("DESTINATION_BUCKET")) %>%
  attributes() %>%
  tibble::as_tibble() %>%
  dplyr::rename_with(~ .x %>% stringr::str_remove("x-amz-meta-") %>% stringr::str_remove("x-amz-") %>% stringr::str_replace_all("-", "_")
                    ) %>%
  dplyr::select(session_id, rhythmic, display_modality,
         trial_time_completed, phase, test_id, attempt,
          item_id, stimuli, stimuli_durations, instrument,
          trial_time_started, onset, feedback, feedback_type, trial_paradigm,
         melody_block_paradigm, additional, page_label, module, user_id, pyin_type)

}


get_rhythm_scores <- function(onset_res, stimuli_durations) {

  last_dur <- 0.5
  user_onsets <- if(is.scalar.na.or.null(onset_res)) NA else onset_res$onset
  user_durations <- if(is.scalar.na.or.null(onset_res)) NA else c(diff(user_onsets), last_dur)

  logging::loginfo("user_onsets: %s", paste0(round(user_onsets, 2), collapse = " | "))

  scores <- musicassessr::score_rhythm_production(stimuli_durations, user_durations)

}


# Some experimental stuff:

edit_dist <- function(s, t) {
  utils::adist(s, t)[1,1]
}

edit_sim <- function(s, t) {
  offset <- min(c(s, t))
  s <- s - offset + 128
  t <- t - offset  + 128
  s <- intToUtf8(s)
  t <- intToUtf8(t)
  1 - edit_dist(s, t)/max(nchar(s), nchar(t))
}



weight_rhythmic <- function(pitches, durs) {
  purrr::map2(pitches, durs, function(pitch, dur) {
    rep(pitch, dur)
  }) %>% unlist() %>% as.integer()
}

rhythmic_weighting_sim <- function(pitches, durs, pitches2, durs2, sim_algo = edit_sim) {

  stopifnot(length(pitches) == length(durs),
            length(pitches2) == length(durs2))

  # Assuming durations are in seconds
  durs <- round(durs * 1000)
  durs2 <- round(durs2 * 1000)

  duration_weighted_pitches <- weight_rhythmic(pitches, durs)
  duration_weighted_pitches2 <- weight_rhythmic(pitches2, durs2)

  # Apply some similarity function, e.g., edit sim here

  octs <- 12*-3:3
  purrr::map_dfr(octs, function(oct) {
    tibble::tibble(oct = oct,
                   score = sim_algo(duration_weighted_pitches, duration_weighted_pitches2 + oct))
  }) %>%
    dplyr::slice_max(score) %>%
    dplyr::pull(score)


}


# rhythmic_weighting_sim(60:65+12, rep(1,6), 60:65, rep(1, 6))


benovelent_score <- function(score, attempt) {

  if(is.na(score) || length(score) == 0L) {
    return(0)
  }

  # Give a boost for attempts
  score <- score * (10 + attempt) / 10

  # Apply a quadratic transformation
  benevolentScore <- sqrt(score)

  # Scale to the range 1 to 10
  scaledScore <- 1 + benevolentScore * 9

  # Round up
  scaledScore <- ceiling(scaledScore)

  if(scaledScore > 10) {
    scaledScore <- 10
  }

  return(scaledScore)
}


handle_quick_feedback <- function(feedback, feedback_type, stimuli, stimuli_durations, stim_length, res, audio_file, attempt) {

  if(feedback) {

    logging::loginfo("feedback_type: %s", feedback_type)

    if(feedback_type == "opti3") {

      opti3_res <- musicassessr::get_opti3(stimuli, stimuli_durations, stim_length, res)
      logging::loginfo("opti3_res %s", opti3_res)
      opti3 <- opti3_res$opti3

      transposition <- opti3_res$transposition
      logging::loginfo("transposition %s", transposition)
      notes_with_best_transposition <- res$note + transposition

      benovelent_opti3 <- benovelent_score(opti3, attempt)

      result <- list(benovelent_opti3 = benovelent_opti3,
                     opti3 = opti3_res$opti3,
                     ngrukkon = opti3_res$ngrukkon,
                     rhythfuzz = opti3_res$rhythfuzz,
                     harmcore = opti3_res$harmcore,
                     rhythmic_weighted_edit_sim =  rhythmic_weighting_sim(stimuli,
                                                                          stimuli_durations,
                                                                          notes_with_best_transposition,
                                                                          res$dur),
                     transcribed_notes = paste0(res$note, collapse = ","),
                     notes_with_best_transposition = paste0(notes_with_best_transposition, collapse = ","),
                     stimulus = paste0(stimuli, collapse = ",")
      )
    } else if(feedback_type == "produced_note") {

      result <- round(mean(hrep::freq_to_midi(res$freq), na.rm = TRUE))

    } else if(feedback_type == "onset_tempo") {

      user_onsets <- if(is.scalar.na.or.null(res)) NA else res$onset
      user_durations <- if(is.scalar.na.or.null(user_onsets)) NA else diff(user_onsets)
      user_durations <- user_durations[!is.na(user_durations)]

      mean_dur <- mean(user_durations, na.rm = TRUE)
      result <- round(60/mean_dur) # This is a proxy but not particularly sophisticated..

    } else {
      stop("feedback_type not recognised")
    }

    logging::loginfo("result: %s", result)

    job_id <- digest::digest(audio_file, algo = "md5", serialize = FALSE)

    logging::loginfo("job_id to grab: %s", job_id)

    dynamodb <- paws::dynamodb()

    # Append selected items to DynamoDB
    update_job(dynamodb, job_id = job_id, message = jsonlite::toJSON(list(feedback = result)), status = "FINISHED")

  }
}


# Full mel

# stimuli <- c(68, 66, 65, 66, 67)
# stimuli_durations <- rep(0.5, 5)
# stim_length <- length(stimuli)
#
# res <- tibble::tibble(onset = c(0, cumsum(rep(0.5, 4)) ),
#                       dur = rep(0.5, 5),
#                       freq = hrep::midi_to_freq(c(68, 66, 65, 66, 67)),
#                       note = c(68, 66, 65, 66, 67)) %>%
#   itembankr::produce_extra_melodic_features()
#
#
# opti3 <- musicassessr::get_opti3(stimuli, stimuli_durations, stim_length, res)
# opti3 %>%
#   dplyr::pull(opti3) %>%
#   benovelent_score(attempt = 1)
#
#
# # Miss two notes
#
# res <- tibble::tibble(onset = c(0, cumsum(rep(0.5, 2)) ),
#                       dur = rep(0.5, 3),
#                       freq = hrep::midi_to_freq(c(68, 66, 65)),
#                       note = c(68, 66, 65)) %>%
#   itembankr::produce_extra_melodic_features()
#
#
# opti3 <- musicassessr::get_opti3(stimuli, stimuli_durations, stim_length, res)
# opti3
# opti3 %>%
#   dplyr::pull(opti3) %>%
#   benovelent_score(attempt = 1)
#
#
# # Miss three notes
#
# res <- tibble::tibble(onset = c(0, cumsum(rep(0.5, 1)) ),
#                       dur = rep(0.5, 2),
#                       freq = hrep::midi_to_freq(c(68, 66)),
#                       note = c(68, 66)) %>%
#   itembankr::produce_extra_melodic_features()
#
#
# opti3 <- musicassessr::get_opti3(stimuli, stimuli_durations, stim_length, res)
# opti3
# opti3 %>%
#   dplyr::pull(opti3) %>%
#   benovelent_score(attempt = 1)
#

