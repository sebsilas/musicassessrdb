

#' Add MIDI trial via API
#'
#' @param stimuli
#' @param stimuli_durations
#' @param test_id
#' @param item_id
#' @param user_id
#' @param instrument
#' @param trial_time_started
#' @param trial_time_completed
#' @param score_to_use
#' @param display_modality
#' @param phase
#' @param rhythmic
#' @param session_id
#' @param test_id
#' @param review_items_id
#' @param new_items_id
#' @param dur
#' @param onset
#' @param note
#' @param attempt
#' @param additional
#' @param melody_block_paradigm
#' @param page_label
#' @param module
#'
#' @return
#' @export
#'
#' @examples
midi_add_trial_and_compute_trial_scores_api <- function(stimuli,
                                                       stimuli_durations,
                                                       test_id,
                                                       item_id,
                                                       user_id,
                                                       instrument,
                                                       trial_time_started,
                                                       trial_time_completed,
                                                       score_to_use = "opti3",
                                                       display_modality = "auditory",
                                                       phase,
                                                       rhythmic,
                                                       session_id,
                                                       review_items_id = NA,
                                                       new_items_id = NA,
                                                       dur,
                                                       onset,
                                                       note,
                                                       attempt,
                                                       additional,
                                                       melody_block_paradigm = NA,
                                                       page_label = "",
                                                       module = "NA") {

  # Define the request body as a list
  request_body <- list(stimuli = stimuli,
                       stimuli_durations = stimuli_durations,
                       test_id = test_id,
                       item_id = item_id,
                       user_id = user_id,
                       instrument = instrument,
                       trial_time_started = trial_time_started,
                       trial_time_completed = trial_time_completed,
                       score_to_use = score_to_use,
                       display_modality = display_modality,
                       phase = phase,
                       rhythmic = rhythmic,
                       session_id = session_id,
                       review_items_id = review_items_id,
                       new_items_id = new_items_id,
                       dur = dur,
                       onset = onset,
                       note = note,
                       attempt = attempt,
                       additional = if(is.scalar.character(additional)) additional else jsonlite::toJSON(additional),
                       melody_block_paradigm = melody_block_paradigm,
                       page_label = page_label,
                       module = module)

  res <- endpoint_wrapper(function_name = "midi-add-trial-and-compute-trial-scores",
                          request_body = request_body)

  logging::loginfo(res$message)


  return(res)

}

# This is the function that is called when the endpoint
# is invoked
midi_add_trial_and_compute_trial_scores <- function(stimuli,
                                                    stimuli_durations,
                                                    test_id,
                                                    item_id,
                                                    user_id,
                                                    instrument,
                                                    trial_time_started,
                                                    trial_time_completed,
                                                    score_to_use = "opti3",
                                                    display_modality = "auditory",
                                                    phase,
                                                    rhythmic,
                                                    session_id,
                                                    review_items_id = NA,
                                                    new_items_id = NA,
                                                    dur,
                                                    onset,
                                                    note,
                                                    attempt,
                                                    additional,
                                                    melody_block_paradigm,
                                                    page_label,
                                                    module = "NA") {


  logging::loginfo("Inside midi_add_trial_and_compute_trial_scores function")

  logging::loginfo("stimuli %s", stimuli)
  logging::loginfo("stimuli_durations: %s", stimuli_durations)
  logging::loginfo("test_id: %s", test_id)
  logging::loginfo("item_id", item_id)
  logging::loginfo("user_id: %s", user_id)
  logging::loginfo("instrument: %s", instrument)
  logging::loginfo("trial_time_started: %s", trial_time_started)
  logging::loginfo("trial_time_completed: %s", trial_time_completed)
  logging::loginfo("score_to_use: %s", score_to_use)
  logging::loginfo("display_modality: %s", display_modality)
  logging::loginfo("phase: %s", phase)
  logging::loginfo("rhythmic: %s", rhythmic)
  logging::loginfo("session_id: %s", session_id)
  logging::loginfo("review_items_id: %s", review_items_id)
  logging::loginfo("new_items_id: %s", new_items_id)
  logging::loginfo("dur: %s", dur)
  logging::loginfo("onset: %s", onset)
  logging::loginfo("note: %s", note)
  logging::loginfo("attempt: %s", attempt)
  logging::loginfo("additional: %s", additional)
  logging::loginfo("melody_block_paradigm: %s", melody_block_paradigm)
  logging::loginfo("page_label: %s", page_label)
  logging::loginfo("module: %s", module)

  # Return response

  response <- tryCatch({


    test_id <- as.integer(test_id)
    trial_time_completed <- lubridate::as_datetime(trial_time_completed)
    attempt <- as.integer(attempt)

    if(length(item_id) == 0) {
      item_id <- NA
    }

    # Return quick feedback, if need be
    # feedback <- feedback
    # if(feedback != 'none') {
    #
    #   if(feedback == "opti3") {
    #     result <- get_opti3(stimuli, stimuli_durations, length(stimuli), user_input_as_pyin)
    #   } else if(feedback == "produced_note") {
    #     result <- round(mean(hrep::freq_to_midi(user_input_as_pyin$freq), na.rm = TRUE))
    #   }
    #
    # }

    # Append trial info
    trial_id <- db_append_trials(
      db_con,
      audio_file = "MIDI",
      trial_time_started = lubridate::as_datetime(trial_time_started),
      trial_time_completed = trial_time_completed,
      instrument = instrument,
      attempt = as.integer(attempt),
      item_id = item_id,
      display_modality = display_modality,
      phase = phase,
      rhythmic = as.logical(rhythmic),
      session_id = as.integer(session_id),
      test_id = test_id,
      stimulus_abs_melody = paste0(stimuli, collapse = ","),
      stimulus_durations = paste0(stimuli, collapse = ","),
      review_items_id = if(length(review_items_id) == 0) NA else as.integer(review_items_id),
      new_items_id = if(length(new_items_id) == 0) NA else as.integer(new_items_id),
      trial_type = 'midi',
      additional = additional,
      melody_block_paradigm = melody_block_paradigm,
      page_label = page_label,
      module = module
      )

    logging::loginfo("Got trial_id: %s", trial_id)

    # Get pYIN (or rhythm onset) results

    logging::loginfo("Score melodic production...")


    res <- tibble::tibble(note = as.numeric(note),
                          dur = as.numeric(dur),
                          onset = as.numeric(onset),
                          freq = round(hrep::midi_to_freq(note)))

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

    if(nrow(res) < 1L) {

      logging::loginfo("Invalid input, no scores possible")

      scores_trial_ids <- NA
      melodic_production_ids <- NA


    } else {

      # Store MIDI results in in DB
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


      logging::loginfo("...scored.")
      logging::loginfo("scores: %s", scores)

      # Grab trial scores, which are actually numeric scores and don't have NA-like values
      trial_scores <- scores %>%
        scores_to_long_format() %>%
        dplyr::mutate(trial_id = trial_id)

      logging::loginfo("trial_scores: %s", trial_scores)

      logging::loginfo("Append to scores_trial")

      # Compute a few "change in score/review" vars

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

    }


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


