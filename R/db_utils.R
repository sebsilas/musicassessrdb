

#' Connect to musicassessr DB
#'
#' @param local
#' @param db_name
#' @param pool
#'
#' @return
#' @export
#'
#' @examples
musicassessr_con <- function(local = FALSE,
                             db_name = Sys.getenv("MUSICASSESSR_DB_NAME"),
                             pool = TRUE) {

  logging::loginfo("Connecting to musicassessr database")


  db_con <- if(local) {
    RPostgres::dbConnect(
    RPostgres::Postgres(),
    dbname = Sys.getenv("LOCAL_DB_NAME"),
    user = Sys.getenv("LOCAL_DB_USER"),
    password = Sys.getenv("LOCAL_DB_PASSWORD"),
    host = Sys.getenv("LOCAL_DB_HOST"),
    port = Sys.getenv("LOCAL_DB_PORT"),
    timezone = "UTC")
    } else {

      if(pool) {
        pool::dbPool(
          RPostgres::Postgres(),
          dbname = db_name,
          user = Sys.getenv("MUSICASSESSR_DB_USER"),
          password = Sys.getenv("MUSICASSESSR_DB_PASSWORD"),
          host = Sys.getenv("MUSICASSESSR_DB_HOST"),
          port = Sys.getenv("MUSICASSESSR_DB_PORT"),
          timezone = "UTC")
      } else {
        RPostgres::dbConnect(
        RPostgres::Postgres(),
        dbname = db_name,
        user = Sys.getenv("MUSICASSESSR_DB_USER"),
        password = Sys.getenv("MUSICASSESSR_DB_PASSWORD"),
        host = Sys.getenv("MUSICASSESSR_DB_HOST"),
        port = Sys.getenv("MUSICASSESSR_DB_PORT"),
        timezone = "UTC")
    }
  }

}


#' Compile item trials
#'
#' @param db_con
#' @param current_test_id
#' @param session_ids_filter
#' @param user_ids_filter
#' @param join_item_banks_on
#' @param filter_item_banks
#' @param add_trial_scores
#' @param score_to_use
#' @param trial_filter_fun
#' @param grepl_item_id_filter
#'
#' @returns
#' @export
#'
#' @examples
compile_item_trials <- function(db_con = NULL,
                                current_test_id = NULL,
                                session_ids_filter = NULL,
                                user_ids_filter,
                                join_item_banks_on = FALSE,
                                filter_item_banks = NULL,
                                add_trial_scores = FALSE,
                                score_to_use = "opti3",
                                trial_filter_fun = NULL,
                                grepl_item_id_filter = NULL) {

  if(is.null(db_con)) {
    connected_to_db_locally <- TRUE
    db_con <- musicassessr_con()
  } else {
    connected_to_db_locally <- FALSE
  }

  # Grab session info
  sessions <- get_table(db_con, "sessions") %>%
    dplyr::filter(user_id %in% user_ids_filter) %>%
    dplyr::collect()

  # Grab trial info
  user_trials <- get_table(db_con, "trials")  %>%
    dplyr::filter(session_id %in% sessions$session_id) %>%
    dplyr::collect() %>%
    dplyr::left_join(sessions, by = "session_id")

  if(is.function(trial_filter_fun)) {
    user_trials <- user_trials %>%
      trial_filter_fun()
  }

  if(is.character(grepl_item_id_filter)) {
    user_trials <- user_trials %>%
      dplyr::filter(grepl(grepl_item_id_filter, item_id))
  }


  # Return early if nothing there

  if(get_nrows(user_trials) < 1L) {
    return(user_trials)
  }

  if(!is.null(current_test_id)) {
    user_trials <- user_trials %>%
      dplyr::filter(test_id == current_test_id)
  }


  # Return early if nothing there

  if(get_nrows(user_trials) < 1L) {
    return(user_trials)
  }

  if(!is.null(session_ids_filter)) {
    user_trials <- user_trials %>%
      dplyr::filter(session_id %in% session_ids_filter)
  }

  # Return early if nothing there

  if(get_nrows(user_trials) < 1L) {
    return(user_trials)
  }


  if(join_item_banks_on) {

    # Join items on
    user_trials <- left_join_on_items(db_con, df_with_item_ids = user_trials)

  }

  if(!is.null(filter_item_banks)) {
    item_banks_table <- get_table(db_con, "item_banks", collect = TRUE)
    item_bank_ids <- item_bank_name_to_id(item_banks_table, ib_name = filter_item_banks)
    user_trials <- user_trials %>% dplyr::filter(item_bank_id %in% item_bank_ids)
  }

  if(add_trial_scores) {

    trial_scores <- dplyr::tbl(db_con, "scores_trial") %>%
      dplyr::filter(trial_id %in% user_trials$trial_id)

    if(is.character(score_to_use)) {
      trial_scores <- trial_scores %>%
        dplyr::filter(measure %in% score_to_use)
    }

    trial_scores <- trial_scores %>%
      dplyr::collect() %>%
      tidyr::pivot_wider(-scores_trial_id,
                         names_from = "measure",
                         values_from = "score")

    user_trials <- user_trials %>%
      dplyr::left_join(trial_scores, by = "trial_id")
  }

  if(connected_to_db_locally) {
    db_disconnect(db_con)
  }

  user_trials
}

#' Get a postgres table
#'
#' @param db_con
#' @param name
#' @param collect
#'
#' @return
#' @export
#'
#' @examples
get_table <- function(db_con, name, collect = TRUE) {

  logging::loginfo("Getting table %s", name)

  # tab <- DBI::dbReadTable(db_con, name)
  tab <- dplyr::tbl(db_con, name)

  if(collect) {
    tab <- tab %>% dplyr::collect()
  }

  return(tab)

}


#' Append a data frame to a table in the database
#'
#'
#' @param db_con A database connection environment
#' @param table Name of table to which to append in database
#' @param data dataframe to be appended
#'
#' @return Returns number of rows affected
#' @export
db_append_to_table <- function(db_con, table, data, primary_key_col = NULL){

  entries <- nrow(data)
  logging::loginfo("Appending %s entries to table %s", entries, table)
  res <- DBI::dbAppendTable(db_con, table, data)

  if(!is.null(primary_key_col)) {
    primary_key_col <- as.name(primary_key_col)
    primary_key <- dplyr::tbl(db_con, table) %>%
      dplyr::slice_max(!! primary_key_col, n = nrow(data) ) %>%
      dplyr::collect() %>%
      dplyr::pull(!! primary_key_col)

    return(primary_key)
  } else {
    return(res)
  }

}


check_ids_exist <- function(db_con, experiment_id = NULL, experiment_condition_id = NULL, user_id = NULL) {
  if(!is.null(experiment_id)) {
    if(!check_id_exists(db_con, table_name = "experiments", id_col = "experiment_id", id = experiment_id)) stop(paste0("Experiment ID ", experiment_id, " does not exist."))
  }

  if(!is.null(experiment_condition_id)) {
    if(!check_id_exists(db_con, table_name = "experiment_conditions", id_col = "experiment_condition_id", id = experiment_condition_id)) stop(paste0("Condition ID ", experiment_condition_id, " does not exist."))
  }

  if(!is.null(user_id)) {
    if(!check_id_exists(db_con, table_name = "users", id_col = "user_id", id = user_id)) stop(paste0("User ID ", user_id, " does not exist."))
  }

  logging::loginfo("All specified IDs exist in DB.")

  return(TRUE)

}



#' Add final session info to postgres db
#'
#' @param asynchronous_api_mode
#'
#' @return
#' @export
#'
#' @examples
elt_add_final_session_info_to_db <- function(asynchronous_api_mode) {


  psychTestR::code_block(function(state, ...) {

    logging::loginfo("elt_add_final_session_info_to_db!")

    # Get session info
    test_id <- psychTestR::get_global("test_id", state)
    session_id <- musicassessr::get_promise_value(psychTestR::get_global("session_id", state))  # Created earlier
    if(length(session_id) > 1L && "session_id" %in% names(session_id)) {
      session_id <- session_id$session_id
    }
    user_id <- psychTestR::get_global("user_id", state)
    session_info <- psychTestR::get_session_info(state, complete = FALSE)
    psychTestR_session_id <- session_info$p_id
    user_info <- psychTestR::get_global("user_info", state)

    if(asynchronous_api_mode) {

      logging::loginfo("call compute_session_scores_and_end_session_api...")

        final_session_result <- future::future({
          compute_session_scores_and_end_session_api(test_id, musicassessr::get_promise_value(session_id), user_id, psychTestR_session_id, session_complete = "1", user_info = user_info)
        }, seed = NULL) %...>% (function(result) {
          logging::loginfo("Returning promise result: %s", result)
          if(result$status == 200) {
            return(result)
          } else {
            return(NA)
          }
        })

        psychTestR::set_global("compute_session_scores_and_end_session_api_called", TRUE, state)
        psychTestR::set_global('final_session_result', final_session_result, state)

    }

  })
}


get_session_trials <- function(session_id) {
  trials <- get_table("trials") %>%
    dplyr::filter(session_id == !! session_id)
}


#' Get a random selection of previous trials based on a user ID
#'
#' Note currently this doesn't distinguish between the most recent session and others further back
#'
#' @param db_con
#' @param user_id
#' @param no_reviews
#' @param rhythmic
#'
#' @return
#'
#' @examples
get_review_trials <- function(no_reviews, state, rhythmic = FALSE) {

  # NB. This will be deprecated soon
  db_con <- musicassessr_con()
  user_id <- psychTestR::get_global("user_id", state)
  current_test_id <- psychTestR::get_global("test_id", state)

  cat(file=stderr(), "user_id", user_id, "\n")
  cat(file=stderr(), "current_test_id", current_test_id, "\n")

  user_trials <- compile_item_trials(db_con, current_test_id, user_id = user_id) # Note, that there is a "session_id" argument we probably want to explore using in the future

  cat(file=stderr(), "nrow(user_trials)", nrow(user_trials), "\n")

  if(rhythmic) {
    logging::loginfo("Filtering to use only previously rhythmic trials")
    user_trials <- user_trials %>% dplyr::filter(rhythmic)
  } else {
    logging::loginfo("Filtering to use only previously arrhythmic trials")
    user_trials <- user_trials %>% dplyr::filter(!rhythmic)
  }

  # Sample from previous trials

  cat(file=stderr(), "Sample from previous trials \n")

  user_trials <- user_trials %>%
    dplyr::select(stimulus_abs_melody, stimulus_durations, item_id, rhythmic) %>%
    dplyr::filter(!is.na(stimulus_abs_melody) | !is.na(stimulus_durations)) %>%
    dplyr::slice_sample(n = no_reviews) %>%
    dplyr::collect() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(melody = paste0(diff(itembankr::str_mel_to_vector(stimulus_abs_melody) ), collapse = ",")) %>%
    dplyr::ungroup() %>%
    dplyr::rename(abs_melody = stimulus_abs_melody,
                  durations = stimulus_durations) %>%
    dplyr::mutate(melody_no = dplyr::row_number() )

  cat(file=stderr(), "nrow(user_trials)", nrow(user_trials), "\n")

  db_disconnect(db_con)

  return(user_trials)


}


item_bank_name_to_id <- Vectorize(function(item_banks_table, ib_name) {

    id <- item_banks_table %>%
      dplyr::filter(item_bank_name == ib_name) %>%
      dplyr::collect() %>%
      dplyr::pull(item_bank_id)

    if(length(id) == 0) {
      id <- NA
    }

    id <- as.integer(id)

    return(id)
  }, vectorize.args = "ib_name")

# t <- item_bank_name_to_id(item_banks_table = get_table(db_con, "item_banks"), c("singpause_item", "singpause_phrase"))


#' Get a user's range based on their user ID
#'
#' @param user_id
#'
#' @return
#' @export
#'
#' @examples
get_range_from_user_id <- function(db_con, user_id, instrument_id = 1L) {

  res <- get_table(db_con, 'user_instrument_info') %>%
    dplyr::filter(user_id == user_id, instrument_id == instrument_id)

  list(bottom_range = res$bottom_range, top_range = res$top_range)

}


check_id_exists <- function(db_con, table_name, id_col, id) {

  tb <- get_table(db_con, table_name) %>%
    dplyr::filter(!!as.symbol(id_col) == id)

  return(nrow(tb) != 0)
}



db_disconnect_shiny <- function(state, ...) {
  db_con <- psychTestR::get_global("db_con", state)
  if(!is.null(db_con)) {
    logging::loginfo("Disconnecting from DB")
    db_disconnect(db_con)
  }
}


scores_to_long_format <- function(scores) {

  scores <- scores[purrr::map_lgl(scores, function(x) length(x) == 1)]

  scores[purrr::map_lgl(scores, is.nan)] <- NA

  scores %>%
    tibble::as_tibble() %>%
    tidyr::pivot_longer(dplyr::everything(),
                        names_to = "measure",
                        values_to = "score")
}


username_from_user_id <- function(db_con, user_id) {

  if(!is.null(user_id)) {
    res <-  dplyr::tbl(db_con, "users") %>%
      dplyr::filter(user_id == !!user_id) %>%
      dplyr::pull(username)
  } else {
    res <- NA
  }
  return(res)
}

user_id_from_username <- function(db_con, username) {

  if(!is.null(username)) {
    res <-  dplyr::tbl(db_con, "users") %>%
      dplyr::filter(username == !!username) %>%
      dplyr::pull(user_id)
  } else {
    res <- NA
  }
  return(res)
}


#' Connect to the musicassessrdb and store the connection in the state object.
#'
#' @param state
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
connect_to_db_state <- function(state, ...) {

  logging::loginfo("Connect to DB state...")
  db_con <- safe_db_con(state)

  psychTestR::set_global("db_con", db_con, state)
  return(db_con)
}

safe_db_con <- function(state) {
  # Check if there is already a valid connection to use

  if(exists("db_con")) {

    if(!DBI::dbIsValid(db_con)) {
      db_con <- musicassessr_con()
    }

  } else {

    db_con <- psychTestR::get_global("db_con", state)

    if(is.null(db_con)) {
      db_con <- musicassessr_con()
    } else {
      if(!DBI::dbIsValid(db_con)) {
        db_con <- musicassessr_con()
      }
    }

  }
  return(db_con)
}

#' Disconnect from the musicassessrdb, as stored in the state object
#'
#' @return
#' @export
#'
#' @examples
elt_disconnect_from_db <- function() {
  psychTestR::code_block(function(state, ...) {
    logging::loginfo("Disconnecting from DB")
    db_con <- psychTestR::get_global("db_con", state)
    db_disconnect(db_con)
  })
}


#' Get instrument ID from name
#'
#' @param inst_name
#'
#' @return
#' @export
#'
#' @examples
instrument_name_to_id <- function(inst_name) {

  inst_id <- inst_table_db %>%
    dplyr::filter(instrument_name == !! inst_name) %>%
    dplyr::pull(instrument_id)

}




extract_item_bank_name_from_item_id <- function(db_con, item_id) {
  item_bank_name <- sub("_[^_]+$", "", x = item_id)
}

get_item_bank_names <- function(db_con, item_ids) {

  item_banks <- item_ids %>%
    extract_item_bank_name_from_item_id(db_con = db_con) %>%
    unique()

  official_item_banks <- get_table(db_con, "item_banks", collect = TRUE)$item_bank_name

  item_banks <- item_banks[which(item_banks %in% official_item_banks)] %>%
    unique()
}


#' Left join on items
#'
#' @param db_con
#' @param df_with_item_ids
#'
#' @return
#' @export
#'
#' @examples
left_join_on_items <- function(db_con,
                               df_with_item_ids) {

  logging::loginfo("Join on items...")

  all_item_ids <- df_with_item_ids %>% dplyr::pull(item_id) %>% unique()

  # item_banks_table <- dplyr::tbl(db_con, "item_banks")

  # We use static now for performance
  item_banks_table <- item_banks_table_static

  grand_item_bank_metadata <-
    tibble::tibble(item_id = all_item_ids,
                   item_bank_name = extract_item_bank_name_from_item_id(db_con, item_id),
                   item_bank_id = item_bank_name_to_id(item_banks_table = item_banks_table, ib_name = item_bank_name)) %>%
    na.omit


  item_banks <- grand_item_bank_metadata %>%
    dplyr::select(item_bank_name, item_bank_id) %>%
    unique()

  grand_item_bank <- purrr::pmap_dfr(item_banks, function(item_bank_name, item_bank_id) {

    ib <- get_table(db_con, paste0("item_bank_", item_bank_name), collect = FALSE) %>%
      dplyr::filter(item_id %in% !! all_item_ids) %>%
      dplyr::collect() %>%
      dplyr::mutate(item_bank_id = item_bank_id)

  })


  df_with_item_ids %>%
    dplyr::collect() %>%
    dplyr::left_join(grand_item_bank, by = "item_id")

}

# db_con <- musicassessr_con()
# t <- get_study_history_stats(db_con, user_id = 82L, test_id = 1L, inst = NULL, item_id = "Berkowitz_ngram_334774", current_trial_scores = tibble::tibble(measure = "opti3", score = NA), current_trial_time_completed = Sys.time() )
# db_disconnect(db_con)

# For a user, item, test, and measure combo, get the most recent score before the present moment
get_study_history_stats <- function(db_con,
                                    user_id,
                                    test_id = NULL,
                                    inst = NULL,
                                    item_id = NULL,
                                    measure = "opti3",
                                    current_trial_scores,
                                    current_trial_time_completed) {


  logging::loginfo("get_study_history_stats")
  logging::loginfo("user_id: %s", user_id)
  logging::loginfo("item_id: %s", item_id)


  tryCatch({


    trials <- dplyr::tbl(db_con, "trials")

    trials <- dplyr::tbl(db_con, "sessions") %>%
      dplyr::filter(user_id == user_id) %>%
      dplyr::left_join(trials, by = "session_id") %>%
      dplyr::collect()


    # Join on trial scores
    trial_scores <- dplyr::tbl(db_con, "scores_trial") %>%
      dplyr::filter(measure == !! measure) %>%
      dplyr::collect()

    trials <- trials %>%
      dplyr::collect() %>%
      dplyr::left_join(trial_scores, by = "trial_id")


    if(!is.null(test_id)) {
      trials <- trials %>%
        dplyr::filter(test_id == !! test_id)
    }


    if(!is.null(inst)) {
      trials <- trials %>%
        dplyr::filter(instrument == !! inst)
    }


    if(!is.null(item_id)) {
      trials <- trials %>% dplyr::filter(item_id == !! item_id)
    }

    # Change across all sessions
    first_trial <- trials %>% dplyr::slice_min(trial_time_completed) %>%  dplyr::slice_min(attempt) %>% dplyr::pull(score)
    last_trial <- trials %>% dplyr::slice_max(trial_time_completed) %>% dplyr::pull(score)
    change_across_all_sessions <- last_trial - first_trial

    # Number of times practised
    no_times_practised <- trials %>%
      dplyr::pull(session_id) %>%
      unique() %>%
      length()


    # Average no. attempts
    avg_no_attempts <- trials %>%
      dplyr::group_by(session_id) %>%
      dplyr::slice_max(attempt) %>%
      dplyr::ungroup() %>%
      dplyr::pull(attempt) %>%
      mean(na.rm = TRUE)

    # Average change across attempts
    avg_change_across_attempts <- trials %>%
      dplyr::group_by(session_id) %>%
      dplyr::summarise(change_across_attempts = score[max(attempt)] - score[min(attempt)] ) %>%
      dplyr::ungroup() %>%
      dplyr::pull(change_across_attempts) %>%
      mean(na.rm = TRUE)

    if(is.nan(avg_change_across_attempts)) {
      avg_change_across_attempts <- NA
    }



    if(length(unique(trials$session_id)) > 1L) {

      # We use a date not so far in the past, to put the values on a reasonable scale for model fitting
      reference_date <- as.numeric(as.POSIXct("2024-01-01 00:00:00", tz="UTC"))

      trials <- trials %>%
        dplyr::mutate(trial_time_completed_days =  as.numeric((current_trial_time_completed - reference_date)) / (60 * 60 * 24))

      lm_score <- lm(score ~ trial_time_completed_days + attempt, data = trials)

      gradient_across_all_scores <- coef(lm_score)[['trial_time_completed_days']]
      item_intercept <- coef(lm_score)[['(Intercept)']]

    } else {
      gradient_across_all_scores <- NA
      item_intercept <- NA
    }

    # Get last score
    last_score <- trials %>%
      dplyr::slice_max(trial_time_completed)  %>%
      dplyr::select(score, trial_time_completed) %>%
      dplyr::collect()

    logging::loginfo("last_score: %s", last_score)

    last_score_value <- last_score %>%
      dplyr::pull(score)

    logging::loginfo("last_score_value: %s", last_score_value)

    if(length(last_score_value) == 0L) {
      last_score_value <- NA
    }


    logging::loginfo("last_score_value again: %s", last_score_value)

    last_score_time_completed <- last_score %>%
      dplyr::pull(trial_time_completed)

    logging::loginfo("last_score_time_completed: %s", last_score_time_completed)

    current_score <- current_trial_scores %>%
      dplyr::filter(measure == !! measure) %>%
      dplyr::pull(score)


    logging::loginfo("last_score_value: %s", last_score_value)
    logging::loginfo("current_score: %s", current_score)


    if(is.na(last_score_value) && is.na(current_score)) {
      learned_in_current_session <-  0L
    } else if(!is.na(last_score_value) && is.na(current_score)) {
      learned_in_current_session <- 0L
    } else if(is.na(last_score_value) && dplyr::near(current_score, 1)) {
      learned_in_current_session <- 1L
    } else if(last_score_value < 1 && dplyr::near(current_score, 1)) {
      learned_in_current_session <- 1L
    } else {
      learned_in_current_session <- 0L
    }

    logging::loginfo("learned_in_current_session: %s", learned_in_current_session)

    change_in_score_from_last_session <- current_score - last_score_value

    logging::loginfo("change_in_score_from_last_session: %s", change_in_score_from_last_session)


    item_stats <- tibble::tibble(change_across_all_sessions = change_across_all_sessions,
                                 no_times_practised = no_times_practised,
                                 avg_no_attempts = avg_no_attempts,
                                 avg_change_across_attempts = avg_change_across_attempts,
                                 gradient_across_all_scores = gradient_across_all_scores,
                                 item_intercept = item_intercept,
                                 learned_in_current_session = learned_in_current_session,
                                 last_score = last_score_value,
                                 last_score_completed = last_score_time_completed,
                                 change_in_score_from_last_session = change_in_score_from_last_session,
                                 increase_since_last_session = dplyr::case_when(change_in_score_from_last_session > 0 ~ 1L, TRUE ~ 0L),
                                 time_since_last_item_studied = lubridate::as_datetime(current_trial_time_completed) - lubridate::as_datetime(last_score_completed))

    return(item_stats)



    return(item_stats)
  }, error = function(err) {

    logging::logerror(err)

    logging::loginfo("Score not found, assuming not learned before and returning NA.")

    return(tibble::tibble(change_across_all_sessions = NA,
                          no_times_practised = NA,
                          avg_no_attempts = NA,
                          avg_change_across_attempts = NA,
                          gradient_across_all_scores = NA,
                          item_intercept = NA,
                          learned_in_current_session = NA,
                          last_score = NA,
                          last_score_completed = NA,
                          change_in_score_from_last_session = NA,
                          increase_since_last_session = NA,
                          time_since_last_item_studied = NA))

  })

}


db_disconnect <- function(db_con) {
  logging::loginfo("Disconnecting from db")
  if(is(db_con, "Pool"))   {
    pool::poolClose(db_con)
  } else {
    DBI::dbDisconnect(db_con)
  }
}


get_melodic_production <- function(db_con = NULL, trial_id) {

  if(is.null(db_con)) {
    db_con <- musicassessr_con()
  }

  dplyr::tbl(db_con, "melodic_production") %>%
    dplyr::filter(trial_id %in% trial_id) %>%
    dplyr::collect() %>%
    dplyr::group_by(trial_id) %>%
    musicassessr::to_string_df() %>%
    dplyr::ungroup()
}

# db_con <- musicassessr_con()
# t <- get_study_history_stats(db_con, 2L, 2L, "Voice", "WJD_phrase_8308")
# t <- get_study_history_stats(db_con, 2L, 2L, "Voice", "fail")
# db_disconnect(db_con)

# tests
# extract_item_bank_id_from_item_id(db_con, "Berkowitz_ngram_76196")
# extract_item_bank_id_from_item_id(db_con, "Berkowitz_ngram_76196")
