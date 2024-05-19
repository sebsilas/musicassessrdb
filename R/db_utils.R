


musicassessr_con <- function(local = FALSE) {

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
      RPostgres::dbConnect(
        RPostgres::Postgres(),
        dbname = Sys.getenv("MUSICASSESSR_DB_NAME"),
        user = Sys.getenv("MUSICASSESSR_DB_USER"),
        password = Sys.getenv("MUSICASSESSR_DB_PASSWORD"),
        host = Sys.getenv("MUSICASSESSR_DB_HOST"),
        port = Sys.getenv("MUSICASSESSR_DB_PORT"),
        timezone = "UTC")
  }


}


compile_item_trials <- function(db_con,
                                current_test_id = NULL,
                                session_id = NULL,
                                user_id,
                                join_item_banks_on = FALSE,
                                filter_item_banks = NULL,
                                add_trial_scores = FALSE,
                                score_to_use = "opti3") {


  # Grab session info
  sessions <- get_table(db_con, "sessions", collect = FALSE) %>%
    dplyr::filter(user_id %in% !! user_id)

  # Grab trial info
  trials <- get_table(db_con, "trials", collect = FALSE)  %>%
    dplyr::left_join(sessions, by = "session_id")

  # Grab trials only for the given user on the given test
  user_trials <- trials %>%
    dplyr::filter(user_id %in% !! user_id)

  # Return early if nothing there

  if(get_nrows(user_trials) < 1L) {
    return(user_trials)
  }

  if(!is.null(current_test_id)) {
    user_trials <- user_trials %>%
      dplyr::filter(test_id == !! current_test_id)
  }

  # Return early if nothing there

  if(get_nrows(user_trials) < 1L) {
    return(user_trials)
  }

  if(!is.null(session_id)) {
    user_trials <- user_trials %>%
      dplyr::filter(session_id %in% !! session_id)
  }

  # Return early if nothing there

  if(get_nrows(user_trials) < 1L) {
    return(user_trials)
  }

  if(join_item_banks_on) {

    # Join items on
    item_banks <- user_trials %>%
      dplyr::pull(item_id) %>%
      get_item_bank_names(db_con = db_con)

    user_trials <- left_join_on_items(db_con, item_banks, df_with_item_ids = user_trials)

  }


  if(!is.null(filter_item_banks)) {
    item_banks_table <- get_table(db_con, "item_banks", collect = TRUE)
    item_bank_ids <- item_bank_name_to_id(item_banks_table, ib_name = filter_item_banks)
    user_trials <- user_trials %>% dplyr::filter(item_bank_id %in% item_bank_ids)
  }

  if(add_trial_scores) {

    trial_scores <- dplyr::tbl(db_con, "scores_trial") %>%
      dplyr::filter(measure == !! score_to_use) %>%
      dplyr::collect()

    user_trials <- user_trials %>%
      dplyr::left_join(trial_scores, by = "trial_id")
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
    session_id <- psychTestR::get_global("session_id", state) # Created earlier
    user_id <- psychTestR::get_global("user_id", state)
    session_info <- psychTestR::get_session_info(state, complete = FALSE)
    psychTestR_session_id <- session_info$p_id

    if(asynchronous_api_mode) {

      logging::loginfo("call compute_session_scores_and_end_session_api...")

        final_session_result <- future::future({
          compute_session_scores_and_end_session_api(test_id, musicassessr::get_promise_value(session_id), user_id, psychTestR_session_id)
        }) %...>% (function(result) {
          logging::loginfo("Returning promise result: %s", result)
          if(result$status == 200) {
            return(result)
          } else {
            return(NA)
          }
        })

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

  # Get DB con
  db_con <- psychTestR::get_global("db_con", state)
  user_id <- psychTestR::get_global("user_id", state)
  current_test_id <- psychTestR::get_global("test_id", state)

  user_trials <- compile_item_trials(db_con, current_test_id, user_id = user_id) # Note, that there is a "session_id" argument we probably want to explore using in the future

  if(rhythmic) {
    logging::loginfo("Filtering to use only previously rhythmic trials")
    user_trials <- user_trials %>% dplyr::filter(rhythmic)
  } else {
    logging::loginfo("Filtering to use only previously arrhythmic trials")
    user_trials <- user_trials %>% dplyr::filter(!rhythmic)
  }

  # Sample from previous trials

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


}


item_bank_name_to_id <- Vectorize(function(item_banks_table, ib_name) {
    item_banks_table %>%
      dplyr::filter(item_bank_name == ib_name) %>%
      dplyr::collect() %>%
      dplyr::pull(item_bank_id)
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
    DBI::dbDisconnect(db_con)
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
    DBI::dbDisconnect(db_con)
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


left_join_on_items <- function(db_con, item_banks, df_with_item_ids) {

  item_banks_table <- get_table(db_con, "item_banks", collect = TRUE)
  ib_ids <- item_bank_name_to_id(item_banks_table = item_banks_table, ib_name = item_banks)

  purrr::map2_dfr(names(ib_ids), ib_ids, function(item_bank, ib_id) {

    ib <- get_table(db_con, paste0("item_bank_", item_bank), collect = FALSE)

    user_trials_sub <- df_with_item_ids %>%
      dplyr::mutate(item_bank_id = ib_id) %>%
      dplyr::left_join(ib, by = "item_id") %>%
      dplyr::collect()

  })

}


# For a user, item, test, and measure combo, get the most recent score before the present moment
get_latest_score <- function(db_con, user_id, test_id = NULL, inst = NULL, item_id = NULL, measure = "opti3") {

  tryCatch({

    trials <- dplyr::tbl(db_con, "trials")

    df <- dplyr::tbl(db_con, "sessions") %>%
      dplyr::filter(user_id == !! user_id) %>%
      dplyr::left_join(trials, by = "session_id")

    if(!is.null(test_id)) {
      df <- df %>% dplyr::filter(test_id == !! test_id)
    }

    if(!is.null(inst)) {
      df <- df %>% dplyr::filter(instrument == !! inst)
    }

    if(!is.null(item_id)) {
      df <- df %>% dplyr::filter(item_id == !! item_id)
    }

    df <- df %>%
      dplyr::slice_max(trial_time_completed) %>%
      dplyr::collect()

    trial_id <- df$trial_id

    trial_scores <- dplyr::tbl(db_con, "scores_trial") %>%
      dplyr::filter(trial_id == !! trial_id,
                    measure == !! measure) %>%
      dplyr::collect()

    df <- df %>%
      dplyr::left_join(trial_scores, by = "trial_id")

    score <- df %>% dplyr::select(score, trial_time_completed)

    return(score)
  }, error = function(err) {
    logging::logerror(err)
    logging::loginfo("Score not found, assuming not learned before and returning NA.")
    return(tibble::tibble(score = NA, trial_time_completed = NA))
  })

}

# db_con <- musicassessr_con()
# t <- get_latest_score(db_con, 2L, 2L, "Voice", "WJD_phrase_8308")
# t <- get_latest_score(db_con, 2L, 2L, "Voice", "fail")
# DBI::dbDisconnect(db_con)

# tests
# extract_item_bank_id_from_item_id(db_con, "Berkowitz_ngram_76196")
# extract_item_bank_id_from_item_id(db_con, "Berkowitz_ngram_76196")
