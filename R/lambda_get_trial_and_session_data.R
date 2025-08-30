
# db_con <- musicassessr_con(db_name = "melody_prod")


# user_data <- get_trial_and_session_data(group_id_filter = 5L, filter_pseudo_anonymous_ids = TRUE, app_name_filter = "songbird")
# u <- user_data$scores_trial
# u <- user_data$review_melodies_class
# u <- user_data$group_stats$class_stats

# u <- user_data$user_stats %>%
#   compute_ids_from_singpause_username()

# u %>%
#   select(minutes_spent, no_practice_sessions) %>%
#     pivot_longer(everything()) %>%
#     filter(value < 50) %>%
#       ggplot() +
#       geom_histogram(aes(x = value, fill = name)) +
#         facet_wrap(~name)

# u %>%
#   filter(minutes_spent > 0)



# db_con <- musicassessr_con(db_name = "melody_dev")

# user_dat2 <- get_trial_and_session_data(user_id = 138, app_name_filter = "songbird")

# user_dat2 <- get_trial_and_session_data(user_id = 174, app_name_filter = "songbird")



# db_disconnect(db_con)



# u <- tbl(db_con, "users") %>% filter(username == "000000XX00_02") %>% collect()

get_trial_and_session_data_api <- function(user_id_filter = NULL,
                                           group_id_filter = NULL,
                                           trial_score_measure = "opti3",
                                           session_score_measure_arrhythmic = "mean_opti3_arrhythmic",
                                           session_score_measure_rhythmic = "mean_opti3_rhythmic",
                                           app_name_filter = NULL) {

  # Define the request body as a list
  request_body <- list(user_id_filter = user_id_filter,
                       group_id_filter = group_id_filter,
                       trial_score_measure = trial_score_measure,
                       session_score_measure_arrhythmic = session_score_measure_arrhythmic,
                       session_score_measure_rhythmic = session_score_measure_rhythmic,
                       app_name_filter = app_name_filter)

  endpoint_wrapper(function_name = "get-trial-and-session-data",
                   request_body = request_body)

}



# This is the function that is called when the endpoint
# is invoked
get_trial_and_session_data <- function(user_id_filter = NULL,
                                       group_id_filter = NULL,
                                       trial_score_measure = "opti3",
                                       session_score_measure_arrhythmic = "mean_opti3_arrhythmic",
                                       session_score_measure_rhythmic = "mean_opti3_rhythmic",
                                       filter_pseudo_anonymous_ids = FALSE,
                                       app_name_filter = NULL) {

  stopifnot(
    is.null.or(app_name_filter, is.character)
  )


  logging::loginfo("Inside get_trial_and_session_data function")

  logging::loginfo("filter_pseudo_anonymous_ids: %s", filter_pseudo_anonymous_ids)


  # Choose group OR user
  if(!is.null(user_id_filter) && !is.null(group_id_filter)) {
    stop("Only choose one of user_id_filter or group_id_filter")
  }

  logging::loginfo("user_id_filter = %s", user_id_filter)
  logging::loginfo("group_id_filter = %s", group_id_filter)
  logging::loginfo("trial_score_measure = %s", trial_score_measure)
  logging::loginfo("session_score_measure_arrhythmic = %s", session_score_measure_arrhythmic)
  logging::loginfo("session_score_measure_rhythmic = %s", session_score_measure_rhythmic)
  logging::loginfo("app_name_filter = %s", app_name_filter)

  if(!is.null(group_id_filter)) {
    user_id_filter <- get_users_in_group(group_id_filter)
  } else {
  }

  response <- tryCatch({

    # Main logic

    browser()

    # Get sessions associated with user
    sessions <- get_table(db_con, "sessions", collect = FALSE) %>%
        dplyr::filter(user_id %in% user_id_filter) %>% # Note this could be multiple user_ids
        dplyr::mutate(Date = lubridate::as_date(session_time_started))  %>%
        dplyr::left_join(get_table(db_con, "users", collect = FALSE), by = "user_id") %>%
      { if(is.character(app_name_filter)) dplyr::filter(., app_name == !! app_name_filter) else . } %>%
        dplyr::collect() %>%
        { if(filter_pseudo_anonymous_ids) dplyr::filter(., filter_pseudo_anonymous_ids(username)) else . } %>%
      { if(app_name_filter == "songbird") compute_ids_from_singpause_username(.) else . }

    session_ids <- sessions$session_id

    # Get trials
    trials <- compile_item_trials(db_con,
                                  session_ids_filter = session_ids,
                                  user_ids_filter = user_id_filter) %>%
                dplyr::mutate(Date = lubridate::as_date(session_time_started)) %>%
                dplyr::left_join(get_table(db_con, "users"), by = "user_id") %>%
      { if(is.character(app_name_filter)) dplyr::filter(., app_name == !! app_name_filter) else . }

    # Get scores

    scores_trial <- get_table(db_con, "scores_trial", collect = TRUE) %>%
      dplyr::select(-scores_trial_id) %>%
      dplyr::filter(measure == !! trial_score_measure) %>%
      dplyr::filter(!is.na(measure) & !is.na(score))

    # Instantiate and potentially rewrite later
    session_scores_agg_class <- NA
    session_scores_rhythmic_class <- NA
    session_scores_arrhythmic_class <- NA
    review_melodies_class <- NA

    if("songbird" %in% trials$app_name || app_name_filter == "songbird") {

      songbird_phrase <- get_table(db_con, "item_bank_singpause_2025_phrase") %>%
        dplyr::select(item_id, phrase_name, song_name)

      scores_trial <- trials %>%
        dplyr::left_join(songbird_phrase, by = "item_id") %>%
        dplyr::mutate(phrase_name = dplyr::case_when(grepl("singpause", item_id) & is.na(phrase_name) ~ as.character(song_name), TRUE ~ as.character(phrase_name)),
                      songbird_type = dplyr::case_when(grepl("Berkowitz", item_id) ~ "Sing-Training",
                                                       grepl("singpause", item_id) & trial_paradigm == "simultaneous_recall" ~ "SingPause Singalong",
                                                       grepl("singpause", item_id) & trial_paradigm == "call_and_response" ~ "SingPause Solo",
                                                       TRUE ~ as.character(NA)
                                                       ) ) %>%
        dplyr::left_join(scores_trial, by = "trial_id") %>%
        dplyr::select(Date, user_id, username, trial_id, trial_time_started,
                      trial_time_completed, instrument, attempt, item_id, display_modality, phase,
                      rhythmic, stimulus_abs_melody, stimulus_durations, score, phrase_name, trial_paradigm, songbird_type, new_items_id, review_items_id) %>%
        compute_ids_from_singpause_username()


      # Compute class-level aggregates
      if ("class_id" %in% names(scores_trial)) {

        session_scores_agg_class <- scores_trial %>%
          dplyr::group_by(class_id, Date) %>%
          dplyr::summarise(score = mean(score, na.rm = TRUE)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(score = dplyr::case_when(is.nan(score) ~ NA, TRUE ~ score))

        session_scores_rhythmic_class <- scores_trial %>%
          dplyr::filter(rhythmic) %>%
          dplyr::group_by(class_id, Date) %>%
          dplyr::summarise(score = mean(score, na.rm = TRUE)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(score = dplyr::case_when(is.nan(score) ~ NA, TRUE ~ score))

        session_scores_arrhythmic_class <- scores_trial %>%
          dplyr::filter(!rhythmic) %>%
          dplyr::group_by(class_id, Date) %>%
          dplyr::summarise(score = mean(score, na.rm = TRUE)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(score = dplyr::case_when(is.nan(score) ~ NA, TRUE ~ score))

      }

      # For phrases with names we remove the constraint that a phrase must have been played more than once to be returned

      review_melodies_over_time <- scores_trial %>%
        dplyr::group_by(Date, user_id, username, phrase_name, item_id, songbird_type) %>%
        dplyr::summarise(score = mean(score, na.rm = TRUE) ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(score = dplyr::case_when(is.na(score) ~ 0, TRUE ~ score)) %>%
        dplyr::filter(!is.na(phrase_name)) # Typically this will be a standard SAA/PBET test

      # Class-level aggregation of review melodies
      if ("class_id" %in% names(scores_trial) && is.data.frame(review_melodies_over_time)) {

        review_melodies_class <- review_melodies_over_time %>%
          dplyr::left_join(scores_trial %>% dplyr::select(user_id, class_id) %>% dplyr::distinct(), by = "user_id") %>%
          dplyr::group_by(class_id, phrase_name) %>%
          dplyr::summarise(mean_score = mean(score, na.rm = TRUE), .groups = "drop") %>%
          dplyr::filter(!is.na(phrase_name))

      }

    } else {
      scores_trial <- trials %>%
        dplyr::left_join(scores_trial, by = "trial_id") %>%
        dplyr::select(Date, user_id, username, trial_id, trial_time_started, trial_time_completed, instrument,
                      attempt, item_id, display_modality, phase, trial_paradigm,
                      rhythmic, stimulus_abs_melody, stimulus_durations, score, new_items_id, review_items_id)


      # Melodies we aggregate at the session level (i.e, can see improvements in the same day)

      review_melodies <- scores_trial %>%
        dplyr::count(user_id, stimulus_abs_melody, trial_time_started) %>%
        dplyr::count(user_id, stimulus_abs_melody) %>%
        dplyr::filter(n > 1 & !is.na(stimulus_abs_melody))

      stimulus_abs_melody_review <- review_melodies %>% dplyr::pull(stimulus_abs_melody)

      review_melodies_over_time <- scores_trial %>%
        dplyr::filter(stimulus_abs_melody %in% stimulus_abs_melody_review) %>%
        dplyr::group_by(user_id, username, trial_time_started, stimulus_abs_melody) %>%
        dplyr::summarise(score = mean(score, na.rm = TRUE) ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(score = dplyr::case_when(is.na(score) ~ 0, TRUE ~ score))
    }

    # Compute session scores post-hoc just using trials..

    # But session scores we aggregate over the day
    # Aggregate across rhythmic and arrhythmic
    session_scores_agg <- scores_trial %>%
      dplyr::group_by(user_id, username, Date) %>%
      dplyr::summarise(score = mean(score, na.rm = TRUE) ) %>%
      dplyr::ungroup()

    session_scores_rhythmic <- scores_trial %>%
      dplyr::filter(rhythmic) %>%
      dplyr::group_by(user_id, username, Date) %>%
      dplyr::summarise(score = mean(score, na.rm = TRUE)) %>%
      dplyr::ungroup()

    session_scores_arrhythmic <- scores_trial %>%
      dplyr::filter(!rhythmic) %>%
      dplyr::group_by(user_id, username, Date) %>%
      dplyr::summarise(score = mean(score, na.rm = TRUE)) %>%
      dplyr::ungroup()

    # Aggregate session scores by songbird_type
    if ("songbird_type" %in% names(scores_trial)) {
      session_scores_by_songbird_type <- scores_trial %>%
        dplyr::filter(!is.na(songbird_type)) %>%
        dplyr::group_by(user_id, username, Date, songbird_type) %>%
        dplyr::summarise(score = mean(score, na.rm = TRUE), .groups = "drop")
    } else {
      session_scores_by_songbird_type <- NA
    }



    # Compute engagement with app measures:
    # - Time using app: i) overall, ii) in last month; iii) in last week
    # - Average scores of song; number of times students practised each song;

    user_stats <- compute_user_stats(sessions)

    # If group, then do some extra aggregation
    if(is.null(group_id_filter)) {

      group_stats <- NA

    } else {

      # Average minutes per day
      overall_avg_minutes_per_day <- compute_overall_avg_minutes_per_day(user_stats)
      last_month_avg_minutes_per_day <- compute_avg_minutes_per_day(user_stats, last_month)
      last_week_avg_minutes_per_day <- compute_avg_minutes_per_day(user_stats, last_week)

      # Average number of practice sessions per user
      overall_avg_no_practice_session_per_user <- compute_avg_no_practice_session_per_user(user_stats, scores_trial)
      last_month_avg_no_practice_session_per_user <- compute_avg_no_practice_session_per_user(user_stats, scores_trial, last_month)
      last_week_avg_no_practice_session_per_user <- compute_avg_no_practice_session_per_user(user_stats, scores_trial, last_week)

      # Average number of practice sessions
      overall_avg_no_practice_sessions <- compute_avg_no_practice_sessions(overall_avg_no_practice_session_per_user)
      last_month_avg_no_practice_sessions <- compute_avg_no_practice_sessions(last_month_avg_no_practice_session_per_user)
      last_week_avg_no_practice_sessions <- compute_avg_no_practice_sessions(last_week_avg_no_practice_session_per_user)

      # Song statistics (scores and practice counts combined)
      overall_song_stats <- compute_song_stats(scores_trial)
      last_month_song_stats <- compute_song_stats(scores_trial, last_month)
      last_week_song_stats <- compute_song_stats(scores_trial, last_week)

      group_stats <- list(
        overall_avg_minutes_per_day = overall_avg_minutes_per_day,
        last_month_avg_minutes_per_day = last_month_avg_minutes_per_day,
        last_week_avg_minutes_per_day = last_week_avg_minutes_per_day,

        overall_avg_no_practice_session_per_user = overall_avg_no_practice_session_per_user,
        last_month_avg_no_practice_session_per_user = last_month_avg_no_practice_session_per_user,
        last_week_avg_no_practice_session_per_user = last_week_avg_no_practice_session_per_user,

        overall_avg_no_practice_sessions = overall_avg_no_practice_sessions,
        last_month_avg_no_practice_sessions = last_month_avg_no_practice_sessions,
        last_week_avg_no_practice_sessions = last_week_avg_no_practice_sessions,

        overall_song_stats = overall_song_stats,
        last_month_song_stats = last_month_song_stats,
        last_week_song_stats = last_week_song_stats
      )

      # By class too
      # Class-level group_stats
      if ("class_id" %in% names(scores_trial)) {
        group_stats_by_class <- user_stats %>%
          dplyr::left_join(scores_trial %>% dplyr::select(user_id, class_id) %>% dplyr::distinct(), by = "user_id") %>%
          dplyr::filter(!is.na(class_id))
      } else {
        group_stats_by_class <- NA
      }

      if (is.data.frame(group_stats_by_class) && nrow(group_stats_by_class) > 0) {

        class_stats <- group_stats_by_class %>%
          dplyr::group_by(class_id) %>%
          dplyr::summarise(
            avg_minutes_per_day = mean(minutes_spent, na.rm = TRUE),
            avg_no_practice_sessions = mean(no_practice_sessions, na.rm = TRUE),
            .groups = "drop"
          )

        class_song_stats <- scores_trial %>%
          dplyr::filter(songbird_type %in% c("SingPause Singalong", "SingPause Solo"),
                        !is.na(phrase_name),
                        grepl("Phrase ", phrase_name)) %>%
          dplyr::filter(!is.na(class_id)) %>%
          dplyr::group_by(class_id, phrase_name) %>%
          dplyr::summarise(
            Score = round(mean(score, na.rm = TRUE) * 10),
            NoTimesPractised = dplyr::n(),
            .groups = "drop"
          )

      } else {
        class_stats <- NA
        class_song_stats <- NA
      }

      group_stats$class_stats <- class_stats
      group_stats$class_song_stats <- class_song_stats


    }

    # Remove nans
    if(is.data.frame(review_melodies_over_time)) {
      review_melodies_over_time <- review_melodies_over_time %>%
        dplyr::mutate(score = dplyr::case_when(is.nan(score) ~ 0, TRUE ~ score)) %>%
        dplyr::mutate(singleiter_id = substr(username, 1, 2),
                      school_id = substr(username, 3, 4),
                      class_id = substr(username, 5, 6),
                      student_id = substr(username, 7, 8) )
    }


    # Return response

   list(
      status = 200,
      message = paste0("You successfully got trials for user(s) ", paste0(user_id_filter, collapse = ", ")),
      user_id = user_id_filter,
      scores_trial = scores_trial,
      session_scores_rhythmic = session_scores_rhythmic,
      session_scores_arrhythmic = session_scores_arrhythmic,
      scores_session = session_scores_agg,
      session_scores_by_songbird_type = session_scores_by_songbird_type,
      class_scores_session = session_scores_agg_class,
      class_scores_rhythmic = session_scores_rhythmic_class,
      class_scores_arrhythmic = session_scores_arrhythmic_class,
      review_melodies = review_melodies_over_time,
      review_melodies_class = review_melodies_class,
      user_stats = user_stats,
      group_stats = group_stats)


  }, error = function(err) {
    logging::logerror(err)
     list(
        status = 400,
        message = "Something went wrong",
        user_id = NA,
        scores_trial = NA,
        session_scores_rhythmic = NA,
        session_scores_arrhythmic = NA,
        scores_session = NA,
        review_melodies = NA,
        user_stats = NA,
        group_stats = NA
      )
  })


  return(response)

}


compute_ids_from_singpause_username <- function(df) {

  assertthat::assert_that("username" %in% names(df), msg = 'There must be a "username" column.')

  res <- df %>%
    dplyr::filter(grepl("^\\d{8}$", username)) %>%
    dplyr::filter(!username %in% c("99999900", "99999900", "99999990", "99999991", "99999992", "99999993", "99999993", "99999998", "99999999")) %>%
    dplyr::mutate(singleiter_id = substr(username, 1, 2),
                  school_id = substr(username, 3, 4),
                  class_id = substr(username, 5, 6),
                  student_id = substr(username, 7, 8) )

  return(res)
}

compute_user_stats <- function(sessions) {

  res <- sessions %>%
    dplyr::mutate(seconds_spent = as.numeric(session_time_completed - session_time_started),
                  seconds_spent = dplyr::case_when(seconds_spent < 0 ~ 0, TRUE ~ seconds_spent),
                  minutes_spent = seconds_spent / 60) %>%
    dplyr::group_by(user_id, username, Date) %>%
    dplyr::summarise(minutes_spent = sum(minutes_spent, na.rm = TRUE),
                     no_practice_sessions = dplyr::n() ) %>%
    dplyr::ungroup()

  return(res)
}

compute_overall_avg_minutes_per_day <- function(user_stats) {

  res <- user_stats %>%
    dplyr::group_by(Date) %>%
    dplyr::summarise(AvgMinutesSpent = mean(minutes_spent, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(AvgMinutesSpentPerDay = mean(AvgMinutesSpent, na.rm = TRUE)) %>%
    dplyr::pull()

  return(res)
}

compute_avg_minutes_per_day <- function(user_stats, filter_function) {
  res <- user_stats %>%
    filter_function() %>%
    dplyr::group_by(Date) %>%
    dplyr::summarise(AvgMinutesSpent = mean(minutes_spent, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(AvgMinutesSpentPerDay = mean(AvgMinutesSpent, na.rm = TRUE)) %>%
    dplyr::pull()
  return(res)
}

compute_avg_no_practice_session_per_user <- function(user_stats, scores_trial = NULL, filter_function = NULL) {
  data <- if (!is.null(filter_function)) user_stats %>% filter_function() else user_stats

  res <- data %>%
    dplyr::group_by(user_id, username) %>%
    dplyr::summarise(overall_no_practice_sessions = sum(no_practice_sessions, na.rm = TRUE), .groups = "drop")

  # Join in class_id if scores_trial is provided
  if (!is.null(scores_trial) && "class_id" %in% names(scores_trial)) {
    res <- res %>%
      dplyr::left_join(scores_trial %>% dplyr::select(user_id, class_id) %>% dplyr::distinct(), by = "user_id")
  }

  return(res)
}


# t <- get_trial_and_session_data(group_id = 5, filter_pseudo_anonymous_ids = TRUE, app_name_filter = "songbird")

compute_avg_no_practice_sessions <- function(practice_sessions_data) {

  res <- practice_sessions_data %>%
    dplyr::summarise(avg_no_practice_session = mean(overall_no_practice_sessions, na.rm = TRUE)) %>%
    dplyr::pull() %>%
    round()

  return(res)
}

compute_song_scores <- function(scores_data, filter_function = NULL) {

  scores_data <- if (!is.null(filter_function)) scores_data %>% filter_function() else scores_data

  item_identifier <- if("phrase_name" %in% names(scores_data)) "phrase_name" else "item_id"
  item_identifier <- rlang::sym(item_identifier)

  trial_type_identifier <- if("songbird_type" %in% names(scores_data)) "songbird_type" else "trial_paradigm"
  trial_type_identifier <- rlang::sym(trial_type_identifier)

  res <- scores_data %>%
    dplyr::group_by(user_id, username, !! item_identifier, !!trial_type_identifier) %>%
    dplyr::slice_max(Date)

  res <- res %>%
    dplyr::group_by(!!item_identifier, !!trial_type_identifier) %>%
    dplyr::summarise(Score = round(mean(score, na.rm = TRUE) * 10)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Score = dplyr::case_when(is.nan(Score) ~ NA, TRUE ~ Score))

  return(res)
}

compute_song_stats <- function(scores_data,
                               filter_function = NULL) {

  data <- if (!is.null(filter_function)) scores_data %>% filter_function() else scores_data

  item_identifier <- if("phrase_name" %in% names(scores_data)) "phrase_name" else "item_id"
  item_identifier <- rlang::sym(item_identifier)

  data <- data %>%
    dplyr::filter( grepl("Phrase ", phrase_name) )

  scores_data <- scores_data %>%
    dplyr::filter( grepl("Phrase ", phrase_name) )

  stats <- data %>%
    dplyr::count(user_id, !! item_identifier, name = "NoTimesPractised") %>%
    dplyr::group_by(!! item_identifier) %>%
    dplyr::summarise(NoTimesPractised = sum(NoTimesPractised, na.rm = TRUE)) %>%
    dplyr::ungroup()

  scores <- compute_song_scores(scores_data, filter_function)

  res <- stats %>%
    dplyr::left_join(scores, by = as.character(item_identifier)) %>%
    dplyr::filter(!is.na(!!item_identifier))

  return(res)

}

# t <- get_trial_and_session_data(group_id = 5, filter_pseudo_anonymous_ids = TRUE, app_name_filter = "songbird")

get_users_in_group <- function(group_id) {
  dplyr::tbl(db_con, "users_groups") %>%
    dplyr::filter(group_id == !! group_id) %>%
    dplyr::pull(user_id)
}


last_week <- function(df) {
  df %>%
    dplyr::mutate(week = lubridate::week(Date) ) %>%
    dplyr::slice_max(week)
}

last_month <- function(df) {
  df %>%
    dplyr::mutate(month = lubridate::month(Date) ) %>%
    dplyr::slice_max(month)
}



