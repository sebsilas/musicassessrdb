

# t <- get_trial_and_session_data_api(111)
# db_con <- musicassessr_con()
# t <- get_trial_and_session_data(111)

# t <- get_trial_and_session_data(111)$scores_trial
# t <- get_trial_and_session_data(111, app_name_filter = "songbird")$scores_trial

get_trial_and_session_data_api <- function(user_id = NULL,
                                           group_id = NULL,
                                           trial_score_measure = "opti3",
                                           session_score_measure_arrhythmic = "mean_opti3_arrhythmic",
                                           session_score_measure_rhythmic = "mean_opti3_rhythmic",
                                           app_name_filter = NULL) {

  # Define the request body as a list
  request_body <- list(user_id = user_id,
                       group_id = group_id,
                       trial_score_measure = trial_score_measure,
                       session_score_measure_arrhythmic = session_score_measure_arrhythmic,
                       session_score_measure_rhythmic = session_score_measure_rhythmic,
                       app_name_filter = app_name_filter)

  endpoint_wrapper(function_name = "get-trial-and-session-data",
                   request_body = request_body)

}



# This is the function that is called when the endpoint
# is invoked
get_trial_and_session_data <- function(user_id = NULL,
                                       group_id = NULL,
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
  if(!is.null(user_id) && !is.null(group_id)) {
    logging::logerror("Only choose one of user_id or group_id")
    stop("Only choose one of user_id or group_id")
  }

  logging::loginfo("user_id = %s", user_id)
  logging::loginfo("group_id = %s", group_id)
  logging::loginfo("trial_score_measure = %s", trial_score_measure)
  logging::loginfo("session_score_measure_arrhythmic = %s", session_score_measure_arrhythmic)
  logging::loginfo("session_score_measure_rhythmic = %s", session_score_measure_rhythmic)
  logging::loginfo("app_name_filter = %s", app_name_filter)

  if(!is.null(group_id)) {
    user_id <- get_users_in_group(group_id)
  }


  response <- tryCatch({

    # Main logic

    # Get sessions associated with user
    sessions <- get_table(db_con, "sessions", collect = TRUE) %>%
        dplyr::filter(user_id %in% !! user_id) %>% # Note this could be multiple user_ids
        dplyr::mutate(Date = lubridate::as_date(session_time_started))  %>%
        dplyr::left_join(get_table(db_con, "users"), by = "user_id") %>%
        { if(is.character(app_name_filter)) dplyr::filter(., app_name == !! app_name_filter) else . } %>%
        { if(filter_pseudo_anonymous_ids) dplyr::filter(., filter_pseudo_anonymous_ids(username)) else . }

    session_ids <- sessions$session_id

    session_scores <- get_table(db_con, "scores_session", collect = TRUE) %>%
        dplyr::filter(!is.na(measure) & !is.na(score)) %>%
        dplyr::filter(measure %in% c(session_score_measure_arrhythmic, session_score_measure_rhythmic)) %>%
        dplyr::filter(session_id %in% !! session_ids) %>%
        dplyr::select(-scores_session_id) %>%
        dplyr::left_join(sessions, by = "session_id")

    trials <- compile_item_trials(db_con,
                                  session_id = session_ids,
                                  user_id = user_id,
                                  join_item_banks_on = TRUE) %>% # We need this for phrase_name.. but could timeout Lambdas..
              dplyr::mutate(Date = lubridate::as_date(session_time_started)) %>%
      dplyr::left_join(get_table(db_con, "users"), by = "user_id")


    scores_trial <- get_table(db_con, "scores_trial", collect = TRUE) %>%
      dplyr::select(-scores_trial_id) %>%
      dplyr::filter(measure == !! trial_score_measure) %>%
      dplyr::filter(!is.na(measure) & !is.na(score))

    if("phrase_name" %in% names(trials)) {

      # We assume this is a SongBird app
      scores_trial <- trials %>%
        { if(is.null(group_id)) . else dplyr::filter(., !email %in% c("sebsilas@gmail.com")) } %>%
        dplyr::mutate(phrase_name = dplyr::case_when(grepl("singpause", item_id) & is.na(phrase_name) ~ as.character(song_name), TRUE ~ as.character(phrase_name)),
                      songbird_type = dplyr::case_when(grepl("Berkowitz", item_id) ~ "Sing-Training",
                                                       grepl("singpause", item_id) & trial_paradigm == "simultaneous_recall" ~ "SingPause Singalong",
                                                       grepl("singpause", item_id) & trial_paradigm == "call_and_response" ~ "SingPause Solo",
                                                       TRUE ~ as.character(NA)
                                                       ) ) %>%
        dplyr::left_join(scores_trial, by = "trial_id") %>%
        dplyr::select(Date, user_id, username, trial_id, trial_time_started,
                      trial_time_completed, instrument, attempt, item_id, display_modality, phase,
                      rhythmic, stimulus_abs_melody, stimulus_durations, score, phrase_name, trial_paradigm, songbird_type, new_items_id, review_items_id)


      # For phrases with names we  remove the constraint that a phrase must have been played more than once to be returned

      review_melodies_over_time <- scores_trial %>%
        dplyr::group_by(Date, user_id, username, phrase_name, item_id, songbird_type) %>%
        dplyr::summarise(score = mean(score, na.rm = TRUE) ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(score = dplyr::case_when(is.na(score) ~ 0, TRUE ~ score)) %>%
        dplyr::filter(!is.na(phrase_name)) # Typically this will be a standard SAA/PBET test

    } else {
      scores_trial <- trials %>%
        dplyr::left_join(scores_trial, by = "trial_id") %>%
        dplyr::select(Date, user_id, username, trial_id, trial_time_started, trial_time_completed, instrument,
                      attempt, item_id, display_modality, phase,
                      rhythmic, stimulus_abs_melody, stimulus_durations, score, new_items_id, review_items_id)


      # Melodies we aggregate at the session level (i.e, can see improvements in the same day)

      review_melodies <- scores_trial %>%
        dplyr::count(user_id, stimulus_abs_melody, trial_time_started) %>%
        dplyr::count(user_id, stimulus_abs_melody) %>%
        dplyr::filter(n > 1 & !is.na(stimulus_abs_melody))

      stimulus_abs_melody <- review_melodies %>% dplyr::pull(stimulus_abs_melody)

      review_melodies_over_time <- scores_trial %>%
        dplyr::filter(stimulus_abs_melody %in% !! stimulus_abs_melody) %>%
        dplyr::group_by(user_id, username, trial_time_started, stimulus_abs_melody) %>%
        dplyr::summarise(score = mean(score, na.rm = TRUE) ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(score = dplyr::case_when(is.na(score) ~ 0, TRUE ~ score))
    }

    # But session scores we aggregate over the day
    # Aggregate across rhythmic and arrhythmic
    session_scores_agg <- session_scores %>%
      dplyr::group_by(user_id, username, Date) %>%
      dplyr::summarise(score = mean(score, na.rm = TRUE) ) %>%
      dplyr::ungroup()

    session_scores_rhythmic <- session_scores %>%
      dplyr::filter(grepl("_rhythmic", measure)) %>%
      dplyr::select(user_id, username, Date, session_id, session_time_started, session_time_completed, score) %>%
      dplyr::group_by(user_id, username, Date) %>%
      dplyr::summarise(score = mean(score, na.rm = TRUE)) %>%
      dplyr::ungroup()

    session_scores_arrhythmic <- session_scores %>%
      dplyr::filter(grepl("_arrhythmic", measure)) %>%
      dplyr::select(user_id, username, Date, session_id, session_time_started, session_time_completed, score) %>%
      dplyr::group_by(user_id, username, Date) %>%
      dplyr::summarise(score = mean(score, na.rm = TRUE)) %>%
      dplyr::ungroup()


    # Compute engagement with app measures:
    # - Time using app: i) overall, ii) in last month; iii) in last week
    # - Average scores of song; number of times students practised each song;


    user_stats <- sessions %>%
      dplyr::mutate(seconds_spent = as.numeric(session_time_completed - session_time_started),
                    seconds_spent = dplyr::case_when(seconds_spent < 0 ~ 0, TRUE ~ seconds_spent),
                    minutes_spent = seconds_spent / 60) %>%
      dplyr::group_by(user_id, username, Date) %>%
      dplyr::summarise(minutes_spent = sum(minutes_spent, na.rm = TRUE),
                       no_practice_sessions = dplyr::n() ) %>%
      dplyr::ungroup()


    # If group, then do some extra aggregation
    if(is.null(group_id)) {

      group_stats <- NA

    } else {

      # Average minutes practice per day

      overall_avg_minutes_per_day <- user_stats %>%
        dplyr::group_by(Date) %>%
        dplyr::summarise(AvgMinutesSpent = mean(minutes_spent, na.rm = TRUE) ) %>%
        dplyr::ungroup() %>%
        dplyr::summarise(AvgMinutesSpentPerDay = mean(AvgMinutesSpent, na.rm = TRUE)) %>%
        dplyr::pull()

      last_month_avg_minutes_per_day <- user_stats %>%
        last_month() %>%
        dplyr::group_by(Date) %>%
        dplyr::summarise(AvgMinutesSpent = mean(minutes_spent, na.rm = TRUE) ) %>%
        dplyr::ungroup() %>%
        dplyr::summarise(AvgMinutesSpentPerDay = mean(AvgMinutesSpent, na.rm = TRUE)) %>%
        dplyr::pull()

      last_week_avg_minutes_per_day <- user_stats %>%
        last_week() %>%
        dplyr::group_by(Date) %>%
        dplyr::summarise(AvgMinutesSpent = mean(minutes_spent, na.rm = TRUE) ) %>%
        dplyr::ungroup() %>%
        dplyr::summarise(AvgMinutesSpentPerDay = mean(AvgMinutesSpent, na.rm = TRUE)) %>%
        dplyr::pull()

      # Average number practice sessions per user

      overall_avg_no_practice_session_per_user <- user_stats %>%
        dplyr::group_by(user_id, username) %>%
        dplyr::summarise(overall_no_practice_sessions = sum(no_practice_sessions, na.rm = TRUE)) %>%
        dplyr::ungroup()

      last_month_avg_no_practice_session_per_user <- user_stats %>%
        last_month() %>%
        dplyr::group_by(user_id, username, month) %>%
        dplyr::summarise(last_month_no_practice_sessions = sum(no_practice_sessions, na.rm = TRUE)) %>%
        dplyr::ungroup()


      last_week_avg_no_practice_session_per_user <- user_stats %>%
        last_week() %>%
        dplyr::group_by(user_id, username, week) %>%
        dplyr::summarise(last_week_no_practice_sessions = sum(no_practice_sessions, na.rm = TRUE)) %>%
        dplyr::ungroup()


      # Average number of practice sessions

      overall_avg_no_practice_sessions <- overall_avg_no_practice_session_per_user %>%
        dplyr::summarise(avg_no_practice_session = mean(overall_no_practice_sessions, na.rm = TRUE) ) %>%
        dplyr::pull() %>%
        round()

      last_month_avg_no_practice_sessions <- last_month_avg_no_practice_session_per_user %>%
        dplyr::summarise(avg_no_practice_session_in_last_month = mean(last_month_no_practice_sessions, na.rm = TRUE) ) %>%
        dplyr::pull() %>%
        round()

      last_week_avg_no_practice_sessions <- last_week_avg_no_practice_session_per_user %>%
        dplyr::summarise(avg_no_practice_session_in_last_week = mean(last_week_no_practice_sessions, na.rm = TRUE) ) %>%
        dplyr::pull() %>%
        round()

      # Song scores

      overall_song_scores <- scores_trial %>%
        dplyr::group_by(user_id, username, phrase_name, songbird_type) %>%
        dplyr::slice_max(Date) %>%  # Get latest score
        dplyr::group_by(phrase_name, songbird_type) %>%
        dplyr::summarise(Score = round(mean(score, na.rm = TRUE) * 100) ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(Score = dplyr::case_when(is.nan(Score) ~ NA, TRUE ~ Score))

      overall_song_stats <- scores_trial %>%
        dplyr::count(user_id, phrase_name, songbird_type, name = "NoTimesPractised") %>%
        dplyr::group_by(phrase_name, songbird_type) %>%
        dplyr::summarise(NoTimesPractised = sum(NoTimesPractised, na.rm = TRUE)) %>%
        dplyr::ungroup()

      overall_song_stats <- overall_song_stats %>%
        dplyr::select(-songbird_type) %>%
        dplyr::left_join(overall_song_scores, by = "phrase_name") %>%
        dplyr::filter(!is.na(phrase_name))

      ## Last month

      last_month_song_scores <- scores_trial %>%
        last_month() %>%
        dplyr::group_by(user_id, username, phrase_name) %>%
        dplyr::slice_max(Date) %>%  # Get latest score
        dplyr::group_by(phrase_name) %>%
        dplyr::summarise(Score = round(mean(score, na.rm = TRUE) * 100) ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(Score = dplyr::case_when(is.nan(Score) ~ NA, TRUE ~ Score))

      last_month_song_stats <- scores_trial %>%
        last_month() %>%
        dplyr::count(user_id, phrase_name, name = "NoTimesPractised") %>%
        dplyr::group_by(phrase_name) %>%
        dplyr::summarise(NoTimesPractised = sum(NoTimesPractised, na.rm = TRUE)) %>%
        dplyr::ungroup()

      last_month_song_stats <- last_month_song_stats %>%
        dplyr::left_join(last_month_song_scores, by = "phrase_name")


      ## Last week

      last_week_song_scores <- scores_trial %>%
        last_week() %>%
        dplyr::group_by(user_id, username, phrase_name) %>%
        dplyr::slice_max(Date) %>%  # Get latest score
        dplyr::group_by(phrase_name) %>%
        dplyr::summarise(Score = round(mean(score, na.rm = TRUE) * 100) ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(Score = dplyr::case_when(is.nan(Score) ~ NA, TRUE ~ Score))

      last_week_song_stats <- scores_trial %>%
        last_week() %>%
        dplyr::count(user_id, phrase_name, name = "NoTimesPractised") %>%
        dplyr::group_by(phrase_name) %>%
        dplyr::summarise(NoTimesPractised = sum(NoTimesPractised, na.rm = TRUE)) %>%
        dplyr::ungroup()

      last_week_song_stats <- last_week_song_stats %>%
        dplyr::left_join(last_week_song_scores, by = "phrase_name")

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
        last_week_song_stats = last_week_song_stats)

    }

    # Remove nans
    if(is.data.frame(review_melodies_over_time)) {
      review_melodies_over_time <- review_melodies_over_time %>%
        dplyr::mutate(score = dplyr::case_when(is.nan(score) ~ 0, TRUE ~ score))
    }



    # Return response

   list(
      status = 200,
      message = paste0("You successfully got trials for user(s) ", paste0(user_id, collapse = ", ")),
      user_id = user_id,
      scores_trial = scores_trial,
      session_scores_rhythmic = session_scores_rhythmic,
      session_scores_arrhythmic = session_scores_arrhythmic,
      scores_session = session_scores_agg,
      review_melodies = review_melodies_over_time,
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


# db_con <- musicassessr_con()
# dat_filt <- get_trial_and_session_data(group_id = 5L, filter_pseudo_anonymous_ids = TRUE)
# tt2 <- dat_filt$group_stats$overall_song_stats
