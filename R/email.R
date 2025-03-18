
# send_daily_summary()

send_email <- function(subject, body) {
  smtp_password <- Sys.getenv("SLONIMSKY_EMAIL_PW")
  if (smtp_password == "") {
    stop("SLONIMSKY_EMAIL_PW environment variable is not set or empty.")
  }

  smtp <- emayili::server(
    host = "smtp.gmail.com",
    port = 465,
    username = "slonimskyapp@gmail.com",
    password = smtp_password
  )

  email_content <- emayili::envelope() %>%
    emayili::from('"musicassessr" <slonimskyapp@gmail.com>') %>%
    emayili::to("sebsilas@gmail.com") %>%
    emayili::subject(subject) %>%
    emayili::html(body)

  smtp(email_content, verbose = TRUE)
}

send_daily_summary <- function() {

  db_con <- musicassessr_con()

  # Get yesterday's date
  today <- Sys.Date()
  yesterday <- today - 1

  # Retrieve overall session statistics
  session_stats <- db_con %>%
    dplyr::tbl("sessions") %>%
    dplyr::filter(lubridate::as_date(session_time_started) == yesterday) %>%
    dplyr::summarise(
      total_sessions = dplyr::n(),
      unique_users = dplyr::n_distinct(user_id),
      .groups = "drop"
    ) %>%
    dplyr::collect()

  # Retrieve overall trial statistics
  trial_stats <- db_con %>%
    dplyr::tbl("trials") %>%
    dplyr::filter(lubridate::as_date(trial_time_started) == yesterday) %>%
    dplyr::summarise(
      total_trials = dplyr::n(),
      unique_trial_types = dplyr::n_distinct(trial_paradigm),
      .groups = "drop"
    ) %>%
    dplyr::collect()

  # Retrieve per-app statistics
  app_session_stats <- db_con %>%
    dplyr::tbl("sessions") %>%
    dplyr::left_join(dplyr::tbl(db_con, "users", by = "user_id")) %>%
    dplyr::mutate(app_name = dplyr::case_when(is.na(app_name) | !app_name %in% c("slonimsky", "songbird") ~ "Other", TRUE ~ app_name)) %>%
    dplyr::filter(lubridate::as_date(session_time_started) == yesterday) %>%
    dplyr::group_by(app_name) %>%
    dplyr::summarise(
      total_sessions = dplyr::n(),
      unique_users = dplyr::n_distinct(user_id),
      .groups = "drop"
    ) %>%
    dplyr::collect()

  app_trial_stats <- db_con %>%
    dplyr::tbl("trials") %>%
    dplyr::left_join(dplyr::tbl(db_con, "sessions", by = "session_id")) %>%
    dplyr::left_join(dplyr::tbl(db_con, "users", by = "user_id")) %>%
    dplyr::mutate(app_name = dplyr::case_when(is.na(app_name) | !app_name %in% c("slonimsky", "songbird") ~ "Other", TRUE ~ app_name)) %>%
    dplyr::filter(lubridate::as_date(trial_time_started) == yesterday) %>%
    dplyr::group_by(app_name) %>%
    dplyr::summarise(
      total_trials = dplyr::n(),
      unique_trial_types = dplyr::n_distinct(trial_paradigm),
      .groups = "drop"
    ) %>%
    dplyr::collect()

  # Create HTML for overall stats
  overall_table <- paste0(
    "<h2>Overall Summary</h2>",
    "<table border='1'><tr><th>Metric</th><th>Value</th></tr>",
    "<tr><td>Total Sessions</td><td>", session_stats$total_sessions, "</td></tr>",
    "<tr><td>Unique Users</td><td>", session_stats$unique_users, "</td></tr>",
    "<tr><td>Total Trials</td><td>", trial_stats$total_trials, "</td></tr>",
    "<tr><td>Unique Trial Types</td><td>", trial_stats$unique_trial_types, "</td></tr>",
    "</table><br><hr><br>"
  )

  # Create per-app tables
  app_tables <- lapply(unique(app_session_stats$app_name), function(app) {
    app_sessions <- app_session_stats %>% dplyr::filter(app_name == app)
    app_trials <- app_trial_stats %>% dplyr::filter(app_name == app)

    paste0(
      "<h3>App: ", app, "</h3>",
      "<table border='1'><tr><th>Metric</th><th>Value</th></tr>",
      "<tr><td>Total Sessions</td><td>", app_sessions$total_sessions, "</td></tr>",
      "<tr><td>Unique Users</td><td>", app_sessions$unique_users, "</td></tr>",
      "<tr><td>Total Trials</td><td>", app_trials$total_trials, "</td></tr>",
      "<tr><td>Unique Trial Types</td><td>", app_trials$unique_trial_types, "</td></tr>",
      "</table><br>"
    )
  })

  # Combine all tables into the email body
  email_body <- paste0("<h2>Daily MusicAssessr Report - ", today, "</h2>", overall_table, paste0(app_tables, collapse = ""))

  # Send email with the stats table
  send_email(subject = paste("Daily MusicAssessr Report -", today), body = email_body)

  # Close DB connection
  db_disconnect(db_con)
}




# email_slonimsky_lambda("Bug Report", "sebsilas@gmail.com", "test")

email_slonimsky_lambda <- function(type, email, message, user_id = NA) {
  response <- tryCatch({
    logging::loginfo('type: %s', type)
    logging::loginfo('email: %s', email)
    logging::loginfo('message: %s', message)
    logging::loginfo('user_id: %s', user_id)

    # Retrieve SMTP password securely
    smtp_password <- Sys.getenv("SLONIMSKY_EMAIL_PW")

    if (smtp_password == "") {
      stop("SLONIMSKY_EMAIL_PW environment variable is not set or empty.")
    }

    # Set up SMTP server connection using emayili::server
    smtp <- emayili::server(
      host = "smtp.gmail.com",
      port = 465,  # Using SSL (not TLS)
      username = "slonimskyapp@gmail.com",
      password = smtp_password
    )

    # Create Email Content
    email_html <- paste0(
      "<p>You have received a new email via Slonimsky</p>",
      "<p><strong>Type:</strong> ", type, "</p>",
      "<p><strong>Email:</strong> ", email, "</p>",
      "<p><strong>Message:</strong> ", message, "</p>",
      "<p><strong>User ID:</strong> ", user_id, "</p>"
    )

    # Create the email message using emayili::envelope
    email_content <- emayili::envelope() %>%
      emayili::from("slonimskyapp@gmail.com") %>%
      emayili::to("slonimskyapp@gmail.com") %>%
      emayili::subject("New Slonimsky Email!") %>%
      emayili::html(email_html)

    # Send email
    smtp(email_content)

    # Store email record in the database
    email_data <- tibble::tibble(
      type = type,
      email = email,
      message = message,
      date_sent = Sys.time(),
      user_id = user_id
    )

    db_con <- musicassessr_con()

    DBI::dbWriteTable(
      db_con,
      "slonimsky_contact_form",
      email_data,
      row.names = FALSE,
      append = TRUE
    )

    db_disconnect(db_con)

    # Return response
    list(status = 200, message = "You have successfully sent an email via Slonimsky!")

  }, error = function(err) {
    logging::logerror(err)
    list(status = 400, message = "Something went wrong")
  })

  return(response)
}



youve_got_melodies_email_cron_script <- function() {

  db_con <- musicassessr_con()

  user_ids_to_email <- dplyr::tbl(db_con, "user_preferences") %>%
    dplyr::group_by(user_id) %>%
    dplyr::slice_max(user_preferences_id) %>%
    dplyr::ungroup() %>%
    dplyr::filter(allow_daily_email_reminders == 1L) %>%
    dplyr::select(user_id) %>%
    dplyr::left_join(dplyr::tbl(db_con, "users"), by = "user_id") %>%
    dplyr::select(email, username) %>%
    dplyr::filter(!is.na(email)) %>%
    dplyr::collect()

  db_disconnect(db_con)

  user_ids_to_email %>%
    purrr::pwalk(function(email, username) {
      send_youve_got_melodies_email(email, username, env =  c("dev", "prod"))

    })





}

# send_youve_got_melodies_email("sebsilas@gmail.com", "seb_slonim")

send_youve_got_melodies_email <- function(email_address, username, env = c("dev", "prod")) {
  env <- match.arg(env)

  # Define the site URL based on environment
  site_url <- if (env == "prod") {
    "http://slonimsky.app"
  } else {
    "http://dev.slonimsky.app"
  }

  response <- tryCatch({
    logging::loginfo('Sending "You\'ve Got Melodies" email to: %s', email_address)

    # Retrieve SMTP password from environment variable
    smtp_password <- Sys.getenv("SLONIMSKY_EMAIL_PW")

    if (smtp_password == "") {
      stop("SLONIMSKY_EMAIL_PW environment variable is not set or empty.")
    }

    # Set up SMTP server connection using emayili::server
    smtp <- emayili::server(
      host = "smtp.gmail.com",
      port = 465,  # Secure SSL connection
      username = "slonimskyapp@gmail.com",
      password = smtp_password
    )

    # Generate Email HTML Content
    email_html <- paste0(
      "<div style='font-family: Arial, sans-serif; color: #333; width: 100%; max-width: 600px; margin: 0 auto;'>",
      "<div style='padding: 10px 20px; text-align: center;'>",
      "<img src='https://musicassessr.com/assets/img/slonim.png' alt='Company Logo' width='80'>",
      "</div>",
      "<div style='padding: 20px;'>",
      "<p>Hey <strong>", username, ",</strong></p>",
      "<p>We just wanted to let you know that you've got some new melodies to learn as well as some previous melodies to review.</p>",
      "<p>Click <a href='", site_url, "' style='color: #1a82e2;'>here</a> to login.</p>",
      "<p>Best Regards,<br>The Slonimsky Team</p>",
      "</div>",
      "<div style='background-color: #f4f4f4; padding: 10px 20px; text-align: center; font-size: 12px;'>",
      "If you want to stop receiving these emails, login and adjust your account settings to unsubscribe.",
      "</div>",
      "</div>"
    )

    # Create email message using emayili::envelope
    email <- emayili::envelope() %>%
      emayili::from('"Slonimsky App" <slonimskyapp@gmail.com>') %>%
      emayili::to(email_address) %>%
      emayili::subject("You've got melodies!") %>%
      emayili::html(email_html)

    # Send email using SMTP server
    smtp(email, verbose = TRUE)

    # Return response
    list(
      status = 200,
      message = paste("Email successfully sent to", email_address)
    )

  }, error = function(err) {
    logging::logerror(err)
    list(status = 400, message = "Something went wrong while sending the email.")
  })

  return(response)
}



# Init DB

# email_init <- tibble::tibble(type = "Bug Report",
#                              email = "test@test.com",
#                              message = "test",
#                              date_sent = Sys.time(),
#                              user_id = 1L)

# db_con <- musicassessr_con()

# DBI::dbWriteTable(db_con, "slonimsky_contact_form", email_init, row.names = FALSE, overwrite = TRUE)
