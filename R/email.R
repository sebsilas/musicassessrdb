

# email_slonimsky_lambda("Bug Report", "sebsilas@gmail.com", "test")

email_slonimsky_lambda <- function(type, email, message) {

  response <- tryCatch({

    logging::loginfo('type: %s', type)
    logging::loginfo('email: %s', email)
    logging::loginfo('message: %s', message)

    # Authenticate Gmail API using OAuth 2.0
    options(gargle_oauth_email = "slonimskyapp@gmail.com")
    gmailr::gm_auth_configure(path = Sys.getenv("OAUTH_SLONIMSKY") )
    gmailr::gm_auth()

    # Create Email Content
    email_content <- gmailr::gm_mime() %>%
      gmailr::gm_to("slonimskyapp@gmail.com") %>%
      gmailr::gm_from("Slonimsky App <slonimskyapp@gmail.com>") %>%
      gmailr::gm_subject("New Slonimsky Email!") %>%
      gmailr::gm_html_body(
        paste0(
          "<p>You have received a new email via Slonimsky</p>",
          "<p><strong>Type:</strong> ", type, "</p>",
          "<p><strong>Email:</strong> ", email, "</p>",
          "<p><strong>Message:</strong> ", message, "</p>"
        )
      )

    # Send email
    gmailr::gm_send_message(email_content)

    # Return response
    list(
      status = 200,
      message = "You have successfully sent an email via Slonimsky!"
    )

  }, error = function(err) {
    logging::logerror(err)
    list(
      status = 400,
      message = "Something went wrong"
    )
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

        send_youve_got_melodies_email(email,
                                      username,
                                      env =  c("dev", "prod"))

      })





}

# send_youve_got_melodies_email("sebsilas@gmail.com", "seb_slonim")

send_youve_got_melodies_email <- function(email_address,
                                          username,
                                          env = c("dev", "prod")) {

  env <- match.arg(env)

  # Define the site URL based on environment
  site_url <- if (env == "prod") {
    "http://slonimsky.app"
  } else {
    "http://dev.slonimsky.app"
  }

  response <- tryCatch({

    logging::loginfo('Sending "You\'ve Got Melodies" email to: %s', email_address)

    # Authenticate Gmail API using OAuth 2.0
    options(gargle_oauth_email = "slonimskyapp@gmail.com")
    gmailr::gm_auth_configure(path = system.file("client_secret_438502282297-kfq471tjn2bpqjonkgelnrgf0mqj6c67.apps.googleusercontent.com.json", package = "musicassessrdb"))
    gmailr::gm_auth()

    # Generate Email HTML Content
    email_html <- paste0(
      "<div style='font-family: Arial, sans-serif; color: #333; width: 100%; max-width: 600px; margin: 0 auto;'>",
      "<div style='padding: 10px 20px; text-align: center;'>",
      "<img src='https://musicassessr.com/assets/img/slonim.png' alt='Company Logo' width='80'>",
      "</div>",
      "<div style='padding: 20px;'>",
      "<p>Hi <strong>", username, ",</strong></p>",
      "<p>We just wanted to let you know that you've got some new melodies to learn as well as some previous melodies to review.</p>",
      "<p>Click <a href='", site_url, "' style='color: #1a82e2;'>here</a> to login.</p>",
      "<p>Best Regards,<br>The Slonimsky Team</p>",
      "</div>",
      "<div style='background-color: #f4f4f4; padding: 10px 20px; text-align: center; font-size: 12px;'>",
      "If you want to stop receiving these emails, login and adjust your account settings to unsubscribe.",
      "</div>",
      "</div>"
    )

    # Create Email Content
    email_content <- gmailr::gm_mime() %>%
      gmailr::gm_to(email_address) %>%
      gmailr::gm_from("Slonimsky App <slonimskyapp@gmail.com>") %>%
      gmailr::gm_subject("You've got melodies!") %>%
      gmailr::gm_html_body(email_html)

    # Send Email
    gmailr::gm_send_message(email_content)

    # Return response
    list(
      status = 200,
      message = paste("Email successfully sent to", email_address)
    )

  }, error = function(err) {
    logging::logerror(err)
    list(
      status = 400,
      message = "Something went wrong while sending the email."
    )
  })

  return(response)
}

