
youve_got_melodies_email_cron_script <- function() {

  db_con <- musicassessr_con()

  user_ids_to_email <- dplyr::tbl(db_con, "user_preferences") %>%
    dplyr::group_by(user_id)
    dplyr::slice_max(user_preferences_id) %>%
    dplyr::ungroup() %>%
    dplyr::filter(allow_daily_email_reminders == 1L) %>%
    dplyr::select(user_id) %>%
    dplyr::left_join(dplyr::tbl(db_con, "users")) %>%
    dplyr::select(email, username)

    db_disconnect(db_con)

    user_ids_to_email %>%
    purrr::pwalk(function(email, username) {

      send_youve_got_melodies_email(email,
                                    username,
                                    env =  c("dev", "prod"))

    })





}

send_youve_got_melodies_email <- function(email_address,
                                          username,
                                          env =  c("dev", "prod")) {

  env <- match.arg(env)


  if(env == "prod") {
    site_url <- "http://slonimsky.app"
  } else {
    env <- "http://dev.slonimsky.app"
  }


  email <- emayili::envelope(
    to = email_address,
    from = "slonimskyapp@gmail.com",
    subject = "You've got melodies!"
  ) %>% emayili::html(

    htmltools::tagList(

      htmltools::tags$head(
        htmltools::tags$style(htmltools::HTML("
        body { font-family: 'Arial', sans-serif; color: #333; }
        .email-wrapper { width: 100%; max-width: 600px; margin: 0 auto; }
        .header { background-color: #f4f4f4; padding: 10px 20px; text-align: center; }
        .content { padding: 20px; }
        .footer { background-color: #f4f4f4; padding: 10px 20px; text-align: center; font-size: 12px; }
        a { color: #1a82e2; }
      "))
      ),
      htmltools::tags$body(
        htmltools::tags$div(class = "email-wrapper",
                            htmltools::tags$div(class = "header",
                                                htmltools::tags$img(src = "https://static.wixstatic.com/media/51ea6c_bba2e6054f824579bb36286ced314d53~mv2.png/v1/fill/w_392,h_110,al_c,q_85,usm_0.66_1.00_0.01,enc_auto/CORP%20SITE%20LOGO%20SoundOut-Transparent-Grey.png", alt = "Company Logo", width = "120")
                            ),
                            htmltools::tags$div(class = "content",

                                                htmltools::tags$p(paste0("Hi ", username, ",")),
                                                htmltools::tags$p("We just wanted to let you know, you've got some new melodies to learn today, and some that you've already learned to review."),
                                                htmltools::tags$p("Click ", htmltools::tags$a("here", href = site_url), " to login."),
                                                htmltools::tags$p("Best Regards,", htmltools::tags$br(), "The Slonimsky Team")
                            ),
                            htmltools::tags$div(class = "footer",
                                                "If you want to stop receiving these emails, login and adjust your account settings to unsubscribe.")
        )
      )
    )
  )

  # Now create an object to communicate with the server.

  smtp <- emayili::server(
    host = "smtp.gmail.com",
    port = 465,
    username = Sys.getenv("EMAIL"),
    password = Sys.getenv("EMAIL_PW")
  )

  # Finally send the message.


  smtp(email, verbose = TRUE)

}
