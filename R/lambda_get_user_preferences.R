


get_user_preferences_api <- function(user_id) {

  # Define the request body as a list
  request_body <- list(user_id = user_id)

  endpoint_wrapper(endpoint = "https://0xe6kj7po4.execute-api.eu-west-2.amazonaws.com/get-user-preferences",
                   request_body = request_body)

}



get_user_preferences_lambda <- function(user_id) {

  logging::loginfo("Inside get_user_preferences lambda")

  logging::loginfo("user_id: %s", user_id)

  # Return response

  response <- tryCatch({

      msg <- "You successfully got user preferences!"

      db_con <- musicassessr_con()

      user_preferences <- dplyr::tbl(db_con, "user_preferences") %>%
        dplyr::filter(user_id == !! user_id) %>%
        dplyr::slice_max(user_preferences_id) %>%
        dplyr::select(selected_instrument, allow_daily_email_reminders, music_input_device) %>% # At least for now, this is all we want.
        dplyr::collect()

      if(nrow(user_preferences) < 1L) {
        user_preferences <- NA
        msg <- "There were no user preferences for this user yet."
      }


    list(
      status = 200,
      message = msg,
      user_preferences = user_preferences
    )

  }, error = function(err) {

    logging::logerror(err)

    list(
      status = 400,
      message = "You did not manage to get user preferences.",
      user_preferences = NA
    )

  })

  return(response)

}

# Init table

# db_con <- musicassessr_con()

# DBI::dbExecute(db_con, "
#   CREATE TABLE IF NOT EXISTS user_preferences (
#     user_preferences_id SERIAL PRIMARY KEY,
#     user_id INTEGER NOT NULL,
#     selected_instrument TEXT NOT NULL,
#     preference_change_date_time TIMESTAMP NOT NULL
#   );
# ")
#
#
# pref_init <- tibble::tibble(
#   user_id = 1L,
#   selected_instrument = "Piano",
#   preference_change_date_time = Sys.time()
# )
#
# DBI::dbWriteTable(db_con, "user_preferences", pref_init, overwrite = FALSE, append = TRUE, row.names = FALSE)
#
# # Add second datapoint for testing
#
# pref_2 <- tibble::tibble(
#   user_id = 1L,
#   selected_instrument = "Tenor Saxophone",
#   preference_change_date_time = Sys.time()
# )
#
# DBI::dbWriteTable(db_con, "user_preferences", pref_2, overwrite = FALSE, append = TRUE, row.names = FALSE)
#
# db_disconnect(db_con)
