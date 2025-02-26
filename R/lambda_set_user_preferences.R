


set_user_preferences_api <- function(user_id,
                                     preferences) {

  # Define the request body as a list
  request_body <- list(user_id = user_id,
                       preferences = preferences)

  endpoint_wrapper("set-user-preferences",
                   request_body = request_body)

}



set_user_preferences_lambda <- function(user_id,
                                        preferences) {

  logging::loginfo("Inside set_user_preferences lambda")

  logging::loginfo("user_id: %s", user_id)
  logging::loginfo("preferences: %s", preferences)


  # Return response

  response <- tryCatch({

    preferences <- preferences %>%
      tibble::as_tibble()

    logging::loginfo("preferences after tibble coercion: %s", preferences)

    preferences <- preferences %>%
      dplyr::mutate(user_id = user_id,
                    preference_change_date_time = Sys.time() )

      db_con <- musicassessr_con()

      # Write to table
      DBI::dbWriteTable(db_con,
                        name = 'user_preferences',
                        value = preferences,
                        row.names = FALSE,
                        append = TRUE)

      db_disconnect(db_con)



    list(
      status = 200,
      message = "You have successfully changed user preferences!"
    )

  }, error = function(err) {

    logging::logerror(err)
    stop(err)

    list(
      status = 400,
      message = "You did not manage to change user preferences."
    )

  })

  return(response)

}


# t <- set_user_preferences_lambda(user_id = 1L, preferences = tibble::tibble(selected_instrument = "Trumpet"))
