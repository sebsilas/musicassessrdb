



get_users_api <- function() {

  # Define the request body as a list
  request_body <- list()

  endpoint_wrapper(function_name = "get-users",
                   request_body = request_body)

}



# This is the function that is called when the endpoint
# is invoked
get_user_ids <- function() {


  logging::loginfo("Inside get_users function")

  response <- tryCatch({

    # Main logic

    user_ids <- get_table(db_con, "users", collect = TRUE) %>%
      dplyr::pull(user_id)

    # Return response

    list(
      status = 200,
      message = "You successfully got users",
      user_id = user_ids
    )


  }, error = function(err) {

      logging::logerror(err)

      list(
        status = 400,
        message = "Something went wrong",
        user_ids = NA
      )
  })

}
