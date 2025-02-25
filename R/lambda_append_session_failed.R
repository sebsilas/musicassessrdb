

#' Append a failed session to the DB with user info
#'
#' @param session_time_failed
#' @param user_info
#'
#' @return
#' @export
#'
#' @examples
append_failed_session_api <- function(session_time_failed = Sys.time(),
                                      user_info = NA) {

  # Define the request body as a list
  request_body <- list(
    session_time_failed = session_time_failed,
    user_info = user_info
  )

  endpoint_wrapper(function_name = "append-failed-session",
                   request_body = request_body)

}



# This is the function that is called when the endpoint
# is invoked
append_failed_session <- function(session_time_failed = Sys.time(),
                                  user_info = NA) {

  logging::loginfo("Inside append_session_failed function")

  logging::loginfo("session_time_failed = %s", session_time_failed)

  logging::loginfo("user_info = %s", user_info)

  logging::loginfo("class(user_info) = %s", class(user_info))

  if(!is.character(user_info)) {
    logging::loginfo("user_info is not character, coerce to character")
    user_info <- user_info %>% jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE)
    logging::loginfo("class(user_info) = %s", class(user_info))
  }


  response <- tryCatch({

    # Append session_failed
    failed_session_df <- tibble::tibble(session_time_failed = session_time_failed,
                                         user_info = user_info)

    failed_sessions_id <- db_append_to_table(db_con, table = "failed_sessions", data = failed_session_df, primary_key_col = "failed_sessions_id")

    # Return response

   list(
      status = 200,
      message = "You have successfully added a failed session!",
      failed_sessions_id = failed_sessions_id
    )


  }, error = function(err) {
    logging::logerror(err)
     list(
        status = 400,
        message = "Something went wrong",
        session_id = integer(0)
      )
  })


  return(response)

}


