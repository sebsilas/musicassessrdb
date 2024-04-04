

store_db_session_api <- function(condition_id = NA,
                                 user_id,
                                 psychTestR_session_id,
                                 time_started = Sys.time(),
                                 experiment_id = NA) {

  # Define the request body as a list
  request_body <- list(
    condition_id = condition_id,
    user_id = user_id,
    psychTestR_session_id = psychTestR_session_id,
    time_started = time_started,
    experiment_id = experiment_id
  )

  endpoint_wrapper(function_name = "append-session",
                   request_body = request_body)

}



# This is the function that is called when the endpoint
# is invoked
append_session <- function(condition_id = NA,
                           user_id,
                           psychTestR_session_id,
                           time_started = Sys.time(),
                           experiment_id = NA) {

  logging::loginfo("Inside append_session function")

  logging::loginfo("condition_id = %s", condition_id)

  logging::loginfo("user_id = %s", user_id)

  logging::loginfo("psychTestR_session_id = %s", psychTestR_session_id)

  logging::loginfo("time_started= %s", time_started)

  logging::loginfo("experiment_id = %s", experiment_id)

  condition_id <- if(length(condition_id) == 0) NA_integer_ else condition_id
  experiment_id <- if(length(experiment_id) == 0) NA_integer_ else experiment_id


  response <- tryCatch({

    # Append session
    session_id <- db_append_session(db_con,
                                    condition_id = as.integer(condition_id),
                                    user_id = as.integer(user_id),
                                    psychTestR_session_id = psychTestR_session_id,
                                    time_started = time_started,
                                    experiment_id = as.integer(experiment_id))

    # Return response

   list(
      status = 200,
      message = "You have successfully added a session!",
      session_id = session_id
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



#' Store session information in sessions table
#'
#' @param db_con
#' @param condition_id
#' @param user_id
#' @param psychTestR_session_id
#' @param time_started
#' @param experiment_id
#'
#' @return
#' @export
#'
#' @examples
db_append_session <- function(db_con,
                              condition_id = NA,
                              user_id,
                              psychTestR_session_id,
                              time_started = Sys.time(),
                              experiment_id = NA) {


  condition_id <- if(length(condition_id) == 0) NA_integer_ else condition_id
  experiment_id <- if(length(experiment_id) == 0) NA_integer_ else experiment_id

  session_df <- tibble::tibble(condition_id = condition_id,
                               user_id = user_id,
                               psychTestR_session_id = psychTestR_session_id,
                               time_started = time_started,
                               experiment_id = experiment_id)

  session_id <- db_append_to_table(db_con, table = "sessions", data = session_df, primary_key_col = "session_id")
  session_id
}


