

store_db_session_api <- function(experiment_id = NA,
                                 experiment_condition_id = NA,
                                 user_id,
                                 session_time_started = Sys.time()) {

  # Define the request body as a list
  request_body <- list(
    experiment_id = experiment_id,
    experiment_condition_id = experiment_condition_id,
    user_id = user_id,
    session_time_started = session_time_started
  )

  endpoint_wrapper(function_name = "append-session",
                   request_body = request_body)

}



# This is the function that is called when the endpoint
# is invoked
append_session <- function(experiment_condition_id = NA,
                           user_id,
                           session_time_started = Sys.time(),
                           experiment_id = NA) {

  logging::loginfo("Inside append_session function")

  logging::loginfo("experiment_condition_id = %s", experiment_condition_id)

  logging::loginfo("user_id = %s", user_id)

  logging::loginfo("session_time_started= %s", session_time_started)

  logging::loginfo("experiment_id = %s", experiment_id)

  experiment_condition_id <- if(length(experiment_condition_id) == 0) NA_integer_ else experiment_condition_id
  experiment_id <- if(length(experiment_id) == 0) NA_integer_ else experiment_id


  response <- tryCatch({

    # Append session
    session_id <- db_append_session(db_con,
                                    experiment_condition_id = as.integer(experiment_condition_id),
                                    user_id = as.integer(user_id),
                                    session_time_started = session_time_started,
                                    experiment_id = as.integer(experiment_id),
                                    user_info = NA) # We do this at the end

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
#' @param experiment_condition_id
#' @param user_id
#' @param psychTestR_session_id
#' @param session_time_started
#' @param experiment_id
#' @param user_info
#'
#' @return
#' @export
#'
#' @examples
db_append_session <- function(db_con,
                              experiment_condition_id = NA,
                              user_id,
                              psychTestR_session_id = NA,
                              session_time_started = Sys.time(),
                              experiment_id = NA,
                              user_info = NA) {


  experiment_condition_id <- if(length(experiment_condition_id) == 0) NA_integer_ else experiment_condition_id
  experiment_id <- if(length(experiment_id) == 0) NA_integer_ else experiment_id

  session_df <- tibble::tibble(experiment_condition_id = experiment_condition_id,
                               user_id = user_id,
                               psychTestR_session_id = psychTestR_session_id,
                               session_time_started = session_time_started,
                               experiment_id = experiment_id,
                               user_info = user_info)

  session_id <- db_append_to_table(db_con, table = "sessions", data = session_df, primary_key_col = "session_id")
  session_id
}


