
create_session_token_api <- function(user_id,
                                     expiration_in_seconds = 600) {

  # Define the request body as a list
  request_body <- list(user_id = user_id,
                       expiration_in_seconds = expiration_in_seconds)

  res <- endpoint_wrapper(function_name = "create-session-token",
                          request_body = request_body)

  logging::loginfo(res$message)


  return(res)

}



create_session_token_lambda <- function(user_id,
                                        expiration_in_seconds = 600) {

  # We give this one a _lambda suffix to differentiate it from the musicassessr function of the same name, otherwise we have namespacing issues

  logging::loginfo("Inside create_session_token_lambda function")

  logging::loginfo("user_id = %s", user_id)
  logging::loginfo("expiration_in_seconds = %s", expiration_in_seconds)


  stopifnot(
    length(user_id) == 1,
    is.scalar.numeric(expiration_in_seconds)
  )


  # Return response

  response <- tryCatch({

    token <- db_append_session_tokens(as.integer(user_id), expiration_in_seconds)

    list(status = 200,
         message = paste0("You have successfully created a session token for ", user_id, "!"),
         session_token = token)


  }, error = function(err) {

    logging::logerror(err)

    list(
      status = 400,
      message = "Something went wrong!",
      sample = NA
    )

  })

  return(response)

}



#' Append a session token to the session_tokens table
#'
#' @param user_id
#' @param expiration_in_seconds
#'
#' @return
#' @export
#'
#' @examples
db_append_session_tokens <- function(user_id, expiration_in_seconds = 600) {

  stopifnot(
    is.integer(user_id),
    is.numeric(expiration_in_seconds)
  )

  token <- create_session_token()

  created_at <- lubridate::as_datetime(Sys.time())

  expires <- created_at + lubridate::seconds(expiration_in_seconds)


  session_tokens_df <- tibble::tibble(
    user_id = user_id,
    token = token,
    created_at = lubridate::as_datetime(created_at),
    expires = lubridate::as_datetime(expires),
    active = TRUE
    )


  db_append_to_table(db_con, table = "session_tokens", data = session_tokens_df)

  return(token)
}

