

store_db_demographics_api <- function(user_id, demographics_df) {

  logging::loginfo("store_db_demographics_api")
  logging::loginfo('dim(demographics_df)')
  logging::loginfo(dim(demographics_df))

  stopifnot(
    is.data.frame(demographics_df),
    dim(demographics_df)[1] == 1L,
    dim(demographics_df)[2] %in% c(84L, 85L, 86L)
  )

  # Define the request body as a list
  request_body <- list(user_id = user_id,
                       demographics = as.list(demographics_df))

  endpoint_wrapper(function_name = "append-demographics",
                   request_body = request_body)

}






# This is the function that is called when the endpoint
# is invoked
append_demographics <- function(user_id, demographics_list) {

  logging::loginfo("Appending to demographics table")

  logging::loginfo("user_id = %s", user_id)

  logging::loginfo("demographics_df = %s", demographics_list)

  demographics_df <- tibble::as_tibble(demographics_list)

  # Make sure this is done before the following step:

  demographics_df[demographics_df == 987654321] <- NA

  if(!'DEG.q3' %in% names(demographics_df)) {
    demographics_df <- demographics_df %>%
      dplyr::mutate(DEG.q3 = NA_character_)
  }

  if(!'SES.q4' %in% names(demographics_df)) {
    demographics_df <- demographics_df %>%
      dplyr::mutate(SES.q4 = NA_character_)
  }

  if(!'SES.q5' %in% names(demographics_df)) {
    demographics_df <- demographics_df %>%
      dplyr::mutate(SES.q5 = NA_character_)
  }


  # Return response

  response <- tryCatch({

    # Append condition

    db_append_demographics(db_con, user_id, demographics_df)

    list(
      status = 200,
      message = paste0("You have successfully added an entry to the demographic table for user_id ", user_id, "!")
      )

  }, error = function(err) {

    logging::logerror(err)

    list(
      status = 400,
      message = "Something went wrong!",
      experiment_condition_id = NA
    )

  })

  return(response)

}




#' Append a condition to the conditions table
#'
#' @param db_con The DB connection.
#' @param user_id The user ID.
#' @param demographics_df A 1 x 86 dataframe of the demographic information.
#'
#' @return
#' @export
#'
#' @examples
db_append_demographics <- function(db_con, user_id, demographics_df) {

  logging::loginfo('In db_append_demographics')

  logging::loginfo('dim(demographics_df)')
  logging::loginfo(dim(demographics_df))

  stopifnot(
    is.integer(user_id),
    is.data.frame(demographics_df),
    dim(demographics_df)[1] == 1L,
    dim(demographics_df)[2] %in% c(85L, 86L, 87L)
  )


  tryCatch({

    now <- as.POSIXct(format(Sys.time()),tz="UTC") # Timezone must be in UTC

    demographics_df <- cbind(tibble::tibble(user_id = user_id, time_added = now ), demographics_df) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        session.time_started = lubridate::as_datetime(session.time_started),
        session.current_time = lubridate::as_datetime(session.current_time)
      )

    check_id_exists(db_con, table_name = "users", id_col = "user_id", id = user_id)

    db_append_to_table(db_con, table = "demographics", data = demographics_df, primary_key_col = "user_id")


  }, error = function(err) {
    logging::logerror(err)
  })



  return(user_id)
}
