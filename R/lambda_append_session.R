


store_db_session_api <- function(experiment_id = NA,
                                 experiment_condition_id = NA,
                                 user_id,
                                 session_time_started = Sys.time(),
                                 shiny_app_name = "") {

  # Define the request body as a list
  request_body <- list(
    experiment_id = experiment_id,
    experiment_condition_id = experiment_condition_id,
    user_id = user_id,
    session_time_started = session_time_started,
    shiny_app_name = shiny_app_name
  )

  endpoint_wrapper(function_name = "append-session",
                   request_body = request_body)

}


# This is the function that is called when the endpoint
# is invoked
append_session <- function(experiment_condition_id = NA,
                           user_id,
                           session_time_started = Sys.time(),
                           experiment_id = NA,
                           user_info = NA,
                           shiny_app_name = "") {

  logging::loginfo("Inside append_session function")

  logging::loginfo("experiment_condition_id = %s", experiment_condition_id)

  logging::loginfo("user_id = %s", user_id)

  logging::loginfo("session_time_started= %s", session_time_started)

  logging::loginfo("experiment_id = %s", experiment_id)

  logging::loginfo("user_info: %s", user_info)

  logging::loginfo("shiny_app_name: %s", shiny_app_name)

  experiment_condition_id <- if(length(experiment_condition_id) == 0) NA_integer_ else experiment_condition_id
  experiment_id <- if(length(experiment_id) == 0) NA_integer_ else experiment_id


  response <- tryCatch({

    # Append session
    session_id <- db_append_session(db_con,
                                    experiment_condition_id = as.integer(experiment_condition_id),
                                    user_id = as.integer(user_id),
                                    session_time_started = session_time_started,
                                    experiment_id = as.integer(experiment_id),
                                    user_info = user_info,# In psychTestR, this will be NA and updated at the end
                                    shiny_app_name = shiny_app_name)
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
#' @param shiny_app_name
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
                              user_info = NA,
                              shiny_app_name = "") {

  logging::loginfo("db_append_session")

  experiment_condition_id <- if(length(experiment_condition_id) == 0) NA_integer_ else experiment_condition_id
  experiment_id <- if(length(experiment_id) == 0) NA_integer_ else experiment_id

  session_df <- tibble::tibble(experiment_condition_id = experiment_condition_id,
                               user_id = user_id,
                               psychTestR_session_id = psychTestR_session_id,
                               session_time_started = session_time_started,
                               experiment_id = experiment_id,
                               shiny_app_name = if(length(shiny_app_name) == 0) NA else "")

  session_id <- db_append_to_table(db_con, table = "sessions", data = session_df, primary_key_col = "session_id")

  logging::loginfo("is.scalar.character(user_info) %s", is.scalar.character(user_info))
  logging::loginfo("class(user_info) %s", class(user_info))
  logging::loginfo("length(user_info) %s", length(user_info))

  if(is.scalar.character(user_info)) {

    tryCatch({

      session_info_names <- dplyr::tbl(db_con, "session_info") %>%
        colnames()

      logging::loginfo("session_info_names: %s", session_info_names)

      # Append to session_info table
      user_info_parsed <- user_info %>%
        jsonlite::fromJSON(flatten = TRUE)

      location_info <- user_info_parsed$locationInfo %>%
        unlist() %>%
        as.list() %>%
        tibble::as_tibble()

      hardware_info <- user_info_parsed$hardwareInfo %>%
        unlist() %>%
        as.list() %>%
        tibble::as_tibble()

      if(nrow(location_info) > 0L && nrow(hardware_info) > 0L) {
        user_info_tbl <- cbind(location_info, hardware_info)
      } else if(nrow(location_info) > 0L && nrow(hardware_info) < 1L) {
        user_info_tbl <- location_info
      } else if(nrow(hardware_info) > 0L && nrow(location_info) < 1L) {
        user_info_tbl <- hardware_info
      }

      # First, to normalise all language names, collect and all name the same thing
      lng_cols <- which(grepl("language", names(user_info_tbl)))
      lng_names <- paste0("language", 1:length(lng_cols))
      colnames(user_info_tbl)[lng_cols] <- lng_names

      user_info_tbl <- user_info_tbl %>%
        dplyr::select(dplyr::any_of(session_info_names)) %>%
        dplyr::mutate(session_id = session_id) %>%
        dplyr::relocate(session_id)

      logging::loginfo("user_info_tbl: %s", user_info_tbl)
      logging::loginfo("nrow(user_info_tbl): %s", nrow(user_info_tbl))


      missing_names <- setdiff(session_info_names, names(user_info_tbl))

      logging::loginfo("missing_names: %s", missing_names)

      # Create a tibble with NAs for the missing values
      na_tibble <- tibble::tibble(!!!setNames(rep(list(NA_character_), length(missing_names)), missing_names))

      # Join to table
      user_info_tbl <- cbind(user_info_tbl, na_tibble)

      logging::loginfo("user_info_tbl updated: %s", user_info_tbl)

      # Reorder names
      user_info_tbl <- user_info_tbl %>%
        dplyr::select(dplyr::all_of(session_info_names))

      DBI::dbWriteTable(db_con, "session_info", user_info_tbl, row.names = FALSE, append = TRUE)
    }, error = function(err) {
        logging::logerror("Error: %s", err)
    })

  }

  session_id
}



# Create normalised session_info table

# db_con <- musicassessr_con()
# s <- tbl(db_con, "sessions") %>% collect()
# user_info <- s$user_info[[3194]]

# user_info_parsed <- user_info %>%
#   jsonlite::fromJSON() %>%
#   purrr::pluck(1)
#
# user_info_tbl <- user_info_parsed %>%
#   tibble::as_tibble() %>%
#   dplyr::mutate(session_id = 1L) %>%
#   dplyr::relocate(session_id)

# DBI::dbWriteTable(db_con, "session_info", user_info_tbl, row.names = FALSE, overwrite = TRUE)
