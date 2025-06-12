

#' Append singpause survey completion API
#'
#' @param user_id
#' @param singpause_id
#' @param psychTestR_id
#' @param type
#'
#' @returns
#' @export
#'
#' @examples
append_singpause_survey_completion_api <- function(user_id,
                                                   singpause_id, # Same as username in users
                                                   psychTestR_id,
                                                   type = c("pretest", "start_pretest", "posttest")) {

  type <- match.arg(type)

  # Define the request body as a list
  request_body <- list(user_id = user_id,
                       singpause_id = singpause_id,
                       psychTestR_id = psychTestR_id,
                       type = type)

  res <- endpoint_wrapper(function_name = "append-singpause-survey-completion",
                          request_body = request_body)

  logging::loginfo(res$message)


  return(res)

}


#' Append singpause survey completion
#'
#' @param user_id
#' @param singpause_id
#' @param psychTestR_id
#' @param type
#'
#' @returns
#' @export
#'
#' @examples
append_singpause_survey_completion <- function(user_id,
                                               singpause_id, # Same as username in users
                                               psychTestR_id,
                                               type = c("pretest", "posttest", "start_pretest")) {

  type <- match.arg(type)

  stopifnot(
    type %in% c("pretest", "posttest", "start_pretest")
  )

  logging::loginfo("Inside append_singpause_survey_completion function")

  logging::loginfo("user_id: %s", user_id)
  logging::loginfo("singpause_id: %s", singpause_id)
  logging::loginfo("psychTestR_id: %s", psychTestR_id)
  logging::loginfo("type: %s", type)

  complete_time <- Sys.time()


  response <- tryCatch({

    df <- tibble::tibble(user_id = user_id,
                         singpause_id = singpause_id,
                         psychTestR_id = psychTestR_id,
                         type = type,
                         complete_time = complete_time,
                         complete = TRUE)

    db_append_to_table(db_con, table = "singpause_survey_completions", data = df)

    # Return response

   list(
      status = 200,
      message = "You have successfully added a survey completion!"
    )


  }, error = function(err) {
    logging::logerror(err)
     list(
        status = 400,
        message = "Something went wrong"
      )
  })


  return(response)

}



# init_df <- tibble::tibble(user_id = 1L,
#                            singpause_id = "test",
#                            psychTestR_id = "test123456789",
#                            type = "pretest",
#                            complete_time = Sys.time(),
#                            complete = TRUE)
#
# DBI::dbWriteTable(db_con, "singpause_survey_completions", init_df, row.names = FALSE, overwrite = TRUE)



# t <- check_singpause_survey_completion(user_id = 174L)
# t <- check_singpause_survey_completion(user_id = 178L)

check_singpause_survey_completion <- function(user_id) {


  logging::loginfo("Inside check_singpause_survey_completion function")

  logging::loginfo("user_id: %s", user_id)


  response <- tryCatch({


    user_info <- dplyr::tbl(db_con, "singpause_survey_completions") %>%
      dplyr::filter(user_id == !! user_id) %>%
      dplyr::collect()

    complete <- user_info %>%
      dplyr::select(type, complete) %>%
      unique() %>%
      tidyr::pivot_wider(dplyr::everything(), names_from = "type", values_from = "complete")

    start_pretest_psychTestR_id <- user_info %>%
      dplyr::filter(type == "start_pretest") %>%
      dplyr::pull(psychTestR_id)

    start_pretest <- if(length(complete$start_pretest) == 0L) FALSE else complete$start_pretest
    pretest <- if(length(complete$pretest) == 0L) FALSE else complete$pretest
    posttest <- if(length(complete$posttest) == 0L) FALSE else complete$posttest
    psychTestR_id  <- if(length(start_pretest_psychTestR_id) == 0L) NA else start_pretest_psychTestR_id


    # Return response

    list(
      status = 200,
      message = "You have successfully added checked a survey completion!",
      start_pretest = start_pretest,
      pretest = pretest,
      posttest = posttest,
      psychTestR_id = psychTestR_id
    )


  }, error = function(err) {
    logging::logerror(err)
    list(
      status = 400,
      message = "Something went wrong",
      start_pretest = start_pretest,
      pretest = pretest,
      posttest = posttest,
      psychTestR_id = psychTestR_id
    )
  })


  return(response)

}
