

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
                                               type = c("pretest", "posttest")) {

  stopifnot(
    type %in% c("pretest", "posttest")
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



check_singpause_survey_completion <- function(user_id,
                                              type = c("pretest", "posttest")) {

  type <- match.arg(type)

  stopifnot(
    type %in% c("pretest", "posttest")
  )

  logging::loginfo("Inside check_singpause_survey_completion function")

  logging::loginfo("user_id: %s", user_id)
  logging::loginfo("type: %s", type)


  response <- tryCatch({

    complete <- dplyr::tbl(db_con, "singpause_survey_completions") %>%
      dplyr::filter(user_id == !! user_id,
                    type == !! type) %>%
      dplyr::pull(complete)

    if(length(complete) > 1L) {
      logging::logerror("More than two completions!")
    }

    if(length(complete) == 0) {
      complete <- FALSE
    }

    # Return response

    list(
      status = 200,
      message = "You have successfully added checked a survey completion!",
      complete = complete
    )


  }, error = function(err) {
    logging::logerror(err)
    list(
      status = 400,
      message = "Something went wrong",
      complete = complete
    )
  })


  return(response)

}
