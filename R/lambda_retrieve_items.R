
retrieve_items_api <- function(item_bank_name, num_items_per_page, page_number) {

  # Define the request body as a list
  request_body <- list(item_bank_name = item_bank_name,
                       num_items_per_page = num_items_per_page,
                       page_number = page_number)

  endpoint_wrapper(function_name = "retrieve-items", request_body = request_body)

}

#
# t <- retrieve_items("DTL1000", 10, 1)$items
# t2 <- retrieve_items("DTL1000", 10, 1, sort_direction = "desc", sort_key = "arrhythmic_difficulty_percentile")$items
# t3 <- retrieve_items("DTL1000", 10, 1)$items



# t <- retrieve_items("DTL1000", num_items_per_page = 10, page_number = 1, user_id = 136)

# This is the function that is called when the endpoint
# is invoked
retrieve_items <- memoise::memoise(function(item_bank_name,
                                            num_items_per_page,
                                            page_number,
                                            sort_key = NULL,
                                            sort_direction = NULL,
                                            user_id = NULL) {

  stopifnot(
    is.scalar.character(item_bank_name),
    is.scalar.integerlike(num_items_per_page),
    is.scalar.integerlike(page_number),
    is.null.or(sort_key, is.scalar.character),
    is.null.or(sort_direction, function(x) {
      x %in% c('asc', 'desc')
    }),
    is.null.or(user_id, is.integerlike)
  )

  logging::loginfo("Inside retrieve_items function")

  logging::loginfo("item_bank_name = %s", item_bank_name)
  logging::loginfo("num_items_per_page = %s", num_items_per_page)
  logging::loginfo("page_number = %s", page_number)
  logging::loginfo("sort_key = %s", sort_key)
  logging::loginfo("sort_direction = %s", sort_direction)
  logging::loginfo("user_id = %s", user_id)

  response <- tryCatch({

    db_con <- musicassessr_con()

    nos <- get_item_numbers(page_number, num_items_per_page)


    items <- dplyr::tbl(db_con, paste0("item_bank_", item_bank_name)) %>%
      dplyr::collect() # We only expect to use small dataframes here


    if(!is.null(sort_direction)) {
      sort_key_name <- as.symbol(sort_key)

      if (sort_direction == "asc") {
        items <- items %>%
          dplyr::arrange(!!sort_key_name)
      } else {
        items <- items %>%
          dplyr::arrange(dplyr::desc(!!sort_key_name))
      }

    }

    items <- items %>%
      dplyr::slice(nos$start_number:nos$end_number)

    if(is.null(user_id)) {
      study_history <- NA
    } else {

      browser()

      study_history <- compile_item_trials(db_con,
                                           user_id = user_id,
                                           join_item_banks_on = FALSE,
                                           add_trial_scores = TRUE) %>%
        dplyr::filter(item_id %in% !! items$item_id)

      if(nrow(study_history) > 0L) {
        # Aggregate
        study_history <- study_history %>%
          dplyr::mutate(Date = lubridate::as_date(session_time_started)) %>%
          dplyr::group_by(Date, item_id) %>%
          dplyr::summarise(score = mean(na.rm = TRUE)) %>%
          dplyr::ungroup()
      } else {
        study_history <- NA
      }


    }


    db_disconnect(db_con)

    # Return response

    list(
      status = 200,
      message = paste0("You successfully got items from the following item bank: ", item_bank_name),
      items = items,
      study_history = study_history
    )


  }, error = function(err) {
    logging::logerror(err)
    list(
      status = 400,
      message = "Something went wrong",
      items = NA,
      study_history = study_history
    )
  })


  return(response)

})


get_item_numbers <- function(page_number, num_items_per_page) {

  end_number <- num_items_per_page * page_number
  start_number <- end_number - (num_items_per_page - 1)

  return(list(start_number = start_number,
              end_number = end_number))
}


