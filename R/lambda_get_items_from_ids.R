
get_items_from_ids_api <- function(item_bank_name, item_ids) {

  # Define the request body as a list
  request_body <- list(item_bank_name = item_bank_name,
                       item_ids = item_ids)

  endpoint_wrapper(function_name = "get-items-from-ids", request_body = request_body)

}



# This is the function that is called when the endpoint
# is invoked
get_items_from_ids <- function(item_bank_name,
                               item_ids) {

  stopifnot(
    is.scalar.character(item_bank_name),
    is.character(item_ids)
  )

  logging::loginfo("Inside get_items_from_ids function")

  logging::loginfo("item_bank_name = %s", item_bank_name)
  logging::loginfo("item_ids = %s", item_ids)


  response <- tryCatch({

    # Main logic

    # Get sessions associated with user
    items <- get_items_from_db(db_con, item_bank_name, item_ids)

    # Return response

    list(
      status = 200,
      message = paste0("You successfully got items from the following item bank: ", item_bank_name),
      items = items
    )


  }, error = function(err) {
    logging::logerror(err)
    list(
      status = 400,
      message = "Something went wrong",
      items = NA
    )
  })


  return(response)

}



get_items_from_db <- function(db_con, item_bank_name, item_ids) {
  get_table(db_con, item_bank_name) %>%
    dplyr::filter(item_id %in% !! item_ids) %>%
    dplyr::collect()
}

get_selected_items_from_db <- function(db_con, user_id, review_items_ids = NULL, new_items_ids = NULL) {


  if(length(review_items_ids) > 0L) {

    logging::loginfo("Getting %s review items", length(review_items_ids))
    logging::loginfo(review_items_ids)

    review_items <- get_table(db_con, 'review_items', collect = FALSE)

    review_items_filtered <- review_items %>%
      dplyr::filter(review_items_id %in% !! review_items_ids)

    item_bank_names <- review_items_filtered %>%
      dplyr::pull(item_id) %>%
      get_item_bank_names(db_con = db_con)

    review_items_filtered <- left_join_on_items(db_con, review_items_filtered)

    review_items_filtered <- review_items_filtered %>% dplyr::collect()

    logging::loginfo("Retrieved %s rows", nrow(review_items_filtered))

    # Deactivate grabbed items (note this needs to be done after collecting)

    review_update <- review_items %>% dplyr::mutate(active = 0L)
    dplyr::rows_update(review_items, review_update, in_place = TRUE, by = "review_items_id", unmatched = "ignore")

  } else {
    review_items_filtered <- tibble::tibble()
  }


  if(length(new_items_ids) > 0L) {

    logging::loginfo("Getting %s new items", length(new_items_ids))
    logging::loginfo(new_items_ids)

    new_items <- get_table(db_con, 'new_items', collect = FALSE)

    new_items_filtered <- new_items %>%
      dplyr::filter(new_items_id %in% !! new_items_ids)

    item_bank_names <- new_items_filtered %>%
      dplyr::pull(item_id) %>%
      get_item_bank_names(db_con = db_con)

    new_items_filtered <- left_join_on_items(db_con, new_items_filtered)

    new_items_filtered <- new_items_filtered %>% dplyr::collect()

    logging::loginfo("Retrieved %s rows", nrow(new_items_filtered))

    # Deactivate grabbed items (note this needs to be done after collecting)

    new_update <- new_items %>% dplyr::mutate(active = 0L)
    dplyr::rows_update(new_items, new_update, in_place = TRUE, by = "new_items_id", unmatched = "ignore")


  } else {
    new_items_filtered <- tibble::tibble()
  }

  return(list(review_items = review_items_filtered, new_items = new_items_filtered))

}

