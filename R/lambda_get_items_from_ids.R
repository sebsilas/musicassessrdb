
get_items_from_ids_api <- function(item_bank_name,
                                   item_ids) {

  # Define the request body as a list
  request_body <- list(item_bank_name = item_bank_name,
                       item_ids = item_ids)

  endpoint_wrapper(function_name = "get-items-from-ids",
                   request_body = request_body)

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

get_selected_items_from_db <- function(db_con, user_id, get_review_items = FALSE, get_new_items = FALSE) {

  if(get_review_items) {

    review_items <- get_table(db_con, 'review_items', collect = FALSE)

    review_items_filtered <- review_items %>%
      dplyr::filter(user_id == !! user_id,
                    active == 1L)

    item_bank_names <- review_items_filtered %>%
      dplyr::pull(item_id) %>%
      get_item_bank_names(db_con = db_con)

    review_items_filtered <- left_join_on_items(db_con, item_bank_names, review_items_filtered)


  } else {
    review_items_filtered <- tibble::tibble()
  }


  if(get_new_items) {

    new_items <- get_table(db_con, 'new_items', collect = FALSE)

    new_items_filtered <- new_items %>%
      dplyr::filter(user_id == !! user_id,
                    active == 1L)

    item_bank_names <- new_items_filtered %>%
      dplyr::pull(item_id) %>%
      get_item_bank_names(db_con = db_con)

    new_items_filtered <- left_join_on_items(db_con, item_bank_names, new_items_filtered)

  } else {
    new_items_filtered <- tibble::tibble()
  }

  review_items_filtered <- review_items_filtered %>% dplyr::collect()
  new_items_filtered <- new_items_filtered %>% dplyr::collect()

  # Inactivate grabbed items (note this needs to be done after collecting)

  new_update <- new_items %>% dplyr::mutate(active = 0L)
  dplyr::rows_update(new_items, new_update, in_place = TRUE, by = "new_items_id", unmatched = "ignore")

  review_update <- review_items %>% dplyr::mutate(active = 0L)
  dplyr::rows_update(review_items, review_update, in_place = TRUE, by = "review_items_id", unmatched = "ignore")


  joint_names <- intersect(names(review_items_filtered), names(new_items_filtered))

  review_items_filtered <- review_items_filtered %>% dplyr::select(dplyr::all_of(joint_names))
  new_items_filtered <- new_items_filtered %>% dplyr::select(dplyr::all_of(joint_names))

  joined_items <- rbind(review_items_filtered, new_items_filtered) %>%
    dplyr::slice_sample(n = nrow(.)) %>%
    dplyr::select(-active) # Randomise

  return(joined_items)

}

#  items <- get_selected_items_from_db(db_con, user_id = 58, get_review_items = TRUE, get_new_items = TRUE)
# db_con <- musicassessr_con()
# t <- get_items_from_ids("item_bank_singpause_phrase", c("singpause_phrase_5", "singpause_phrase_4"))
# DBI::dbDisconnect(db_con)
