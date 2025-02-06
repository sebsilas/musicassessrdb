
retrieve_items_api <- function(item_bank_name, num_items_per_page, page_number) {

  # Define the request body as a list
  request_body <- list(item_bank_name = item_bank_name,
                       num_items_per_page = num_items_per_page,
                       page_number = page_number)

  endpoint_wrapper(function_name = "retrieve-items", request_body = request_body)

}


# t <- retrieve_items("DTL1000", 10, 1)$items

# This is the function that is called when the endpoint
# is invoked
retrieve_items <- memoise::memoise(function(item_bank_name, num_items_per_page, page_number) {

  stopifnot(
    is.scalar.character(item_bank_name),
    is.scalar.integerlike(num_items_per_page),
    is.scalar.integerlike(page_number)
  )

  logging::loginfo("Inside retrieve_items function")

  logging::loginfo("item_bank_name = %s", item_bank_name)
  logging::loginfo("num_items_per_page = %s", num_items_per_page)
  logging::loginfo("page_number = %s", page_number)


  response <- tryCatch({

    db_con <- musicassessr_con()

    nos <- get_item_numbers(page_number, num_items_per_page)

    items <- dplyr::tbl(db_con, paste0("item_bank_", item_bank_name)) %>%
      dplyr::mutate(row_num = dplyr::row_number()) %>%
      dplyr::filter(dplyr::between(row_num, nos$start_number, nos$end_number)) %>%
      # N.B, slice doesn't work on backends
      dplyr::collect()

    db_disconnect(db_con)

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

})


get_item_numbers <- function(page_number, num_items_per_page) {

  end_number <- num_items_per_page * page_number
  start_number <- end_number - (num_items_per_page - 1)

  return(list(start_number = start_number,
              end_number = end_number))
}


