

add_custom_melody_to_db <- function(db_con = NULL,
                                    abs_melody,
                                    durations,
                                    original_item_bank,
                                    original_item_id) {

  logging::loginfo("add_custom_melody_to_db fun...")

  if(!is.scalar.character(abs_melody)) {
    abs_melody <- paste0(abs_melody, collapse = ",")
  }

  if(!is.scalar.character(durations)) {
    durations <- paste0(durations, collapse = ",")
  }

  if(is.null(db_con)) {
    db_con <- musicassessr_con()
    local_db_con <- TRUE
  } else {
    local_db_con <- FALSE
  }

  if(original_item_id == "NONE") {

    item_exists <- FALSE

  } else {
    # First check if item exists in the original item_bank as an N-gram
    item_exists <- check_item_exists_in_db(db_con,
                                           original_item_bank,
                                           abs_melody,
                                           durations,
                                           original_item_id)

    if(!is.scalar.character(item_exists)) {
      logging::loginfo("Item does not exist in the original item bank, %s, as an ngram", original_item_bank)
    }
  }


  if(is.scalar.character(item_exists)) {
    return(
      list(
        message = "You have successfully added a new item",
        item_id = item_exists
      )
    )
  } else {

    item_exists <- # Then check if the item_bank exists in the custom items item bank as an N-gram
      check_item_exists_in_db(db_con,
                              "item_bank_custom_items",
                              abs_melody,
                              durations,
                              original_item_id)

    if(!is.scalar.character(item_exists)) {

      logging::loginfo("Item does not exist in the custom_items item bank as an ngram")

      logging::loginfo("Append it to custom_items...")

      # If it exists in neither, append it to the custom_items item bank:

      melody <- tibble::tibble(
        original_item_bank = original_item_bank,
        original_item_id = original_item_id,
        abs_melody = abs_melody,
        durations = durations
      ) %>%
        itembankr::get_melody_features()

      logging::loginfo("melody.. %s", melody)


      nrows <- get_nrows(dplyr::tbl(db_con, "item_bank_custom_items"))

      melody <- melody %>%
        dplyr::mutate(item_id = paste0("custom_items_", nrows + 1)) %>%
        dplyr::relocate(item_id)

      DBI::dbAppendTable(db_con, "item_bank_custom_items", melody)

      if(local_db_con) {
        db_disconnect(db_con)
      }

      return(
        list(
          message = "You have successfully added a new item",
          item_id = melody$item_id
        )
      )
    }

  }
}

check_item_exists_in_db <- function(db_con,
                                    item_bank_name,
                                    abs_melody,
                                    durations,
                                    original_item_id) {

  if(!grepl("item_bank_", item_bank_name)) {
    item_bank_name <- paste0("item_bank_", item_bank_name)
  }

  if(item_bank_name == "item_bank_custom_items") {
    item_check <- dplyr::tbl(db_con, "item_bank_custom_items") %>%
      dplyr::filter(abs_melody == !! abs_melody,
                    durations == !! durations,
                    original_item_id == !! original_item_id) %>%
      dplyr::collect()
  } else {
    # In this case we filter on item_id, not original_item_id
    item_check <- dplyr::tbl(db_con, item_bank_name) %>%
      dplyr::filter(abs_melody == !! abs_melody,
                    durations == !! durations,
                    item_id == !! original_item_id) %>%
      dplyr::collect()
  }

  if(nrow(item_check) > 0L) {

    return(item_check$item_id)
  } else {
    return(FALSE)
  }

}


# t <- add_custom_melody_to_db("60,62,63", "1, 1, 1", "test", "test_id_1")
