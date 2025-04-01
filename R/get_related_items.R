


# A simple get related items helper based on parent melodies
# Note that this only works with itembankr N-Gram databases
# Because it relies on parent_abs_melody and ngrukkon
get_related_items <- function(item_bank,
                              item_id,
                              no_to_return = 3) {

  parent_melody <- item_bank %>%
    dplyr::filter(item_id == !! item_id) %>%
    dplyr::pull(parent_abs_melody)


  similar_items <- item_bank %>%
    dplyr::filter(parent_abs_melody == !! parent_melody,
                  item_id != !! item_id)

  if(!is.null(no_to_return)) {
    similar_items <- similar_items %>%
      dplyr::slice_max(ngrukkon, n = no_to_return)
  }

  if(nrow(similar_items) < 1L) {
    logging::loginfo("No similar melodies found!")
  }

  return(similar_items)

}



sample_and_test <- function(item_bank) {

  smp <- item_bank %>%
    dplyr::slice_sample(n = 1)

  smp_id <- smp %>% dplyr::pull(item_id)

  items <- get_related_items(item_bank, smp_id)

  return(items)

}

# pbet_hmtm_2024_item_bank <- musicassessr::pbet_hmtm_2024_item_bank %>%
#   tibble::as_tibble()
# t <- sample_and_test(pbet_hmtm_2024_item_bank)


# db_con <- musicassessr_con()
#
# dplyr::tbl(db_con, "item_bank_Slonimsky_phrase") %>%
#   sample_and_test()
# db_disconnect(db_con)
