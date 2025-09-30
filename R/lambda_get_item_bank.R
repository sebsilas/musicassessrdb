

# db_con <- musicassessr_con()

# t <- get_item_bank("item_bank_so_so_index")

get_item_bank <- function(item_bank = NULL) {

  stopifnot(grepl("item_bank_", item_bank)) # Security

  response <- tryCatch({

    ib <- dplyr::tbl(db_con, item_bank) %>%
      dplyr::select(item_id,
                    midi_file,
                    abs_melody,
                    durations) %>%
      dplyr::collect()


    list(status = 200,
         message = paste0("You have successfully got the item bank ", item_bank, "!"),
         item_bank = ib)


  }, error = function(err) {

    logging::logerror(err)

    list(
      status = 400,
      message = "Something went wrong!",
      sample = NA
    )

  })


  return(response)
}
