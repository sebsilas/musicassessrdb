

# db_con <- musicassessr_con(db_name = "melody_dev")

# curl -X POST 'https://api.dev.songbird.training/get-item-bank' \
# -H 'Content-Type: application/json' \
# -d '{
#   "item_bank": "item_bank_so_so_index"
# }'

# t <- get_item_bank("item_bank_so_so_index")
# t2 <- get_item_bank("item_bank_so_so_index", min_N = 3)

get_item_bank <- function(item_bank = NULL,
                          min_N = NULL) {

  stopifnot(grepl("item_bank_", item_bank)) # Security

  response <- tryCatch({

    ib <- dplyr::tbl(db_con, item_bank) %>%
      dplyr::select(item_id,
                    midi_file,
                    abs_melody,
                    durations,
                    N) %>%   # assumes table has column "N"
      {
        if (!is.null(min_N)) dplyr::filter(., N >= min_N) else .
      } %>%
      dplyr::collect()

    list(status = 200,
         message = paste0("You have successfully got the item bank ", item_bank, "!"),
         item_bank = ib)

  }, error = function(err) {

    logging::logerror(err)

    list(
      status = 400,
      message = "Something went wrong!",
      item_bank = NA
    )

  })

  return(response)
}

