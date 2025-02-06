


# curl -X POST 'https://api.songbird.training/sample-in-ntiles-wjd' \
# -H 'Content-Type: application/json' \
# -d '{
#   "num_items": 12
# }'


sample_in_ntiles_wjd <- function(num_items = 12L) {

  tryCatch({

    sample <- musicassessr::sample_in_ntiles_wrapper(musicassessr::pbet_hmtm_2024_item_bank,
                                                     num_items = num_items,
                                                     col_name = "SAA_arrhythmic_difficulty_percentile",
                                                     n = 4)

    list(
      status = 200,
      message = "You successfully got a sample!",
      sample = sample
    )

  }, error = function(err) {

    logging::logerror(err)

    list(
      status = 400,
      message = "There was an error getting a sample!",
      sample = NA
    )

  })

}


