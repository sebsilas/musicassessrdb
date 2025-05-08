


# curl -X POST 'https://api.songbird.training/sample-in-ntiles-wjd' \
# -H 'Content-Type: application/json' \
# -d '{
#   "num_items": 12
# }'


# t <- sample_in_ntiles_wjd()

sample_in_ntiles_wjd <- function(num_items_arrhythmic = 8L,
                                 num_items_rhythmic = 8L) {

  tryCatch({



    sample_arrhythmic <- musicassessr::sample_in_ntiles_wrapper(musicassessr::pbet_hmtm_2024_item_bank,
                                                                num_items = num_item_arrhythmic,
                                                                ntile_sampling_upper_bound = 4L,
                                                                col_name = "arrhythmic_difficulty_percentile",
                                                                n = 4) %>%
      dplyr::mutate(rhythmic = FALSE) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(durations = paste0(rep(0.5, N), collapse = ",") ) %>%
      dplyr::ungroup()

    sample_rhythmic <- musicassessr::sample_in_ntiles_wrapper(musicassessr::pbet_hmtm_2024_item_bank,
                                                              num_items = num_items_rhythmic,
                                                              ntile_sampling_upper_bound = 4L,
                                                              col_name = "rhythmic_difficulty_percentile",
                                                              n = 4) %>%
      dplyr::mutate(rhythmic = TRUE)


    sample <- rbind(sample_arrhythmic, sample_rhythmic) %>%
      dplyr::slice_sample(n = nrow(.))


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


