

# t <- lambda_sample_demo_slonimsky()



lambda_sample_demo_slonimsky <- function() {

  tryCatch({


    db_con <- musicassessr_con()

    sample <- dplyr::tbl(db_con, "item_bank_DTL1000_phrase") %>%
      dplyr::filter(item_id %in% c("DTL1000_phrase_3", "DTL1000_phrase_4", "DTL1000_phrase_7", "DTL1000_phrase_10", "DTL1000_phrase_11")) %>%
      dplyr::collect()

    db_disconnect(db_con)


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
