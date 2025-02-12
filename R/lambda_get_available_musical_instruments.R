

# curl -X POST 'https://api.dev.songbird.training/get-available-musical-instruments' \
# -H 'Content-Type: application/json'


get_available_musical_instruments_api <- function() {

  endpoint_wrapper(function_name = "get-available-musical-instruments", request_body = NULL)

}



# This is the function that is called when the endpoint
# is invoked
lambda_get_available_musical_instruments <- function() {

  logging::loginfo("Inside get_available_musical_instrument function")


  response <- tryCatch({

    # Main logic
    db_con <- musicassessr_con()

    instruments <- dplyr::tbl(db_con, "instruments") %>%
      dplyr::collect()

    # Return response

    list(
      status = 200,
      message = "You successfully got musical instruments!",
      instruments = instruments
    )


  }, error = function(err) {
    logging::logerror(err)
    list(
      status = 400,
      message = "Something went wrong",
      instruments = NA
    )
  })


  return(response)

}
