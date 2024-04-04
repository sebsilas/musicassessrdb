
# t <- sample_melody_in_key_api(item_bank_name = "WJD_phrase",
#                               inst = "Piano",
#                               bottom_range = 48,
#                               top_range = 72,
#                               key_difficulty = "easy",
#                               melody_length = 10,
#                               no_melodies = 1)




# curl -X POST 'https://kuqchtwsfb.execute-api.us-east-1.amazonaws.com/sample-melody-in-key' \
# -H 'Content-Type: application/json' \
# -d '{
#   "item_bank_name": "WJD_phrase",
#   "inst": "Piano",
#   "bottom_range": 48,
#   "top_range": 72,
#   "key_difficulty": "easy",
#   "melody_length": 10,
#   "no_melodies": 10
# }'



#' Sample from item bank via API
#'
#' @param item_bank_name
#' @param num_items
#' @param key_difficulty
#' @param melody_length
#'
#' @return
#' @export
#'
#' @examples
sample_melody_in_key_elts <- function(item_bank_name = "WJD_ngram", num_items, key_difficulty, melody_length) {

  psychTestR::code_block(function(state, ...) {

    logging::loginfo("Sampling melody in key from %s item bank via API", item_bank_name)

    span <- psychTestR::get_global('span', state)
    inst <- psychTestR::get_global('inst', state)
    bottom_range <- psychTestR::get_global('bottom_range', state)
    top_range <- psychTestR::get_global('top_range', state)


    # sampler_status <- future::future({
    #
    #   call_sample_melody_in_key_api_and_add_to_state(create_test_spec_sample_melody_in_key_api_parameters(), state)
    #
    # })  %...>% (function(result) {
    #   logging::loginfo("Returning promise result: %s", result)
    # })

    sampler_status <-  NULL

    psychTestR::set_global('sample_melody_in_key_status', sampler_status, state)



  })

}

sample_melody_in_key_api <- function(item_bank_name,
                                     inst,
                                     bottom_range,
                                     top_range,
                                     key_difficulty,
                                     melody_length,
                                     no_melodies) {

  # Define the request body as a list
  request_body <- list(
    item_bank_name = item_bank_name,
    inst = inst,
    bottom_range = bottom_range,
    top_range = top_range,
    key_difficulty = key_difficulty,
    melody_length = melody_length,
    no_melodies = no_melodies)

  res <- endpoint_wrapper(function_name = "sample-melody-in-key",
                          request_body = request_body)

  logging::loginfo(res$message)

  res <- res$melodies %>%
    dplyr::bind_rows()

  return(res)

}

# t <- sample_melody_in_key_lambda("WJD_ngram", "Tenor Saxophone", "easy", bottom_range = 48, top_range = 82, melody_length = 4, no_melodies = 10)

# This is the function that is called when the endpoint
# is invoked
sample_melody_in_key_lambda <- function(item_bank_name, inst, bottom_range, top_range, key_difficulty, melody_length, no_melodies) {

  # We give this one a _lambda suffix to differentiate it from the musicassessr function of the same name, otherwise we have namespacing issues

  logging::loginfo("Inside sample_melody_in_key function")

  logging::loginfo("item_bank_name = %s", item_bank_name)
  logging::loginfo("inst = %s", inst)
  logging::loginfo("bottom_range = %s", bottom_range)
  logging::loginfo("top_range = %s", top_range)
  logging::loginfo("key_difficulty = %s", key_difficulty)
  logging::loginfo("melody_length = %s", melody_length)
  logging::loginfo("no_melodies = %s", no_melodies)

  stopifnot(
    melody_length > 3,
    item_bank_name %in% c("Berkowitz_ngram", "Berkowitz_phrase", "WJD_ngram", "WJD_phrase")
  )

  # Return response

  response <- tryCatch({

    item_bank <- dplyr::tbl(db_con, item_bank_name)

    #tictoc::tic()

    # melodies <- purrr::map_dfr(1:no_melodies, function(n) {
    #   musicassessr::sample_melody_in_key(item_bank, inst, bottom_range, top_range, key_difficulty, melody_length)
    # })

    # # Set a "plan" for how the code should run.
    future::plan(future::multisession, workers = 2)



    melodies <- furrr::future_map_dfr(1:no_melodies, function(n) {
      musicassessr::sample_melody_in_key(item_bank, inst, bottom_range, top_range, key_difficulty, melody_length)
    })

    #tictoc::toc()

    list(status = 200,
         message = paste0("You have successfully sampled from the ", item_bank_name, " item bank!"),
         melodies = melodies)


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


call_sample_melody_in_key_api_and_add_to_state <- function(parameters, state) {
  # This is actually a bad idea
  stopifnot(
    is.data.frame(parameters) && length(setdiff(names(parameters), c("item_bank", "inst", "bottom_range", "top_range", "key_difficulty", "melody_length"))) == 0
  )

  parameters <- parameters %>%
    dplyr::mutate(row = dplyr::row_number())

  purrr::pmap(parameters, function(item_bank, inst, bottom_range, top_range, key_difficulty, melody_length, row) {

    logging::loginfo("Adding item to buffer from API: %s/%s", row, nrow(parameters))

    new <- sample_melody_in_key_api(item_bank, inst, bottom_range, top_range, key_difficulty, melody_length)

    if(!is.data.frame(new)) {
      stop("Something went wrong.")
    }

    ib <- psychTestR::get_global("item_bank", state)

    if(is.null(ib)) {
      ib <- new
    } else {
      ib <- rbind(ib, new)
    }

    psychTestR::set_global("item_bank", ib, state)

  })

  return(list(success = TRUE))

}

# call_sample_melody_in_key_api_and_add_to_state(create_test_spec_sample_melody_in_key_api_parameters())


create_test_row_call_sample_melody_in_key_api_parameters <- function(item_bank = "WJD_ngram",
                                                                     inst = "Tenor Saxophone",
                                                                     bottom_range = 48,
                                                                     top_range = 72) {

  key_difficulty = sample(c("easy", "hard"), 1)
  melody_length =  sample(4:15, 1)

  tibble::tibble(
    item_bank = item_bank,
    inst = inst,
    bottom_range = bottom_range,
    top_range = top_range,
    key_difficulty = key_difficulty,
    melody_length = melody_length
    )


}

create_test_spec_sample_melody_in_key_api_parameters <- function(no_items = 2L) {
  purrr::map_dfr(1:no_items, ~create_test_row_call_sample_melody_in_key_api_parameters())
}

