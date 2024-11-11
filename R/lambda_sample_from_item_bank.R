
#' Sample from item bank via API
#'
#' @param item_bank_name
#' @param num_items
#' @param melody_length
#'
#' @return
#' @export
#'
#' @examples
sample_from_item_bank_elts <- function(item_bank_name = "WJD_ngram", num_items, melody_length) {

  psychTestR::code_block(function(state, ...) {

    span <- psychTestR::get_global('span', state)

    span <- 10

    logging::logwarn("Forcing span of 10 for now..")

    logging::loginfo("Sampling %s items from %s item bank via API", num_items, item_bank_name)
    logging::loginfo("Melody length: %s", melody_length)
    logging::loginfo("Span: %s", span)

    # Make sure in correct future mode...
    future::plan(future::multisession)

    item_bank_sample <- future::future({

      #sample_from_item_bank_api(item_bank_name, num_items, span, melody_length)

      store_db_session_api(experiment_condition_id = NA,
                           user_id = 1L,
                           session_time_started = Sys.time(),
                           experiment_id = NA)

    }) %...>% (function(result) {

      logging::loginfo("Returning promise message: %s", result$message)

      if(result$status == 200) {
        sample <- result
        #sample <- dplyr::bind_rows(result$sample)
        logging::loginfo("Returning promise result: %s", sample)

        return(sample)
      } else {
        return(NA)
      }
    })

    psychTestR::set_global('sampled_item_bank_from_api', item_bank_sample, state)


  })

}



# For now use Berkowitz materialized view..

# t <- sample_from_item_bank_api(item_bank_name = "wjd_narrowed_n_view",
#                                num_items = 20)
# t2 <- t$sample


#' Sample from an item bank via the API
#'
#' @param item_bank_name
#' @param num_items
#' @param span
#' @param melody_length
#' @param shuffle
#'
#' @return
#' @export
#'
#' @examples
sample_from_item_bank_api <- function(item_bank_name,
                                      num_items,
                                      span = NULL,
                                      melody_length = NULL,
                                      shuffle = TRUE) {

  if(!is.scalar.character(melody_length)) {
    melody_length <- paste0(melody_length, collapse = ",")
  }

  logging::loginfo("sample_from_item_api")
  logging::loginfo("melody_length: %s", melody_length)

  # Define the request body as a list
  request_body <- list(
    item_bank_name = item_bank_name,
    num_items = num_items,
    span = span,
    melody_length = melody_length,
    shuffle = shuffle
  )

  endpoint_wrapper(function_name = "sample-from-item-bank",
                   request_body = request_body)
}

# DBI::dbExecute(db_con, 'CREATE INDEX idx_N ON "item_bank_WJD_narrowed"("N")')

# t <- sample_from_item_bank(item_bank_name = "WJD_narrowed",
#                           num_items = 20,
#                           span = 14,
#                           melody_length = "4,15")

# db_con <- musicassessr_con()

# t <- sample_from_item_bank(item_bank_name = "Berkowitz_ngram_n_view",
#                            num_items = 20,
#                            span = 14,
#                            melody_length = "4,15")


# This is the function that is called when the endpoint
# is invoked
sample_from_item_bank <- function(item_bank_name,
                                  num_items = NULL,
                                  span = 14,
                                  melody_length = NULL,
                                  shuffle = TRUE) {

  logging::loginfo("Inside sample_from_item_bank function")

  logging::loginfo("item_bank_name = %s", item_bank_name)
  logging::loginfo("num_items = %s", num_items)
  logging::loginfo("span = %s", span)
  logging::loginfo("melody_length = %s", melody_length)

  stopifnot(
    item_bank_name %in% c("Berkowitz_ngram", "Berkowitz_ngram_n_view", "Berkowitz_phrase", "WJD_ngram", "WJD_phrase", "WJD_narrowed", "wjd_narrowed_n_view")
  )

  if(grepl("n_view", item_bank_name)) {

    logging::loginfo("Using materialised view db")

    sample <- musicassessr::item_sampler_materialized_view(db_con,
                                                           no_items = num_items,
                                                           table = item_bank_name,
                                                           shuffle = shuffle)

  } else {

    item_bank_name <- paste0("item_bank_", item_bank_name)

    melody_length <- itembankr::str_mel_to_vector(melody_length)

    logging::loginfo("Grab item bank")

    item_bank <- dplyr::tbl(db_con, item_bank_name)

    logging::loginfo("Got item bank")

    if(is.null(span) | span < 10) {
      span <- 10
    }

    logging::loginfo("Get subset")

    # Sample
    item_bank_subset <- itembankr::subset_item_bank(item_bank = item_bank, span_max = span, item_length = melody_length)

    logging::loginfo("Got subset")
    logging::loginfo(get_nrows(item_bank_subset))

    if(get_nrows(item_bank_subset) <= 1) {
      item_bank_subset <- item_bank
    }

    logging::loginfo("Sample..")

    sample <- musicassessr::item_sampler(item_bank_subset, num_items, version = "2", shuffle = shuffle)

    logging::loginfo(get_nrows(sample))

    if(get_nrows(sample) < num_items) {
      sample <- musicassessr::item_sampler(item_bank_subset, num_items, replace = TRUE, version = "2", shuffle = shuffle)
    } else {
      sample <- musicassessr::item_sampler(item_bank_subset, num_items, version = "2", shuffle = shuffle)
    }


  }

  # Return response

  response <- tryCatch({

      list(status = 200,
           message = paste0("You have successfully sampled from the ", item_bank_name, " item bank!"),
           sample = sample)

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
