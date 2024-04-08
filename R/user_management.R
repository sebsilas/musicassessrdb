

#' Validate user entry to test based on session token look up in PostgreSQL DB
#'
#' @param validate_user_entry_into_test Logical or a function which accepts username and ... as arguments and returns a page to present to the user.
#' @param elts
#'
#' @return
#' @export
#'
#' @examples
validate_user_entry_into_test <- function(validate_user_entry_into_test, elts, ...) {

  stopifnot(
    is.scalar.logical(validate_user_entry_into_test) || is.function(validate_user_entry_into_test),
    psychTestR::is.timeline(elts) || is.list(elts)
  )

  if(is.function(validate_user_entry_into_test)) {
    page_fun <- validate_user_entry_into_test
    validate_user_entry_into_test <- TRUE
  }

  if(validate_user_entry_into_test) {

    psychTestR::join(


      psychTestR::reactive_page(function(state, ...) {

        # Init DB con
        db_con <- connect_to_db_state(state)

        # Get URL parameters
        url_params <- psychTestR::get_url_params(state)
        user_id <- url_params$user_id
        proposed_token <- url_params$session_token
        item_bank_name <- url_params$item_bank_name
        item_bank_name <- paste0("item_bank_", item_bank_name)
        item_ids <- url_params$item_ids
        review_items_ids <- url_params$review_items_ids
        new_items_ids <- url_params$new_items_ids

        psychTestR::set_global("user_id", user_id, state)

        username <- username_from_user_id(db_con, user_id)

        logging::loginfo("Proposed token: %s",proposed_token )

        success <- authenticate_session_token(db_con, user_id, proposed_token)

        if(!is.null(item_bank_name) && !is.null(item_ids)) {

          logging::loginfo("item_bank_name: %s", item_bank_name)
          logging::loginfo("item_ids: %s", item_ids)

          item_ids <- trimws(strsplit(item_ids, ",")[[1]])
          items <- get_items_from_db(db_con, item_bank_name, item_ids)
          psychTestR::set_global('rhythmic_melody', items, state)
        }


        if(length(review_items_ids) > 0L || length(new_items_ids) > 0L) {

          if(is.null(psychTestR::get_global('rhythmic_melody', state)))  {
            # Note that psychTestR runs reactive_page functions twice.. so we make sure the second time we don't fire this (otherwise active == 0 for selected items and the function will fail)
            items <- get_selected_items_from_db(db_con, user_id, review_items_ids, new_items_ids)
            psychTestR::set_global('rhythmic_melody', items, state)
          }

        }

        logging::loginfo("Disconnecting from the DB")
        if(DBI::dbIsValid(db_con)) {
          DBI::dbDisconnect(db_con)
        }

        if(is.function(page_fun)) page_fun(success, username) else default_validate_page(success, username)

        }),

      elts
    )
  } else {
    elts
  }

}


default_validate_page <- function(success, username, ...) {

  stopifnot(
    is.scalar.logical(success),
    is.scalar.character(username)
  )

  if(success) {
    psychTestR::one_button_page(
      shiny::tags$div(
        shiny::tags$script("var upload_to_s3 = true; console.log('Turning S3 mode on');"),
        paste0("Welcome ", username, "!")
      )
    )
  } else {
    psychTestR::final_page("Unfortunately you were not validated.")
  }
}




authenticate_session_token <- function(db_con, user_id, proposed_token) {

  session_token_table <- get_table(db_con, "session_tokens")


  if(is.null(user_id) || is.null(proposed_token)){
    return(FALSE)
  }

  session_token <- session_token_table %>%
    dplyr::filter(user_id == !!user_id) %>%
    dplyr::slice_max(session_token_id)

  expires <- session_token$expires
  token <- session_token$token



  token == proposed_token && Sys.time() < expires
}

