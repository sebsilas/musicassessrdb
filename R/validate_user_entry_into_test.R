

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

        if(is.null(psychTestR::get_global("username", state))) { # Prevent the below logic firing twice, a weird quirk of psychTestR

          # Init DB con
          db_con <- connect_to_db_state(state)

          # Get URL parameters
          url_params <- psychTestR::get_url_params(state)
          proposed_token <- url_params$session_token
          user_id <- url_params$user_id
          job_id <- url_params$job_id

          psychTestR::set_global("user_id", user_id, state)
          psychTestR::set_global("job_id", job_id, state)

          username <- username_from_user_id(db_con, user_id)

          psychTestR::set_global("username", username, state)

          logging::loginfo("Proposed token: %s", proposed_token)

          success <- authenticate_session_token(db_con, user_id, proposed_token)

          logging::loginfo("Disconnecting from the DB")

          if(DBI::dbIsValid(db_con)) {
            DBI::dbDisconnect(db_con)
          }

        } else {
          success <- TRUE
          username <- psychTestR::get_global("username", state)
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

split_ids <- function(ids) as.integer(strsplit(URLdecode(ids), ",")[[1]])


format_item_ids_in_url <- function() {
  select_items_res <- select_items(2L)
  review_item_ids_str <- paste0(select_items_res$review_items_ids, collapse = ',')
  new_item_ids_str <- paste0(select_items_res$new_items_ids, collapse = ',')
  paste0('new_items_ids=', new_item_ids_str, "&",
         'review_items_ids=', review_item_ids_str)
}

# (t <- format_item_ids_in_url())

