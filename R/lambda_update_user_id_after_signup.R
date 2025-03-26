

lambda_update_user_id_after_signup_api <- function(new_user_id,
                                                   session_id) {

  # Define the request body as a list
  request_body <- list(
    new_user_id = new_user_id,
    session_id = session_id
  )

  endpoint_wrapper(function_name = "update-user-id-after-signup",
                   request_body = request_body)

}



# This is the function that is called when the endpoint
# is invoked
lambda_update_user_id_after_signup <- function(new_user_id,
                                               session_id) {

  logging::loginfo("Inside update_user_id_after_signup function")

  logging::loginfo("new_user_id = %s", new_user_id)
  logging::loginfo("session_id = %s", session_id)


  response <- tryCatch({

    db_con <- musicassessr_con(pool = FALSE)

    new_dat <- tibble::tibble(user_id = new_user_id,
                              session_id = session_id)

    update <- dbplyr::copy_inline(db_con, new_dat)

    dplyr::rows_update(dplyr::tbl(db_con, "sessions"), update, in_place = TRUE, by = "session_id", unmatched = "ignore")

    db_disconnect(db_con)

    # Return response

   list(
      status = 200,
      message = paste0("You have successfully assigned the new user ", new_user_id, " to session ", session_id, ".")
    )


  }, error = function(err) {
    logging::logerror(err)
     list(
        status = 400,
        message = "Something went wrong"
      )
  })


  return(response)

}



# Test
# t <- add_user_to_group_with_login_code_api(
#   user_id = 1L,
#   login_code = "testfail")



