

add_user_to_group_with_login_code_api <- function(user_id,
                                                  login_code) {

  # Define the request body as a list
  request_body <- list(
    user_id = user_id,
    login_code = login_code
  )

  endpoint_wrapper(function_name = "add-user-to-group-with-login-code",
                   request_body = request_body)

}



# This is the function that is called when the endpoint
# is invoked
add_user_to_group_with_login_code <- function(user_id,
                                              login_code) {

  logging::loginfo("Inside append_session function")

  logging::loginfo("user_id = %s", user_id)

  logging::loginfo("login_code = %s", login_code)


  response <- tryCatch({

    # Append session
    groups <- get_table(db_con, "groups")

    potential_group_to_join <- groups %>%
      dplyr::collect() %>%
      dplyr::filter(sign_up_code == !! login_code)

    if(nrow(potential_group_to_join) == 0) {
      stop("No groups with this sign up code")
    } else {

      group_id <- potential_group_to_join %>%
        dplyr::pull(group_id)

      user_group_df <- tibble::tibble(user_id = user_id,
                                      group_id = group_id)

      user_group_id <- db_append_to_table(db_con, table = "user_groups", data = user_group_df, primary_key_col = "user_group_id")

    }


    # Return response

   list(
      status = 200,
      message = paste0("You have successfully added user ", user_id, " to group ", group_id, "."),
      user_group_id = user_group_id,
      user_id = user_id,
      group_id = group_id
    )


  }, error = function(err) {
    logging::logerror(err)
     list(
        status = 400,
        message = "Something went wrong",
        session_id = integer(0)
      )
  })


  return(response)

}



# Test
# t <- add_user_to_group_with_login_code_api(
#   user_id = 1L,
#   login_code = "testfail")



