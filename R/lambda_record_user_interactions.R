

record_user_interaction <- function(user_id,
                                    page_name,
                                    element_name,
                                    action_type = "click",
                                    metadata = NULL,
                                    date_time = Sys.time()) {
  logging::loginfo("Inside record_user_interaction function")

  logging::loginfo("user_id = %s", user_id)
  logging::loginfo("page_name = %s", page_name)
  logging::loginfo("element_name = %s", element_name)
  logging::loginfo("action_type = %s", action_type)
  logging::loginfo("date_time = %s", date_time)

  response <- tryCatch({

    # Build interaction record
    interaction_df <- tibble::tibble(
      user_id      = user_id,
      page_name    = page_name,
      element_name = element_name,
      action_type  = action_type,
      metadata     = ifelse(is.null(metadata), NA, jsonlite::toJSON(metadata, auto_unbox = TRUE)),
      date_time    = date_time
    )

    interaction_id <- db_append_to_table(
      db_con,
      table = "user_interactions",
      data = interaction_df,
      primary_key_col = "interaction_id"
    )

    list(
      status = 200,
      message = paste0("Interaction recorded successfully (interaction_id = ", interaction_id, ")."),
      interaction_id = interaction_id
    )

  }, error = function(err) {
    logging::logerror(err)
    list(
      status = 400,
      message = "Something went wrong",
      interaction_id = integer(0)
    )
  })

  return(response)
}
