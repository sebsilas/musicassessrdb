

demographics_form <- function(welcome_page = musicassessr::songbird_welcome_page_fun,
                              save_results_to_disk = FALSE) {

  psychTestR::make_test(

    psychTestR::join(

      psyquest::DEG(),
      psyquest::SES(),
      psyquest::GMS(),

      psychTestR::code_block(function(state, ...) {




        results <- psychTestR::get_results(state, complete = TRUE, add_session_info = TRUE) %>%
          as.data.frame() %>%
          tidyr::unnest_wider(DEG.q9, names_sep = "_") %>%
          tidyr::unnest_wider(DEG.Handedness, names_sep = "_")

        results[is.na(results)] <- 987654321


        user_id <- as.integer(psychTestR::get_global("user_id", state))

        logging::loginfo("user_id... %s", user_id)
        logging::loginfo("dim(results) %s", dim(results))
        logging::loginfo("results %s", results)

        api_res <- store_db_demographics_api(user_id, results) # Here we don't want a future, we want to be synchronous and show the user the result

        psychTestR::set_global("api_res", api_res, state)


      }),

      if(save_results_to_disk) psychTestR::elt_save_results_to_disk(complete = TRUE),

      # Success or failure
      psychTestR::reactive_page(function(state, ...) {

        api_res <- psychTestR::get_global("api_res", state)

        api_res <- if(length(api_res) == 0) list(status = FALSE) else api_res

        logging::loginfo("API res...")
        logging::loginfo(api_res)

        if(length(api_res$status) == 0) {
          page <- psychTestR::final_page("Something went wrong, please contact the administrator.")
        } else if(api_res$status == 200) {
          page <- musicassessr::redirect_page(text = "Thank you, you will now be redirected back to your SongBird account.",
                                              ms = 3000,
                                              url = "https://songbird.training/dashboard",
                                              final = TRUE)
        } else {
          page <- psychTestR::final_page(api_res$message)
        }

        page
      })


    ) %>% validate_user_entry_into_test(validate_user_entry_into_test = welcome_page),

    opt = psychTestR::test_options(
      title = "Profile Setup",
      admin_password = Sys.getenv('ADMIN_PW'),
      on_start_fun = musicassessr_shiny_init,
      on_stop_fun = musicassessr_shiny_on_stop,
      enable_admin_panel = FALSE,
      display = psychTestR::display_options(css = 'https://adaptiveeartraining.com/assets/css/style_songbird.css')
    )
  )
}


# demographics_form()

