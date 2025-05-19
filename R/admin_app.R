

run_admin_app <- function() {

  # Define UI
  ui <- shiny::fluidPage(

    # App title
    shiny::titlePanel("musicassessr Admin"),

    # Sidebar layout with input and output definitions
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput("user_ids",
                           "Select by user",
                           choices = character(0L),
                           multiple = TRUE)
      ),

      # Main panel for displaying outputs
      shiny::mainPanel(

        shiny::plotOutput("no_sessions_plot") %>%
          shinycssloaders::withSpinner()
      )
    )
  )

  # Define server logic
  server <- function(input, output, session) {

    # Get user ids
    shiny::observe({

      user_ids <- get_user_ids_api()$user_id

      # Can also set the label and select items
      shiny::updateSelectInput(session,
                               "user_ids",
                               choices = user_ids)
    })


    output$no_sessions_plot <- shiny::renderPlot({

      shiny::req(input$user_ids)

      user_data <- get_trial_and_session_data_api(user_id = input$user_ids)

      user_stats <- user_data$user_stats %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(Date = lubridate::as_date(Date)) %>%
        dplyr::arrange(Date)


      user_stats %>%
        dplyr::rename(`No. Sessions` = no_practice_sessions) %>%
        ggplot2::ggplot(ggplot2::aes(x = Date, y = `No. Sessions`)) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        ggplot2::theme_minimal()

    }) %>% bindCache(input$user_ids)

  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)

}


# db_con <- musicassessr_con()
#
# t2 <- dplyr::tbl(db_con, "sessions") %>%
#   dplyr::filter(user_id == 60) %>%
#   dplyr::collect()
#

# user_data <- get_trial_and_session_data_api(user_id = 60)
# user_stats <- user_data$user_stats %>% dplyr::bind_rows()
# db_disconnect(db_con)


# user_data <- get_trial_and_session_data_api(user_id = 138, app_name_filter = "songbird")


# r <- user_data$review_melodies
