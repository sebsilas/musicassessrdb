

# user_ids <-
#
# get_trial_and_session_data_api()


run_admin_app <- function() {

  # Define UI
  ui <- fluidPage(

    # App title
    titlePanel("musicassessr Admin"),

    # Sidebar layout with input and output definitions
    sidebarLayout(
      sidebarPanel(
        shiny::selectInput("user_ids",
                           "Select by user",
                           multiple = TRUE)
      ),

      # Main panel for displaying outputs
      mainPanel(
        # Output
      )
    )
  )

  # Define server logic
  server <- function(input, output, session) {

    observe({


      # Can also set the label and select items
      updateSelectInput(session,
                        "user_ids",
                        choices = x)
    })

  }

  # Run the application
  shinyApp(ui = ui, server = server)

}
