
run_dashboard_app <- function() {

  ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "musicassessr Dashboard"),

    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("User Statistics", tabName = "user_stats", icon = shiny::icon("users")),
        shinydashboard::menuItem("Scores", tabName = "scores", icon = shiny::icon("chart-line")),
        shinydashboard::menuItem("Tables", tabName = "tables", icon = shiny::icon("table")),

        shiny::selectInput("app_name", "App:", choices = c("Songbird", "Slonimsky", "All (including psychTestR etc. apps)")),
        shiny::selectInput("score_measure", "Score measure:", choices = NULL)
      )
    ),

    shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        # User Statistics Tab
        shinydashboard::tabItem(tabName = "user_stats",
                                shiny::fluidRow(
                                  shinydashboard::box(title = "Average Minutes Per Day", width = 6,
                                                      shiny::plotOutput("avg_minutes_graph") %>% shinycssloaders::withSpinner()),
                                  shinydashboard::box(title = "Number of Practice Sessions", width = 6,
                                                      shiny::plotOutput("no_practice_sessions_graph") %>% shinycssloaders::withSpinner())
                                ),
                                shiny::fluidRow(
                                  shinydashboard::box(title = "Trial Type Counts", width = 6,
                                                      shiny::plotOutput("trial_type_counts") %>% shinycssloaders::withSpinner()
                                )
                                )
        ),

        # Scores Tab
        shinydashboard::tabItem(tabName = "scores",
                                shiny::fluidRow(
                                  shinydashboard::box(title = "Scores by Attempt", width = 6,
                                                      shiny::plotOutput("scores_by_attempt") %>% shinycssloaders::withSpinner()),
                                  shinydashboard::box(title = "Scores by Date", width = 6,
                                                      shiny::plotOutput("scores_by_date") %>% shinycssloaders::withSpinner())
                                ),
                                shiny::fluidRow(
                                  shinydashboard::box(title = "Scores by Prediction Method", width = 6,
                                                      shiny::plotOutput("scores_by_prediction_method") %>% shinycssloaders::withSpinner()),
                                  shinydashboard::box(title = "Scores by Trial Type", width = 6,
                                                      shiny::plotOutput("scores_by_trial_type") %>% shinycssloaders::withSpinner())
                                )
        ),

        # Tables Tab
        shinydashboard::tabItem(tabName = "tables",
                                shiny::fluidRow(
                                  shinydashboard::box(title = "Select Table", width = 12,
                                                      shiny::radioButtons("table_select", "Choose Table:",
                                                                          choices = c("Sessions" = "sessions", "Trials" = "trials"),
                                                                          inline = TRUE)
                                  )
                                ),
                                shiny::fluidRow(
                                  shinydashboard::box(title = "Data Table", width = 12,
                                                      shiny::uiOutput("dynamic_table") %>% shinycssloaders::withSpinner())
                                )
        )
      )
    )
  )

  server <- function(input, output, session) {
    db_con <- musicassessr_con()

    session$onSessionEnded(function() {
      db_disconnect(db_con)
    })

    session_data <- shiny::reactive({
      shiny::req(input$app_name)
      app_name <- tolower(input$app_name)

      sessions <- dplyr::tbl(db_con, "sessions") %>%
        dplyr::left_join(dplyr::tbl(db_con, "users"), by = "user_id") %>%
        dplyr::mutate(app_name = dplyr::case_when(is.na(app_name) | !app_name %in% c("slonimsky", "songbird") ~ "Other", TRUE ~ app_name)) %>%
        dplyr::mutate(Date = lubridate::as_date(session_time_started))

      if (app_name != "all (including psychtestr etc. apps)") {
        sessions <- sessions %>% dplyr::filter(app_name == !!app_name)
      }

      sessions %>% dplyr::collect()
    })

    trial_scores <- shiny::reactive({
      dplyr::tbl(db_con, "scores_trial")
    })

    score_measures <- shiny::reactive({
      trial_scores() %>% dplyr::pull(measure) %>% unique()
    })

    shiny::observe({
      shiny::req(score_measures())
      shiny::updateSelectInput(session, "score_measure", choices = score_measures(), selected = "opti3")
    })

    trials_data <- shiny::reactive({

      session_ids <- session_data() %>% dplyr::pull(session_id)

      trials <- dplyr::tbl(db_con, "trials") %>%
        dplyr::filter(session_id %in% !!session_ids)

      trial_ids <- trials %>% dplyr::pull(trial_id)

      trial_scores <- trial_scores() %>%
        dplyr::filter(trial_id %in% trial_ids,
                      measure == !!input$score_measure)

      new_items <- dplyr::tbl(db_con, "new_items")
      review_items <- dplyr::tbl(db_con, "review_items")

      trials <- trials %>%
        dplyr::left_join(trial_scores, by = "trial_id") %>%
        dplyr::left_join(new_items %>%
                           dplyr::select(new_items_id, prediction_method) %>%
                           dplyr::rename(prediction_method_new_items = prediction_method), by = "new_items_id") %>%
        dplyr::left_join(review_items %>%
                           dplyr::select(review_items_id, prediction_method) %>%
                           dplyr::rename(prediction_method_review_items = prediction_method), by = "review_items_id") %>%
        tidyr::pivot_longer(contains("prediction_method"), names_to = "item_selection_type", values_to = "prediction_method") %>%
        dplyr::mutate(item_selection_type = stringr::str_remove_all(item_selection_type, "prediction_method_")) %>%
        dplyr::mutate(Date = lubridate::as_date(trial_time_started)) %>%
        dplyr::collect()

    })

    user_stats <- shiny::reactive({
      compute_user_stats(session_data())
    })

    output$trial_type_counts <- shiny::renderPlot({
      trials_data() %>%
        ggplot2::ggplot(ggplot2::aes(x = trial_paradigm, fill = trial_paradigm)) +
        ggplot2::geom_bar(stat = "count")
    })

    output$avg_minutes_graph <- shiny::renderPlot({
      user_stats() %>%
        dplyr::group_by(Date) %>%
        dplyr::summarise(avg_minutes_spent = mean(minutes_spent, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        ggplot2::ggplot(ggplot2::aes(x = Date, y = avg_minutes_spent)) +
        ggplot2::geom_point() + ggplot2::geom_line(group = 1)
    })

    output$no_practice_sessions_graph <- shiny::renderPlot({
      user_stats() %>%
        dplyr::group_by(Date) %>%
        dplyr::summarise(no_practice_sessions = sum(no_practice_sessions, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        ggplot2::ggplot(ggplot2::aes(x = Date, y = no_practice_sessions)) +
        ggplot2::geom_point() + ggplot2::geom_line(group = 1)
    })

    output$dynamic_table <- shiny::renderUI({
      if (input$table_select == "sessions") {
        DT::DTOutput("sessions_table")
      } else {
        DT::DTOutput("trials_table")
      }
    })


    output$scores_by_attempt <- shiny::renderPlot({
      trials_data() %>%
        dplyr::group_by(attempt) %>%
        dplyr::summarise(score = mean(score, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        ggplot2::ggplot(ggplot2::aes(x = attempt, y = score)) +
        ggplot2::geom_point() + ggplot2::geom_line(group = 1)
    })

    output$scores_by_date <- shiny::renderPlot({
      trials_data() %>%
        dplyr::group_by(Date) %>%
        dplyr::summarise(score = mean(score, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        ggplot2::ggplot(ggplot2::aes(x = Date, y = score)) +
        ggplot2::geom_point() + ggplot2::geom_line(group = 1)
    })

    output$scores_by_prediction_method <- shiny::renderPlot({
      trials_data() %>%
        ggplot2::ggplot(ggplot2::aes(x = prediction_method, y = score, fill = prediction_method)) +
        ggplot2::geom_bar(stat = "summary", fun = "mean")
    })

    output$scores_by_trial_type <- shiny::renderPlot({
      trials_data() %>%
        ggplot2::ggplot(ggplot2::aes(x = trial_paradigm, y = score, fill = trial_paradigm)) +
        ggplot2::geom_bar(stat = "summary", fun = "mean")
    })


    output$sessions_table <- DT::renderDT({
      session_data() %>% DT::datatable()
    })

    output$trials_table <- DT::renderDT({
      trials_data() %>% DT::datatable()
    })
  }

  shiny::shinyApp(ui, server)
}
