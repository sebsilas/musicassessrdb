
# run_dashboard_app()

#' Run dashboard app
#'
#' @returns
#' @export
#'
#' @examples
run_dashboard_app <- function() {

  ui <- shiny::fluidPage(
    shiny::titlePanel("musicassessr Dashboard"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(

        shiny::selectInput("app_name", "App:", choices = c("Songbird",
                                                           "Slonimsky",
                                                           "Both")),

        shiny::selectInput("score_measure", "Score measure:", choices = NULL)

      ),
      shiny::mainPanel(
        shiny::plotOutput("scores_by_attempt") %>% shinycssloaders::withSpinner(),
        shiny::plotOutput("scores_by_date") %>% shinycssloaders::withSpinner(),
        shiny::plotOutput("scores_by_prediction_method") %>% shinycssloaders::withSpinner(),
        shiny::plotOutput("no_practice_sessions_graph") %>% shinycssloaders::withSpinner(),
        shiny::plotOutput("avg_minutes_graph") %>% shinycssloaders::withSpinner(),
        DT::DTOutput("trials_table") %>% shinycssloaders::withSpinner(),
        DT::DTOutput("sessions_table")  %>% shinycssloaders::withSpinner()
      )
    )
  )

  server <- function(input, output, session) {

    # Connect to the database
    db_con <- musicassessr_con()

    session$onSessionEnded(function() {
      db_disconnect(db_con)
    })

    # Fetch data from database
    session_data <- shiny::reactive({

      shiny::req(input$app_name)

      app_name <- tolower(input$app_name)

      sessions <- dplyr::tbl(db_con, "sessions") %>%
        dplyr::left_join(dplyr::tbl(db_con, "users"), by = "user_id") %>%
        dplyr::mutate(Date = lubridate::as_date(session_time_started))

      if(app_name != "both") {
        sessions <- sessions %>%
          dplyr::filter(app_name == !! app_name)
      }

      sessions <- sessions %>%
        dplyr::collect()

      return(sessions)

    })

    trial_scores <- shiny::reactive({

      dplyr::tbl(db_con, "scores_trial")

    })

    score_measures <-  shiny::reactive({
      trial_scores() %>%
        dplyr::pull(measure) %>%
        unique()
    })

    shiny::observe({

      shiny::req( score_measures() )

      shiny::updateSelectInput("score_measure",
                               session = session,
                               choices = score_measures(),
                               selected = "opti3")

    })



    trials_data <- shiny::reactive({

      session_ids <- session_data() %>%
        dplyr::pull(session_id)

      trials <- dplyr::tbl(db_con, "trials") %>%
        dplyr::filter(session_id %in% !! session_ids)

      trial_ids <- trials %>%
        dplyr::pull(trial_id)

      trial_scores <- trial_scores() %>%
        dplyr::filter(trial_id %in% trial_ids,
                      measure == !! input$score_measure)

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
        dplyr::mutate(Date = lubridate::as_date(trial_time_started))

    })

    output$sessions_table <- DT::renderDT({

      session_data() %>%
        DT::datatable()

    })

    # Render table
    output$trials_table <- DT::renderDT({

      trials_data() %>%
        dplyr::collect() %>%
        DT::datatable()

    })

    # Score by attempt

    output$scores_by_attempt <- shiny::renderPlot({

      trials_data() %>%
        dplyr::group_by(attempt) %>%
        dplyr::summarise(score = mean(score, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
          ggplot2::ggplot(ggplot2::aes(x = attempt, y = score)) +
            ggplot2::geom_point() +
            ggplot2::geom_line(group = 1)
    })

    output$scores_by_date <- shiny::renderPlot({

      trials_data() %>%
        dplyr::group_by(Date) %>%
        dplyr::summarise(score = mean(score, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        ggplot2::ggplot(ggplot2::aes(x = Date, y = score)) +
        ggplot2::geom_point() +
        ggplot2::geom_line(group = 1)
    })

    output$scores_by_prediction_method <- shiny::renderPlot({

      trials_data() %>%
        dplyr::collect() %>%
        ggplot2::ggplot(ggplot2::aes(x = prediction_method, y = score, group = item_selection_type, color = prediction_method)) +
          ggplot2::geom_bar(stat = "summary", fun = "mean")

    })


    # User stats

    user_stats <- shiny::reactive({
      compute_user_stats( session_data() )
    })

    # Average minutes per day

    avg_minutes_data <- shiny::reactive({
      print('avg')
      print(user_stats())
      user_stats() %>%
        dplyr::group_by(Date) %>%
        dplyr::summarise(avg_minutes_spent = mean(minutes_spent, na.rm = TRUE) ) %>%
        dplyr::ungroup()

    })

    # No. practice sessions

    no_practice_sessions_data <- shiny::reactive({
      print('no_practice_sessions_data')
      print(user_stats())
      user_stats() %>%
        dplyr::group_by(Date) %>%
        dplyr::summarise(no_practice_sessions = sum(no_practice_sessions, na.rm = TRUE) ) %>%
        dplyr::ungroup()
    })

    # Avg minutes graph
    output$avg_minutes_graph <- shiny::renderPlot({

      print('avg_graph')
      print(avg_minutes_data())

      avg_minutes_data() %>%
        ggplot2::ggplot(ggplot2::aes(x = Date, y = avg_minutes_spent)) +
            ggplot2::geom_point() +
            ggplot2::geom_line(group = 1)
    })

    # No. practice sessions graph
    output$no_practice_sessions_graph <- shiny::renderPlot({

      print('no_practice_sessions_data_graph')
      print(no_practice_sessions_data())

      no_practice_sessions_data() %>%
        ggplot2::ggplot(ggplot2::aes(x = Date, y = no_practice_sessions)) +
          ggplot2::geom_point() +
          ggplot2::geom_line(group = 1)
    })






  }

  # Run the app
  shiny::shinyApp(ui, server)
}




