view_summary_info_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    # tabName = id,
    shiny::h2("Summary Statistics"),
    shiny::br(),
    shiny::fluidRow(
      shinydashboard::box(
        title = "Summary Table",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        div(style = "overflow-x: auto; width: 100%;",
            DT::DTOutput(ns("summary_table_output")))
      )
    ),
    shiny::br(),
    plot_ui(title = "Summary Histograms",
            plot_id = "summary_histogram",
            height = "300px",
            ns = ns),
  )
}

# ----- summmary server --------
summary_info_server <- function(id, con, main_input, summary_sidebar_vals) {
  moduleServer(id, function(input, output, session) {
    observeEvent(main_input$tabs, {
      shinyjs::toggle(
        id = "summary_ui",
        condition = main_input$tabs == "summary_info"
      )
    }, ignoreInit = TRUE)

    # ---- namespaces -----
    ns <- session$ns
    # ----- first create summary data -----
    observeEvent(main_input$tabs, {
      req(main_input$tabs == "summary_info")
      summary_data <- create_summary_data(con = con,
                                          main_input = main_input,
                                          input_source = summary_sidebar_vals,
                                          tab = "summary_info")
      # Cehck if summary is being triggered
      check_summary_data(summary_data)

      # filtered summary by waterbody and species
      filtered_summary_data <- create_filtered_data(
        input_source = summary_sidebar_vals,
        data = summary_data)

    # # ---- Generate Summary Statistics with Dynamic Grouping -----
    display_table(data = summary_mean_df, output)
      # # ---- Generate Summary Statistics with Dynamic Grouping -----
      summary_mean_df <- create_mean_data(input_source = summary_sidebar_vals,
                                          data = filtered_summary_data)

      #  ----- Render Summary Table -----
      display_table(data = summary_mean_df, output)

      # # ---- add in histogram ----
      display_hist(data = filtered_summary_data,
                   input_source = summary_sidebar_vals,
                   output)

  }
  )
      # we need to return fileted summary to then use in donload
      return(list(
        # summary_data = mean_summary_data
      ))

    }, ignoreInit = TRUE)
  })
}

