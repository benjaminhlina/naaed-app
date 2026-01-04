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

    # ---- namespaces -----
    ns <- session$ns
    # ----- first create summary data -----
    # cli::cli_alert_info(
    #   "summary_info_server: y_variable = {.val {summary_sidebar_vals$y_variable()}}"
    # )
    y_vars <- reactive({
      cli::cli_inform("y_vars entered")
      # req(summary_sidebar_vals$y_variable())

      val <- summary_sidebar_vals$y_variable()

      cli::cli_ul(c(
        "y_variable value: {if (is.null(val)) 'NULL' else paste(val, collapse = ', ')}",
        "length(y_variable): {length(val)}"
      ))
      req(val)

      val
    })
    # group_vars <- reactive({
    #   req(summary_sidebar_vals$grouping_vars())
    #
    #   summary_sidebar_vals$grouping_vars()
    # })
    summary_data <- create_summary_data(con = con,
                                        main_input = main_input,
                                        tab = "summary_info")

    check_summary_data(summary_data)

    numeric_cols <- create_numeric_col(data = summary_data)
    # # ---- Generate Summary Statistics with Dynamic Grouping -----
    filtered_summary_data <- create_filtered_data(
      input_source = summary_sidebar_vals,
      data = summary_data)


    mean_summary_data <- create_mean_data(input_source = summary_sidebar_vals,
                                          data = filtered_summary_data,
                                          numeric_cols = numeric_cols)


    #  ----- Render Summary Table -----
    display_table(data = mean_summary_data, output)

    # # ---- add in histogram ----
    display_hist(data = filtered_summary_data,
                 input_source = summary_sidebar_vals,
                 output)

    # we need to return fileted summary to then use in donload
    return(list(
      summary_data = mean_summary_data
    ))

  }
  )
}

