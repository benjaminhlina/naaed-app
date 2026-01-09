view_scatter_plot_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::tabItem(tabName = id,
                          h2("Scatter Plot"),
                          plot_ui(title = "Scatter Plot",
                                  plot_id = "scatter_plot",
                                  height = "600px",
                                  ns = ns)
  )
}


scatter_plot_server <- function(id, con, main_input, scatter_sidebar_vals) {
  moduleServer(id, function(input, output, session) {

    # ---- namespaces
    ns <- session$ns

    scatter_data <- reactiveVal(NULL)
    observeEvent(main_input$tabs, {
      req(main_input$tabs == "scatter_plot")
    # Make scatter raw data
    scatter_data <- create_summary_data(con = con,
                                        main_input = main_input,
                                        input_source = scatter_sidebar_vals,
                                        tab = "scatter_plot",
                                        var_field = c(
                                          "x_choices",
                                          "y_choices"

                                        ))
    cli::cli_alert_info("scatter_data is: {.val {class(scatter_data)}}")
    check_summary_data(scatter_data())



    filtered_scatter_data <- create_filtered_data(
      input_source = scatter_sidebar_vals,
      data = scatter_data)
    #
    display_scatter_plot(data = filtered_scatter_data,
                         input_source = scatter_sidebar_vals,
                         output)
  },
  ignoreInit = TRUE
  )
  }
  )
}
