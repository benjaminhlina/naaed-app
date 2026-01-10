view_scatter_plot_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::tabItem(

    useShinyjs(),
    div(id = ns("scatter_ui"),
        style = "display:none;",
                          h2("Scatter Plot"),
                          plot_ui(title = "Scatter Plot",
                                  plot_id = "scatter_plot",
                                  height = "600px",
                                  ns = ns)
  )
  )
}


scatter_plot_server <- function(id, con, main_input, scatter_sidebar_vals) {
  moduleServer(id, function(input, output, session) {

    observeEvent(main_input$tabs, {
      shinyjs::toggle(
        id = "scatter_ui",
        condition = main_input$tabs == "scatter_plot"
      )
    }, ignoreInit = TRUE)



    # ---- namespaces
    ns <- session$ns

    # scatter_data <- reactiveVal(NULL)
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


    cli::cli_alert_warning("Class of scatter_data: {.val {class(scatter_data)}}")
    # ---- allow filtering -----
    filtered_summary_data <- create_filtered_data(
      input_source = scatter_sidebar_vals,
      data = scatter_data)
                         input_source = scatter_sidebar_vals,
                         output)
  },
  ignoreInit = TRUE
  )
  }
  )
}
