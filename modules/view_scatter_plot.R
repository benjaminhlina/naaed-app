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
