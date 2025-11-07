plot_ui <- function(title, plot_id, height, ...) {
  args <- list(...)
  ns <- args$ns

  shiny::fluidRow(
    shinydashboard::box(
      title = title,
      status = "info",
      solidHeader = TRUE,
      width = 12,
      # plotlyOutput("summary_histogram", height = "300px")
      shiny::plotOutput(ns(plot_id), height = height)
    )
  )
}
