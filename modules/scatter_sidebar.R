# mod_scatter_sidebar.R

scatter_sidebar_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::conditionalPanel(
      condition = "input.tabs == 'scatter_plots'",
      shiny::selectInput(ns("scatter_plots"),
                         "Select Table",
                         choices = c("tbl_calorimetry", "tbl_proxcomp",
                                     "tbl_isotope")),
      shiny::selectInput(ns("scatter_grouping_vars"),
                         "Select Grouping Variables",
                         choices = NULL, multiple = TRUE),
      shiny::selectInput(ns("scatter_waterbody_filter"),
                         "Select Waterbody", choices = NULL),
      shiny::selectInput(ns("scatter_species_filter"),
                         "Select Species", choices = NULL),
      shiny::selectInput(ns("x_var"),
                         "Select X Variable", choices = NULL)
    )
  )
}
