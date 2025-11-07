summary_sidebar_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::conditionalPanel(
      condition = "input.tabs == 'summary_info'",
      shiny::selectInput(ns("summary_table"), "Select Table",
                         choices = c("tbl_calorimetry", "tbl_proxcomp",
                                     "tbl_isotope")),
      shiny::selectInput(ns("summary_grouping_vars"),
                         "Select Grouping Variables",
                         choices = NULL, multiple = TRUE),
      shiny::selectInput(ns("summary_waterbody_filter"),
                         "Select Waterbody", choices = NULL),
      shiny::selectInput(ns("summary_species_filter"),
                         "Select Species",
                         choices = NULL),
      shiny::selectInput(ns("summary_y_variable"),
                         "Select Summary Columns of Interest",
                         choices = NULL,
                         multiple = TRUE),
      shiny::selectizeInput(
        inputId = ns("hist_var"),
        label = "Select Variable for Histogram",
        choices = NULL,
        options = list(
          render = I("
          {
            option: function(item, escape) {
              return '<div>' + item.label + '</div>';
            },
            item: function(item, escape) {
              return '<div>' + item.label + '</div>';
            }
          }
        ")
        )
      ),
      shiny::downloadButton(ns("download_summary"),
                            "Download Summary as Excel",
                            class = "btn-primary")
    )
  )
}
