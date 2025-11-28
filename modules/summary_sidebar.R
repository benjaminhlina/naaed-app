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
      shiny::selectizeInput(ns("summary_y_variable"),
                         "Select Summary Columns of Interest",
                         choices = NULL,
                         multiple = TRUE,
                         options = list(placeholder = 'Select columns...')),
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


summary_sidebar_server <- function(id, con, main_input) {
  moduleServer(id, function(input, output, session) {

    observe({

      df <- get_summary_data(con, get_selected_table(main_input))
      grouping_choices <- get_good_groups(df)
      numeric_choices <- get_numeric_cols(df)

      cat("\n[DEBUG] Updating dropdowns...\n")
      # df is summary data
      cat("[DEBUG] Waterbody unique values:", length(unique(df$Waterbody)), "\n")
      cat("[DEBUG] Species unique values:", length(unique(df$`Common Name`)), "\n")
      # Grouping Variables: Allow dynamic selection


      updateSelectInput(session, "summary_grouping_vars",
                        choices = grouping_choices,
                        selected = c("Waterbody",
                                     "Common Name")
      )
      updateSelectInput(session, "summary_waterbody_filter",
                        choices = c("All", sort(unique(df$Waterbody))),
                        selected = "All")

      # Species Drop-down
      updateSelectInput(session, "summary_species_filter",
                        choices = c("All", sort(unique(df$`Common Name`))),
                        selected = "All")


        # Update histogram variable choices
        updateSelectizeInput(session, "hist_var",
                             choices = setNames(numeric_choices, numeric_choices),
                             # selected = "test",
                             server = TRUE)
        # Update histogram variable choices
        updateSelectizeInput(session, "summary_y_variable",
                             choices = setNames(numeric_choices, numeric_choices),
                             # selected = "test",
                             server = TRUE)

    })
  })
}
