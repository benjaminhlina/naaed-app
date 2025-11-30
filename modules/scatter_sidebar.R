# mod_scatter_sidebar.R

scatter_sidebar_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::conditionalPanel(
      condition = "input.tabs == 'scatter_plot'",
      shiny::selectInput(ns("scatter_plots"),
                         "Select Table",
                         choices = c("Calorimetry" = "tbl_calorimetry",
                                     "Proximate Composition" = "tbl_proxcomp",
                                     "Isotopes" = "tbl_isotope")),
      shiny::selectInput(ns("scatter_grouping_vars"),
                         "Select Grouping Variables",
                         choices = NULL, multiple = TRUE),
      shiny::selectInput(ns("scatter_waterbody_filter"),
                         "Select Waterbody", choices = NULL),
      shiny::selectInput(ns("scatter_species_filter"),
                         "Select Species", choices = NULL),
      shiny::selectInput(ns("x_var"),
                         "Select X Variable", choices = NULL),
      shiny::selectInput(ns("scatter_var"),
                         "Select Y Variable", choices = NULL),
      shiny::downloadButton(ns("download_plot"),
                            "Save Plot",
                            class = "btn-primary")
    )
  )
}

scatter_sidebar_server <- function(id, con, main_input) {
  moduleServer(id, function(input, output, session) {
    observe({
      df <- get_summary_data(con, get_selected_table(main_input))
      grouping_choices <- get_good_groups(df)
      numeric_choices <- get_numeric_cols(df)
      req(df)

      #       # Grouping Variables: Allow dynamic selection
      updateSelectInput(session, "scatter_grouping_vars",
                        choices = grouping_choices,
                        # selected = c("Waterbody",
                        #              "Common Name")
      )

      # Waterbody Drop-down
      updateSelectInput(session, "scatter_waterbody_filter",
                        choices = c("All", sort(unique(df$Waterbody))),
                        selected = "All")

      # Species Drop-down
      updateSelectInput(session, "scatter_species_filter",
                        choices = c("All", sort(unique(df$`Common Name`))),
                        selected = "All")

      # Update histogram variable choices
      updateSelectizeInput(session, "scatter_var",
                           choices = setNames(numeric_choices, numeric_choices),
                           server = TRUE)
      # X Variable Options
      x_choices <- c(
        # "Total Length (mm)",
        # "Fork Length (mm)",
        "Weight")

      x_choices <- x_choices[x_choices %in% names(df)]  # Ensure they exist

      updateSelectInput(session, "x_var",
                        choices = x_choices,
                        selected = x_choices[1]
      )

    })
    register_plot <- function(plot) {
      observe({
        # df <- summary_info$summary_data()  # reactive  from summary
        # output$download_summary <- downloadHandler(
        #   filename = function() {
        #     tbl <- get_selected_table(main_input)
        #     paste0(tbl, "_summary_", Sys.Date(), ".xlsx")
        #   },
        #   content = function(file) {
        #     req(df)
        #     writexl::write_xlsx(df, file)
      }
      )

      # toggle button
      shinyjs::toggleState(session$ns("download_plot"))
    }


    # ----- export what we need from the severer ----
    # we need grouping and hist variables we also need the function
    #


    return(list(
      selected_table = reactive(input$scatter_plots),
      grouping_vars = reactive(input$scatter_grouping_vars),
      waterbody_filter = reactive(input$scatter_waterbody_filter),
      species_filter = reactive(input$scatter_species_filter),
      y_choices = reactive(input$scatter_var),
      x_choices = reactive(input$x_var)
    )
    )
  }
  )
}

