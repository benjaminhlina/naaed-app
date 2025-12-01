# mod_scatter_sidebar.R

scatter_sidebar_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    useShinyjs(),
    div(id = ns("scatter_ui"),
        style = "display:none;",
      shiny::selectInput(ns("scatter_plots"),
                         "Select Table",
                         choices = c("Calorimetry" = "tbl_calorimetry",
                                     "Proximate Composition" = "tbl_proxcomp",
                                     "Isotopes" = "tbl_isotope"),
                         selected = "tbl_calorimetry"),
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
                         "Select Y Variable", choices = NULL)
    )
  )
}

scatter_sidebar_server <- function(id, con, main_input) {
  moduleServer(id, function(input, output, session) {

    observe({
      shinyjs::toggle(id = "scatter_ui",
                      condition = main_input$tabs == "scatter_plot")
    })

    observeEvent(input$scatter_plots, {
      req(input$scatter_plots)

      df <- get_summary_data(con, input$scatter_plots)
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


      x_choices <- make_scatter_choices(df, numeric_choices, "x")

      updateSelectInput(session, "x_var",
                        choices = x_choices,
                        selected = names(x_choices)[1])

      # Update scatter variable choices
      y_choices <- make_scatter_choices(df, numeric_choices, "y")

      updateSelectizeInput(session, "scatter_var",
                           choices = y_choices,
                           server = TRUE)

    })
    # ---- export what we need from the severer ----
    # we need grouping and hist variables we also need the function


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

