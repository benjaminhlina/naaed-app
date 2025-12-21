summary_sidebar_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::conditionalPanel(
      condition = "input.tabs == 'summary_info'",
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
                            options = list(placeholder = 'Select columns...',
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

    summary_df <- reactive({

      con_db <- if (inherits(con, "reactive")) con() else con
      req(con_db)

      get_summary_data(
        con = con_db
      )
    })

    observeEvent(summary_df(), {
      # get df
      df <- summary_df()

      # get grouping snad numerical values
      grouping_choices <- get_good_groups(df)

      numeric_choices <- get_numeric_cols(df)

      # remove Length (mm) from numerical choices
      numeric_choices <- setdiff(numeric_choices, "Length (mm)")
      # get length variables
      length_vars <- get_length_vars(df)

      # create summary choices
      summary_choices <- sort(c(setNames(numeric_choices,
                                         numeric_choices),
                                length_vars))

      waterbody_choices <- unique(df$Waterbody) |>
        sort()

      common_name_choices <- unique(df$`Common Name`) |>
        sort()

      # make console talk about what it is doing
      # check_dropdowns()
      cli::cli_alert_success("Updating dropdowns")
      cli::cli_ul(c(
        "Waterbody unique values: {length(waterbody_choices)}",
        "Species unique values: {length(common_name_choices)}",
        "Grouping choices: {paste(grouping_choices, collapse = ', ')}",
        "Numeric choices: {paste(numeric_choices, collapse = ', ')}"
      ))
      # Grouping Variables: Allow dynamic selection


      updateSelectInput(session, "summary_grouping_vars",
                        choices = grouping_choices,
                        selected = c("Waterbody",
                                     "Common Name")
      )
      updateSelectInput(session, "summary_waterbody_filter",
                        choices = c("All", waterbody_choices),
                        selected = "All")


      # Species Drop-down
      updateSelectInput(session, "summary_species_filter",
                        choices = c("All", common_name_choices),
                        selected = "All")


      # Update y summary  variable choices
      updateSelectizeInput(session, "summary_y_variable",
                           choices = summary_choices,
                           server = TRUE)

      # Update histogram variable choices
      updateSelectizeInput(session, "hist_var",
                           choices = summary_choices,
                           server = TRUE)

    }, ignoreInit = TRUE)
    # make this into a function that sidebar exports out
    register_summary <- function(summary_info) {
      observe({
        df <- summary_info$summary_data()  # reactive from summary
        output$download_summary <- downloadHandler(
          filename = function() {
            tbl <- get_selected_table(main_input)
            paste0(tbl, "_summary_", Sys.Date(), ".xlsx")
          },
          content = function(file) {
            req(df)
            writexl::write_xlsx(df, file)
          }
        )

        # toggle button
        shinyjs::toggleState(session$ns("download_summary"),
                             condition = !is.null(df) && nrow(df) > 0)
      })
    }

    # ----- export what we need from the severer ----
    # we need grouping and hist variables we also need the function
    #

    return(list(
      grouping_vars = reactive(input$summary_grouping_vars),
      waterbody_filter = reactive(input$summary_waterbody_filter),
      species_filter = reactive(input$summary_species_filter),
      y_variable = reactive(input$summary_y_variable),
      hist_vars = reactive(input$hist_var),
      register_summary = register_summary
    ))



  })
}
