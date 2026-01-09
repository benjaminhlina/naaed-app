summary_sidebar_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    useShinyjs(),
    div(id = ns("summary_ui"),
        style = "display:none;",
        shiny::conditionalPanel(
          condition = "input.tabs == 'summary_info'",
          shiny::selectInput(ns("summary_grouping_vars"),
                             "Select Grouping Variables",
                             choices = NULL, multiple = TRUE),
          shiny::selectInput(ns("summary_waterbody_filter"),
                             "Select Waterbody", choices = NULL,
                             multiple = TRUE),
          shiny::selectInput(ns("summary_species_filter"),
                             "Select Species",
                             choices = NULL,
                             multiple = TRUE),
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
  )
}



summary_sidebar_server <- function(id, con, main_input) {
  moduleServer(id, function(input, output, session) {

    observe({
      shinyjs::toggle(id = "summary_ui",
                      condition = main_input$tabs == "summary_info")
    })

    # --- get sidebar info -----
    sidebar_df <- get_sidebar_df(con)

    exclusive_all_observer(input, session, "summary_waterbody_filter")
    exclusive_all_observer(input, session, "summary_species_filter")
    # ---- go into observe event
    observeEvent(main_input$tabs, {
      req(main_input$tabs == "summary_info")
      # sidebar_df <- get_sidebar_df(con)
      #
      # exclusive_all_observer(input, session, "summary_waterbody_filter")
      # exclusive_all_observer(input, session, "summary_species_filter")
      # get df
      df <- sidebar_df()
      req(df)

      # get grouping snad numerical values
      grouping_choices <- get_groups(df) |>
        sort()

      grouping_choices <- setNames(grouping_choices,
                                   convert_nice_name(grouping_choices))

      numeric_choices <- get_numeric_vars(con)

      # ---- remove grouping or non needed variblaes ----

      numeric_choices <-  setdiff(numeric_choices, c(
        "calorimeter_conversion_factor",
        "issue",
        "length_mm",
        "energy_measurement",
        "latitude",
        "longitude",
        "month",
        "publication_year",
        "site",
        "site_depth",
        "source_id",
        "user_sample_id",
        "sample_year",
        "volume")
      )
      numeric_names <- convert_nice_name(numeric_choices)
      # get length variables
      length_vars <- get_var_types(df, var = "length_type")
      energy_vars <- get_var_types(df, var = "energy_units")

      # create summary choices
      summary_choices <- sort(c(setNames(numeric_choices,
                                         numeric_names),
                                length_vars, energy_vars))

      waterbody_choices <- df |>
        distinct(waterbody) |>
        arrange(waterbody) |>
        pull(waterbody)

      # species
      species_choices <-  df |>
        distinct(scientific_name) |>
        arrange(scientific_name) |>
        pull(scientific_name)

      n_wb <- length(waterbody_choices)
      n_sp <- length(species_choices)
      grp <- paste(grouping_choices, collapse = ', ')
      nc <- paste(summary_choices, collapse = ', ')
      # check_dropdowns()
      cli::cli_alert_success("Updating dropdowns")
      cli::cli_ul(c(
        "Waterbody unique values: {.val {n_wb}}",
        "Species unique values: {.val {n_sp}}",
        "Grouping choices: {.val {grp}}",
        "Numeric choices: {.val {nc}}"
      ))
      # Grouping Variables: Allow dynamic selection


      updateSelectInput(session, "summary_grouping_vars",
                        choices = grouping_choices,
                        selected = c("waterbody",
                                     "scientific_name")
      )
      updateSelectInput(session, "summary_waterbody_filter",
                        choices = c("All", waterbody_choices),
                        selected = "All")


      # Species Drop-down
      #

      updateSelectInput(session, "summary_species_filter",
                        choices = c("All", species_choices),
                        selected = "All")
      #
      #
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
    register_summary <- function(input_source) {

      output$download_summary <- downloadHandler(
        filename = function() {
          paste0("glatar_summary_tbl_", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          req(input_source)
          df <- input_source$summary_df()()
          req(df)
          writexl::write_xlsx(df, file)
        }
      )

      observe({
        req(input$tabs == "summary_info")
        req(input_source)
        df <- input_source$summary_df()()

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
