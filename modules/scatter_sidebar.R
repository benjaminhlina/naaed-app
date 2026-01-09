# mod_scatter_sidebar.R

scatter_sidebar_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    useShinyjs(),
    div(id = ns("scatter_ui"),
        style = "display:none;",
        shiny::selectInput(ns("scatter_grouping_vars"),
                           "Select Grouping Variables",
                           choices = NULL, multiple = TRUE),
        shiny::selectInput(ns("scatter_waterbody_filter"),
                           "Select Waterbody", choices = NULL,
                           multiple = TRUE),
        shiny::selectInput(ns("scatter_species_filter"),
                           "Select Species", choices = NULL,
                           multiple = TRUE),
        shiny::selectizeInput(ns("x_var"),
                           "Select X Variable", choices = NULL,
                           options = list(
                             render = I("
                             {
            option: function(item, escape) {
              return '<div>' + item.label + '</div>';
            },
            item: function(item, escape) {
              return '<div>' + item.label + '</div>';
            }
          }"
                             )
                           )
        ),
        shiny::selectizeInput(
          ns("y_var"),
          "Select Y Variable",
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
        )
    )
  )
}

scatter_sidebar_server <- function(id, con, main_input) {
  moduleServer(id, function(input, output, session) {

    observe({
      shinyjs::toggle(id = "scatter_ui",
                      condition = main_input$tabs == "scatter_plot")
    })

    sidebar_df <- get_sidebar_df(con)

    exclusive_all_observer(input, session, "summary_waterbody_filter")
    exclusive_all_observer(input, session, "summary_species_filter")
    observeEvent(input$scatter_plots, {
      # req(input$scatter_plots)
      # req(main_input$tabs == "scatter_plots")
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



      numeric_choices <- setdiff(numeric_choices, c(
        "calorimeter_conversion_factor",
        "issue",
        "length_mm",
        "energy_measurment",
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
      #       # Grouping Variables: Allow dynamic selection
      updateSelectInput(session, "scatter_grouping_vars",
                        choices = grouping_choices,
      )

      # Waterbody Drop-down
      updateSelectInput(session, "scatter_waterbody_filter",
                        choices = c("All", waterbody_choices),
                        selected = "All")

      # Species Drop-down
      updateSelectInput(session, "scatter_species_filter",
                        choices = c("All", species_choices),
                        selected = "All")


      x_choices <- make_scatter_choices(df, numeric_choices) |>
        sort()

      x_choices_sel <- names(x_choices)[1]

      updateSelectInput(session, "x_var",
                        choices = x_choices,
                        selected = x_choices_sel)

      # Update scatter variable choices
      y_choices <- make_scatter_choices(df, numeric_choices) |>
        sort()

      updateSelectizeInput(session, "y_var",
                           choices = y_choices,
                           server = TRUE)

    }, ignoreInit = FALSE)
    # ---- export what we need from the severer ----
    # we need grouping and hist variables we also need the function


    return(list(
      grouping_vars = reactive(input$scatter_grouping_vars),
      waterbody_filter = reactive(input$scatter_waterbody_filter),
      species_filter = reactive(input$scatter_species_filter),
      y_choices = reactive(input$y_var),
      x_choices = reactive(input$x_var)
    )
    )
  }
  )
}

