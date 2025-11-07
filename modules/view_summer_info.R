view_summary_info_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(tabName = id,
                 shiny::h2("Summary Statistics"),
                 # fluidRow(
                 #   valueBoxOutput("total_records", width = 4),
                 #   valueBoxOutput("unique_species", width = 4)
                 # ),
                 shiny::br(),

                 shiny::fluidRow(
                   shinydashboard::box(
                     title = "Summary Table",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     DT::DTOutput(ns("summary_table_output"))
                   )
                 ),
                 shiny::br(),
                 plot_ui(title = "Summary Histograms",
                         plot_id = "summary_histogram",
                         height = "300px",
                         ns = ns)

  )
}

# ----- summmary server --------
summary_info_server <- function(id, con, main_input) {
  moduleServer(id, function(input, output, session) {
    cat("[DEBUG] summary_info_server initialized for ID:", id, "\n")
    cat("[DEBUG] con object class:", class(con), "\n")

    ns <- session$ns
    # ----- first create summary datsa -----
    summary_data <- reactive({
      table_name <- get_selected_table(main_input)
      if (is.null(table_name) || is.na(table_name)) {
        cat("[DEBUG] table_name is NULL or NA! Cannot run query.\n")
      } else {
        cat("[DEBUG] table_name from get_selected_table():", table_name, "\n")
      }
      req(table_name)

      con_db <- if (inherits(con, "reactive")) con() else con
      # req(table_name)
      # con_db <- con
      # con_db <- con_reactive()
      # req(DBI::dbIsValid(con_db))
      get_summary_data(con = con_db, table_name)
    })

    observe({
      cat("\n[DEBUG] summary_data triggered\n")
      df <- try(summary_data(), silent = TRUE)
      if (inherits(df, "try-error")) {
        cat("[DEBUG] summary_data() failed completely\n")
      } else if ("Message" %in% names(df)) {
        cat("[DEBUG] get_summary_data() returned error message:", df$Message[1], "\n")
      } else {
        cat("[DEBUG] summary_data() rows:", nrow(df), "cols:", ncol(df), "\n")
        cat("[DEBUG] column names:\n")
        print(names(df))
      }
    })
    # ----- Dynamically Update Sidebar Dropdowns -----
    # Get numeric columns globally
    numeric_cols <- reactive({
      df <- summary_data()
      req(df)
      get_numeric_cols(df)
    })
    # ----- get groupsing columns
    grouping_cols <- reactive({
      df <- summary_data()
      req(df)
      get_good_groups(df)
    })

    observeEvent(summary_data(), {

      cat("\n[DEBUG] Updating dropdowns...\n")
      # df is summary data
      df <- summary_data()
      req(df)
      cat("[DEBUG] Waterbody unique values:", length(unique(df$Waterbody)), "\n")
      cat("[DEBUG] Species unique values:", length(unique(df$`Common Name`)), "\n")
      # Grouping Variables: Allow dynamic selection

      grouping_choices <- grouping_cols()

      updateSelectInput(session, "summary_grouping_vars",
                        choices = grouping_choices,
                        selected = c("Waterbody",
                                     "Common Name")
      )
      #
      # Waterbody Drop-down
      updateSelectInput(session, "summary_waterbody_filter",
                        choices = c("All", sort(unique(df$Waterbody))),
                        selected = "All")

      # Species Drop-down
      updateSelectInput(session, "summary_species_filter",
                        choices = c("All", sort(unique(df$`Common Name`))),
                        selected = "All")
      # Get numeric column choices
      numeric_choices <- numeric_cols()
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
    #
    #
    # # ---- Generate Summary Statistics with Dynamic Grouping -----
    filtered_summary_df <- reactive({
      df <- summary_data()


      req(df, input$summary_grouping_vars)
      # Apply filters - set ALL to no filter the data at all
      if (!(input$summary_waterbody_filter %in% "All")) {
        df <- df %>%
          filter(Waterbody == input$summary_waterbody_filter)
      }
      if (!(input$summary_species_filter %in% "All")) {
        df <- df %>%
          filter(`Common Name` == input$summary_species_filter)
      }
      return(df)
    })
    #   if (!(input$summary_y_variable %in% "All")) {
    #     df <- df %>%
    #       filter(`Common Name` == input$summary_species_filter)
    #   }
    #   return(df)
    # })

    filtered_summary <- reactive({
      df <- filtered_summary_df()  # Use the filtered data
      # req(df, input$summary_grouping_vars)
      # req(input$summary_y_variable)
      # ------ Dynamically group data -----
      summary_grouping_vars <- input$summary_grouping_vars
      summary_numeric_cols <- numeric_cols()  # Now calling the reactive expression
      # Ensure numeric columns exist and remove ids
      summary_numeric_cols <- names(df)[sapply(df, is.numeric)]
      summary_numeric_cols <- setdiff(summary_numeric_cols,
                                      c("sample_id",
                                        "source_id",
                                        "cal_id",
                                        "proxcomp_id",
                                        "iso_id",
                                        "Conversion Factor",
                                        "Composite (n)"))

      # if (length(summary_grouping_vars) > 0) {
      #   summary_df <- df %>%
      #     group_by(across(all_of(summary_grouping_vars)))
      # } else {
      #   summary_df <- df
      # }

      # create dynamic groupings and summary round all values to 2 decimials

      summary_df <- df %>%
        group_by(across(all_of(summary_grouping_vars))) %>%
        summarise(
          n = n(),
          across(all_of(intersect(input$summary_y_variable, summary_numeric_cols)),
                 list(
                   mean = ~mean(.x, na.rm = TRUE),
                   sd = ~sd(.x, na.rm = TRUE)
                 ),
                 .names = "{.col} ({.fn})"),
          .groups = "drop"
        ) %>%
        mutate_if(is.numeric, round, digits = 2)

      return(summary_df)
    })
    # )}

    #  ----- Render Summary Table -----
    output$summary_table_output <- renderDT({
      req(filtered_summary())
      df <- filtered_summary()
      # if there is nothing in df print no data available
      if (is.null(df) || nrow(df) == 0) {
        return(datatable(data.frame(Message = "No data available"),
                         escape = FALSE))
      }
      # colnames(df) <- get_nice_name(colnames(df))

      datatable(df,
                options = list(pageLength = 10,
                               scrollX = TRUE

                               # autoWidth = TRUE
                ), escape = FALSE)
    })


    # Update histogram variable choices
    # updateSelectInput(session, "hist_var",
    #                   choices = setNames(numeric_cols, numeric_cols),
    #                   selected = "energy_density_mean")



    # # ---- add in histogram ----
    # output$summary_histogram <- renderPlot({
    #   # Get raw data (not summarized)
    #   df <- filtered_summary_df()
    #   req(df, nrow(df) > 0, input$hist_var)
    #
    #   # Ensure the selected column exists in the raw data
    #   var <- input$hist_var
    #   req(var %in% names(df))
    #
    #   # Remove NAs from the selected column
    #   df <- df %>%
    #     filter(!is.na(.data[[var]]))
    #
    #   nice_label <- get_nice_name(var)[[1]]
    #
    #   # Plot the histogram of the selected variable
    #   p <- ggplot(data = df, aes(x = !!sym(var))) +
    #     geom_histogram(fill = "#4DB6AC",
    #                    color = "black") +
    #     # facet_wrap(~ common_name) +
    #     theme_bw(
    #       base_size = 15
    #     ) +
    #     theme(
    #       panel.grid = element_blank(),
    #       plot.title = element_markdown(hjust = 0.5),
    #       axis.title.x = element_markdown()
    #     ) +
    #     labs(
    #       x = nice_label,
    #       y = "Frequency",
    #       title = paste("Histogram of", nice_label,
    #                     "for", input$summary_species_filter)
    #     )
    #   p
    # })

    # # ---- Download Summary Table ----
    # output$download_summary <- downloadHandler(
    #   # dynamicaly create file name
    #   filename <- function() {
    #     paste0(input$summary_table, "_summary.xlsx")
    #   },
    #   # dynamically grob properly fillterd data
    #   content <- function(file) {
    #     df <- filtered_summary()
    #     req(df)
    #     # if df has nothing download_summary button will not work
    #     is_disabled <- is.null(df) || nrow(df) == 0
    #
    #     shinyjs::toggleState("download_summary",
    #                          condition = !is_disabled)
    #     # create workbook, add sheets and write and svae workbook
    #     wb <- openxlsx::createWorkbook()
    #     openxlsx::addWorksheet(wb, "Summary Data")
    #     openxlsx::writeData(wb, "Summary Data", df)
    #     openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    #   }
    # )
  }
  )
}

