view_summary_info_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    # tabName = id,
                 shiny::h2("Summary Statistics"),
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
                         ns = ns),
  )
}

# ----- summmary server --------
summary_info_server <- function(id, con, main_input, summary_sidebar_vals) {
  moduleServer(id, function(input, output, session) {
    # ---- errrors -----
    cat("[DEBUG] summary_info_server initialized for ID:", id, "\n")
    cat("[DEBUG] con object class:", class(con), "\n")

    # ---- namespaces
    ns <- session$ns
    # ----- first create summary datsa -----
    summary_data <- reactive({
      table_name <- get_selected_table(main_input)
      # errors
      if (is.null(table_name) || is.na(table_name)) {
        cat("[DEBUG] table_name is NULL, Cannot run query.\n")
      } else {
        cat("[DEBUG] table_name from get_selected_table():", table_name, "\n")
      }
      # db connections
      con_db <- if (inherits(con, "reactive")) con() else con
      # ---- acctuat gert data =----
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
    numeric_cols <- reactive({
      df <- summary_data()
      req(df)
      get_numeric_cols(df)
    })
    # # ---- Generate Summary Statistics with Dynamic Grouping -----
    filtered_summary_df <- reactive({
      df <- summary_data()
      grouping_vars <- summary_sidebar_vals$grouping_vars()
      waterbody_f <- summary_sidebar_vals$waterbody_filter()
      species_f <- summary_sidebar_vals$species_filter()

      req(df, grouping_vars)

      if (!(waterbody_f %in% "All")) {
        df <- df %>%
          filter(Waterbody == waterbody_f)
      }

      if (!(species_f %in% "All")) {
        df <- df %>%
          filter(`Common Name` == species_f)
      }

      return(df)
    })


    filtered_summary <- reactive({

      df <- filtered_summary_df()
      req(df)

      summary_grouping_vars <- summary_sidebar_vals$grouping_vars()
      summary_numeric_cols  <- numeric_cols()
      y_vals <- summary_sidebar_vals$y_variable()


      if (is.null(y_vals) || length(y_vals) == 0) {
        # Return just the grouped counts
        summary_df <- df %>%
          group_by(across(all_of(summary_grouping_vars))) %>%
          summarise(n = n(), .groups = "drop")
        return(summary_df)
      }

      summary_numeric_cols <- setdiff(summary_numeric_cols,
                                      c("sample_id","source_id","cal_id",
                                        "proxcomp_id","iso_id",
                                        "Conversion Factor","Composite (n)"))

      # if empty after filtering
      if (nrow(df) == 0) return(df)

      vars_to_summarise <- intersect(y_vals, summary_numeric_cols)

      req(length(vars_to_summarise) > 0)

      summary_df <- df %>%
        group_by(across(all_of(summary_grouping_vars))) %>%
        summarise(
          n = n(),
          across(
            all_of(vars_to_summarise),
            list(mean = ~ mean(.x, na.rm = TRUE),
                 sd   = ~ sd(.x, na.rm = TRUE)),
            .names = "{.col} ({.fn})"
          ),
          .groups = "drop"
        ) %>%
        mutate(across(where(is.numeric), ~ round(.x, 2)))

      return(summary_df)
    })


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

    # # ---- add in histogram ----
    output$summary_histogram <- renderPlot({
      # Get raw data (not summarized)
      df <- filtered_summary_df()

      # Ensure the selected column exists in the raw data
      var <- summary_sidebar_vals$hist_vars()
      req(df, nrow(df) > 0, vars)
      req(var %in% names(df))

      species_f <- summary_sidebar_vals$species_filter()
      # Remove NAs from the selected column
      df <- df %>%
        filter(!is.na(.data[[var]]))

      nice_label <- get_nice_name(var)[[1]]

      # Plot the histogram of the selected variable
      p <- ggplot(data = df, aes(x = !!sym(var))) +
        geom_histogram(fill = "#4DB6AC",
                       color = "black") +
        # facet_wrap(~ common_name) +
        theme_bw(
          base_size = 15
        ) +
        theme(
          panel.grid = element_blank(),
          plot.title = element_markdown(hjust = 0.5),
          axis.title.x = element_markdown()
        ) +
        labs(
          x = nice_label,
          y = "Frequency",
          title = paste("Histogram of", nice_label,
                        "for", species_f)
        )
      return(p)
    })

    # we need to return fileted summary to then use in donload
    return(list(
      summary_data = filtered_summary
    ))

  }
  )
}

