view_scatter_plot_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::tabItem(tabName = id,
                          h2("Scatter Plot"),
                          plot_ui(title = "Scatter Plot",
                                  plot_id = "scatter_plot",
                                  height = "600px",
                                  ns = ns)
  )
}


scatter_plot_server <- function(id, con, main_input, scatter_sidebar_vals) {
  moduleServer(id, function(input, output, session) {

    cat("[DEBUG] summary_info_server initialized for ID:", id, "\n")
    cat("[DEBUG] con object class:", class(con), "\n")

    # ---- namespaces
    ns <- session$ns

    scatter_data <- reactive({
      req(main_input$tabs == "scatter_plot")

      table_name <- scatter_sidebar_vals$selected_table()
        req(table_name)
      # errors
      if (is.null(table_name) || is.na(table_name)) {
        cat("[DEBUG] table_name is NULL, Cannot run query.\n")
      } else {
        cat("[DEBUG] table_name from get_selected_table():", table_name, "\n")
      }
      # db connections
      con_db <- if (inherits(con, "reactive")) con() else con
      # ---- acctuat gert data =----
      df <- get_summary_data(con = con_db, table_name)
      df
    })

    observe({
      cat("\n[DEBUG] scatter_data triggered\n")
      df <- try(scatter_data(), silent = TRUE)
      if (inherits(df, "try-error")) {
        cat("[DEBUG] scatter_data() failed completely\n")
      } else if ("Message" %in% names(df)) {
        cat("[DEBUG] get_summary_data() returned error message:", df$Message[1], "\n")
      } else {
        cat("[DEBUG] scatter_data() rows:", nrow(df), "cols:", ncol(df), "\n")
        cat("[DEBUG] column names:\n")
        print(names(df))
      }
    })
    numeric_cols <- reactive({
      df <- scatter_data()
      req(df)
      get_numeric_cols(df)
    })

    filtered_scatter_df <- reactive({
      df <- scatter_data()
      scatter_grouping_vars <- scatter_sidebar_vals$grouping_vars()
      waterbody_f <- scatter_sidebar_vals$waterbody_filter()
      species_f <- scatter_sidebar_vals$species_filter()

      req(df, scatter_grouping_vars)

      # Apply filters - set ALL to no filter the data at all
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
      output$scatter_plot <- renderPlot({
        #     # Get raw data (not summarized)

        df <- filtered_scatter_df()
        # get the x var
        x_var <- scatter_sidebar_vals$x_choices()
        # get the y var
        y_var <- scatter_sidebar_vals$y_choices()
        # get the basic grouping
        scatter_grouping_vars <- scatter_sidebar_vals$grouping_vars()

        # expose the names/require the names
        req(x_var %in% names(df),
            y_var %in% names(df))

      # filter df by x and y vars
      df <- df %>%
        filter(!is.na(.data[[x_var]]), !is.na(.data[[y_var]]))
      # nice_label <- get_nice_name(var)[[1]]
      x_label <- get_nice_name(x_var)[[1]]
      y_label <- get_nice_name(y_var)[[1]]
      p <- ggplot(df, aes(
        x = !!sym(x_var),
        y = !!sym(y_var))) +
        theme_bw(base_size = 15) +
        theme(
          panel.grid = element_blank(),
          plot.title = element_markdown(hjust = 0.5),
          axis.title = element_markdown(),
          legend.title = element_markdown(),
          legend.text = element_markdown()
        ) +
        labs(
          x = x_label,
          y = y_label,
          title = paste("Scatter Plot of", y_label, "vs", x_label)
        )

      if (scatter_grouping_vars != "None") {
        p <- p +
          geom_point(
            aes(fill = !!sym(scatter_grouping_vars)),
            alpha = 0.7,
            size = 5,
            shape = 21
          ) +
          scale_fill_viridis_d(name = scatter_grouping_vars,
                               option = "B",
                               begin = 0.1,
                               end = 0.9,
                               alpha = 0.5
          )
      } else {
        p <- p + geom_point(
          alpha = 0.7,
          size = 3,
          shape = 21
        )
      }


      p
    })

  }
  )
}
