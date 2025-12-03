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




    output$scatter_plot <- renderPlot({
      #     # Get raw data (not summarized)

      df <- filtered_scatter_df()
      # get the x var

      x_var_raw <- scatter_sidebar_vals$x_choices()


      # get the y var
      y_var_raw <- scatter_sidebar_vals$y_choices()
      # get the basic grouping
      scatter_grouping_vars <- scatter_sidebar_vals$grouping_vars()

      # use generic function to filter and grab the correct for length only
      fix_x <- fix_var_generic(
        df = df,
        var_raw = x_var_raw,
        get_nice_name = get_nice_name
      )

      # get the returned objects which are returned in a list
      df <- fix_x$df
      x_var <- fix_x$var
      x_label <- fix_x$var_label

      # now do the same for y
      fix_y <- fix_var_generic(
        df = df,
        var_raw = y_var_raw,
        get_nice_name = get_nice_name
      )

      y_var <- fix_y$var
      y_label <- fix_y$var_label
   # filter df by x and y vars
      df <- df %>%
        filter(!is.na(.data[[x_var]]), !is.na(.data[[y_var]]))

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
      return(p)
    })
    scatter_data <- create_summary_data(
      con = con,
      main_input = main_input,
      tab = "scatter_plot",
      table_name_reactive = scatter_sidebar_vals$selected_table
    )
    check_summary_data(scatter_data)
    numeric_cols <- create_numeric_col(data = scatter_data)
    filtered_scatter_data <- create_filtered_data(
      input_source = scatter_sidebar_vals,
      data = scatter_data)
  }
  )
}
