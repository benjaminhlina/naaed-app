display_hist <- function(data,
                         input_source,
                         output,
                         output_id = "summary_histogram") {
  output[[output_id]] <- renderPlot({
    # Get raw data (not summarized)
    df <- data() |>
      collect()
    req(df, nrow(df) > 0)
    # Ensure the selected column exists in the raw data
    var <- input_source$hist_vars()

    # detect length-type UI choices
    is_length <- grepl("length_mm", var, ignore.case = TRUE) &&
      !var %in% colnames(df)

    if (is_length) {

      # Convert UI label to the length_type in the data
      length_type_val <- case_when(
        grepl("fork", var, ignore.case = TRUE) ~ "fork",
        grepl("total", var, ignore.case = TRUE) ~ "total",
        grepl("standard", var, ignore.case = TRUE) ~ "standard",
        grepl("carapace", var, ignore.case = TRUE) ~ "carapace",

        .default = NA
      )
      cli::cli_alert_info("UI var: {var}")
      cli::cli_alert_info("Mapped length_type_val: {length_type_val}")
      cli::cli_alert_info(
        "Unique df$length_type: {paste(unique(df$length_type), collapse=', ')}")
      req(!is.na(length_type_val))
      req("Length (mm)" %in% names(df))
      req("length_type" %in% names(df))

      df <- df |>
        filter(length_type == length_type_val) |>
        mutate(`Length (mm)` = suppressWarnings(as.numeric(`Length (mm)`))) |>
        filter(!is.na(`Length (mm)`))

      var <- "Length (mm)"
      nice_label <- var



    } else {

      # ---- NON-LENGTH VARIABLES ----
      req(var %in% names(df))
      df <- df |>
        mutate(across(all_of(var), ~ suppressWarnings(as.numeric(.)))) |>
        filter(!is.na(.data[[var]]))

      nice_label <- convet_nice_names(var)[[1]]
    }

    species_f <- input_source$species_filter()
    waterbody_f <- input_source$waterbody_filter()
    # Remove NAs from the selected column
    # df <- df |>
    #   filter(!is.na(.data[[var]]))

    nice_label <- convert_nice_name(var)[[1]]

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
                      "for", species_f, "in", waterbody_f)
      )
    return(p)
  })

}

# ----- display scatter plot -----
display_scatter_plot <- function(data,
                                 input_source,
                                 output,
                                 output_id = "scatter_plot") {
  # ----- display scatter plot -----
  output[[output_id]] <- renderPlot({
    #     # Get raw data (not summarized)

    df <- data()
    # get the x var

    x_var_raw <- input_source$x_choices()


    # get the y var
    y_var_raw <- input_source$y_choices()
    # get the basic grouping
    scatter_grouping_vars <- input_source$grouping_vars()

    # use generic function to filter and grab the correct for length only
    fix_x <- fix_var_generic(
      df = df,
      var_raw = x_var_raw,
      get_nice_name = convert_nice_name
    )

    # get the returned objects which are returned in a list
    df <- fix_x$df
    x_var <- fix_x$var
    x_label <- fix_x$var_label

    # now do the same for y
    fix_y <- fix_var_generic(
      df = df,
      var_raw = y_var_raw,
      get_nice_name = convert_nice_name
    )

    y_var <- fix_y$var
    y_label <- fix_y$var_label
    # filter df by x and y vars
    df <- df |>
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
}


# ---- display summary_table -----

display_table <- function(data, output, output_id = "summary_table_output") {
  output[[output_id]] <- renderDT({
    req(data())
    df <- data()
    # if there is nothing in df print no data available
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(data.frame(Message = "No data available"),
                       escape = FALSE))
    }

    datatable(df,
              options = list(pageLength = 10,
                             scrollX = TRUE
                             # autoWidth = TRUE
              ), escape = FALSE)
  })
}




