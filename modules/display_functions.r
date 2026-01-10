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

    cli::cli_alert_info("selected var initially is: {.field {var}}")

    cli::cli_alert_info("colnames is present: {.val {any(colnames(df) %in% var)}}")
    # detect length-type UI choices
    is_length <- grepl("length_mm", var, ignore.case = TRUE) &&
      !var %in% colnames(df)

    is_energy <- grepl("Joules/g", var, ignore.case = TRUE) &&
      !var %in% colnames(df)

    cli::cli_alert_info("colnames are: {.val {colnames(df)}}")
    if (is_length) {

      # Convert UI label to the length_type in the data
      length_type_val <- case_when(
        grepl("fork", var, ignore.case = TRUE) ~ "fork",
        grepl("total", var, ignore.case = TRUE) ~ "total",
        grepl("standard", var, ignore.case = TRUE) ~ "standard",
        grepl("carapace", var, ignore.case = TRUE) ~ "carapace",
        .default = NA
      )

      # check_hist_ui(df = df, var = var, type_val = length_type_val,
      #               col = "length_type")

      req(!is.na(length_type_val))
      req("length_mm" %in% colnames(df))
      req("length_type" %in% colnames(df))

      check_hist_vars(df, var = "length_mm", ba = "before")

      df <- df |>
        filter(length_type == length_type_val) |>
        mutate(length_mm = suppressWarnings(as.numeric(length_mm))) |>
        filter(!is.na(length_mm))

      check_hist_vars(df, var, ba = "after")

      var <- "length_mm"

    } else if (is_energy) {

      # Convert UI label to the length_type in the data
      energy_type_val <- case_when(
        grepl("Joules/g dry weight", var,
              ignore.case = TRUE) ~ "Joules/g dry weight",
        grepl("Joules/g wet weight", var,
              ignore.case = TRUE) ~ "Joules/g wet weight",

        .default = NA
      )

      # check_hist_ui(df, var, type_val = energy_type_val)

      req(!is.na(energy_type_val))
      req("energy_measurement" %in% colnames(df))
      req("energy_units" %in% colnames(df))

      check_hist_vars(df, var = "energy_measurement", ba = "before")

      df <- df |>
        filter(energy_units == energy_type_val) |>
        mutate(energy_measurement = suppressWarnings(
          as.numeric(energy_measurement))) |>
        filter(!is.na(energy_measurement))
      check_hist_vars(df, var, ba = "after")

      var <- "energy_measurement"

    } else {

      # ---- NON-LENGTH VARIABLES ----
      cli::cli_alert_success("entered else statement")

      req(var %in% colnames(df))
      check_hist_vars(df, var, ba = "before")

      df <- df |>
        mutate(across(all_of(var), ~ suppressWarnings(as.numeric(.)))) |>
        filter(!is.na(.data[[var]]))

      check_hist_vars(df, var, ba = "after")

    }

    species_f <- input_source$species_filter()
    waterbody_f <- input_source$waterbody_filter()
    # Remove NAs from the selected column
    # df <- df |>
    #   filter(!is.na(.data[[var]]))

    nice_label <- convert_nice_name(var)[[1]]


    if (nice_label %in% "Length (mm)") {
      nice_label <- paste(stringr::str_to_title(length_type_val),
                          convert_nice_name(var)[[1]],
                          sep = " ")
    } else if (nice_label %in% "Energy Density") {
      nice_label <- paste(convert_nice_name(var)[[1]], " (",
                          energy_type_val, ")", sep = "")
    }

    title_text <- paste0(
      "Histogram of ", nice_label,
      "<br><b>Species:</b> ", fix_title_label(species_f),
      "<br><b>Waterbody:</b> ", fix_title_label(waterbody_f)
    )

    cli::cli_alert_info("selected var prior to plotting is: {.field {var}}")

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
        title = title_text
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
    cli::cli_alert_warning("df class: {.val {class(df)}}")
    # get the basic grouping
    scatter_grouping_vars <- input_source$grouping_vars()

    # # Normalize "None"
    # if (identical(scatter_grouping_vars, "None")) {
    #   scatter_grouping_vars <- character(0)
    # }

    n_groups <- length(scatter_grouping_vars)

    # ---- if 4 or more groups
    if (n_groups >= 4) {
      plot.new()
      text(
        x = 0.5, y = 0.5,
        labels = "You have selected 4 or more grouping variables.\nPlease select 3 or fewer.",
        cex = 1.2
      )
      return(invisible())
    }

    # get the x var

    x_var_raw <- input_source$x_choices()

    # get the y var
    y_var_raw <- input_source$y_choices()

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

    # ---- create nice_title -----
    # nice_label_y <- convert_nice_name(y_var)[[1]]
    #
    #
    # if (nice_label %in% "Length (mm)") {
    #   nice_label <- paste(stringr::str_to_title(length_type_val),
    #                       convert_nice_name(var)[[1]],
    #                       sep = " ")
    # } else if (nice_label %in% "Energy Density") {
    #   nice_label <- paste(convert_nice_name(var)[[1]], " (",
    #                       energy_type_val, ")", sep = "")
    # }
    species_f <- input_source$species_filter()
    waterbody_f <- input_source$waterbody_filter()

    title_text <- paste0(
      "Scatter plot of ", y_label, " by ", x_label,
      "<br><b>Species:</b> ", fix_title_label(species_f),
      "<br><b>Waterbody:</b> ", fix_title_label(waterbody_f)
    )
      x = !!sym(x_var),
      y = !!sym(y_var))) +
      scale_fill_viridis_d(name = scatter_grouping_vars[1],
                           option = "B",
                           begin = 0.1,
                           end = 0.9,
                           alpha = 0.5
      ) +
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
    # get data
    df <- data()

    # validate data
    validate(
      need(is.data.frame(df), "Waiting for data…"),
      need(nrow(df) > 0, "No data available")
    )

    # display data

    datatable(df,
              options = list(pageLength = 10,
                             scrollX = TRUE
                             # autoWidth = TRUE
              ), escape = FALSE)
  })
}




