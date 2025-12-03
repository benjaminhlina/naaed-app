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

display_hist <- function(data,
                         input_source,
                         output,
                         output_id = "summary_histogram") {
  output[[output_id]] <- renderPlot({
    # Get raw data (not summarized)
    df <- data()
    req(df, nrow(df) > 0)
    # Ensure the selected column exists in the raw data
    var <- input_source$hist_vars()

    # detect length-type UI choices
    is_length <- grepl("length", var, ignore.case = TRUE) &&
      !var %in% names(df)

    if (is_length) {

      # Convert UI label to the length_type in the data
      length_type_val <- case_when(
        grepl("fork", var, ignore.case = TRUE) ~ "fork",
        grepl("total", var, ignore.case = TRUE) ~ "total",
        grepl("standard", var, ignore.case = TRUE) ~ "standard",
        TRUE ~ NA_character_
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

      nice_label <- get_nice_name(var)[[1]]
    }

    species_f <- input_source$species_filter()
    waterbody_f <- input_source$waterbody_filter()
    # Remove NAs from the selected column
    # df <- df %>%
    #   filter(!is.na(.data[[var]]))

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
                      "for", species_f, "in", waterbody_f)
      )
    return(p)
  })

}



