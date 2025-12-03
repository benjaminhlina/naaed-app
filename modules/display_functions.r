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

    # Ensure the selected column exists in the raw data
    var <- input_source$hist_vars()
    req(df, nrow(df) > 0, var)
    req(var %in% names(df))


    if (var %in% unique(df$length_type)) {
      var <- "Length (mm)"
      df <- df |>
        filter(length_type == var)
    } else {
      var <- var
    }

    req(var %in% names(df))

    df <- df |>
      mutate(across(all_of(var), ~ suppressWarnings(as.numeric(.)))) |>
      filter(!is.na(.data[[var]]))


    species_f <- input_source$species_filter()
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
                      "for", species_f)
      )
    return(p)
  })

}
