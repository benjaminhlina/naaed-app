
# ---- create fileted summary ----
create_filtered_data <- function(input_source,
                                 data) {

  reactive({


    df <- data()

    waterbody_f <- input_source$waterbody_filter()
    species_f <- input_source$species_filter()

    req(df)

    if (!is.null(waterbody_f) && !"All" %in% waterbody_f) {
      df <- df |>
        filter(waterbody %in% waterbody_f)
    }

    if (!is.null(species_f) && !"All" %in% species_f) {
      df <- df |>
        filter(scientific_name %in% species_f)
    }
    # run query x
    # df <- df |>
    #   collect()
    return(df)
  })
}



# ----- mean summarized table -----
create_mean_data <- function(input_source,
                          data) {
  reactive({

    df <- data()


    summary_grouping_vars <- input_source$grouping_vars()
    y_vals <- input_source$y_variable()

    # cli check
    check_mean_data(df = df,
                    summary_grouping_vars = summary_grouping_vars,
                    y_vals = y_vals)

    # if selected_vars is null just produce base query -----
    if (is.null(y_vals) || length(y_vals) == 0) {
      # Return just the grouped counts
      cli::cli_inform("No y_variable selected → returning grouped n only")
      grouped_summary_df <- df |>
        group_by(across(all_of(summary_grouping_vars))) |>
        summarise(n = n()) |>
        ungroup()

      cli::cli_ul(c(
        "collected class: {paste(class(grouped_summary_df), collapse = ', ')}",
        "rows: {nrow(grouped_summary_df)}"
      ))
    }

    #     # run query x
    #     grouped_summary_df <- grouped_summary_df |>
    #       collect()
    #
    #     return(grouped_summary_df)
    # })

    # summary_list <- lapply(y_vals, function(v) {
    #   mapped_var <- fix_var_generic(df, v, convert_nice_name)
    #   df_filtered <- mapped_var$df
    #   var_to_summarise <- mapped_var$var
    #   var_label <- mapped_var$var_label
    #
    #   # Check if variable exists after filtering
    #   if (nrow(df) == 0 || !var_to_summarise %in% colnames(df)) {
    #     return(NULL)
    #   }
    #
    #   grouped_summary_df <- df |>
    #     group_by(across(all_of(summary_grouping_vars))) |>
    #     summarise(
    #       n = n(),
    #       !!paste0(var_label, " (mean)") := mean(.data[[var_to_summarise]],
    #                                              na.rm = TRUE),
    #       !!paste0(var_label, " (sd)") := sd(.data[[var_to_summarise]],
    #                                          na.rm = TRUE),
    #       .groups = "drop"
    #     ) |>
    #     mutate(across(where(is.numeric), ~ round(.x, 2)))
    #   return(grouped_summary_df)
    # })
    #
    # # Remove NULL results
    # summary_list <- summary_list[!sapply(summary_list, is.null)]
    # req(length(summary_list) > 0)
    # # Combine all summaries by joining on grouping vars
    # grouped_summary_df <- Reduce(function(x, y) {
    #   full_join(x, y, by = summary_grouping_vars)
    # }, summary_list)
    #
    # # Combine 'n' columns if multiple exist
    # n_cols <- grep("^n($|\\.)", colnames(grouped_summary_df), value = TRUE)
    # if (length(n_cols) > 1) {
    #   # Multiple n columns exist, combine them
    #   grouped_summary_df <- grouped_summary_df |>
    #     mutate(n_combined = rowSums(across(all_of(n_cols)), na.rm = TRUE)) |>
    #     select(-all_of(n_cols)) |>
    #     rename(n = n_combined) |>
    #     relocate(n, .after = all_of(summary_grouping_vars))
    # } else if (length(n_cols) == 1 && n_cols != "n") {
    #   # Single n column but it's named n.x or similar, rename it
    #   grouped_summary_df <- grouped_summary_df |>
    #     rename(n = all_of(n_cols)) |>
    #     relocate(n, .after = all_of(summary_grouping_vars))
    # }

    # run query x
    grouped_summary_df <- grouped_summary_df |>
      collect()

    return(grouped_summary_df)
  })
}

# ---- sumary data -----
# args here are con and main input with tab being used in view_summary and
# view_plot
create_summary_data <- function(con,
                                main_input,
                                input_source,
                                tab = NULL
) {
  reactive({

    # use for other tabs ---
    if (!is.null(tab)) {
      check_tab_name(tab)

      req(main_input$tabs == tab)
    }

    # get connection
    con_db <- if (inherits(con, "reactive")) con() else con

    # get selected vars
    vars <- input_source$y_variable

    selected_vars <- if (inherits(vars, "reactive")) vars() else vars
    check_selected_vars(selected_vars = selected_vars)
    # get groups

    gv <- input_source$grouping_vars

    group_vars <- if(inherits(gv, "reactive")) gv() else gv
    req(con_db, group_vars)

    # ---- acctuat gert data =----
    df <- get_summary_data(con = con_db,
                           selected_vars = selected_vars,
                           grouping_vars = group_vars)

    # run query
    # df <- df |>
    #   collect()
    return(df)
  })
}

    df
  })
}
