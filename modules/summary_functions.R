
# ---- create fileted summary ----
create_filtered_data <- function(input_source,
                                 data) {

  reactive({


    df <- data()

    grouping_vars <- input_source$grouping_vars()
    waterbody_f <- input_source$waterbody_filter()
    species_f <- input_source$species_filter()

    req(df, grouping_vars)

    if (!(waterbody_f %in% "All")) {
      df <- df |>
        filter(Waterbody == waterbody_f)
    }

    if (!(species_f %in% "All")) {
      df <- df |>
        filter(`Common Name` == species_f)
    }

    return(df)
  })
}



# ----- mean summarized table -----
create_mean_data <- function(input_source,
                             data,
                             numeric_cols) {
  reactive({

    df <- data()
    req(df)

    summary_grouping_vars <- input_source$grouping_vars()
    summary_numeric_cols  <- numeric_cols()
    y_vals <- input_source$y_variable()


    if (is.null(y_vals) || length(y_vals) == 0) {
      # Return just the grouped counts
      summary_df <- df |>
        group_by(across(all_of(summary_grouping_vars))) |>
        summarise(n = n(), .groups = "drop") |>
        ungroup()
      return(summary_df)
    }

    summary_numeric_cols <- setdiff(summary_numeric_cols,
                                    c("sample_id","source_id","cal_id",
                                      "proxcomp_id","iso_id",
                                      "Conversion Factor","Composite (n)"))


    if (nrow(df) == 0) return(df)

    summary_list <- lapply(y_vals, function(v) {
      mapped_var <- fix_var_generic(df, v, get_nice_name)
      df_filtered <- mapped_var$df
      var_to_summarise <- mapped_var$var
      var_label <- mapped_var$var_label

      # Check if variable exists after filtering
      if (nrow(df_filtered) == 0 || !var_to_summarise %in% names(df_filtered)) {
        return(NULL)
      }

      summary_df <- df_filtered |>
        group_by(across(all_of(summary_grouping_vars))) |>
        summarise(
          n = n(),
          !!paste0(var_label, " (mean)") := mean(.data[[var_to_summarise]],
                                                 na.rm = TRUE),
          !!paste0(var_label, " (sd)") := sd(.data[[var_to_summarise]],
                                             na.rm = TRUE),
          .groups = "drop"
        ) |>
        mutate(across(where(is.numeric), ~ round(.x, 2)))
      return(summary_df)
    })

    # Remove NULL results
    summary_list <- summary_list[!sapply(summary_list, is.null)]
    req(length(summary_list) > 0)
    # Combine all summaries by joining on grouping vars
    summary_df <- Reduce(function(x, y) {
      full_join(x, y, by = summary_grouping_vars)
    }, summary_list)

    # Combine 'n' columns if multiple exist
    n_cols <- grep("^n($|\\.)", names(summary_df), value = TRUE)
    if (length(n_cols) > 1) {
      # Multiple n columns exist, combine them
      summary_df <- summary_df |>
        mutate(n_combined = rowSums(across(all_of(n_cols)), na.rm = TRUE)) |>
        select(-all_of(n_cols)) |>
        rename(n = n_combined) |>
        relocate(n, .after = all_of(summary_grouping_vars))
    } else if (length(n_cols) == 1 && n_cols != "n") {
      # Single n column but it's named n.x or similar, rename it
      summary_df <- summary_df |>
        rename(n = all_of(n_cols)) |>
        relocate(n, .after = all_of(summary_grouping_vars))
    }

    return(summary_df)
  })

}

# ---- create numerical_col
create_numeric_col <- function(data) {
  reactive({
    df <- data()
    req(df)
    get_numeric_cols(df)
  })
}

# ---- sumary data -----
# args here are con and main input with tab being used in view_summary and
# view_plot
create_summary_data <- function(con, main_input, tab = NULL,
                                table_name_reactive = NULL) {
  reactive({

    if (!is.null(tab)) {
      check_tab_name(tab)

      req(main_input$tabs == tab)
    }

    table_name <- if (!is.null(table_name_reactive)) {
      table_name_reactive()
    } else {
      get_selected_table(main_input)
    }

    # req(table_name)
    #
    # table_name <- get_selected_table(main_input)

    req(table_name)

    check_table_name(table_name)
    con_db <- if (inherits(con, "reactive")) con() else con
    # ---- acctuat gert data =----
    df <- get_summary_data(con = con_db, table_name)

    df
  })
}
