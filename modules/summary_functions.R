
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

    # can create base_df
    base_df <- df |>
      group_by(across(all_of(summary_grouping_vars))) |>
      summarise(n = n()) |>
      ungroup()

    # if selected_vars is null just produce base query -----
    if (is.null(y_vals) || length(y_vals) == 0) {
      # Return just the grouped counts
      cli::cli_inform("No y_variable selected → returning grouped n only")
      grouped_summary_df <- base_df |>
        collect() |>
        arrange(across(all_of(summary_grouping_vars)))

      cli::cli_ul(c(
        "collected class: {paste(class(grouped_summary_df), collapse = ', ')}",
        "rows: {nrow(grouped_summary_df)}"
      ))
      return(grouped_summary_df)

    }


    summary_list <- lapply(y_vals, function(v) {
      mapped_var <- fix_var_generic(df = df, var_raw = v,
                                    get_nice_name = convert_nice_name)


      df_filtered <- mapped_var$df
      var_to_summarise <- mapped_var$var
      var_label <- mapped_var$var_label

      cli::cli_inform("var_to_summarise: {.feild {var_to_summarise}}")
      cli::cli_inform("Available columns: {.val {colnames(df_filtered)}}")

      # Check if variable exists after filtering
      if (!var_to_summarise %in% colnames(df_filtered)) {
        cli::cli_warn("Skipping {.field {v}} — column not present after mapping")
        return(NULL)
      }

      grouped_summary_df <- df_filtered |>
        group_by(across(all_of(summary_grouping_vars))) |>
        summarise(
          !!paste0(var_label, " (mean)") := mean(.data[[var_to_summarise]],
                                                 na.rm = TRUE),
          !!paste0(var_label, " (sd)") := sd(.data[[var_to_summarise]],
                                             na.rm = TRUE),
        ) |>
        ungroup()

    })
    # Remove NULL results
    summary_list <- summary_list[!sapply(summary_list, is.null)]
    req(length(summary_list) > 0)

    # Combine all summaries by joining on grouping vars
    #  # can use  init = base_df
    grouped_summary_df <- Reduce(function(x, y) {
      full_join(x, y, by = summary_grouping_vars)
    }, summary_list,
    init = base_df)

    # run query x
    grouped_summary_df <- grouped_summary_df |>
      collect() |>
      arrange(across(all_of(summary_grouping_vars))) |>
      mutate(across(where(is.numeric), ~ round(.x, 2)))

    return(grouped_summary_df)

  })
}

# ---- sumary data -----
# args here are con and main input with tab being used in view_summary and
# view_plot
create_summary_data <- function(con,
                                main_input,
                                input_source,
                                var_field,
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
    # Handle multiple var_fields
    selected_vars <- c()

    for (field in var_field) {
      vars <- input_source[[field]]
      vars_val <- if (inherits(vars, "reactive")) vars() else vars
      cli::cli_alert_info("Field: {field}, Value: {vars_val}, Length: {length(vars_val)}")
      selected_vars <- c(selected_vars, vars_val)
    }
    selected_vars <- unique(selected_vars[!is.null(selected_vars)])
    # vars <- input_source[[var_field]]
    # # Get y_variable
    # selected_vars <- if (inherits(vars, "reactive")) vars() else vars

    cli::cli_alert("selected vars is: {.var {selected_vars}}")
    # if (length(selected_vars) > 1) {
    #   lapply(selected_vars, check_selected_vars)
    # } else {
      check_selected_vars(selected_vars = selected_vars)
    # }
    # get groups

    gv <- input_source$grouping_vars

    group_vars <- if(inherits(gv, "reactive")) gv() else gv
    req(con_db, group_vars)

    # ---- acctuat gert data =----
    df <- get_summary_data(con = con_db,
                           selected_vars = selected_vars,
                           grouping_vars = group_vars)

    return(df)
  })
}
