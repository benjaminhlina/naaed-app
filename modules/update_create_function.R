
create_n_df <- function(
    con,
    main_input,
    input_source,
    tab = NULL
) {
  reactive({
    if (!is.null(tab)) {
      check_tab_name(tab)

      req(main_input$tabs == tab)
    }

    # cli::cli_inform("y_vars entered in {.fun {create_summary_data()}}")
    # vars <- summary_sidebar_vals$y_variable()



    con_db <- if (inherits(con, "reactive")) con() else con

    # cli::cli_inform("y_vars entered in {.fun {create_summary_data()}}")
    selected_vars <- if (inherits(input_source$y_variable, "reactive"))
      input_source$y_variable() else input_source$y_variable

    cli::cli_ul(c(
      "y_variable value: {if (is.null(selected_vars)) 'NULL' else paste(selected_vars, collapse = ', ')}",
      "length(y_variable): {length(selected_vars)}"
    ))
    group_vars <- if(is.reactive(input_source$grouping_vars))
      input_source$grouping_vars() else input_source$grouping_vars

    req(con_db, group_vars)

    # ---- acctuat gert data =----
    df <- get_summary_data(con = con_db,
                           selected_vars = selected_vars,
                           grouping_vars = group_vars)


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

    check_mean_data(df = df,
                    summary_grouping_vars = group_vars,
                    y_vals = selected_vars)

    # if (is.null(selected_vars) || length(selected_vars) == 0) {
    if (is.null(selected_vars) || length(selected_vars) == 0) {

      cli::cli_inform("No y_variable selected → returning grouped n only")
      grouped_summary_df <- df |>
        group_by(across(all_of(group_vars))) |>
        summarise(n = n()) |>
        ungroup() |>
        collect()
      cli::cli_ul(c(
        "collected class: {paste(class(grouped_summary_df), collapse = ', ')}",
        "rows: {nrow(grouped_summary_df)}"
      ))
      # return(grouped_summary_df)
      # run query x
    }

    collected_df <- grouped_summary_df |>
      dplyr::collect()


    return(collected_df)
  })
}
