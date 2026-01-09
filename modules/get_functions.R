# ----- get COLUMN_MPA ------

get_column_map <- function(con) {
  tbl(con, "tbl_data_dictionary")
}

get_data <- function(con, debug_sql = FALSE) {

  req(con)

  # Always start from samples
  # --grab location
  tbl_loc <- tbl(con, "tbl_location")
  # ---- grab sampels
  df <- tbl(con, "tbl_samples")

  # grab_+length
  tbl_length <- tbl(con, "tbl_length")

  df <- df |>
    left_join(
      tbl_loc, by = "sample_id"
    ) |>
    left_join(
      tbl_length, by = "sample_id"
    )
  if (debug_sql == TRUE) {
    cli::cli_alert_info(dbplyr::sql_render(df))
  }
  return(df)
}

# ---- get good groups -----
get_good_groups <- function(df) {

  good_groups <- c(
    "pi_name",
    "month",
    "sample_year",
    "common_name",
    "scientific_name",
    "genus",
    "tribe",
    "subfamily",
    "family",
    "superfamily",
    "suborder",
    "order",
    "superorder",
    "class",
    "superclass",
    "tsn",
    "sex",
    "life_stage",
    "wild_lab",
    "age",
    "composite",
    "tissue_type",
    "sample_procedure",
    "trt_description",
    "waterbody",
    "area",
    "site",
    "site_depth"
  )

  # get column names
  cols <- dplyr::tbl_vars(df) |>
    as.character()


    # # Return only those that are in good_groups
    groups <- sort(intersect(cols, good_groups))

    cli::cli_alert_info("Converted names: {.val {cols}}"
    )
  return(groups)
}


get_groups <- function(df) {
  req(df)

  groups <- get_good_groups(df)

  cli::cli_inform(c(
    "v" = "Selecting groups.",
    "•" = "Groups selected: {.val {groups}}"
  )
  )
  return(groups)
}


# ----- simple function to get a tb use dbplyr -----
#
get_join_table <- function(df, table, con) {
  df |>
    left_join(tbl(con, table))
}

# ---- get vart types ----

get_var_types <- function(df, var) {

  var_types <- df |>
    distinct(.data[[var]]) |>
    arrange(.data[[var]]) |>
    dplyr::pull(.data[[var]])
  # Only keep non-NA length types
  var_types <- var_types[!is.na(var_types)]

  # Create synthetic variable names and labels
  if (any(var_types %in% c("fork", "standard", "total"))) {
    vars <- paste0("length_mm__", var_types)
    labels <- paste0(stringr::str_to_title(var_types), " Length (mm)")
  }
  if (any(var_types %in% c(
    "Joules/g dry weight",
    "Joules/g wet weight"
  ))) {

    # cleaned_var_types <- gsub("/", " ", var_types)
    # cleaned_var_types <- gsub("\\s+", "_", cleaned_var_types)

    vars <- paste0("energy_units__", var_types)
    labels <- paste0("Energy Density (", var_types, ")")
  }

  setNames(vars, labels)  # names = labels, values = synthetic variable codes
}

# ----- get nice names -----
convert_nice_name <- function(cols, lookup = nice_name_lookup) {
  unname(sapply(cols, function(col) {
    if (col %in% names(lookup)) {
      lookup[[col]]
    } else {
      col
    }
  }
  )
  )
}

# ---- get numeric vars -----
get_numeric_vars <- function(con) {
  get_column_map(con) |>
    dplyr::filter(
      field_class %in% c("integer", "numeric", "double")
    )  |>
    dplyr::distinct(field_name) |>
    dplyr::arrange(field_name) |>
    dplyr::pull(field_name)
}

# ---- Helper: determine which tab is selected ----
get_selected_tab <- function(input) {
  current_tab <- input$tabs
  if (is.null(current_tab) || length(current_tab) != 1) {
    cli::cli_alert_info("current_tab is NULL or invalid")
    return(NULL)
  }
  out <- switch(
    current_tab,
    "summary_info" = input[["summary_sidebar-summary_table"]],
    "scatter_plot" = input[["scatter_sidebar-scatter_plot"]],
    NULL
  )
  cli::cli_alert_info("Active tab: {current_tab}")
  cli::cli_alert_info("Selected table from sidebar: {out}")
  if (is.null(out) || is.na(out) || out == "") return(NULL)
  out
}

# ----- get sidebr df -----
get_sidebar_df <- function(con) {
  reactive({
    # create connection reactively
    con_db <- if (inherits(con, "reactive")) con() else con
    req(con_db)

    # get sample_ids and locatiosn
    df <- get_data(
      con = con_db
    ) |>
      left_join(tbl(con_db, "tbl_calorimetry") |>
                  select(sample_id, energy_units))

    cli::cli_alert_success("sidebar base tbl has completed")
    return(df)
  })
}

# ---- get summary data frame -----

get_summary_data <- function(con,
                             selected_vars = NULL,
                             grouping_vars = NULL,
                             debug_sql = FALSE) {

  req(con)

  if (is.null(selected_vars)) {
    selected_vars <- NULL
  }

  cli::cli_inform(c(
    "v" = "Starting summary data query.",
    "•" = "Variables selected: {.val {selected_vars}}"
  ))


  # Always start from samples
  # --grab location
  df <- get_data(con) |>
    left_join(
      tbl(con, "tbl_calorimetry")
    )

  # ----- grab seelected vars ----

  if (!is.null(selected_vars) && length(selected_vars) > 0) {
    needed_tables <- setdiff(get_tables_needed(con = con,
                                               vars = selected_vars),
                             "tbl_samples")

    if (!is.null(needed_tables)) {
      df <- needed_tables |>
        reduce(.init = df, ~ get_join_table(.x, .y, con))
    }

    # --- get selected vars -----
    vars_for_select <- selected_vars


    vars_for_select <- dplyr::case_when(
      grepl("^length_mm__(fork|total|standard)$",
            vars_for_select) ~ "length_mm",
      grepl("^energy_units__.*$",
            vars_for_select) ~ "energy_measurement",
      .default = vars_for_select
    )

    vars_for_select <- unique(vars_for_select)

    if (is.null(grouping_vars)) {
      # Select only requested columns (plus keys if needed)
      df <- df |>
        select(waterbody,
               scientific_name,
               length_type,
               energy_units,
               any_of(vars_for_select))
      # }
    } else {
      df <- df |>
        select(waterbody,
               scientific_name,
               length_type,
               energy_units,
               any_of(grouping_vars),
               any_of(vars_for_select))
    }
  } else {
    df
  }

  if (debug_sql) {
    cli::cli_alert_info(dbplyr::sql_render(df))
  }

  return(df)
}

# ---- get teh tables we need to filter by based on what the user selects -----
get_tables_needed <- function(con, vars) {

  req(con)

  if (is.null(vars) || length(vars) == 0) {
    return(character(0))
  }


  get_column_map(con) |>
    filter(field_name %in% vars) |>
    distinct(table_name) |>
    pull(table_name)
}
