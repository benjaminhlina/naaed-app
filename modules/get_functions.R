# ----- get COLUMN_MPA ------

get_column_map <- function(con) {
    tbl(con, "tbl_data_dictionary")
    # select(table_name, field_name)
}



# ---- get good groups -----
get_good_groups <- function(df) {
  good_groups <- c(
    "PI Name",
    "Month",
    "Sample Year",
    "Common Name",
    "Scientific Name",
    "Genus",
    "Tribe",
    "Subfamily",
    "Family",
    "Superfamily",
    "Suborder",
    "Order",
    "Superorder",
    "Class",
    "Superclass",
    "TSN code",
    "Sex",
    "Life Stage",
    "Wild Lab",
    "Age (yrs)",
    "omposite",
    "Tissue Type",
    "Sampling Procedure",
    "Treatment Description",
    "Waterbody",
    "Area",
    "Site",
    "Site Depth (m)"
  )

  # Return only those that are in good_groups
  groups <- sort(intersect(names(df), good_groups))
  return(groups)
}

# ----- simple function to get a tb use dbplyr -----
#
get_join_table <- function(df, table, con) {
  df |>
    left_join(tbl(con, table))
}

# ---- get length_vars ----
get_length_vars <- function(df) {
  # Only keep non-NA length types
  length_types <- unique(df$length_type)
  length_types <- length_types[!is.na(length_types)]

  # Create synthetic variable names and labels
  vars <- paste0("length_mm__", length_types)
  labels <- paste0(stringr::str_to_title(length_types), " Length (mm)")

  setNames(vars, labels)  # names = labels, values = synthetic variable codes
}

# ----- get nice names -----
get_nice_name <- function(cols, lookup = nice_name_lookup) {
  unname(sapply(cols, function(col) {
    if (col %in% names(lookup)) {
      lookup[[col]]
    } else {
      col
    }
  }))
}

# ---- get numerical columns -----
get_numeric_cols <- function(df) {
  default_exclude <- c("sample_id",
                       "source_id",
                       "cal_id",
                       "proxcomp_id",
                       "iso_id",
                       "Conversion Factor",
                       "Composite (n)")

  # Ensure numeric columns exist and remove ids
  cols <- names(df)[sapply(df, is.numeric)]
  setdiff(cols, default_exclude)
}

get_numeric_vars <- function(con) {
  get_column_map(con) |>
    dplyr::filter(
      field_class %in% c("integer", "numeric", "double")
    )  |>
    distinct(field_name) |>
    arrange(field_name) |>
    dplyr::pull(field_name)
}

# ---- Helper: determine which table is selected ----
get_selected_table <- function(input) {
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



get_summary_data <- function(con, selected_vars = NULL, debug_sql = FALSE) {

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
  tbl_loc <- tbl(con, "tbl_location")
  # ---- grab sampels
  df <- tbl(con, "tbl_samples")

  df <- df |>
    left_join(
      tbl_loc
    )

  if (!is.null(selected_vars)) {
    needed_tables <- setdiff(get_tables_needed(con = con,
                                               vars = selected_vars),
                             "tbl_samples")

    # if (!is.null(needed_tables)) {
      df <- needed_tables |>
        reduce(.init = df, ~ get_join_table(.x, .y, con))

      # Select only requested columns (plus keys if needed)
      df <- df |>
        select(waterbody,
               scientific_name,
               any_of(selected_vars))
    # }
  } else {
    df
  }

  if (debug_sql) {
    message(dbplyr::sql_render(df))
  }

  df
}

# ---- get teh tables we need to filter by based on what the user selects -----
get_tables_needed <- function(con, vars) {

  req(con)

  if (length(vars) == 0) return(NULL)

  get_column_map(con) |>
    filter(field_name %in% vars) |>
    distinct(table_name) |>
    pull(table_name)
}
