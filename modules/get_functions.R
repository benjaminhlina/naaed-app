# ----- get COLUMN_MPA ------

get_column_map <- function(con) {
  tbl(con, "tbl_data_dictionary") |>
    select(table_name, field_name)
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
    "tribe",
    "subfamily",
    "Family",
    "superfamily",
    "suborder",
    "Order",
    "superorder",
    "Class",
    "Superclass",
    "TSN code",
    "Sex",
    "Life Stage",
    "Wild Lab",
    "Age (yrs)",
    "composite",
    "Tissue Type",
    "sample_procedure",
    "trt_description",
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

# ---- Helper: run SQL query and clean the data ----
get_summary_data <- function(con, table_name, debug_sql = FALSE) {

  req(con, table_name)


  # select df.* from data_frame df left join other_thing ot on df.key = ot.key
  # Build SQL query dynamically based on selected table

  cli::cli_inform(c(
    "v" = "Starting summary data query.",
    "•" = "Table selected: {.val {table_name}}"
  ))

  # dbplyr table objects
  desired_tbl <- tbl(con, table_name)
  sample_tbl <- tbl(con, "tbl_samples")
  length_tbl <- tbl(con, "tbl_length")
  loc_tbl <- tbl(con, "tbl_location")


  # Build lazy query
  df_db <- desired_tbl |>
    left_join(sample_tbl) |>
    left_join(length_tbl ) |>
    left_join(loc_tbl |>
                select(-lon, -lat)) |>
    mutate(
      sample_year = as.character(sample_year),
      month = as.character(month),
      age = as.character(age),
      tsn = as.character(tsn),
      site_depth = as.character(site_depth)
    )

  # Show SQL if requested
  if (debug_sql) {
    cli::cli_inform(c(
      ">" = "Generated SQL using {.pkg dbplyr}:"
    ))

    # Print pretty SQL
    df_db |>
      show_query()
  }

  df <- df_db |>
    collect()


  names(df) <- get_nice_name(names(df), lookup = nice_name_lookup)

  df <- df[, !(names(df) %in% c("sample_id",
                                "source_id",
                                "cal_id",
                                "proxcomp_id",
                                "iso_id",
                                "len_id",
                                "loc_id",
                                "user_sample_id"))]
  # return the data
  return(df)
}


get_summary_data <- function(con, selected_vars, debug_sql = FALSE) {

  req(con, selected_vars)

  cli::cli_inform(c(
    "v" = "Starting summary data query.",
    "•" = "Variables selected: {.val {selected_vars}}"
  ))

  # Always start from samples
  df <- tbl(con, "tbl_samples")

  needed_tables <- setdiff(tables_needed(selected_vars), "tbl_samples")

  # Controlled joins
  if ("tbl_length" %in% needed_tables) {
    df <- df |> left_join(tbl(con, "tbl_length"), by = "sample_id")
  }

  if ("tbl_location" %in% needed_tables) {
    df <- df |>
      left_join(
        tbl(con, "tbl_location") |> select(-lon, -lat),
        by = "sample_id"
      )
  }

  # Select only requested columns (plus keys if needed)
  df <- df |> select(any_of(selected_vars))

  if (debug_sql) {
    message(dbplyr::sql_render(df))
  }

  df
}

# ---- get teh tables we need to filter by based on what the user selects -----
get_tables_needed <- function(con, vars) {

  req(con, vars)
  get_column_map(con) |>
    filter(field_name %in% vars) |>
    distinct(table_name) |>
    pull(table_name)
}
