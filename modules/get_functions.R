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
get_summary_data <- function(con, table_name) {

  req(con, table_name)


  # select df.* from data_frame df left join other_thing ot on df.key = ot.key
  # Build SQL query dynamically based on selected table
  query <- paste0('
      SELECT t.*,
      l.waterbody, l.area, l.site, l.site_depth,
      le.length_mm, le.length_type,
      s.pi_name, s.month, s.sample_year, s.weight,
      s.common_name, s.scientific_name, s.genus, s.tribe, s.subfamily,
      s.family, s.superfamily, s.suborder, s.order_sci, s.superorder,
      s.class_sci, s.superclass, s.tsn, s.sex, s.lifestage, s.wild_lab,
      s.age, s.composite,s.composite_n,
      s.tissue_type, s.sample_procedure, s.trt_description
      FROM ', table_name, ' t
      LEFT JOIN tbl_samples s ON t.sample_id = s.sample_id
      LEFT JOIN tbl_length le ON t.sample_id = le.sample_id
      LEFT JOIN tbl_location l ON t.sample_id = l.sample_id;
    ')

  # cat("[DEBUG] SQL query:\n", query, "\n")
  # # create error if query failed
  #
  df <- tryCatch({
    dbGetQuery(con, query)
  }, error = function(e) {
    message("Database Query Failed: ", e$message)
    return(data.frame(Message = "Error retrieving data from database."))
  })
  #
  df <- df |>
    mutate(
      sample_year = as.character(sample_year),
      month = as.character(month),
      age = as.character(age),
      tsn = as.character(tsn),
      site_depth = as.character(site_depth)
    )

  names(df) <- get_nice_name(names(df), lookup = nice_name_lookup)
  df <- df[, !(names(df) %in% c("sample_id",
                                "source_id", "cal_id",
                                "proxcomp_id",
                                "iso_id"))]
  # return the data
  return(df)
}
