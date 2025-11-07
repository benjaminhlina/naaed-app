# ---- Helper: determine which table is selected ----
get_selected_table <- function(input) {
  current_tab <- input$tabs
  if (is.null(current_tab) || length(current_tab) != 1) {
    cat("[DEBUG] current_tab is NULL or invalid\n")
    return(NULL)
  }

  out <- switch(
    current_tab,
    "summary_info" = input[["summary_sidebar-summary_table"]],
    "scatter_plot" = input[["scatter_sidebar-scatter_plot"]],
    NULL
  )
  cat("[DEBUG] Active tab:", current_tab, "\n")
  cat("[DEBUG] Selected table from sidebar:", out, "\n")
  if (is.null(out) || is.na(out) || out == "") return(NULL)
  out
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


# ---- Helper: run SQL query and clean the data ----
get_summary_data <- function(con, table_name) {

  req(con, table_name)


  # select df.* from data_frame df left join other_thing ot on df.key = ot.key
  # Build SQL query dynamically based on selected table
  query <- paste0('
      SELECT t.*,
      l.waterbody, l.area, l.site, l.site_depth,
      s.pi_name, s.month, s.year, s.weight,
      s.common_name, s.scientific_name, s.genus, s.tribe, s.subfamily,
      s.family, s.superfamily, s.suborder, s.order_sci, s.superorder,
      s.class_sci, s.superclass, s.tsn, s.sex, s.lifestage, s.wild_lab,
      s.age, s.composite,s.composite_n,
      s.tissue_type, s.sample_procedure, s.trt_description
      FROM ', table_name, ' t
      LEFT JOIN tbl_sample s ON t.sample_id = s.sample_id
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
  df <- df %>%
    mutate(
      year = as.character(year),
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

# ---- get good groups -----
get_good_groups <- function(df) {
good_groups <- c(
  "PI Name",
  "Month",
  "Year",
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
