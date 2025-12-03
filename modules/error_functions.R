check_dropdowns <- function(waterbody_choices,
                            common_name_choices,
                            grouping_choices,
                            numeric_choices) {
  cli::cli_alert_success("Updating dropdowns")
  cli::cli_ul(c(
    "Waterbody unique values: {length(waterbody_choices)}",
    "Species unique values: {length(common_name_choices)}",
    "Grouping choices: {paste(grouping_choices, collapse = ', ')}",
    "Numeric choices: {paste(numeric_choices, collapse = ', ')}"
  ))
}

check_input_source <- function(input_source_name, envir = parent.frame()) {
  valid_sources <- c("summary_sidebar_vals", "scatter_sidebar_vals")

  # Check if it's a valid name
  if (length(input_source_name) != 1 || !input_source_name %in% valid_sources) {
    cli::cli_abort(c(
      "Invalid {.arg input_source_name} provided",
      "x" = "You supplied: {.val {input_source_name}}",
      "i" = "Must be one of: {.val {valid_sources}}"
    ))
  }

  # DON'T check existence here - it might not exist yet at module initialization
}

# ---- check table name -----
check_table_name <- function(table_name) {

  if (is.null(table_name) || is.na(table_name)) {
    cli::cli_alert_danger("table_name is NULL, cannot run query")
  } else {
    cli::cli_alert_success("table_name from get_selected_table():
                               {.val {table_name}}")
  }
}
# ---- check tab name -----

check_tab_name <- function(tab) {
  if (!(tab %in% c("summary_info",
                   "scatter_plot"))) {

    cli::cli_abort("Cannot execute function for {.val {tab}} tab")
  }
}

# ---- ehck if summary data is being triggered ----
check_summary_data <- function() {
  observe({
  # tell me what is being triggered
  cli::cli_alert_success("summary_data triggered")

  # if try is false
  df <- try(summary_data(),
            silent = TRUE)

  if (inherits(df, "try-error")) {
    cli::cli_alert_danger("summary_data() failed completely")
  } else if ("Message" %in% names(df)) {
    cli::cli_alert_danger(
      "get_summary_data() returned error message: {.val {df$Message[1]}}")
  } else {
    cli::cli_alert_success(
      "summary_data() rows: {.val {nrow(df)}}, cols: {.val {ncol(df)}}")
    cli::cli_alert_info("Column names:")

  }
  # cli::cli_ul(names(df))  # Bulleted list
  # # or
  cli::cli_text("{.field {sort(names(df))}}")  # Inline with field styling
})
}
