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

# ---- check lenght_ui -----
check_hist_ui <- function(
    df,
    var,
    type_val,
    col) {
  cli::cli_alert_info("UI var: {var}")
  cli::cli_alert_info("Mapped type_val: {type_val}")
  cli::cli_alert_info(
    "Unique df$type_val: {paste(unique(df$col), collapse=', ')}")
}

check_hist_vars <- function(df,
                            var,
                            ba
                            ) {
  if (ba == "before") {
    cli::cli_alert_info("Variable: {.var {var}}")
    cli::cli_alert_info("Rows before filtering: {.val {nrow(df)}}")
    cli::cli_alert_info("Sample values: {.val {paste(head(sort(unique(df[[var]]))),
                        collapse = ', ')}}")
  }
  if (ba == "after") {
    cli::cli_alert_success("Rows after filtering: {.val {nrow(df)}}")}
}

# ----- chekc mean_data ------
check_mean_data <- function(df,
                            summary_grouping_vars,
                            y_vals) {
  cli::cli_h2("create_mean_data() diagnostics")

  cli::cli_ul(c(
    "df class: {paste(class(df), collapse = ', ')}",
    "df rows (if local): {tryCatch(nrow(df), error = function(e) 'lazy tbl')}",
    "grouping_vars: {if (is.null(summary_grouping_vars)) 'NULL' else paste(summary_grouping_vars, collapse = ', ')}",
    "length(grouping_vars): {length(summary_grouping_vars)}",
    "y_vals: {if (is.null(y_vals)) 'NULL' else paste(y_vals, collapse = ', ')}",
    "length(y_vals): {length(y_vals)}"
  ))

  cli::cli_rule()
}

# ---- check table name -----
check_table_name <- function(table_name) {

  if (is.null(table_name) || is.na(table_name)) {
    cli::cli_alert_danger("table_name is NULL, cannot run query")
  } else {
    cli::cli_alert_success("table_name from get_selected_tab():
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
check_summary_data <- function(df, name = deparse(substitute(df))) {

  cli::cli_alert_success("{name} triggered")

  # Lazy dbplyr table → do NOT validate rows
  if (inherits(df, "tbl_lazy")) {
    cli::cli_alert_info("{name} is lazy (query constructed)")
    return(invisible(TRUE))
  }

  # Try/catch safety
  if (inherits(df, "try-error")) {
    cli::cli_alert_danger("{name} failed completely")
    return(invisible(FALSE))
  }

  # Empty data
  if (nrow(df) == 0) {
    cli::cli_alert_warning("{name} has 0 rows")
    return(invisible(FALSE))
  }

  cli::cli_alert_success(
    "{name} rows: {.val {nrow(df)}}, cols: {.val {ncol(df)}}"
  )

  cli::cli_text("{.field {sort(names(df))}}")

  invisible(TRUE)
}


check_selected_vars <- function(selected_vars) {
  cli::cli_ul(c(
    "y_variable value: {if (is.null(selected_vars)) 'NULL' else paste(selected_vars, collapse = ', ')}",
    "length(y_variable): {length(selected_vars)}"
  ))
}
