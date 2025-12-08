fix_var_generic <- function(df, var_raw, get_nice_name) {


  # detect if it's one of the synthetic length vars
  if (grepl("^length_mm__", var_raw)) {
    # split if length_mm__ is presnet in

    parts <- strsplit(var_raw, "__")[[1]]
    # grab the second element of part
    var_type <- parts[2]

    # Filter df to matching length type
    df <- df %>%
      dplyr::filter(length_type == var_type)

    # Dynamic label
    var_label <- paste0(stringr::str_to_title(var_type), " Length (mm)")
    var <- "Length (mm)"

  } else {

    req(var_raw %in% names(df))
    # Normal variable
    var <- var_raw
    var_label <- get_nice_name(var)[[1]]
  }

  list(
    df = df,
    var = var,
    var_label = var_label
  )

}


# ----- make scater choices -----
make_scatter_choices <- function(df, numeric_choices) {

  # ----------- Build synthetic length choices -----------
  length_types <- df |>
    dplyr::filter(!is.na(`Length (mm)`), !is.na(length_type)) |>
    dplyr::pull(length_type) %>%
    unique()

  length_choices <- setNames(
    paste0("length_mm__", length_types),
    paste0(stringr::str_to_title(length_types), " Length (mm)")
  )

    numeric_clean <- numeric_choices[numeric_choices != "Length (mm)"]
    return(c(
      length_choices,
      "Weight",
      setNames(numeric_clean, numeric_clean)
    ))
}
