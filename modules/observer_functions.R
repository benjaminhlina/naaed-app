# ----- filter observserer ----
exclusive_all_observer <- function(input, session, id) {
  observeEvent(input[[id]], {
    sel <- input[[id]]
    if ("All" %in% sel && length(sel) > 1) {
      updateSelectInput(session, id, selected = setdiff(sel, "All"))
    }
  }, ignoreInit = TRUE)
}
