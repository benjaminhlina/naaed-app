# register_summary <- function(summary_info) {
#
#   observe({
#     df <- summary_info$summary_data()  # reactive from summary
#     output$download_summary <- downloadHandler(
#       filename = function() {
#         # get table name
#         tables <- get_selected_tab(main_input)
#         # make file name
#         paste0(tbl, "_summary_", Sys.Date(), ".xlsx")
#       },
#       # white out xslx
#       content = function(file) {
#         req(df)
#         writexl::write_xlsx(df, file)
#       }
#     )
#
#     # toggle button
#     shinyjs::toggleState(session$ns("download_summary"),
#                          condition = !is.null(df) && nrow(df) > 0)
#   })
# }
