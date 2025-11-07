upload_data_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::tabItem(tabName = id,
                          shiny::h2("Upload Excel File"),
                          shiny::fileInput(ns("file_upload"),
                                           "Choose an Excel File",
                                           accept = c(".xlsx")),
                          shiny::actionButton(ns("upload_btn"),
                                              "Upload & Process",
                                              icon = icon("upload")),
                          shiny::uiOutput(ns("upload_status"))
  )
}

upload_data_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$upload_btn, {

      req(input$file_upload)

      # Read the uploaded Excel file
      file_path <- input$file_upload$datapath

      # Read sheet names to check if file is correct
      sheets <- readxl::excel_sheets(file_path)

      # Ensure required sheets exist
      if (!("tbl_sample" %in% sheets) | !("tbl_location" %in% sheets)) {
        output$upload_status <- renderUI({
          p("Error: Missing required sheets (tbl_sample or tbl_location).",
            style = "color:red;")
        })
        return()
      }

      # Read the sheets into data frames
      tbl_sample <- readxl::read_excel(file_path, sheet = "tbl_sample")
      tbl_location <- readxl::read_excel(file_path, sheet = "tbl_location")


      # Append to PostgreSQL database
      tryCatch({
        dbWriteTable(con,
                     "tbl_sample",
                     tbl_sample,
                     append = TRUE,
                     row.names = FALSE)
        dbWriteTable(con,
                     "tbl_location",
                     tbl_location,
                     append = TRUE,
                     row.names = FALSE)

        output$upload_status <- renderUI({
          p("Data uploaded successfully!",
            style = "color:green;")
        })
      }, error = function(e) {
        output$upload_status <- renderUI({
          p(paste("Upload failed:", e$message),
            style = "color:red;")
        })
      })
    })
  }
  )
}
