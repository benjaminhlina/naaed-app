view_data_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::tabItem(tabName = id,
                 shiny::h2("Viewing Selected Table"),
                 shiny::selectInput(ns("table_select"),
                                    "Select a Table",
                                    choices = NULL),
                 DT::DTOutput(ns("selected_table"))
  )
}


# ---- server ----
view_data_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {

    # Get all table names and update selectInput dynamically
    observe({
      test <- DBI::dbListTables(con)
      updateSelectInput(session, "table_select", choices = test)
    })

    # Render the selected table
    output$selected_table <- DT::renderDT({
      req(input$table_select)
      DBI::dbGetQuery(con, paste0('SELECT * FROM "', input$table_select, '"'))
    },
    options = list(
      pageLength = 15,
      scrollX = TRUE
    ))
  })
}
