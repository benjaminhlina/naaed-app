home_tab_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::tabItem(
    tabName = "home",
    h2("Welcome to the Great Lakes Aquatic Tissue Analysis Repository (GLATAR)"),
    p("This toolbox allows you to explore, visualize, and manage
    energy density, proximate composition and stable isotope data
    related to for fish and aquatic invertabrates throughout North America."),
    shiny::br(),
    shiny::fluidRow(
      shinydashboard::box(
        title = "Get Started",
        width = 6,
        status = "primary",
        solidHeader = TRUE,
        shiny::p("Use the sidebar to:"),
        tags$ul(
          tags$li("Visualize sampling locations on a map"),
          tags$li("View summary statistic tables"),
          tags$li("View visualizations of energy density data"),
          tags$li("View and filter your uploaded raw data"),
          tags$li("Upload new data into the database")
        )
      ),
      shinydashboard::box(
        title = "About the Database",
        width = 6,
        status = "info",
        solidHeader = TRUE,
        shiny::p("The Great Lakes Aquatic Tissue Analysis Repository (GLATAR)
          contains data on energy density, proximate composition,
          and stable isotopes for fish and aquatic invertebrats
          collected across North America.
          The data helps researchers and managers understand
          ecosystem health, bioenergtics, enegery transfer, and food web d
                 ynamics."),
        tags$ul(
          tags$li(
            tags$span("Data entry templates can be found by "),
            tags$a(
              "clicking here",
              href = "data-entry-template/GLATAR_data_entry_template_v12.xlsx",
              download = "GLATAR_data_entry_template_v12.xlsx"
            ),
            tags$span(" and in the documentation pane.")
          ),
          tags$li("If you would like to contribute to this database,
          please create a username using your email address on the data upload
                  pane and wait for registeration code to be sent to you."),
          tags$li("Any quesations can be directed to the ATLAS
                  manager at:", tags$a(href = "mailto:benjamin.hlina@gmail.com",
                                     "benjamin.hlina@gmail.com"))
        )
      )
    )
  )
}
