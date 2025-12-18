home_tab_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::tabItem(
    tabName = "home",
    h2("Welcome to the Aquatic Tissue Library for Analyses & Synthesis (ATLAS)"),
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
        shiny::p("The North American Aquatic Energy Density Toolbox
          contains data on energy densities of fish and other
          aquatic organisms collected across North America.
          The data helps researchers and managers understand
          ecosystem health and food web dynamics."),
        tags$ul(
          tags$li("Data templates can be found at the following links."),
          tags$li("If you would like to contribute to this toolbox,
                  please contact the NAAEDT manager at:",
                  tags$a(href = "mailto:benjamin.hlina@gmail.com",
                         "benjamin.hlina@gmail.com")),
          tags$li("Any questions can be directed to the NAAEDT
                  manager at: benjamin.hlina@gmail.com.")
        )
      )
    )
  )
}
