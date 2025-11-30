# ---- load packages ----
{
  library(dplyr)
  library(DT)
  library(ggplot2)
  library(ggtext)
  library(leaflet)
  library(mapview)
  library(plotly)
  library(readr)
  library(sf)
  library(shiny)
  library(shinydashboard)
  library(shinyjs)
}


lapply(list.files("modules", full.names = TRUE), source, local = FALSE)

# ---- create ui ----

ui <- dashboardPage(
  # ----- title -----
  dashboardHeader(title = "North American Aquatic Energy Density Toolbox",
                  titleWidth = 500),
  # ---- sidebar -----
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("View Data",tabName = "view_data", icon = icon("table")),
      menuItem("Map", tabName = "view_map", icon = icon("map")),
      menuItem("Upload Data", tabName = "insert_data", icon = icon("plus")),
      menuItem("Summmary", tabName = "summary_info", icon = icon("bar-chart")),
      menuItem("Scatter Plot", tabName = "scatter_plot",
               icon = icon("chart-line")
      )
    ),
    useShinyjs(),
    # Modularized panels
      conditionalPanel(
    "input.tabs == 'summary_info'",
    summary_sidebar_ui("summary_sidebar")
    ),
    conditionalPanel(
      "input.tabs == 'scatter_plot'",
    scatter_sidebar_ui("scatter_sidebar")
    )
  ),
  # ---- create display panes ----
  dashboardBody(
    tabItems(
      tabItem(tabName = "home", home_tab_ui("home")),
      tabItem(tabName = "view_data", view_data_ui("view_data")),
      tabItem(tabName = "view_map", view_map_ui("view_map")),
      tabItem(tabName = "insert_data", upload_data_ui("insert_data")),
      tabItem(tabName = "summary_info", view_summary_info_ui("summary_info")),
      tabItem(tabName = "scatter_plot", view_scatter_plot_ui("scatter_plot"))
    )
  )
)

server <- function(input, output, session) {
  # ----- get tables -----
  view_data_server("view_data", con)
  # ---- get map -----
  view_map_server("view_map", con)
  # ---- upload data -----
  upload_data_server("insert_data", con)
  # ----- summary table -----
  summary_sidebar_vals <- summary_sidebar_server("summary_sidebar", con,
                                         main_input = input)

  summary_info <- summary_info_server("summary_info", con, main_input = input,
                      summary_sidebar_vals = summary_sidebar_vals)

  summary_sidebar_vals$register_summary(summary_info)

  scatter_sidebar_vals <- scatter_sidebar_server("scatter_sidebar",
                                                 con,
                                                 main_input = input)

  scatter_plot <- scatter_plot_server("scatter_plot",
                      con,
                      main_input = input,
                      scatter_sidebar_vals = scatter_sidebar_vals)

  # scatter_sidebar_vals$register_plot(scatter_plot)
}

# render ui and serve together to create dashboard
shinyApp(ui = ui, server = server)

