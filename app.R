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
    # Modularized panels
    summary_sidebar_ui("summary_sidebar"),
    scatter_sidebar_ui("scatter_sidebar")
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
  sidebar_vals <- summary_sidebar_server("summary_sidebar", con,
                                         main_input = input)

  # summary_sidebar_server("summary_sidebar", con, main_input)
  summary_info_server("summary_info", con, main_input = input,
                      sidebar_vals = sidebar_vals )
  # summary_data <- reactive({
  #   # Ensure table is selected before running query
  #   current_tab <- input$tabs
  #
  #   table_name <- switch(
  #     current_tab,
  #     "summary_info" = input$summary_table,
  #     "scatter_plot" = input$scatter_plot,
  #     NULL
  #   )
  #   req(table_name)
  #
  #   # req(input$summary_table)
  #
  #   # select df.* from data_frame df left join other_thing ot on df.key = ot.key
  #   # Build SQL query dynamically based on selected table
  #   query <- paste0('
  #     SELECT t.*,
  #     l.waterbody, l.area, l.site, l.site_depth,
  #     s.pi_name, s.month, s.year, s.weight,
  #     s.common_name, s.scientific_name, s.genus, s.tribe, s.subfamily,
  #     s.family, s.superfamily, s.suborder, s.order_sci, s.superorder,
  #     s.class_sci, s.superclass, s.tsn, s.sex, s.lifestage, s.wild_lab,
  #     s.age, s.composite,s.composite_n,
  #     s.tissue_type, s.sample_procedure, s.trt_description
  #     FROM ', table_name, ' t
  #     LEFT JOIN tbl_sample s ON t.sample_id = s.sample_id
  #     LEFT JOIN tbl_location l ON t.sample_id = l.sample_id;
  #   ')
  #
  #   # create error if query failed
  #
  #   df <- tryCatch({
  #     dbGetQuery(con, query)
  #   }, error = function(e) {
  #     message("Database Query Failed: ", e$message)
  #     return(data.frame(Message = "Error retrieving data from database."))
  #   })
  #
  #
  #   df <- df %>%
  #     mutate(
  #       year = as.character(year),
  #       month = as.character(month),
  #       age = as.character(age),
  #       tsn = as.character(tsn),
  #       site_depth = as.character(site_depth)
  #     )
  #
  #   names(df) <- get_nice_name(names(df))
  #   # df <- df[, !(names(df) %in% c("sample_id",
  #   #                               "source_id", "cal_id",
  #   #                               "proxcomp_id",
  #   #                               "iso_id"))]
  #   # return the data
  #   return(df)
  # })
  #
  #
  # # ----- Dynamically Update Sidebar Dropdowns -----
  # # Get numeric columns globally
  # numeric_cols <- reactive({
  #   df <- summary_data()
  #   req(df)
  #   # Ensure numeric columns exist and remove ids
  #   cols <- names(df)[sapply(df, is.numeric)]
  #   setdiff(cols, c("sample_id", "source_id", "cal_id", "proxcomp_id",
  #                   "iso_id", "Conversion Factor",
  #                   "Composite (n)"))
  # })
  #
  #
  # grouping_cols <- reactive({
  #   df <- summary_data()
  #   req(df)
  #   good_groups <- c("PI Name", "Month", "Year",
  #                    "Common Name", "Scientific Name", "Genus",
  #                    "tribe", "subfamily", "Family",
  #                    "superfamily", "suborder", "Order",
  #                    "superorder", "Class", "Superclass",
  #                    "TSN code", "Sex", "Life Stage",
  #                    "Wild Lab", "Age (yrs)", "composite",
  #                    "Tissue Type", "sample_procedure", "trt_description",
  #                    "Waterbody", "Area", "Site", "Site Depth (m)")
  #
  #   # Return only those that are in good_groups
  #   groups <- sort(intersect(names(df), good_groups))
  #   return(groups)
  #
  # })
  #
  # observe({
  #   # df is summary data
  #   df <- summary_data()
  #   req(df)
  #
  #   # Grouping Variables: Allow dynamic selection
  #
  #   grouping_choices <- grouping_cols()
  #
  #   updateSelectInput(session, "summary_grouping_vars",
  #                     choices = grouping_choices,
  #                     selected = c("Waterbody",
  #                                  "Common Name")
  #   )
  #
  #   # Waterbody Drop-down
  #   updateSelectInput(session, "summary_waterbody_filter",
  #                     choices = c("All", sort(unique(df$Waterbody))),
  #                     selected = "All")
  #
  #   # Species Drop-down
  #   updateSelectInput(session, "summary_species_filter",
  #                     choices = c("All", sort(unique(df$`Common Name`))),
  #                     selected = "All")
  #   # Get numeric column choices
  #   numeric_choices <- numeric_cols()
  #   # Update histogram variable choices
  #   updateSelectizeInput(session, "hist_var",
  #                        choices = setNames(numeric_choices, numeric_choices),
  #                        # selected = "test",
  #                        server = TRUE)
  #   # Update histogram variable choices
  #   updateSelectizeInput(session, "summary_y_variable",
  #                        choices = setNames(numeric_choices, numeric_choices),
  #                        # selected = "test",
  #                        server = TRUE)
  # })
  #
  #
  # # ---- Generate Summary Statistics with Dynamic Grouping -----
  # filtered_summary_df <- reactive({
  #   df <- summary_data()
  #
  #
  #   req(df, input$summary_grouping_vars)
  #   # Apply filters - set ALL to no filter the data at all
  #   if (!(input$summary_waterbody_filter %in% "All")) {
  #     df <- df %>%
  #       filter(Waterbody == input$summary_waterbody_filter)
  #   }
  #   if (!(input$summary_species_filter %in% "All")) {
  #     df <- df %>%
  #       filter(`Common Name` == input$summary_species_filter)
  #   }
  #   return(df)
  # })
  # #   if (!(input$summary_y_variable %in% "All")) {
  # #     df <- df %>%
  # #       filter(`Common Name` == input$summary_species_filter)
  # #   }
  # #   return(df)
  # # })
  #
  # filtered_summary <- reactive({
  #   df <- filtered_summary_df()  # Use the filtered data
  #   # req(df, input$summary_grouping_vars)
  #   # req(input$summary_y_variable)
  #   # ------ Dynamically group data -----
  #   summary_grouping_vars <- input$summary_grouping_vars
  #   summary_numeric_cols <- numeric_cols()  # Now calling the reactive expression
  #   # Ensure numeric columns exist and remove ids
  #   summary_numeric_cols <- names(df)[sapply(df, is.numeric)]
  #   summary_numeric_cols <- setdiff(summary_numeric_cols,
  #                                   c("sample_id",
  #                                     "source_id",
  #                                     "cal_id",
  #                                     "proxcomp_id",
  #                                     "iso_id",
  #                                     "Conversion Factor",
  #                                     "Composite (n)"))
  #
  #   # if (length(summary_grouping_vars) > 0) {
  #   #   summary_df <- df %>%
  #   #     group_by(across(all_of(summary_grouping_vars)))
  #   # } else {
  #   #   summary_df <- df
  #   # }
  #
  #   # create dynamic groupings and summary round all values to 2 decimials
  #
  #   summary_df <- df %>%
  #     group_by(across(all_of(summary_grouping_vars))) %>%
  #     summarise(
  #       n = n(),
  #       across(all_of(intersect(input$summary_y_variable, summary_numeric_cols)),
  #              list(
  #                mean = ~mean(.x, na.rm = TRUE),
  #                sd = ~sd(.x, na.rm = TRUE)
  #              ),
  #              .names = "{.col} ({.fn})"),
  #       .groups = "drop"
  #     ) %>%
  #     mutate_if(is.numeric, round, digits = 2)
  #
  #   return(summary_df)
  # })
  # # )}
  #
  # #  ----- Render Summary Table -----
  # output$summary_table_output <- renderDT({
  #   req(filtered_summary())
  #   df <- filtered_summary()
  #   # if there is nothing in df print no data available
  #   if (is.null(df) || nrow(df) == 0) {
  #     return(datatable(data.frame(Message = "No data available"),
  #                      escape = FALSE))
  #   }
  #   # colnames(df) <- get_nice_name(colnames(df))
  #
  #   datatable(df,
  #             options = list(pageLength = 10,
  #                            scrollX = TRUE
  #
  #                            # autoWidth = TRUE
  #             ), escape = FALSE)
  # })
  #
  #
  # # Update histogram variable choices
  # # updateSelectInput(session, "hist_var",
  # #                   choices = setNames(numeric_cols, numeric_cols),
  # #                   selected = "energy_density_mean")
  #
  #
  #
  # # ---- add in histogram ----
  # output$summary_histogram <- renderPlot({
  #   # Get raw data (not summarized)
  #   df <- filtered_summary_df()
  #   req(df, nrow(df) > 0, input$hist_var)
  #
  #   # Ensure the selected column exists in the raw data
  #   var <- input$hist_var
  #   req(var %in% names(df))
  #
  #   # Remove NAs from the selected column
  #   df <- df %>%
  #     filter(!is.na(.data[[var]]))
  #
  #   nice_label <- get_nice_name(var)[[1]]
  #
  #   # Plot the histogram of the selected variable
  #   p <- ggplot(data = df, aes(x = !!sym(var))) +
  #     geom_histogram(fill = "#4DB6AC",
  #                    color = "black") +
  #     # facet_wrap(~ common_name) +
  #     theme_bw(
  #       base_size = 15
  #     ) +
  #     theme(
  #       panel.grid = element_blank(),
  #       plot.title = element_markdown(hjust = 0.5),
  #       axis.title.x = element_markdown()
  #     ) +
  #     labs(
  #       x = nice_label,
  #       y = "Frequency",
  #       title = paste("Histogram of", nice_label,
  #                     "for", input$summary_species_filter)
  #     )
  #   p
  # })
  #


  # observe({
  #   df <- summary_data()
  #   req(df)
  #
  #   # if ("tbl_calorimetry" %in% input$selected_table) {
  #   #   if ("Units" %in% names(df)) {
  #   #     df <- df %>%
  #   #       mutate("Energy Density",
  #   #                     ~ case_when(
  #   #                       # Units == "cal/g" ~ .x * 4.184,
  #   #                       Units == "Joules / g" ~ .x,
  #   #                       TRUE ~ .x
  #   #                     )))
  #   #   }
  #   # }
  #   # Grouping Variables: Allow dynamic selection
  #   grouping_choices <- grouping_cols()
  #
  #   updateSelectInput(session, "scatter_grouping_vars",
  #                     choices = grouping_choices,
  #                     # selected = c("Waterbody",
  #                     #              "Common Name")
  #   )
  #
  #   # Waterbody Drop-down
  #   updateSelectInput(session, "scatter_waterbody_filter",
  #                     choices = c("All", sort(unique(df$Waterbody))),
  #                     selected = "All")
  #
  #   # Species Drop-down
  #   updateSelectInput(session, "scatter_species_filter",
  #                     choices = c("All", sort(unique(df$`Common Name`))),
  #                     selected = "All")
  #   # Get numeric column choices
  #   numeric_choices <- numeric_cols()
  #
  #   # Update histogram variable choices
  #   updateSelectizeInput(session, "scatter_var",
  #                        choices = setNames(numeric_choices, numeric_choices),
  #                        # selected = "",
  #                        server = TRUE)
  #   # X Variable Options
  #   x_choices <-c(
  #     # "Total Length (mm)",
  #     # "Fork Length (mm)",
  #     "Weight")
  #
  #
  #
  #   x_choices <- x_choices[x_choices %in% names(df)]  # Ensure they exist
  #
  #   updateSelectInput(session, "x_var",
  #                     choices = x_choices,
  #                     selected = x_choices[1]
  #   )
  #
  #
  # })
  # filtered_summary_dats <- reactive({
  #   df <- summary_data()
  #
  #
  #   req(df, input$scatter_grouping_vars)
  #   # Apply filters - set ALL to no filter the data at all
  #   if (!(input$scatter_waterbody_filter %in% "All")) {
  #     df <- df %>%
  #       filter(Waterbody == input$scatter_waterbody_filter)
  #   }
  #   if (!(input$scatter_species_filter %in% "All")) {
  #     df <- df %>%
  #       filter(`Common Name` == input$scatter_species_filter)
  #   }
  #   return(df)
  # })
  #
  #
  # output$scatter_plot <- renderPlot({
  #   # Get raw data (not summarized)
  #
  #   df <- filtered_summary_dats()
  #   req(df, input$scatter_var, input$x_var)
  #
  #   x_var <- input$x_var
  #   y_var <- input$scatter_var
  #   scatter_grouping_vars <- input$scatter_grouping_vars
  #   req(x_var %in% names(df),
  #       y_var %in% names(df))
  #
  #   df <- df %>%
  #     filter(!is.na(.data[[x_var]]), !is.na(.data[[y_var]]))
  #   # nice_label <- get_nice_name(var)[[1]]
  #   x_label <- get_nice_name(x_var)[[1]]
  #   y_label <- get_nice_name(y_var)[[1]]
  #
  #   p <- ggplot(df, aes(
  #     x = !!sym(x_var),
  #     y = !!sym(y_var))) +
  #     theme_bw(base_size = 15) +
  #     theme(
  #       panel.grid = element_blank(),
  #       plot.title = element_markdown(hjust = 0.5),
  #       axis.title = element_markdown(),
  #       legend.title = element_markdown(),
  #       legend.text = element_markdown()
  #     ) +
  #     labs(
  #       x = x_label,
  #       y = y_label,
  #       title = paste("Scatter Plot of", y_label, "vs", x_label)
  #     )
  #
  #   if (scatter_grouping_vars != "None") {
  #     p <- p +
  #       geom_point(
  #         aes(fill = !!sym(scatter_grouping_vars)),
  #         alpha = 0.7,
  #         size = 5,
  #         shape = 21
  #       ) +
  #       scale_fill_viridis_d(name = scatter_grouping_vars,
  #                            option = "B",
  #                            begin = 0.1,
  #                            end = 0.9,
  #                            alpha = 0.5
  #       )
  #   } else {
  #     p <- p + geom_point(
  #       alpha = 0.7,
  #       size = 3,
  #       shape = 21
  #     )
  #   }
  #
  #
  #   p
  # })


  # ---- Download Summary Table ----
  # output$download_summary <- downloadHandler(
  #   # dynamicaly create file name
  #   filename <- function() {
  #     paste0(input$summary_table, "_summary.xlsx")
  #   },
  #   # dynamically grob properly fillterd data
  #   content <- function(file) {
  #     df <- filtered_summary()
  #     req(df)
  #     # if df has nothing download_summary button will not work
  #     is_disabled <- is.null(df) || nrow(df) == 0
  #
  #     shinyjs::toggleState("download_summary",
  #                          condition = !is_disabled)
  #     # create workbook, add sheets and write and svae workbook
  #     wb <- openxlsx::createWorkbook()
  #     openxlsx::addWorksheet(wb, "Summary Data")
  #     openxlsx::writeData(wb, "Summary Data", df)
  #     openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  #   }
  # )
}

# render ui and serve together to create dashboard
shinyApp(ui = ui, server = server)

