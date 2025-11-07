view_map_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::tabItem(tabName = id,
                          shiny::h2("Map of Locations"),
                          leaflet::leafletOutput(ns("map"),
                                                 height = "700px",
                                                 width = "100%")
  )
}

# ---- server -----
view_map_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {

    output$map <-  leaflet::renderLeaflet({

      # Check if the table exists before proceeding
      if (!"tbl_location" %in% dbListTables(con)) {
        return(leaflet::leaflet() %>%
                 leaflet::addTiles() %>%
                 leaflet::addMarkers(lng = 0,
                                     lat = 0,
                                     popup = "No Data"))
      }

      # Fetch location data
      locs <- dbGetQuery(con, 'SELECT * FROM tbl_location')

      # Ensure required columns exist
      missing_cols <- setdiff(c("lat", "lon",
                                "waterbody",
                                "area",
                                "site",
                                "site_depth"),
                              colnames(locs))

      # if columns are missing return blank map with
      if (length(missing_cols) > 0) {
        return(leaflet::leaflet() %>%
                 leaflet::addTiles() %>%
                 leaflet::addMarkers(lng = 0,
                                     lat = 0,
                                     popup = "Missing Required Columns"))
      }

      # Ensure data is not empty
      if (nrow(locs) == 0) {
        return(leaflet::leaflet() %>%
                 leaflet::addTiles() %>%
                 leaflet::addMarkers(lng = 0,
                                     lat = 0,
                                     popup = "No Data Available"))
      }

      # remove locations that don't have lon
      locs <- locs %>%
        dplyr::filter(!(is.na(lon)))

      # Create popup content dynamically
      locs$popup_info <- paste(
        "<b>Waterbody:</b>", locs$waterbody, "<br>",
        "<b>Area:</b>", locs$area, "<br>",
        "<b>Site:</b>", locs$site, "<br>",
        "<b>Site Depth:</b>", locs$site_depth, "m"
      )

      # Create map with popups
      leaflet::leaflet(locs) %>%
        leaflet::addTiles() %>%
        leaflet::addCircleMarkers(
          lng = ~lon,
          lat = ~lat,
          popup = ~popup_info,
          radius = 5,
          color = "blue",
          fillOpacity = 0.7
        )
    })
  }
  )

}

view_map_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {

    output$map <- leaflet::renderLeaflet({

      # --- Check if the table exists ---
      if (!"tbl_location" %in% DBI::dbListTables(con)) {
        return(leaflet::leaflet() %>%
                 leaflet::addTiles() %>%
                 leaflet::addMarkers(lng = 0, lat = 0,
                                     popup = "No Data"))
      }

      # --- Fetch location data ---
      locs <- DBI::dbGetQuery(con, 'SELECT * FROM tbl_location')

      # --- Ensure required columns exist ---
      missing_cols <- setdiff(c("lat", "lon", "waterbody", "area", "site", "site_depth"),
                              colnames(locs))

      if (length(missing_cols) > 0) {
        return(leaflet::leaflet() %>%
                 leaflet::addTiles() %>%
                 leaflet::addMarkers(lng = 0, lat = 0,
                                     popup = "Missing Required Columns"))
      }

      # --- Ensure data is not empty ---
      if (nrow(locs) == 0) {
        return(leaflet::leaflet() %>%
                 leaflet::addTiles() %>%
                 leaflet::addMarkers(lng = 0, lat = 0,
                                     popup = "No Data Available"))
      }

      # --- Remove rows missing lon ---
      locs <- dplyr::filter(locs, !is.na(lon))

      # --- Create popup content ---
      locs$popup_info <- paste(
        "<b>Waterbody:</b>", locs$waterbody, "<br>",
        "<b>Area:</b>", locs$area, "<br>",
        "<b>Site:</b>", locs$site, "<br>",
        "<b>Site Depth:</b>", locs$site_depth, "m"
      )

      # --- Render map ---
      leaflet::leaflet(locs) %>%
        leaflet::addTiles() %>%
        leaflet::addCircleMarkers(
          lng = ~lon, lat = ~lat,
          popup = ~popup_info,
          radius = 5,
          color = "blue",
          fillOpacity = 0.7
        )
    })
  })
}

