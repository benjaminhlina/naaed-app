app_version_head <- function() {

  tags$head(
    tags$style(HTML("
        #app-version {
          position: fixed;
          bottom: 6px;
          left: 10px;
          font-size: 12px;
          color: #888;
          z-index: 1000;
        }
      "))
  )
}

app_version_label <- function(app_version) {
  tags$div(
    id = "app-version",
    paste("v", app_version)
  )
}

