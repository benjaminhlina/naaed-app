ram_tracker <- function() {
  observe({
    mem <- pryr::mem_used()
    mem_mb <- round(as.numeric(mem) / 1024 ^ 2, 2)
    timestamp <- format(Sys.time(), "%H:%M:%S")

    cli::cli_alert_info("RAM: {mem_mb} mb at {timestamp}")

    invalidateLater(5000)  # Print every 5 seconds
  })
}
