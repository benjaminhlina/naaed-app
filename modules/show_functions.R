show_plot_message <- function(msg) {
  grid.newpage()
  grid.text(
    msg,
    x = 0.5,
    y = 0.5,
    gp = gpar(
      fontsize = 20,
      fontface = "bold",
      col = "black"
    )
  )
}
