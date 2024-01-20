
get_guide_angle <- function(plot, guide = "x") {
  if (inherits(plot$guides, "Guides")) {
    plot$guides$guides[[guide]]$params$angle
  } else {
    plot$guides$x$angle
  }
}
