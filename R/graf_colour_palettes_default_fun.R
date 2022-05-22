#' Call palettes for scale & fill with default colorRampPalette
#'
#' @param palette internal
#' @param reverse internal
#' @param ... additional parameters
#' @import grDevices
#' @return This generates required number of distant colours from the chosen grafify palette when called by scale functions of  ggplot2.
#' @export graf_col_palette_default
#'
#'
graf_col_palette_default <- function(palette = "okabe_ito",
                             reverse = FALSE, ...){
  pal <- graf_palettes[[palette]]
  if(reverse) pal <- rev(pal)
  colorRampPalette(pal, ...)
}
