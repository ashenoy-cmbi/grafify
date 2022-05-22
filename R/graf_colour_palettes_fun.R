#' Call palettes for scale & fill
#' 
#' @param palette internal
#' @param reverse internal
#' @param ... additional parameters
#' @return This generates required number of sequential colours from the chosen grafify palette when called by scale functions of  ggplot2.
#' @import grDevices
#' @export graf_col_palette
#'
#'
graf_col_palette <- function(palette = "okabe_ito",
                             reverse = FALSE, ...){
  pal <- graf_palettes[[palette]]
  if(reverse) pal <- rev(pal)
  colorRampPalette_d(pal, ...)
}

