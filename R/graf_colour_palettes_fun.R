#' Call palettes for scale & fill
#'
#' @param palette internal
#' @param reverse internal
#' @param ... additional parameters
#' @import grDevices
#' @export graf_col_palette
#'
#'
graf_col_palette <- function(palette = "all_grafify",
                             reverse = FALSE, ...){
  pal <- graf_palettes[[palette]]
  if(reverse) pal <- rev(pal)
  colorRampPalette_d(pal, ...)
}

