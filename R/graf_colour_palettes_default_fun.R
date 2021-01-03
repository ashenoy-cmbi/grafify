#' Call palettes for scale & fill with default colorRampPalette
#'
#' @param palette internal
#' @param reverse internal
#' @param ...
#'
#'
graf_col_palette_default <- function(palette = "all_grafify",
                             reverse = FALSE, ...){
  pal <- graf_palettes[[palette]]
  if(reverse) pal <- rev(pal)
  colorRampPalette(pal, ...)
}

