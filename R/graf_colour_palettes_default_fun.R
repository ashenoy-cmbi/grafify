#' Call `grafify` palettes for scale & fill functions
#' 
#' `graf_col_palette` and `graf_col_palette_default` functions generate colours for `grafify` scale functions. 
#' `graf_col_palette` picks sequential colours when the number of discrete colours needed is less than that in the palette. This is the default for `grafify` with `ColoSeq = TRUE`. If the number of colours required is more than that in the discrete palette, it fills intervening colours using the \code{\link{colorRampPalette}[grDevices]} function.
#' 
#' `graf_col_palette_default` picks the most distant colours within the palette, rather than in the sequence they are in the palette, when the number of colours required is less than that in the palette. 
#' 
#' Colour order can be reversed in both functions.
#' 
#' When only one colour discreet is required, and you want to `reverse` the colour palette, `ColSeq` should be set to `FALSE`.
#'
#' @param palette internal
#' @param reverse internal
#' @param ... additional parameters
#' @importFrom grDevices colorRampPalette
#' @return This generates required number of distant colours from the chosen grafify palette when called by scale functions of  ggplot2.
#' @export graf_col_palette_default
#'
#'
graf_col_palette_default <- function(palette = "okabe_ito",
                                     reverse = FALSE, ...){
  #old function
  #pal <- graf_palettes[[palette]]
  #if(reverse) pal <- rev(pal)
  #colorRampPalette(pal, ...)
  
  #new function
  function(n) {
    pal <- graf_palettes[[palette]]
    if(reverse) pal <- rev(pal)
    pal <- colorRampPalette(pal)(n)
  }
}
