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
#' @return This generates required number of sequential colours from the chosen grafify palette when called by scale functions of  ggplot2.
#' @importFrom grDevices colorRampPalette
#' @export graf_col_palette
#'
#'
graf_col_palette <- function(palette = "okabe_ito",
                             reverse = FALSE, ...){
  #old function
  #pal <- graf_palettes[[palette]]
  #if(reverse) pal <- rev(pal)
  #colorRampPalette_d(pal, ...)
  #new function that works with n = 1
  function(n) {
    if (n > length(graf_palettes[[palette]])) {
      pal <- colorRampPalette(graf_palettes[[palette]])(n)
    } else {
      pal <- unname(graf_palettes[[palette]][1:n])}
    if (reverse) {rev(pal)} else {pal}
  }
}

