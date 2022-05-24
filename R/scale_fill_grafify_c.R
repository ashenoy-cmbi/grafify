#' Scale_fill continuous scheme
#'
#' `grafify` internally includes colour-blind compatible schemes for fill and colour/color aesthetics.
#' Note that this scheme is **only** for continuous variables and take four options, which include (\code{yellow_conti}) [modified from](http://personal.sron.nl/~pault/#sec:sequential) the YlOrBr scheme from RColorBrewer, `blue_conti`, `PrGn_div` and `OrBl_div`.
#' 
#' Colours available can be seen quickly with \code{\link{plot_grafify_palette}}.
#'
#' @param reverse Whether the colour order should be reversed.
#' @param palette One of five `grafify` palettes for quantiative data `blue_conti`, `yellow_conti`, `grey_conti`, `PrGn_div`, `OrBl_div`
#' @param ... Additional parameters for `scale_fill` or `scale_colour`.
#'
#' @return ggplot scale_fill function for continuous colours.
#' @noRd
#' @import ggplot2
#'
#' @examples
#' #basic usage on mtcars data with x and y quantitative axes
#' ggplot(mtcars, aes(x = mpg, y = disp))+
#' geom_point(aes(fill = disp), shape = 21, size = 3)+
#' scale_fill_grafify_c()
#' 

scale_fill_grafify_c <- function(palette = c("blue_conti", "yellow_conti", "grey_conti", "PrGn_div", "OrBl_div"), reverse = FALSE, ...){
  palette <- match.arg(palette)
  pal <- graf_col_palette(palette = palette, 
                          reverse = reverse)
  scale_fill_gradientn(colours = pal(255), ...)
}