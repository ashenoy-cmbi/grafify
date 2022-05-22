#' Scale_color continuous scheme
#'
#' `grafify` internally includes color-blind compatible schemes for fill and color/color aesthetics.
#' Note that this scheme is **only** for continuous variables and take four options, which include (\code{yellow_conti}) [modified from](http://personal.sron.nl/~pault/#sec:sequential) the YlOrBr scheme from RColorBrewer, `blue_conti`, `PrGn_div` and `OrBl_div`.
#'  
#' Colour palettes available are as follows:
#' 
#' \if{html}{\out{<div style="text-align: center">}\figure{grafify_palettesv020.jpg}{options: style="width:750px;max-width:70\%;"}\out{</div>}}
#'
#' Colours available can be seen quickly with \code{\link{plot_grafify_palette}}. 
#' 
#' @param reverse Whether the color order should be reversed.
#' @param palette One of five `grafify` palettes for quantiative data `blue_conti`, `yellow_conti`, `grey_conti`, `PrGn_div`, `OrBl_div`
#' @param ... Additional parameters for `scale_fill` or `scale_color`.
#'
#' @return ggplot scale_fill function for continuous colours.
#' @export scale_color_grafify_c
#' @import ggplot2
#'
#' @examples
#' #basic usage on mtcars data with x and y quantitative axes
#' ggplot(mtcars, aes(x = mpg, y = disp))+
#' geom_point(aes(color= disp), size = 3)+
#' scale_color_grafify_c()
#' 

scale_color_grafify_c <- function(palette = c("blue_conti", "yellow_conti", "grey_conti", "PrGn_div", "OrBl_div"), reverse = FALSE, ...){
  palette <- match.arg(palette)
  pal <- graf_col_palette(palette = palette, 
                          reverse = reverse)
  scale_color_gradientn(colors = pal(255), ...)
}
