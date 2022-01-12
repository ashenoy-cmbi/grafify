#' Scale_color continuous scheme
#'
#' `grafify` internally includes color-blind compatible schemes for fill and color/color aesthetics.
#' Note that this scheme is **only** for continuous variables and has one palette (\code{yellow_conti}) [modified from](http://personal.sron.nl/~pault/#sec:sequential) the YlOrBr scheme from RColorBrewer. 
#'
#' Colour palettes available are as follows:
#' 
#' \if{html}{\out{<div style="text-align: center">}\figure{grafify_palettesv020.jpg}{options: style="width:750px;max-width:70\%;"}\out{</div>}}
#'
#' 
#' 
#' @param reverse Whether the color order should be reversed.
#' @param ... Additional parameters for `scale_fill` or `scale_color`
#'
#' @return ggplot scale_fill function
#' @export scale_color_grafify_c
#' @import ggplot2
#'
#' @examples
#' #basic usage on mtcars data with x and y quantitative axes
#' ggplot(mtcars, aes(x = mpg, y = disp))+
#' geom_point(aes(color= disp), size = 3)+
#' scale_color_grafify_c()
#' 

scale_color_grafify_c <- function(reverse = FALSE, ...){
  pal <- graf_col_palette(palette = c("yellow_conti"), 
                          reverse = reverse)
  scale_color_gradientn(colors = pal(255), ...)
}
