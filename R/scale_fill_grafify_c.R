#' Scale_fill continuous scheme
#'
#' `grafify` internally includes colour-blind compatible schemes for fill and colour/color aesthetics.
#' Note that this scheme is **only** for continuous variables and has one palette (\code{yellow_conti}) [modified from](http://personal.sron.nl/~pault/#sec:sequential) the YlOrBr scheme from RColorBrewer. 
#' 
#' Colours available can be seen quickly with \code{\link{plot_grafify_palette}}.
#'
#' @param reverse Whether the colour order should be reversed.
#' @param ... Additional parameters for `scale_fill` or `scale_colour`.
#'
#' @return ggplot scale_fill function for continuous colours.
#' @export scale_fill_grafify_c
#' @import ggplot2
#'
#' @examples
#' #basic usage on mtcars data with x and y quantitative axes
#' ggplot(mtcars, aes(x = mpg, y = disp))+
#' geom_point(aes(fill = disp), shape = 21, size = 3)+
#' scale_fill_grafify_c()
#' 

scale_fill_grafify_c <- function(reverse = FALSE, ...){
  pal <- graf_col_palette(palette = c("yellow_conti"), 
                          reverse = reverse)
  scale_fill_gradientn(colours = pal(255), ...)
}
