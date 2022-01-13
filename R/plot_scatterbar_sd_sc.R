#' Plot scatter dots on a bar graph with SD error bars with two variables.
#'
#' This function is related to \code{plot_scatterbar_sd}, but this one maps a single or same colour, therefore `_sc`.
#' The only new argument is `colour`, which can be any hexcode or name of colours in the `all_grafify` [palette](https://grafify-vignettes.netlify.app/colour_palettes.html#full-list-of-hexcodes).
#' The default colour is `ok_orange`. `ColPal` and `ColRev` arguments are not available. Colours available can be seen quickly with \code{\link{plot_grafify_palette}}.
#'
#' @param data a data table object, e.g. data.frame or tibble.
#' @param xcol name of the column to plot on X axis. This should be a categorical variable.
#' @param ycol name of the column to plot on quantitative Y axis. This should be a quantitative variable.
#' @param colour colour of boxes and dots; a number between 1-64, any hexcode or names from `grafify` colour palettes. Default is `ok_orange`.
#' @param symsize size of point symbols, default set to 2
#' @param symthick thickness of symbol border, default set to 1
#' @param bwid width of bars, default set to 0.7
#' @param ewid width of error bars, default set to 0.3
#' @param jitter extent of jitter (scatter) of symbols, default is 0 (i.e. aligned symbols). To reduce symbol overlap, try 0.1-0.3 or higher.  
#' @param b_alpha fractional opacity of bars, default set to 1 (i.e. maximum opacity & zero transparency)
#' @param s_alpha fractional opacity of symbols, default set to 1 (i.e. maximum opacity & zero transparency)
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#'
#' @return This function returns a \code{ggplot2} object on which additional geometries etc. can be added.
#' @export plot_scatterbar_sd_sc
#' @import ggplot2 Hmisc
#'
#' @examples
#'
#' plot_scatterbar_sd_sc(data = data_doubling_time, 
#' xcol = Student, ycol = Doubling_time)
#' plot_scatterbar_sd_sc(data = data_doubling_time, 
#' xcol = Student, ycol = Doubling_time, 
#' colour = "ok_grey")

plot_scatterbar_sd_sc <- function(data, xcol, ycol, colour = "ok_orange", symsize = 2.5, symthick = 1, bwid = 0.7, ewid = 0.3, jitter = 0, b_alpha = 1, s_alpha = 1, TextXAngle = 0, fontsize = 20){

  ifelse(grepl("#", colour), 
         a <- colour,
         a <- get_graf_colours({{ colour }}))
  ggplot2::ggplot(data, aes(x = factor({{ xcol }}),
                            y = {{ ycol }}))+
    stat_summary(geom = "bar", colour = "black", 
                 width = {{ bwid }},
                 fun = "mean", 
                 alpha = {{ b_alpha }}, size = 1,
                 fill = a)+
    geom_point(size = {{ symsize }}, 
               alpha = {{ s_alpha }}, shape = 21,
               position = position_jitter(width = {{ jitter }}), 
               stroke = {{ symthick }},
               fill = a)+
    stat_summary(geom = "errorbar", size = 1,
                 fun.data = "mean_sdl",
                 fun.args = list(mult = 1),
                 width = {{ ewid }} )+
    labs(x = enquo(xcol))+
    theme_classic(base_size = {{ fontsize }})+
    theme(strip.background = element_blank())+
    guides(x = guide_axis(angle = {{ TextXAngle }}))
}
