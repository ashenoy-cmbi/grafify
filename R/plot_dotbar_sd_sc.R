#' Plot a dotplot on a bar graph with SD error bars with two variables.
#'
#' This function is related to \code{plot_dotbar_sd}, but this one maps a single or same colour, therefore `_sc`. The only new argument is `colour`, which can be any hexcode or name of colours in the `all_grafify` [palette](https://grafify-vignettes.netlify.app/colour_palettes.html#full-list-of-hexcodes).
#' 
#' The default colour is `ok_orange`. `ColPal` and `ColRev` arguments are not available. Colours available can be seen quickly with \code{\link{plot_grafify_palette}}.
#'
#' @param data a data table object, e.g. data.frame or tibble.
#' @param xcol name of the column to plot on X axis. This should be a categorical variable.
#' @param ycol name of the column to plot on quantitative Y axis. This should be a quantitative variable.
#' @param colour colour of boxes and dots; a number between 1-64, any hexcode or names from `grafify` colour palettes. Default is `ok_orange`.
#' @param dotsize size of dots relative to binwidth used by \code{\link[ggplot2]{geom_dotplot}}. Default set to 1.5, increase/decrease as needed.
#' @param dotthick thickness of dot border (`stroke` parameter of `geom_dotplot`), default set to 1.
#' @param bwid width of bars, default set to 0.7
#' @param ewid width of error bars, default set to 0.2.
#' @param b_alpha fractional opacity of bars, default set to 1 (i.e. maximum opacity & zero transparency).
#' @param d_alpha fractional opacity of dots, default set to 1 (i.e. maximum opacity & zero transparency).
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text.
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param ... any additional arguments to pass to \code{ggplot2}[geom_dotplot].
#'
#' @return This function returns a \code{ggplot2} object of class "gg" and "ggplot".
#' @export plot_dotbar_sd_sc
#' @import ggplot2 Hmisc
#'
#' @examples
#' #default "okabe_ito" colour
#' plot_dotbar_sd_sc(data = data_doubling_time, 
#' xcol = Student, ycol = Doubling_time)
#' 
#' #a different colour
#' plot_dotbar_sd_sc(data = data_doubling_time, 
#' xcol = Student, ycol = Doubling_time, 
#' colour = "#88ccee")

plot_dotbar_sd_sc <- function(data, xcol, ycol, colour = "ok_orange", dotsize = 1.5, dotthick = 1, bwid = 0.7, ewid = 0.2,  b_alpha = 1, d_alpha = 1, TextXAngle = 0, fontsize = 20, ...){

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
    geom_dotplot(dotsize = {{ dotsize }}, 
                 stroke = {{ dotthick }},
                 binaxis = 'y', 
                 alpha = {{ d_alpha }},
                 stackdir = 'center',
                 fill = a,
                 ...)+
    stat_summary(geom = "errorbar", size = 1,
                 fun.data = "mean_sdl",
                 fun.args = list(mult = 1),
                 width = {{ ewid }}) +
    labs(x = enquo(xcol))+
    theme_classic(base_size = {{ fontsize }})+
    theme(strip.background = element_blank())+
    guides(x = guide_axis(angle = {{ TextXAngle }}))
}
