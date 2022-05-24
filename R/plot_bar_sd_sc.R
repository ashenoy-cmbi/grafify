#' Plot a bar graph indicating mean with error bars (SD) using two variables.
#'
#' This function is related to \code{\link{plot_bar_sd}}, but this one maps a single or same colour, therefore `_sc`.
#' The only new argument is `colour`, which can be any hexcode or name of colours in the `all_grafify` [palette](https://grafify-vignettes.netlify.app/colour_palettes.html#full-list-of-hexcodes).
#' The default colour is `ok_orange`. `ColPal` and `ColRev` arguments are not available. Colours available can be seen quickly with \code{\link{plot_grafify_palette}}.
#'
#' You are instead encouraged to show all data using the following functions: \code{\link{plot_scatterbar_sd}}, \code{\link{plot_scatterbox}}, \code{\link{plot_dotbox}}, \code{\link{plot_dotbar_sd}}, \code{\link{plot_scatterviolin}} or \code{\link{plot_dotviolin}}.
#'
#' @param data a data table object, e.g. a data.frame or tibble.
#' @param xcol name of the column to plot on X axis. This should be a categorical variable.
#' @param ycol name of the column to plot on the Y axis. This should be a quantitative variable.
#' @param colour colour of boxes and dots; a number between 1-64, any hexcode or names from. `grafify` colour palettes. Default is `ok_orange`.
#' @param b_alpha fractional opacity of bars, default set to 1 (i.e. maximum opacity & zero transparency).
#' @param bwid width of bars (default 0.7).
#' @param bthick thickness of bar borders; default 1.
#' @param ewid width of error bars, default 0.3.
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text.
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param ... any additional arguments to pass to \code{stat_summary}.
#'
#' @return This function returns a \code{ggplot2} object of class "gg" and "ggplot".
#' @noRd
#' @import ggplot2 Hmisc
#'
#' @examples
#' plot_bar_sd_sc(data = data_doubling_time, 
#' xcol = Student, ycol = Doubling_time, 
#' colour = "ok_grey")
#' 

plot_bar_sd_sc <- function(data, xcol, ycol, colour = "ok_orange", b_alpha = 1, bwid = 0.7, bthick = 1, ewid = 0.3, TextXAngle = 0, fontsize = 20, ...){
  warning("Use `SingleColour` argument in `plot_` functions, as `plot_..._sc` functions have been deprecated.")
ifelse(grepl("#", colour),
       a <- colour,
       a <- get_graf_colours({{ colour }}))

  ggplot2::ggplot(data, aes(x = factor({{ xcol }}),
                            y = {{ ycol }}))+
    stat_summary(geom = "bar", 
                 width = {{ bwid }}, size = {{ bthick }},
                 fun = "mean", colour = "black",
                 alpha = {{ b_alpha }},
                 fill = a, ...)+
    stat_summary(geom = "errorbar", size = 1,
                 fun.data = "mean_sdl",
                 fun.args = list(mult = 1),
                 width = {{ ewid }})+
    labs(x = enquo(xcol))+
    theme_classic(base_size = {{ fontsize }})+
    theme(strip.background = element_blank())+
    guides(x = guide_axis(angle = {{ TextXAngle }}))
}
