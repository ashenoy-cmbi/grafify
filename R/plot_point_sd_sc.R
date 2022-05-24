#' Plot a point as mean with SD error bars using two variables.
#'
#' This function is related to \code{plot_point_sd}, but this one maps a single or same colour, therefore `_sc`. The only new argument is `colour`, which can be any hexcode or name of colours in the `all_grafify` [palette](https://grafify-vignettes.netlify.app/colour_palettes.html#full-list-of-hexcodes).
#' The default colour is `ok_orange`. `ColPal` and `ColRev` arguments are not available. Colours available can be seen quickly with \code{\link{plot_grafify_palette}}. 
#'
#' You are instead encouraged to show all data using the following functions: \code{\link{plot_scatterbar_sd}}, \code{\link{plot_scatterbox}}, \code{\link{plot_dotbox}}, \code{\link{plot_dotbar_sd}}, \code{\link{plot_scatterviolin}} or \code{\link{plot_dotviolin}}.
#'
#' @param data a data table object, e.g. data.frame or tibble.
#' @param xcol name of the column with a categorical X variable.
#' @param ycol name of the column with quantitative Y variable.
#' @param colour colour of boxes and dots; a number between 1-64, any hexcode or names from `grafify` colour palettes. Default is `ok_orange`.
#' @param s_alpha fractional opacity of symbols, default set to 1 (i.e. maximum opacity & zero transparency).
#' @param symsize size of point symbols, default set to 3.5.
#' @param symthick thickness of symbol border, default set to 1
#' @param ewid width of error bars, default set to 0.2.
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text.
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#'
#' @return This function returns a \code{ggplot2} object of class "gg" and "ggplot".
#' @noRd
#' @import ggplot2 Hmisc
#'
#' @examples
#' #Basic usage
#' plot_point_sd_sc(data = data_doubling_time, 
#' xcol = Student, ycol = Doubling_time)
#' plot_point_sd_sc(data = data_doubling_time, 
#' xcol = Student, ycol = Doubling_time, 
#' colour = "ok_grey")
#'

plot_point_sd_sc <- function(data, xcol, ycol, colour = "ok_orange", s_alpha = 1, symsize = 3.5, symthick = 1, ewid = 0.2, TextXAngle = 0, fontsize = 20){
  warning("Use `SingleColour` argument in `plot_` functions, as `plot_..._sc` functions have been deprecated.")
  
ifelse(grepl("#", colour), 
         a <- colour,
         a <- get_graf_colours({{ colour }}))

  ggplot2::ggplot(data, aes(x = factor({{ xcol }}),
                            y = {{ ycol }}))+
    stat_summary(geom = "errorbar",
                 fun.data = "mean_sdl", size = 1,
                 fun.args = list(mult = 1),
                 alpha = {{ s_alpha }},
                 width = {{ ewid }})+
    stat_summary(geom = "point", shape = 21,
                 size = {{ symsize }}, 
                 stroke = {{ symthick }},
                 fun = "mean",
                 fill = a)+
    labs(x = enquo(xcol))+
    theme_classic(base_size = {{ fontsize }})+
    theme(strip.background = element_blank())+
    guides(x = guide_axis(angle = {{ TextXAngle }}))
}
