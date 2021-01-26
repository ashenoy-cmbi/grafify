#' Plot a bar graph indicating mean with error bars (SD) using two variables.
#'
#' This function takes a data table, X and Y variables, and plots a graph with a summary bar graph showing the mean of data using \code{ggplot2}.
#'
#' The function uses \code{\link[ggplot2]{stat_summary}} with \code{geom = "bar"}.
#' Standard deviation (SD) is plotted through \code{\link[ggplot2]{stat_summary}} calculated using \code{mean_sdl} from the \code{ggplot2} package (get help with \code{?mean_sdl}), and 1x SD is plotted (\code{fun.arg = list(mult = 1)}.
#' The X variable is mapped to the \code{fill} aesthetic in the point geometry. Colour can be changed using \code{scale_fill_brewer} (or related) options.
#'
#' You are instead encouraged to show all data using the following functions: \code{\link{plot_scatterbar_sd}}, \code{\link{plot_dotbox}}, \code{\link{plot_dotbar_sd}} and \code{\link{plot_dotviolin}}.
#'
#' @param data a data table object, e.g. a data.frame or tibble.
#' @param xcol name of the column to plot on X axis. This should be a categorical variable.
#' @param ycol name of the column to plot on the Y axis. This should be a quantitative variable.
#' @param bwid width of bars, default 0.7
#' @param ewid width of error bars, default 0.3
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param ColPal grafify colour palette to apply, default "all_grafify"; alternatives: "okabe_ito", "bright", "pale", "vibrant", "contrast", "muted" "dark", "light".
#' @param ColRev whether to reverse order of colour choice, default F (FALSE); can be set to T (TRUE)
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text
#'
#' @return This function returns a \code{ggplot2} object on which additional geometries etc. can be added.
#' @export plot_bar_sd
#' @import ggplot2 Hmisc
#'
#' @examples
#'
#' #Basic usage
#' plot_bar_sd(data_doubling_time, Student, Doubling_time)
#'


plot_bar_sd <- function(data, xcol, ycol, bwid = 0.7, ewid = 0.3, fontsize = 20, ColPal = "all_grafify", ColRev = F, TextXAngle = 0){
  ggplot2::ggplot(data, aes(x = factor({{ xcol }}),
                            y = {{ ycol }}))+
    stat_summary(geom = "bar", 
                 width = {{ bwid }}, size = 1,
                 fun = "mean", colour = "black",
                 aes(fill = factor({{ xcol }})))+
    stat_summary(geom = "errorbar", size = 1,
                 fun.data = "mean_sdl",
                 fun.args = list(mult = 1),
                 width = {{ ewid }})+
    labs(x = enquo(xcol),
         fill = enquo(xcol))+
    theme_classic(base_size = {{ fontsize }})+
    theme(strip.background = element_blank())+
    guides(x = guide_axis(angle = {{ TextXAngle }}))+
    scale_fill_grafify(palette = {{ ColPal }}, 
                       reverse = {{ ColRev }})
}
