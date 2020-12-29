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
#'
#' @return This function returns a \code{ggplot2} object on which additional geometries etc. can be added.
#' @export plot_bar_sd
#'
#' @examples
#'
#' #Basic usage
#' plot_bar_sd(Tab_doublings, Student, Doubling_time)
#'
#' #Additional layers can be added
#' plot_bar_sd(Tab_doublings, Student, Doubling_time)+
#'    labs(title = "Plot with bar (mean) & SD")+
#'    scale_fill_viridis_d()


plot_bar_sd <- function(data, xcol, ycol, bwid = 0.7, ewid = 0.3, fontsize = 20){
  ggplot2::ggplot(data, aes(x = factor({{ xcol }}),
                            y = {{ ycol }}))+
    stat_summary(geom = "bar", width = {{ bwid }},
                 fun = "mean",
                 aes(fill = {{ xcol }}))+
    stat_summary(geom = "errorbar",
                 fun.data = "mean_sdl",
                 fun.args = list(mult = 1),
                 width = {{ ewid }})+
    labs(x = enquo(xcol))+
    theme_classic(base_size = {{ fontsize }})
}
