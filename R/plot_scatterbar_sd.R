#' Plot scatter dots on a bar graph with SD error bars with two variables.
#'
#' This function takes a data table, X and Y variables, and plots a graph with a jitterplot or scatterplot and bars using \code{\link[ggplot2]{ggplot}}.
#'
#' The function uses \code{\link[ggplot2]{stat_summary}} with \code{geom = "bar"}, and \code{\link[ggplot2]{geom_point}} with \code{position = position_jitter(width = 0.05)}.
#' Standard deviation (SD) is plotted through \code{\link[ggplot2]{stat_summary}} calculated using \code{mean_sdl} from the \code{ggplot2} package (get help with \code{?mean_sdl}), and 1x SD is plotted (\code{fun.arg = list(mult = 1)}.
#' The X variable is mapped to the \code{fill} aesthetic in the bar geometry and \code{colour} aesthetic in \code{geom_point}. Fill and colour can be changed using \code{scale_fill_brewer} (or related) and \code{scale_colour_brewer} (or related) options.
#'
#' This function is related to \code{\link{plot_dotbox}}, \code{\link{plot_dotbar_sd}} and \code{\link{plot_dotviolin}}.
#'
#' @param data a data table object, e.g. data.frame or tibble.
#' @param xcol name of the column to plot on X axis. This should be a categorical variable.
#' @param ycol name of the column to plot on quantitative Y axis. This should be a quantitative variable.
#' @param symsize size of point symbols, default set to 2
#' @param symthick thickness of symbol border, default set to 1
#' @param bwid width of bars, default set to 0.7
#' @param ewid width of error bars, default set to 0.2
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param alpha fractional transparency, default set to 1 (i.e. zero transparency, fully opaque)
#'
#' @return This function returns a \code{ggplot2} object on which additional geometries etc. can be added.
#' @export plot_scatterbar_sd
#'
#' @examples
#'
#' #Basic usage requires a data table and X & Y variables#'
#' plot_scatterbar_sd(Chol, Treatment, Cholesterol)
#'
#' #Transformations of Y variable are possible as follows
#' #' plot_scatterbar_sd(Chol, Treatment, log(Cholesterol))
#'
#'
#' #Additional ggplot layering is possible
#' plot_scatterbar_sd(Tab_doublings, Student, Doubling_time)+
#'    labs(title = "Plot with scatter plot, bars (mean) & SD")+
#'    scale_colour_grafify()+scale_fill_grafify()+facet_wrap("Experiment")

plot_scatterbar_sd <- function(data, xcol, ycol, symsize = 2, symthick = 1, bwid = 0.7, ewid = 0.3, fontsize = 20, alpha = 1){
  ggplot2::ggplot(data, aes(x = factor({{ xcol }}),
                            y = {{ ycol }}))+
    stat_summary(geom = "bar", colour = "black", width = {{ bwid }},
                 fun = "mean", alpha = {{ alpha }},
                 aes(fill = factor({{ xcol }})))+
    geom_point(size = {{ symsize }}, alpha = 0.8, shape = 21,
               position = position_jitter(width = 0.05), stroke = {{ symthick }},
               aes(fill = factor({{ xcol }})))+
    stat_summary(geom = "errorbar",
                 fun.data = "mean_sdl",
                 fun.args = list(mult = 1),
                 width = {{ ewid }} )+
    labs(x = enquo(xcol),
         fill = enquo(xcol))+
    theme_classic(base_size = {{ fontsize }})
}
