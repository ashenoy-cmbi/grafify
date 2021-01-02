#' Plot a dotplot on a bar graph with SD error bars with two variables.
#'
#' This function takes a data table, X and Y variables, and plots a graph with a dotplot and bars using \code{\link[ggplot2]{ggplot}}.
#'
#' The function uses \code{\link[ggplot2]{stat_summary}} with \code{geom = "bar"}, and \code{\link[ggplot2]{geom_dotplot}} geometries.
#' Standard deviation (SD) is plotted through \code{\link[ggplot2]{stat_summary}} calculated using \code{mean_sdl} from the \code{ggplot2} package (get help with \code{?mean_sdl}), and 1x SD is plotted (\code{fun.arg = list(mult = 1)}.
#' The X variable is mapped to the \code{fill} aesthetic in both bar and dotplot, and its colour can be changed using \code{scale_fill_brewer} or any \code{scale_fill...} option. The size of dots can be adjusted using the parameter, which is  \code{dotsize = 1} by default.
#'
#' This function is related to \code{\link{plot_scatterbar_sd}}, \code{\link{plot_dotbar_sd}} and \code{\link{plot_dotviolin}}.
#'
#' @param data a data table object, e.g. data.frame or tibble.
#' @param xcol name of the column to plot on X axis. This should be a categorical variable.
#' @param ycol name of the column to plot on quantitative Y axis. This should be a quantitative variable.
#' @param dotsize size of dots relative to binwidth used by \code{\link[ggplot2]{geom_dotplot}}. Default set to 1, increase/decrease as needed.
#' @param ewid width of error bars, default set to 0.2
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param alpha fractional transparency, default set to 1 (i.e. zero transparency, fully opaque)
#'
#' @return This function returns a \code{ggplot2} object on which additional geometries etc. can be added.
#' @export plot_dotbar_sd
#'
#' @examples
#' #Basic usage requires a data table and X & Y variables#'
#' plot_dotbar_sd(Chol, Treatment, Cholesterol)
#'
#' #Transformations of Y variable are possible as follows
#' #' plot_dotbar_sd(Chol, Treatment, log(Cholesterol))
#'
#' #Additional ggplot layering is possible
#' plot_dotbar_sd(Chol, Treatment, Cholesterol, dotsize = 2)+
#'    labs(title = "Plot with scatter dots & boxplot")+
#'    scale_color_viridis_d()+scale_fill_viridis_d()+
#'    facet_wrap("Hospital")


plot_dotbar_sd <- function(data, xcol, ycol, dotsize = 1, ewid = 0.2, alpha = 1, fontsize = 20){
  ggplot2::ggplot(data, aes(x = factor({{ xcol }}),
                            y = {{ ycol }}))+
    stat_summary(geom = "bar", colour = "black",
                 fun = "mean", alpha = {{ alpha }},
                 aes(fill = factor({{ xcol }})))+
    stat_summary(geom = "errorbar",
                 fun.data = "mean_sdl",
                 fun.args = list(mult = 1),
                 width = {{ ewid }})+
    geom_dotplot(dotsize = {{ dotsize }},
                 binaxis = 'y', stackdir = 'center',
                 aes(fill = factor({{ xcol }}))) +
    labs(x = enquo(xcol),
         fill = enquo(xcol))+
    theme_classic(base_size = {{ fontsize }})
}

