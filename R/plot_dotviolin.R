#' Plot a dotplot on a violin plot with two variables.
#'
#' This function takes a data table, X and Y variables, and plots a graph with a dotplot and violinplot using \code{ggplot}.
#'
#' The function uses \code{\link[ggplot2]{geom_violin}} and \code{\link[ggplot2]{geom_dotplot}} geometries.
#' Note that the \code{\link{geom_violin}} options are set as follows: \code{scale = "area", draw_quantiles = c(0.25, .5, .75)}. The \code{trim = T} set by default can be changed when calling the function.
#' The X variable is mapped to the \code{fill} aesthetic in both violinplot and dotplot, and its colour can be changed using \code{scale_fill_brewer} or any \code{scale_fill...} option. The size of dots can be adjusted using the parameter, which is \code{dotsize = 1} by default.
#'
#' This function is related to \code{\link{plot_scatterbar_sd}}, \code{\link{plot_dotbar_sd}} and \code{\link{plot_dotviolin}}.
#'
#' @param data a data table object, e.g. data.frame or tibble
#' @param xcol name of the column to plot on X axis. This should be a categorical variable.
#' @param ycol name of the column to plot on quantitative Y axis. This should be a quantitative variable.
#' @param dotsize size of dots relative to \code{binwidth} used by \code{geom_dotplot}. Default set to 1, increase/decrease as needed.
#' @param trim set whether tips of violin plot should be trimmed at high/low data. Default \code{trim = T}, can be changed to F.
#'
#' @return This function returns a \code{ggplot2} object on which additional geometries etc. can be added.
#' @export plot_dotviolin
#'
#' @examples
#'
#' #Basic usage requires a data table and X & Y variables#'
#' plot_scatterviolin(Tab_doublings, Student, Doubling_time)
#'
#' # or with trim = F
#' plot_scatterviolin(Tab_doublings, Student, Doubling_time, trim = F)
#'
#' #Additional ggplot layering is possible
#' plot_scatterviolin(Chol, Treatment, Cholesterol)+
#'    scale_fill_viridis_d()+
#'    scale_colour_viridis_d()

plot_dotviolin <- function(data, xcol, ycol, dotsize = 1, trim = T, scale = "area"){
  ggplot2::ggplot(data, aes(x = factor({{ xcol }}),
                            y = {{ ycol }}))+
    geom_violin(aes(fill = {{ xcol }}),
                alpha = 0.3,
                trim = {{ trim }},
                scale = {{ scale }},
                draw_quantiles = c(0.25, .5, .75),
                colour = "black",
                adjust = 0.8)+
    geom_dotplot(stackdir = "center", dotsize = {{ dotsize }},
                 binaxis = 'y',
                 aes(fill = {{ xcol }}))+
    labs(x = enquo(xcol))+
    theme_classic(base_size = 14)
}
