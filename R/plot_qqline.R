#' Plot quantile-quantile (QQ) graphs from data.
#'
#' This function takes a data table, X and Y variables, and plots a QQ graph using \code{\link[ggplot2]{ggplot}}.
#'
#' The function uses \code{\link[ggplot2]{stat_qq}} and \code{\link[ggplot2]{stat_qq_line}} geometries.
#' Note that the function requires the quantitative Y variable first, and groups them based on an X variable.
#' The X variable is mapped to the \code{colour} aesthetic in both \code{stat_qq} and \code{stat_qq_line}, and its colour can be changed using \code{\link[ggplot2]{scale_colour_brewer}} or any \code{scale_colour...} option.
#'
#' @param data a data table e.g. data.frame or tibble.
#' @param ycol name of the column containing the quantitative variable whose distribution is to be plotted
#' @param xcol name of the column containing a categorical variable
#'
#' @return This function returns a \code{ggplot2} object.
#' @export plot_qqline
#'
#' @examples
#' #Basic usage
#' plot_qqline(Chol, Cholesterol, Treatment)
#'
#' Additional layers are possible
#' plot_qqline(Chol, Cholesterol, Treatment)+
#'    facet_wrap("Treatment")

plot_qqline <- function(data, ycol, xcol){
  ggplot2::ggplot(data, aes(sample = {{ ycol }}))+
    stat_qq(geom = "point", na.rm = T,
            size = 3,
            alpha = 0.8,
            aes(colour = {{ xcol }}) )+
    stat_qq_line(aes(colour = {{ xcol }}),
                 na.rm = T,
                 size = 1)+
    theme_classic(base_size = 14)
}
