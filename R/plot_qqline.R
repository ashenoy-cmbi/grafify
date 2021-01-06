#' Plot quantile-quantile (QQ) graphs from data.
#'
#' This function takes a data table, X and Y variables, and plots a QQ graph using \code{\link[ggplot2]{ggplot}}.
#'
#' The function uses \code{stat_qq} and \code{stat_qq_line} geometries (get help with \code{?stat_qq_line}).
#' Note that the function requires the quantitative Y variable first, and groups them based on an X variable.
#' The X variable is mapped to the \code{colour} aesthetic in both \code{stat_qq} and \code{stat_qq_line}, and its colour can be changed using \code{scale_colour_brewer} or any \code{scale_colour...} option.
#'
#' @param data a data table e.g. data.frame or tibble.
#' @param ycol name of the column containing the quantitative variable whose distribution is to be plotted
#' @param xcol name of the column containing a categorical variable
#' @param symsize size of symbols, default set to 3
#' @param symthick thickness of symbol border, default set to 1
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param alpha fractional transparency, default set to 1 (i.e. zero transparency, fully opaque)
#'
#' @return This function returns a \code{ggplot2} object.
#' @export plot_qqline
#' @import ggplot2
#'
#' @examples
#' #Basic usage
#' plot_qqline(data_cholesterol, Cholesterol, Treatment)
#'

plot_qqline <- function(data, ycol, xcol, symsize = 3, symthick = 1, fontsize = 20, alpha = 1){
  ggplot2::ggplot(data, aes(sample = {{ ycol }}))+
    stat_qq(geom = "point", na.rm = T, shape = 21,
            size = {{ symsize }}, stroke = {{ symthick }},
            alpha = {{ alpha }},
            aes(fill = {{ xcol }}) )+
    stat_qq_line(aes(colour = {{ xcol }}),
                 na.rm = T,
                 size = 1)+
    theme_classic(base_size = {{ fontsize }})
}
