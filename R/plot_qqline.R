#' Plot quantile-quantile (QQ) graphs from data.
#'
#' This function takes a data table, X and Y variables, and plots a QQ graph using \code{\link[ggplot2]{ggplot}}.
#'
#' The function uses \code{stat_qq} and \code{stat_qq_line} geometries (get help with \code{?stat_qq_line}).
#' Note that the function requires the quantitative Y variable first, and can be passed on a grouping variable as `xcol` if required.
#' The graph plots sample quantiles on Y axis & theoretical quantiles on X axis.
#' The X variable is mapped to the \code{fill} aesthetic in\code{stat_qq} and  \code{colour} aesthetic for the \code{stat_qq_line}.
#' ColPal & ColRev options are applied to both `fill` and `colour` scales.
#'
#' @param data a data table e.g. data.frame or tibble.
#' @param ycol name of the column containing the quantitative variable whose distribution is to be plotted
#' @param xcol name of the column containing a categorical variable
#' @param symsize size of symbols, default set to 3
#' @param symthick thickness of symbol border, default set to 1
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param s_alpha fractional opacity of symbols, default set to 1 (i.e. maximum opacity & zero transparency)
#' @param ColPal grafify colour palette to apply, default "all_grafify"; alternatives: "okabe_ito", "bright", "pale", "vibrant", "contrast", "muted" "dark", "light".
#' @param ColRev whether to reverse order of colour choice, default F (FALSE); can be set to T (TRUE)
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text
#'
#' @return This function returns a \code{ggplot2} object.
#' @export plot_qqline
#' @import ggplot2
#'
#' @examples
#' #Basic usage
#' plot_qqline(data_cholesterol, Cholesterol, Treatment)
#'

plot_qqline <- function(data, ycol, xcol, symsize = 3, symthick = 1, fontsize = 20, s_alpha = 1, ColPal = "all_grafify", ColRev = F, TextXAngle = 0){
  ggplot2::ggplot(data, aes(sample = {{ ycol }}))+
    stat_qq_line(aes(colour = {{ xcol }}),
                 na.rm = T,
                 size = 1)+
    stat_qq(geom = "point", na.rm = T, 
            shape = 21,
            size = {{ symsize }}, 
            stroke = {{ symthick }},
            alpha = {{ s_alpha }},
            aes(fill = {{ xcol }}) )+
    labs(fill = enquo(xcol),
         colour = enquo(xcol))+
    theme_classic(base_size = {{ fontsize }})+
    theme(strip.background = element_blank())+
    guides(x = guide_axis(angle = {{ TextXAngle }}))+
    scale_fill_grafify(palette = {{ ColPal }},
                       reverse = {{ ColRev }})+
    scale_colour_grafify(palette = {{ ColPal }}, 
                         reverse = {{ ColRev }})
}
