#' Plot scatter dots on a bar graph with SD error bars with two variables.
#'
#' This function takes a data table, X and Y variables, and plots a graph with a jitterplot or scatterplot and bars using \code{\link[ggplot2]{ggplot}}.
#'
#' The function uses \code{\link[ggplot2]{stat_summary}} with \code{geom = "bar"}, and \code{\link[ggplot2]{geom_point}} with \code{position = position_jitter(width = 0.05)}.
#' Standard deviation (SD) is plotted through \code{\link[ggplot2]{stat_summary}} calculated using \code{mean_sdl} from the \code{ggplot2} package (get help with \code{?mean_sdl}), and 1x SD is plotted (\code{fun.arg = list(mult = 1)}.
#' The X variable is mapped to the \code{fill} aesthetic in the bar geometry and \code{colour} aesthetic in \code{geom_point}. Fill and colour can be changed using \code{scale_fill_brewer} (or related) and \code{scale_colour_brewer} (or related) options.
#'
#' Three types of plots are available for scatter/jitter symbols and either bars+SD, boxplot or violin plots: \code{\link{plot_scatterbar_sd}}, \code{\link{plot_scatterbox}} and \code{\link{plot_scatterviolin}}.
#' These are related to the three "dot" versions that use a different geometry for symbols: \code{\link{plot_dotbox}}, \code{\link{plot_dotbar_sd}} and \code{\link{plot_dotviolin}}.
#'
#' @param data a data table object, e.g. data.frame or tibble.
#' @param xcol name of the column to plot on X axis. This should be a categorical variable.
#' @param ycol name of the column to plot on quantitative Y axis. This should be a quantitative variable.
#' @param symsize size of point symbols, default set to 2
#' @param symthick thickness of symbol border, default set to 1
#' @param bwid width of bars, default set to 0.7
#' @param ewid width of error bars, default set to 0.3
#' @param jitter extent of jitter (scatter) of symbols, default is 0 (i.e. aligned symbols). To reduce symbol overlap, try 0.1-0.3 or higher.  
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param b_alpha fractional opacity of bars, default set to 1 (i.e. maximum opacity & zero transparency)
#' @param s_alpha fractional opacity of symbols, default set to 1 (i.e. maximum opacity & zero transparency)
#' @param ColPal grafify colour palette to apply, default "all_grafify"; alternatives: "okabe_ito", "bright", "pale", "vibrant", "contrast", "muted" "dark", "light".
#' @param ColRev whether to reverse order of colour choice, default F (FALSE); can be set to T (TRUE)
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text
#'
#' @return This function returns a \code{ggplot2} object on which additional geometries etc. can be added.
#' @export plot_scatterbar_sd
#' @import ggplot2 Hmisc
#'
#' @examples
#'
#' #Basic usage requires a data table and X & Y variables#'
#' plot_scatterbar_sd(data_cholesterol, Treatment, Cholesterol)
#' 
#' #with jitter
#' plot_scatterbar_sd(data_cholesterol, Treatment, Cholesterol, jitter = 0.1)

plot_scatterbar_sd <- function(data, xcol, ycol, symsize = 2.5, symthick = 1, bwid = 0.7, ewid = 0.3, jitter = 0, fontsize = 20, b_alpha = 1, s_alpha = 1, ColPal = "all_grafify", ColRev = F, TextXAngle = 0){
  ggplot2::ggplot(data, aes(x = factor({{ xcol }}),
                            y = {{ ycol }}))+
    stat_summary(geom = "bar", colour = "black", 
                 width = {{ bwid }},
                 fun = "mean", 
                 alpha = {{ b_alpha }}, size = 1,
                 aes(fill = factor({{ xcol }})))+
    geom_point(size = {{ symsize }}, 
               alpha = {{ s_alpha }}, shape = 21,
               position = position_jitter(width = {{ jitter }}), 
               stroke = {{ symthick }},
               aes(fill = factor({{ xcol }})))+
    stat_summary(geom = "errorbar", size = 1,
                 fun.data = "mean_sdl",
                 fun.args = list(mult = 1),
                 width = {{ ewid }} )+
    labs(x = enquo(xcol),
         fill = enquo(xcol))+
    theme_classic(base_size = {{ fontsize }})+
    theme(strip.background = element_blank())+
    guides(x = guide_axis(angle = {{ TextXAngle }}))+
    scale_fill_grafify(palette = {{ ColPal }}, 
                       reverse = {{ ColRev }})
}
