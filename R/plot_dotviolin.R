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
#' @param dotsize size of dots relative to \code{binwidth} used by \code{geom_dotplot}. Default set to 1.5, increase/decrease as needed.
#' @param dotthick thickness of dot border (`stroke` parameter of `geom_dotplot`), default set to 1
#' @param trim set whether tips of violin plot should be trimmed at high/low data. Default \code{trim = T}, can be changed to F.
#' @param scale set to "area" by default, can be changed to "count" or "width".
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param v_alpha fractional opacity of violins, default set to 1 (i.e. maximum opacity & zero transparency)
#' @param d_alpha fractional opacity of dots, default set to 1 (i.e. maximum opacity & zero transparency)
#' @param ColPal grafify colour palette to apply, default "all_grafify"; alternatives: "okabe_ito", "bright", "pale", "vibrant", "contrast", "muted" "dark", "light".
#' @param ColRev whether to reverse order of colour choice, default F (FALSE); can be set to T (TRUE)
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text
#'
#' @return This function returns a \code{ggplot2} object on which additional geometries etc. can be added.
#' @export plot_dotviolin
#' @import ggplot2
#'
#' @examples
#'
#' #plot with trim = F
#' plot_dotviolin(data_t_pdiff, Condition, Mass, dotsize = 2)
#' 
#' #without trimming
#' plot_dotviolin(data_t_pdiff, Condition, Mass, dotsize = 2, trim = FALSE)
#'

plot_dotviolin <- function(data, xcol, ycol, dotsize = 1.5, dotthick = 1, trim = T, scale = "area", fontsize = 20, v_alpha = 1, d_alpha = 1, ColPal = "all_grafify", ColRev = F, TextXAngle = 0){
  ggplot2::ggplot(data, aes(x = factor({{ xcol }}),
                            y = {{ ycol }}))+
    geom_violin(aes(fill = factor({{ xcol }})),
                alpha = {{ v_alpha }},
                trim = {{ trim }},
                scale = {{ scale }},
                draw_quantiles = c(0.25, .5, .75),
                colour = "black", size = 1,
                adjust = 0.8)+
    geom_dotplot(stackdir = "center", 
                 stroke = {{ dotthick }}, 
                 alpha = {{ d_alpha }},
                 dotsize = {{ dotsize }},
                 binaxis = 'y',
                 aes(fill = factor({{ xcol }})))+
    labs(x = enquo(xcol),
         fill = enquo(xcol))+
    theme_classic(base_size = {{ fontsize }})+
    theme(strip.background = element_blank())+
    guides(x = guide_axis(angle = {{ TextXAngle }}))+
    scale_fill_grafify(palette = {{ ColPal }}, 
                       reverse = {{ ColRev }})
}
