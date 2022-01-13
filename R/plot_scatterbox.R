#' Plot a scatter plot on a boxplot with two variables.
#'
#' This function takes a data table, X and Y variables, and plots a graph with a scatter plot and box and whiskers using \code{\link[ggplot2]{geom_boxplot}} and \code{\link[ggplot2]{geom_point}} geometries. The boxplot shows IQR and whiskers depict 1.5*IQR.
#' Note that \code{\link{geom_boxplot}} option for outliers is set to \code{outlier.alpha = 0}.
#' The X variable is mapped to the \code{fill} aesthetic in both boxplot and symbols, and its colour can be changed using `ColPal` option. 
#' Colours can be changed using `ColPal`, `ColRev` or `ColSeq` arguments. Colours available can be seen quickly with \code{\link{plot_grafify_palette}}.
#' `ColPal` can be one of the following: "okabe_ito", "dark", "light", "bright", "pale", "vibrant,  "muted" or "contrast".
#' `ColRev` (logical TRUE/FALSE) decides whether colours are chosen from first-to-last or last-to-first from within the chosen palette. 
#' `ColSeq` (logical TRUE/FALSE) decides whether colours are picked by respecting the order in the palette or the most distant ones using \code{\link[grDevices]{colorRampPalette}}.
#' 
#' The size of symbols can be adjusted using \code{symsize} set to 1 by default.
#' Transparency of boxplot and symbols can be set independently with `b_alpha` and `s_alpha`, respectively.
#'
#' Three types of plots are available for scatter/jitter symbols and either bars+SD, boxplot or violin plots: \code{\link{plot_scatterbar_sd}}, \code{\link{plot_scatterbox}} and \code{\link{plot_scatterviolin}}.
#' These are related to the three "dot" versions that use a different geometry for symbols: \code{\link{plot_scatterbox}}, \code{\link{plot_dotbar_sd}} and \code{\link{plot_dotviolin}}.
#'
#' @param data a data table object, e.g. data.frame or tibble.
#' @param xcol name of the column to plot on X axis. This should be a categorical variable.
#' @param ycol name of the column to plot on quantitative Y axis. This should be a quantitative variable.
#' @param symsize size of symbols used by \code{geom_point}. Default set to 2.5, increase/decrease as needed.
#' @param symthick thickness of symbol border (`stroke` parameter of `geom_point`), default set to 1
#' @param jitter extent of jitter (scatter) of symbols, default is 0 (i.e. aligned symbols). To reduce symbol overlap, try 0.1-0.3 or higher.  
#' @param b_alpha fractional opacity of boxplot, default set to 1 (i.e. maximum opacity & zero transparency)
#' @param s_alpha fractional opacity of symbols, default set to 1 (i.e. maximum opacity & zero transparency)
#' @param ColPal grafify colour palette to apply, default "all_grafify"; alternatives: "okabe_ito", "bright", "pale", "vibrant", "contrast", "muted" "dark", "light".
#' @param ColSeq logical TRUE or FALSE. Default TRUE for sequential colours from chosen palette. Set to FALSE for distant colours, which will be applied using  \code{scale_fill_grafify2}.
#' @param ColRev whether to reverse order of colour choice, default F (FALSE); can be set to T (TRUE)
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param ... any additional arguments to pass to \code{ggplot2}[geom_boxplot].
#'
#' @return This function returns a \code{ggplot2} object on which additional geometries etc. can be added.
#' @export plot_scatterbox
#' @import ggplot2
#'
#' @examples
#' plot_scatterbox(data = data_cholesterol, 
#' xcol = Treatment, ycol = Cholesterol)
#' 
#' #with jitter
#' plot_scatterbox(data = data_cholesterol, 
#' xcol = Treatment, ycol = Cholesterol, jitter = 0.1)
#'

plot_scatterbox <- function(data, xcol, ycol, symsize = 2.5, symthick = 1, jitter = 0, b_alpha = 1, s_alpha = 1, ColPal = "all_grafify", ColSeq = TRUE, ColRev = FALSE, TextXAngle = 0, fontsize = 20, ...){
  P <- ggplot2::ggplot(data, aes(x = factor({{ xcol }}),
                            y = {{ ycol }}))+
    geom_boxplot(aes(fill = factor({{ xcol }})), size = 1,
                 alpha = {{ b_alpha }},
                 outlier.alpha = 0,
                 width = 0.7,
                 ...)+
    geom_point(shape = 21,
               position = position_jitter(width = {{ jitter }}),
               alpha = {{ s_alpha }},
               stroke = {{ symthick }},
               size = {{ symsize }},
               aes(fill = factor({{ xcol }})))+
    labs(x = enquo(xcol),
         fill = enquo(xcol))+
    theme_classic(base_size = {{ fontsize }})+
    theme(strip.background = element_blank())+
    guides(x = guide_axis(angle = {{ TextXAngle }}))
  if (ColSeq) {
    P <- P + scale_fill_grafify(palette = {{ ColPal }}, reverse = {{ ColRev }})
  } else {
    P <- P + scale_fill_grafify2(palette = {{ ColPal }}, reverse = {{ ColRev }})}
  P
}
