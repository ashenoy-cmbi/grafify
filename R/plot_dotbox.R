#' Plot a dotplot on a boxplot with two variables.
#'
#' This function takes a data table, X and Y variables, and plots a graph with a dotplot and boxplot using \code{\link[ggplot2]{geom_boxplot}} and \code{\link[ggplot2]{geom_dotplot}} geometries. Note that \code{\link{geom_boxplot}} option for outliers is set to \code{outlier.alpha = 0}.
#' 
#' The X variable is mapped to the \code{fill} aesthetic in both boxplot and dotplot.
#' Colours can be changed using `ColPal`, `ColRev` or `ColSeq` arguments. Colours available can be seen quickly with \code{\link{plot_grafify_palette}}.
#' `ColPal` can be one of the following: "okabe_ito", "dark", "light", "bright", "pale", "vibrant,  "muted" or "contrast".
#' `ColRev` (logical TRUE/FALSE) decides whether colours are chosen from first-to-last or last-to-first from within the chosen palette. 
#' `ColSeq` decides whether colours are picked by respecting the order in the palette or the most distant ones using \code{\link[grDevices]{colorRampPalette}}.
#' 
#' The size of dots can be adjusted using the parameter, which is \code{dotsize = 1} by default.
#'
#' This function is related to \code{\link{plot_scatterbar_sd}}, \code{\link{plot_dotbar_sd}} and \code{\link{plot_dotviolin}}.
#'
#' @param data a data table object, e.g. data.frame or tibble.
#' @param xcol name of the column to plot on X axis. This should be a categorical variable.
#' @param ycol name of the column to plot on quantitative Y axis. This should be a quantitative variable.
#' @param dotsize size of dots relative to binwidth used by \code{geom_dotplot}. Default set to 1.5, increase/decrease as needed.
#' @param dotthick thickness of dot border (`stroke` parameter of `geom_dotplot`), default set to 1
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param b_alpha fractional opacity of boxes, default set to 1 (i.e. maximum opacity & zero transparency)
#' @param d_alpha fractional opacity of dots, default set to 1 (i.e. maximum opacity & zero transparency)
#' @param ColPal grafify colour palette to apply, default "all_grafify"; alternatives: "okabe_ito", "bright", "pale", "vibrant", "contrast", "muted" "dark", "light".
#' @param ColRev whether to reverse order of colour choice, default F (FALSE); can be set to T (TRUE)
#' @param ColSeq logical TRUE or FALSE. Default TRUE for sequential colours from chosen palette. Set to FALSE for distant colours, which will be applied using  \code{scale_fill_grafify2}.
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text
#' @param ... any additional arguments to pass to \code{ggplot2}[geom_boxplot] or \code{ggplot2}[geom_dotplot].
#'
#' @return This function returns a \code{ggplot2} object on which additional geometries etc. can be added.
#' @export plot_dotbox
#' @import ggplot2
#'
#' @examples
#' plot_dotbox(data = data_1w_death, 
#' xcol = Genotype, ycol = Death)
#' 
#' plot_dotbox(data = data_1w_death, 
#' xcol = Genotype, ycol = Death, 
#' ColPal = "vibrant", b_alpha = 0.5)

plot_dotbox <- function(data, xcol, ycol, dotsize = 1.5, dotthick = 1, b_alpha = 1, d_alpha = 1, ColPal = "all_grafify", ColRev = FALSE, ColSeq = TRUE, TextXAngle = 0, fontsize = 20,  ...){
  suppressWarnings(P <- ggplot2::ggplot(data, aes(x = factor({{ xcol }}),
                            y = {{ ycol }}))+
    geom_boxplot(aes(fill = factor({{ xcol }})), size = 1,
                 alpha = {{ b_alpha }},
                 outlier.alpha = 0,
                 width = 0.7,
                 ...)+
    geom_dotplot(stackdir = "center", 
                 stroke = {{ dotthick }},
                 alpha = {{ d_alpha }},
                 binaxis = 'y', 
                 dotsize = {{ dotsize }},
                 aes(fill = factor({{ xcol }})),
                 ...)+
    labs(x = enquo(xcol),
         fill = enquo(xcol))+
    theme_classic(base_size = {{ fontsize }})+
    theme(strip.background = element_blank())+
    guides(x = guide_axis(angle = {{ TextXAngle }})))
  if (ColSeq) {
    P <- P + scale_fill_grafify(palette = {{ ColPal }}, reverse = {{ ColRev }})
  } else {
    P <- P + scale_fill_grafify2(palette = {{ ColPal }}, reverse = {{ ColRev }})}
  P
}
