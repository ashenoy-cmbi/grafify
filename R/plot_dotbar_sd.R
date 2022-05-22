#' Plot a dotplot on a bar graph with SD error bars with two variables.
#'
#' This function takes a data table, X and Y variables, and plots a graph with a dotplot and bars using \code{\link[ggplot2]{stat_summary}} with \code{geom = "bar"}, and \code{\link[ggplot2]{geom_dotplot}} geometries. Standard deviation (SD) is plotted through \code{\link[ggplot2]{stat_summary}} calculated using \code{mean_sdl} from the \code{ggplot2} package (get help with \code{?mean_sdl}), and 1x SD is plotted (\code{fun.arg = list(mult = 1)}.
#' 
#' The X variable is mapped to the \code{fill} aesthetic in both bar and dotplot.
#' 
#' Colours can be changed using `ColPal`, `ColRev` or `ColSeq` arguments. Colours available can be seen quickly with \code{\link{plot_grafify_palette}}.
#' `ColPal` can be one of the following: "okabe_ito", "dark", "light", "bright", "pale", "vibrant,  "muted" or "contrast".
#' `ColRev` (logical TRUE/FALSE) decides whether colours are chosen from first-to-last or last-to-first from within the chosen palette. 
#' `ColSeq` decides whether colours are picked by respecting the order in the palette or the most distant ones using \code{\link[grDevices]{colorRampPalette}}.
#'  The size of dots can be adjusted using the parameter, which is  \code{dotsize = 1} by default.
#'
#' This function is related to \code{\link{plot_scatterbar_sd}}, \code{\link{plot_dotbar_sd}} and \code{\link{plot_dotviolin}}.
#'
#' @param data a data table object, e.g. data.frame or tibble.
#' @param xcol name of the column to plot on X axis. This should be a categorical variable.
#' @param ycol name of the column to plot on quantitative Y axis. This should be a quantitative variable.
#' @param dotsize size of dots relative to binwidth used by \code{\link[ggplot2]{geom_dotplot}}. Default set to 1.5, increase/decrease as needed.
#' @param dotthick thickness of dot border (`stroke` parameter of `geom_dotplot`), default set to 1.
#' @param bwid width of bars, default set to 0.7
#' @param ewid width of error bars, default set to 0.2.
#' @param b_alpha fractional opacity of bars, default set to 1 (i.e. maximum opacity & zero transparency).
#' @param d_alpha fractional opacity of dots, default set to 1 (i.e. maximum opacity & zero transparency).
#' @param ColPal grafify colour palette to apply, default "okabe_ito"; see \code{\link{graf_palettes}} for available palettes..
#' @param ColSeq logical TRUE or FALSE. Default TRUE for sequential colours from chosen palette. Set to FALSE for distant colours, which will be applied using  \code{scale_fill_grafify2}.
#' @param ColRev whether to reverse order of colour within the selected palette, default F (FALSE); can be set to T (TRUE).
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text.
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param ... any additional arguments to pass to \code{ggplot2}[geom_dotplot].
#'
#' @return This function returns a \code{ggplot2} object of class "gg" and "ggplot".
#' @export plot_dotbar_sd
#' @import ggplot2 Hmisc
#'
#' @examples
#' plot_dotbar_sd(data = data_cholesterol, 
#' xcol = Treatment,
#' ycol = Cholesterol)
#'
#' plot_dotbar_sd(data = data_1w_death, 
#' xcol = Genotype, ycol = Death, 
#' ColPal = "pale", ColSeq = FALSE, ColRev = TRUE)

plot_dotbar_sd <- function(data, xcol, ycol, dotsize = 1.5, dotthick = 1, bwid = 0.7, ewid = 0.2, b_alpha = 1, d_alpha = 1, ColPal = c("okabe_ito", "all_grafify", "bright",  "contrast",  "dark",  "fishy",  "kelly",  "light",  "muted",  "pale",  "r4",  "safe",  "vibrant"), ColRev = FALSE, ColSeq = TRUE, TextXAngle = 0, fontsize = 20, ...){
  ColPal <- match.arg(ColPal)
  P <- ggplot2::ggplot(data, aes(x = factor({{ xcol }}),
                            y = {{ ycol }}))+
    stat_summary(geom = "bar", colour = "black", 
                 width = {{ bwid }},
                 fun = "mean", 
                 alpha = {{ b_alpha }}, size = 1,
                 aes(fill = factor({{ xcol }})))+
    geom_dotplot(dotsize = {{ dotsize }}, 
                 stroke = {{ dotthick }},
                 binaxis = 'y', 
                 alpha = {{ d_alpha }},
                 stackdir = 'center',
                 aes(fill = factor({{ xcol }})),
                 ...)+
    stat_summary(geom = "errorbar", size = 1,
                 fun.data = "mean_sdl",
                 fun.args = list(mult = 1),
                 width = {{ ewid }}) +
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
