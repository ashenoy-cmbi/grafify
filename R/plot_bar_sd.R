#' Plot a bar graph indicating mean with error bars (SD) using two variables.
#'
#' This function takes a data table, categorical X and numeric Y variables, and plots bars showing the mean with SD error bars. The X variable is mapped to the \code{fill} aesthetic of bars. The related \code{plot_bar_sd_sc} plots bars with a single colour.
#' 
#' The function uses \code{\link[ggplot2]{stat_summary}} with \code{geom = "bar"}. Standard deviation (SD) is plotted through \code{\link[ggplot2]{stat_summary}} calculated using \code{mean_sdl} from the \code{ggplot2} package (get help with \code{?mean_sdl}), and 1x SD is plotted (\code{fun.arg = list(mult = 1)}.
#' 
#' Colours can be changed using `ColPal`, `ColRev` or `ColSeq` arguments. Colours available can be seen quickly with \code{\link{plot_grafify_palette}}.
#' `ColPal` can be one of the following: "okabe_ito", "dark", "light", "bright", "pale", "vibrant,  "muted" or "contrast".
#' `ColRev` (logical TRUE/FALSE) decides whether colours are chosen from first-to-last or last-to-first from within the chosen palette. 
#' `ColSeq` decides whether colours are picked by respecting the order in the palette or the most distant ones using \code{\link[grDevices]{colorRampPalette}}.
#'
#' You are instead encouraged to show all data using the following functions: \code{\link{plot_scatterbar_sd}}, \code{\link{plot_scatterbox}}, \code{\link{plot_dotbox}}, \code{\link{plot_dotbar_sd}}, \code{\link{plot_scatterviolin}} or \code{\link{plot_dotviolin}}.
#' 
#' @param data a data table object, e.g. a data.frame or tibble.
#' @param xcol name of the column to plot on X axis. This should be a categorical variable.
#' @param ycol name of the column to plot on the Y axis. This should be a quantitative variable.
#' @param b_alpha fractional opacity of bars, default set to 1 (i.e. maximum opacity & zero transparency).
#' @param bwid width of bars, default 0.7
#' @param bthick thickness of bar borders; default 1.
#' @param ewid width of error bars, default 0.3
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param ColSeq logical TRUE or FALSE. Default TRUE for sequential colours from chosen palette. Set to FALSE for distant colours, which will be applied using  \code{scale_fill_grafify2}.
#' @param ColPal grafify colour palette to apply, default "okabe_ito"; see \code{\link{graf_palettes}} for available palettes..
#' @param ColRev whether to reverse order of colour within the selected palette, default F (FALSE); can be set to T (TRUE).
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text.
#' @param ... any additional arguments to pass to \code{stat_summary}.
#'
#' @return This function returns a \code{ggplot2} object of class "gg" and "ggplot".
#' @export plot_bar_sd
#' @import ggplot2 Hmisc
#'
#' @examples
#'
#' #Basic usage
#' plot_bar_sd(data = data_doubling_time, 
#' xcol = Student, ycol = Doubling_time)
#' 
#' #apply distant colours in the default palette
#' plot_bar_sd(data = data_doubling_time, 
#' xcol = Student, ycol = Doubling_time, 
#' ColSeq = FALSE)
#'

plot_bar_sd <- function(data, xcol, ycol, b_alpha = 1, bwid = 0.7, bthick = 1, ewid = 0.3, ColPal = c("okabe_ito", "all_grafify", "bright",  "contrast",  "dark",  "fishy",  "kelly",  "light",  "muted",  "pale",  "r4",  "safe",  "vibrant"), ColSeq = TRUE, ColRev = FALSE, TextXAngle = 0, fontsize = 20, ...){
  ColPal <- match.arg(ColPal)
  P <- ggplot2::ggplot(data, aes(x = factor({{ xcol }}),
                                 y = {{ ycol }}))+
    stat_summary(geom = "bar", 
                 width = {{ bwid }}, size = {{ bthick }},
                 fun = "mean", colour = "black",
                 alpha = {{ b_alpha }},
                 aes(fill = factor({{ xcol }})), ...)+
    stat_summary(geom = "errorbar", size = 1,
                 fun.data = "mean_sdl",
                 fun.args = list(mult = 1),
                 width = {{ ewid }})+
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
