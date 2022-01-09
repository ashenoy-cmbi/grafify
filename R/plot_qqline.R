#' Plot quantile-quantile (QQ) graphs from data.
#'
#' This function takes a data table, a quantitative variable (`ycol`), and a categorical grouping variable (`group`), if available, and plots a QQ graph using \code{\link[ggplot2]{ggplot}}.
#'
#' The function uses \code{stat_qq} and \code{stat_qq_line} geometries (get help with \code{?stat_qq_line}).
#' Note that the function requires the quantitative Y variable first, and can be passed on a grouping variable as `group` if required.
#' The graph plots sample quantiles on Y axis & theoretical quantiles on X axis.
#' The X variable is mapped to the \code{fill} aesthetic in\code{stat_qq} and  \code{colour} aesthetic for the \code{stat_qq_line}.
#' Colours can be changed using `ColPal`, `ColRev` or `ColSeq` arguments. 
#' `ColPal` can be one of the following: "okabe_ito", "dark", "light", "bright", "pale", "vibrant,  "muted" or "contrast".
#' `ColRev` (logical TRUE/FALSE) decides whether colours are chosen from first-to-last or last-to-first from within the chosen palette. 
#' `ColSeq` decides whether colours are picked by respecting the order in the palette or the most distant ones using \code{\link[grDevices]{colorRampPalette}}.
#'
#' @param data a data table e.g. data.frame or tibble.
#' @param ycol name of the column containing the quantitative variable whose distribution is to be plotted
#' @param group name of the column containing a categorical grouping variable
#' @param symsize size of symbols, default set to 3
#' @param symthick thickness of symbol border, default set to 1
#' @param s_alpha fractional opacity of symbols, default set to 1 (i.e. maximum opacity & zero transparency)
#' @param ColPal grafify colour palette to apply, default "all_grafify"; alternatives: "okabe_ito", "bright", "pale", "vibrant", "contrast", "muted" "dark", "light".
#' @param ColSeq logical TRUE or FALSE. Default TRUE for sequential colours from chosen palette. Set to FALSE for distant colours, which will be applied using  \code{scale_fill_grafify2}.
#' @param ColRev whether to reverse order of colour choice, default F (FALSE); can be set to T (TRUE)
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#'
#' @return This function returns a \code{ggplot2} object.
#' @export plot_qqline
#' @import ggplot2
#'
#' @examples
#' #Basic usage
#' plot_qqline(data = data_cholesterol, 
#' ycol = Cholesterol, group = Treatment)
#'

plot_qqline <- function(data, ycol, group, symsize = 3, symthick = 1, s_alpha = 1, ColPal = "all_grafify", ColSeq = TRUE, ColRev = FALSE, TextXAngle = 0, fontsize = 20){
  P <- ggplot2::ggplot(data, aes(sample = {{ ycol }}))+
    stat_qq_line(aes(colour = {{ group }}),
                 na.rm = T,
                 size = 1)+
    stat_qq(geom = "point", na.rm = T, 
            shape = 21,
            size = {{ symsize }}, 
            stroke = {{ symthick }},
            alpha = {{ s_alpha }},
            aes(fill = {{ group }}) )+
    labs(fill = enquo(group),
         colour = enquo(group))+
    theme_classic(base_size = {{ fontsize }})+
    theme(strip.background = element_blank())+
    guides(x = guide_axis(angle = {{ TextXAngle }}))
  if (ColSeq) {
    P <- P + scale_fill_grafify(palette = {{ ColPal }}, reverse = {{ ColRev }}) + scale_colour_grafify(palette = {{ ColPal }}, reverse = {{ ColRev }})
  } else {
    P <- P + scale_fill_grafify2(palette = {{ ColPal }}, reverse = {{ ColRev }})} + scale_colour_grafify2(palette = {{ ColPal }}, reverse = {{ ColRev }})
  P
}
