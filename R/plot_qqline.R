#' Plot quantile-quantile (QQ) graphs from data.
#'
#' This function takes a data table, a quantitative variable (`ycol`), and a categorical grouping variable (`group`), if available, and plots a QQ graph using \code{\link{ggplot2}[stat_qq]} and \code{\link{ggplot2}[stat_qq_line]}.  Alternatives are \code{\link{plot_histogram}}, or \code{\link{plot_qqline}}.
#' 
#' Note that the function requires the quantitative Y variable first, and a grouping variable as `group` if required. The graph plots sample quantiles on Y axis & theoretical quantiles on X axis. The X variable is mapped to the \code{fill} aesthetic in\code{stat_qq} and \code{colour} aesthetic for the \code{stat_qq_line}.
#' 
#' Colours can be changed using `ColPal`, `ColRev` or `ColSeq` arguments. Colours available can be seen quickly with \code{\link{plot_grafify_palette}}. When only one level is present within `group`, symbols will receive "ok_orange" colour. 
#' `ColPal` can be one of the following: "okabe_ito", "dark", "light", "bright", "pale", "vibrant,  "muted" or "contrast".
#' `ColRev` (logical TRUE/FALSE) decides whether colours are chosen from first-to-last or last-to-first from within the chosen palette. 
#' `ColSeq` decides whether colours are picked by respecting the order in the palette or the most distant ones using \code{\link[grDevices]{colorRampPalette}}.
#'
#' @param data a data table e.g. data.frame or tibble.
#' @param ycol name of the column containing the quantitative variable whose distribution is to be plotted.
#' @param group name of the column containing a categorical grouping variable.
#' @param facet add another variable from the data table to create faceted graphs using \code{\link[ggplot2]{facet_wrap}}.
#' @param symsize size of symbols, default set to 3.
#' @param s_alpha fractional opacity of symbols, default set to 1 (i.e. maximum opacity & zero transparency).
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text.
#' @param facet_scales whether or not to fix scales on X & Y axes for all facet facet graphs. Can be `fixed` (default), `free`, `free_y` or `free_x` (for Y and X axis one at a time, respectively).
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param symthick thickness of symbol border, default set to `fontsize`/22.
#' @param linethick thickness of lines, default set to `fontsize`/22.
#' @param ColPal grafify colour palette to apply, default "okabe_ito"; see \code{\link{graf_palettes}} for available palettes.
#' @param ColSeq logical TRUE or FALSE. Default TRUE for sequential colours from chosen palette. Set to FALSE for distant colours, which will be applied using  \code{scale_fill_grafify2}.
#' @param ColRev whether to reverse order of colour within the selected palette, default F (FALSE); can be set to T (TRUE).
#' @param ... any additional arguments to pass to \code{\link{ggplot2}[geom_qq]} or \code{\link{ggplot2}[geom_qq_line]}.
#'
#' @return This function returns a \code{ggplot2} object of class "gg" and "ggplot".
#' @export plot_qqline
#' @import ggplot2
#'
#' @examples
#' plot_qqline(data = data_cholesterol, 
#' ycol = Cholesterol, group = Treatment)
#'
plot_qqline <- function(data, ycol, group, facet, symsize = 3, s_alpha = 0.8, TextXAngle = 0, facet_scales = "fixed", fontsize = 20, symthick, linethick, ColPal = c("okabe_ito", "all_grafify", "bright",  "contrast",  "dark",  "fishy",  "kelly",  "light",  "muted",  "pale",  "r4",  "safe",  "vibrant"), ColSeq = TRUE, ColRev = FALSE, ...){
  ColPal <- match.arg(ColPal)
  if (missing(symthick)) {symthick = fontsize/22}
  if (missing(linethick)) {linethick = fontsize/22}
  if(missing(group)){
    suppressWarnings(P <- ggplot2::ggplot(data, 
                         aes(sample = {{ ycol }}))+
      stat_qq_line(na.rm = T,
                   linewidth = linethick)+
      stat_qq(na.rm = T, 
              shape = 21, 
              fill = "#E69F00",
              size = symsize, 
              stroke = symthick,
              alpha = s_alpha)+
      theme_grafify(base_size = fontsize)+
      theme(strip.background = element_blank())+
      guides(x = guide_axis(angle = TextXAngle)))
  } else {
    suppressWarnings(P <- ggplot2::ggplot(data, 
                         aes(sample = {{ ycol }},
                             group = factor({{ group }})))+
      stat_qq_line(na.rm = T,
                   aes(group = factor({{ group }})),
                   linewidth = linethick)+
      stat_qq(na.rm = T, 
              shape = 21, 
              aes(fill = factor({{ group }})),
              size = symsize, 
              stroke = symthick,
              alpha = s_alpha)+
      theme_grafify(base_size = fontsize)+
      guides(x = guide_axis(angle = TextXAngle)))
    }
  P <- P+
    scale_fill_grafify(palette = ColPal, 
                       reverse = ColRev, 
                       ColSeq = ColSeq)
  if(!missing(facet)) {
    P <- P + facet_wrap(vars({{ facet }}), 
                        scales = facet_scales, 
                        ...)
  }
  P <- P +
    labs(fill = enquo(group))
  P
}
