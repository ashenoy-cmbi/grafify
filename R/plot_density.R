#' Plot density distribution of data.
#'
#' This function takes a data table, `ycol` of quantitative variable and a categorical grouping variable (`group`), if available, and plots a density graph using \code{\link[ggplot2]{ggplot}}.
#'
#' The function uses \code{\link{geom_density}}).
#' Note that the function requires the quantitative Y variable first, and groups them based on an X variable.
#' The Group variable is mapped to the \code{fill} and \code{colour} aesthetics in \code{geom_density}.
#' Colours can be changed using `ColPal`, `ColRev` or `ColSeq` arguments. 
#' `ColPal` can be one of the following: "okabe_ito", "dark", "light", "bright", "pale", "vibrant,  "muted" or "contrast".
#' `ColRev` (logical TRUE/FALSE) decides whether colours are chosen from first-to-last or last-to-first from within the chosen palette. 
#' `ColSeq` decides whether colours are picked by respecting the order in the palette or the most distant ones using \code{\link[grDevices]{colorRampPalette}}.
#'
#' @param data a data table e.g. data.frame or tibble.
#' @param ycol name of the column containing the quantitative variable whose density distribution is to be plotted
#' @param group name of the column containing a categorical grouping variable
#' @param linethick thickness of symbol border, default set to 1
#' @param c_alpha fractional opacity of filled colours under the curve, default set to 0.2 (i.e. 20% opacity)
#' @param ColPal grafify colour palette to apply, default "all_grafify"; alternatives: "okabe_ito", "bright", "pale", "vibrant", "contrast", "muted" "dark", "light".
#' @param ColSeq logical TRUE or FALSE. Default TRUE for sequential colours from chosen palette. Set to FALSE for distant colours, which will be applied using  \code{scale_fill_grafify2}.
#' @param ColRev whether to reverse order of colour choice, default F (FALSE); can be set to T (TRUE)
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param Group deprecated old argument for `group`; retained for backward compatibility.
#' @param alpha deprecated old argument for `c_alpha`; retained for backward compatibility.
#' @param ... any additional arguments to pass to \code{ggplot2}[geom_density].
#'
#' @return This function returns a \code{ggplot2} object.
#' @export plot_density
#' @import ggplot2
#'
#' @examples
#' plot_density(data = data_t_pratio, 
#' ycol = log(Cytokine), group = Genotype)
#' 
#' #with faceting
#' plot_density(data = data_cholesterol, 
#' ycol = Cholesterol, group = Treatment, 
#' fontsize = 10)+facet_wrap("Treatment")

plot_density <- function(data, ycol, group, linethick = 1, c_alpha = 0.2, ColPal = "all_grafify", ColSeq = TRUE, ColRev = FALSE, TextXAngle = 0, fontsize = 20, Group, alpha, ...){
  if (!missing("Group")) {
    warning("Use `group` argument instead, as `Group` is deprecated.")
    group <- substitute(Group)}
  if (!missing("alpha")) {
    warning("Use `c_alpha` argument instead, as `alpha` is deprecated.")
    c_alpha <- substitute(alpha)}
  P <- ggplot2::ggplot(data, aes(sample = {{ ycol }}))+
    geom_density(size = {{ linethick }},
                 alpha = {{ c_alpha }},
                 aes(x = {{ ycol }},
                     fill = {{ group }}, 
                     colour = {{ group }}),
                 ...)+
    labs(fill = enquo(group),
         colour = enquo(group),
         y = "Density")+
    theme_classic(base_size = {{ fontsize }})+
    theme(strip.background = element_blank())+
    guides(x = guide_axis(angle = {{ TextXAngle }}))
  if (ColSeq) {
    P <- P + scale_fill_grafify(palette = {{ ColPal }}, reverse = {{ ColRev }}) + scale_colour_grafify(palette = {{ ColPal }}, reverse = {{ ColRev }})
  } else {
    P <- P + scale_fill_grafify2(palette = {{ ColPal }}, reverse = {{ ColRev }})} + scale_colour_grafify2(palette = {{ ColPal }}, reverse = {{ ColRev }})
  P
}
