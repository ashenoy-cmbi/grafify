#' Plot density distribution of data.
#'
#' This function takes a data table, `ycol` of quantitative variable and a categorical grouping variable (`group`), if available, and plots a density graph using \code{\link{geom_density}}).
#' 
#' Note that the function requires the quantitative Y variable first, and groups them based on an X variable. The group variable is mapped to the \code{fill} and \code{colour} aesthetics in \code{geom_density}.
#' Colours can be changed using `ColPal`, `ColRev` or `ColSeq` arguments. Colours available can be seen quickly with \code{\link{plot_grafify_palette}}.
#' `ColPal` can be one of the following: "okabe_ito", "dark", "light", "bright", "pale", "vibrant,  "muted" or "contrast".
#' `ColRev` (logical TRUE/FALSE) decides whether colours are chosen from first-to-last or last-to-first from within the chosen palette. 
#' `ColSeq` decides whether colours are picked by respecting the order in the palette or the most distant ones using \code{\link[grDevices]{colorRampPalette}}.
#'
#' @param data a data table e.g. data.frame or tibble.
#' @param ycol name of the column containing the quantitative variable whose density distribution is to be plotted.
#' @param group name of the column containing a categorical grouping variable
#' @param facet add another variable from the data table to create faceted graphs using \code{ggplot2}[facet_wrap].
#' @param c_alpha fractional opacity of filled colours under the curve, default set to 0.2 (i.e. 20% opacity).
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text.
#' @param facet_scales whether orcet graphs not to fix scales on X & Y axes for all facet facet graphs. Can be `fixed` (default), `free`, `free_y` or `free_x` (for Y and X axis one at a time, respectively).
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param linethick thickness of symbol border, default set to `fontsize`/22.
#' @param Group deprecated old argument for `group`; retained for backward compatibility.
#' @param alpha deprecated old argument for `c_alpha`; retained for backward compatibility.
#' @param ColPal grafify colour palette to apply, default "okabe_ito"; see \code{\link{graf_palettes}} for available palettes.
#' @param ColSeq logical TRUE or FALSE. Default TRUE for sequential colours from chosen palette. Set to FALSE for distant colours, which will be applied using  \code{scale_fill_grafify2}.
#' @param ColRev whether to reverse order of colour within the selected palette, default F (FALSE); can be set to T (TRUE).
#' @param ... any additional arguments to pass to \code{ggplot2}[geom_density].
#'
#' @return This function returns a \code{ggplot2} object of class "gg" and "ggplot".
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

plot_density <- function(data, ycol, group, facet,  c_alpha = 0.2, TextXAngle = 0, facet_scales = "fixed", fontsize = 20, linethick, Group, alpha, ColPal = c("okabe_ito", "all_grafify", "bright",  "contrast",  "dark",  "fishy",  "kelly",  "light",  "muted",  "pale",  "r4",  "safe",  "vibrant"), ColSeq = TRUE, ColRev = FALSE, ...){
  if(missing(linethick)) {linethick = fontsize/22}
  if (!missing("Group")) {
    warning("Use `group` argument instead, as `Group` is deprecated.")
    group <- substitute(Group)}
  if (!missing("alpha")) {
    warning("Use `c_alpha` argument instead, as `alpha` is deprecated.")
    c_alpha <- substitute(alpha)}
  ColPal <- match.arg(ColPal)
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
    guides(x = guide_axis(angle = {{ TextXAngle }}))+
    scale_fill_grafify(palette = {{ ColPal }}, 
                       reverse = {{ ColRev }}, 
                       ColSeq = {{ ColSeq }})+
    scale_colour_grafify(palette = {{ ColPal }}, 
                         reverse = {{ ColRev }}, 
                         ColSeq = {{ ColSeq }})
  if(!missing(facet)) {
    P <- P + facet_wrap(vars({{ facet }}), 
                        scales = {{ facet_scales }}, 
                        ...)
  }
  P
}
