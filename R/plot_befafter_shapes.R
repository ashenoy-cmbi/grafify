#' Plot a before-after plot with lines joining shape-matched symbols.
#'
#' The \code{\link{plot_befafter_colours}} and \code{\link{plot_befafter_shapes}} are for graphing matched data joined by lines. 
#' They take X and Y variables along with a data column with matching information (e.g. matched subjects or experiments etc.) and plot symbols matched by colour or shape.
#'
#' Note that \code{ggplot} only allows 25 types of shapes, and \code{\link{plot_befafter_shapes}} function works when there are fewer than 25 matched observations.
#' Colours can be changed using `ColPal`, `ColRev` or `ColSeq` arguments. 
#' `ColPal` can be one of the following: "okabe_ito", "dark", "light", "bright", "pale", "vibrant,  "muted" or "contrast".
#' `ColRev` (logical TRUE/FALSE) decides whether colours are chosen from first-to-last or last-to-first from within the chosen palette. 
#' `ColSeq` decides whether colours are picked by respecting the order in the palette or the most distant ones using \code{\link[grDevices]{colorRampPalette}}.
#'
#' Consider using \code{facet_wrap} or \code{facet_grid} to reduce crowding on a single plot and/or plot additional dimensions.
#'
#' @param data a data table object, e.g. data.frame or tibble.
#' @param xcol name of the column containing the categorical variable to be plotted on the X axis.
#' @param ycol name of the column containing the quantitative Y values.
#' @param match name of the column with the matching variable to pass on to \code{geom_line}.
#' @param symsize size of symbols, default set to 3
#' @param symthick size of outline of symbol lines (\code{stroke = 1}), default set to 1
#' @param s_alpha fractional opacity of symbols, default set to 0.8 (i.e. 80% opacity)
#' @param ColPal grafify colour palette to apply, default "all_grafify"; alternatives: "okabe_ito", "bright", "pale", "vibrant", "contrast", "muted" "dark", "light".
#' @param ColSeq logical TRUE or FALSE. Default TRUE for sequential colours from chosen palette. Set to FALSE for distant colours, which will be applied using  \code{scale_fill_grafify2}.
#' @param ColRev whether to reverse order of colour choice, default F (FALSE); can be set to T (TRUE)
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param groups old argument name for `match`; retained for backward compatibility.
#' @param ... any additional arguments to pass to \code{ggplot2}[geom_line].
#'
#' @return This function returns a \code{ggplot2} object.
#' @export plot_befafter_shapes
#' @import ggplot2
#'
#' @examples
#' #Basic usage with Treatment as the X variable
#' #Subject as the matching variable 
#' plot_befafter_shapes(data = data_cholesterol, 
#' xcol = Treatment, ycol = Cholesterol, 
#' match = Subject)
#' plot_befafter_shapes(data = data_t_pdiff, 
#' xcol = Condition, ycol = Mass, 
#' match = Subject, s_alpha = .9, ColSeq = FALSE)+
#' guides(shape = "none", 
#' colour = "none") #remove guides
#'

plot_befafter_shapes <- function(data, xcol, ycol, match, symsize = 3, symthick = 1, s_alpha = 0.8, ColPal = "all_grafify", ColSeq = TRUE, ColRev = FALSE, TextXAngle = 0, fontsize = 20, groups, ...){
  if (!missing("groups")) {
    warning("Use `match` argument instead, as `groups` is deprecated.")
    match <- substitute(groups)}
  P <- ggplot2::ggplot(data, aes(x = factor({{ xcol }}),
                            y = {{ ycol }},
                            group = factor({{ match }})))+
    geom_line(aes(group = factor({{ match }})),
              colour = "grey35", alpha = 0.8, 
              ...)+
    geom_point(alpha = {{ s_alpha }}, 
               stroke = {{ symthick }},
               size = {{ symsize }},
               aes(colour = factor({{ xcol }}),
                   shape = factor({{ match }})))+
    scale_shape_manual(values = 0:25)+
    labs(x = enquo(xcol),
         colour = enquo(xcol),
         shape = enquo(match))+
    theme_classic(base_size = {{ fontsize }})+
    theme(strip.background = element_blank())+
    guides(x = guide_axis(angle = {{ TextXAngle }}))
  if (ColSeq) {
    P <- P + scale_colour_grafify(palette = {{ ColPal }}, reverse = {{ ColRev }})
  } else {
    P <- P + scale_colour_grafify2(palette = {{ ColPal }}, reverse = {{ ColRev }})}
  P
}
