#' Plot a before-after plot with lines joining colour-matched symbols.
#'
#' The \code{\link{plot_befafter_colours}} and \code{\link{plot_befafter_shapes}} are for graphing matched data joined by lines. 
#' They take X and Y variables along with a grouping factor (e.g. matched subjects or experiments etc.) and plot symbols matched by colour or shape.
#'
#' Note that \code{ggplot} only allows 25 types of shapes, and \code{\link{plot_befafter_shapes}} function works when there are fewer than 25 matched observations.
#' Use \code{scale_colour_brewer} or \code{scale_colour_viridis} or related to get a spectrum of matched colours by the grouping factor.
#'
#' Consider using \code{facet_wrap} or \code{facet_grid} to reduce crowding on a single plot and/or plot additional dimensions.
#'
#' @param data a data table object, e.g. data.frame or tibble.
#' @param xcol name of the column containing the categorical variable to be plotted on the X axis.
#' @param ycol name of the column containing the quantitative Y values.
#' @param groups name of the column with the grouping variable to pass on to \code{geom_line}.
#' @param symsize size of symbols, default set to 3
#' @param symthick thickness of symbol border, default set to 1
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param alpha fractional opacity of symbols, default set to 1 (i.e. maximum opacity & zero transparency)
#' @param ColPal grafify colour palette to apply, default "all_grafify"; alternatives: "okabe_ito", "bright", "pale", "vibrant", "contrast", "muted" "dark", "light".
#' @param ColRev whether to reverse order of colour choice, default F (FALSE); can be set to T (TRUE)
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text
#'
#' @return This function returns a \code{ggplot2} object.
#' @export plot_befafter_colors
#' @import ggplot2
#'
#' @examples
#' #Basic usage with Treatment as the X variable
#' #Subject as the grouping variable 
#' #this variable lists points to join by lines
#' plot_befafter_colors(data_cholesterol, Treatment, Cholesterol, Subject)
#'


plot_befafter_colors <- function(data, xcol, ycol, groups, symsize = 3, symthick = 1, fontsize = 20, alpha = 1, ColPal = "all_grafify", ColRev = F, TextXAngle = 0){
  ggplot2::ggplot({{ data }}, aes(x = factor({{ xcol }}),
                                  y = {{ ycol }},
                                  group = factor({{ groups }})))+
    geom_line(aes(group = factor({{ groups }})),
              colour = "grey35", alpha = 0.8)+
    geom_point(size = {{ symsize }}, stroke = {{ symthick }},
               alpha = {{ alpha }}, 
               shape = 21,
               aes(fill = factor({{ groups }})))+
    labs(x = enquo(xcol),
         fill = enquo(groups))+
    theme_classic(base_size = {{ fontsize }})+
    theme(strip.background = element_blank())+
    guides(x = guide_axis(angle = {{ TextXAngle }}))+
    scale_fill_grafify(palette = {{ ColPal }}, 
                       reverse = {{ ColRev }})
}

