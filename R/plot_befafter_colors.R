#' Plot a before-after plot with lines joining colour-matched symbols.
#'
#' The \code{\link{plot_befafter_colours}} and \code{\link{plot_befafter_shapes}} are for graphing matched data joined by lines. They take X and Y variables along with a grouping factor (e.g. matched subjects or experiments etc.) and plot symbols matched by colour or shape.
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
#' @param alpha fractional transparency of symbols, default set to 1 (i.e. zero transparency, fully opaque)
#'
#' @return This function returns a \code{ggplot2} object.
#' @export plot_befafter_colors
#'
#' @examples
#' #Basic usage with Treatment as the X variable with Subject as the grouping variable that indicates which points to join by lines
#' plot_befafter_colours(Chol, Treatment, Cholesterol, Subject)
#'
#' #Additional layers are possible
#' plot_befafter_colours(Chol, Treatment, Cholesterol, Subject)+
#'    labs(title = "Plot with scatter plot, bars (mean) & SD")+
#'    scale_color_viridis_d()+scale_fill_viridis_d()+
#'    facet_wrap("Hospital")


plot_befafter_colors <- function(data, xcol, ycol, groups, symsize = 3, symthick = 1, fontsize = 20, alpha = 1){
  ggplot2::ggplot({{ data }}, aes(x = factor({{ xcol }}),
                                  y = {{ ycol }},
                                  group = factor({{ groups }})))+
    geom_line(aes(group = factor({{ groups }})),
              colour = "grey35", alpha = 0.8)+
    geom_point(size = {{ symsize }}, stroke = {{ symthick }},
               alpha = {{ alpha }}, shape = 21,
               aes(fill = factor({{ groups }})))+
    scale_shape_manual(values = 0:25)+
    labs(x = enquo(xcol),
         fill = enquo(groups))+
    theme_classic(base_size = {{ fontsize }})
}

