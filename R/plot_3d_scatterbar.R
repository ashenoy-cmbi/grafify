#' Plot a scatter graph with matched shapes on a bar plot using three variables.
#'
#' The functions \code{\link{plot_3d_scatterbar}}, \code{\link{plot_3d_scatterbox}} and \code{\link{plot_4d_scatterbox}} allow 3d or 4d plots with 3 or 4 variables, respectively.
#'
#' These functions take a data table, X and Y variables, and a grouping variable "shapes" in the case of\code{plot_3d_scatterbox} and \code{plot_3d_scatterbar}, or two additional variables "boxes" and "dots" in \code{plot_4d_scatterbox}. All three functions plot scatter plots with matched shapes (e.g. when you want to depict matched subjects or experiments). The grouping variable is useful to plot two-way factorial data or when there are more dimensions in the data table. These functions call \code{\link[ggplot2]{ggplot}} with \code{\link[ggplot2]{geom_point}} and \code{\link[ggplot2]{geom_bar}} (actually through \code{stat_summary}) or \code{\link[ggplot2]{geom_boxplot}} geometries.
#'
#' Bars depict means using \code{\link[ggplot2]{stat_summary}} with \code{geom = "bar", fun = "mean"} , and scatter points are depicted using \code{\link[ggplot2]{geom_point}} with \code{position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.05)}, so dots are jittered and dodged along the X variable.
#' The X and "shapes" variables are mapped with \code{group = interaction{ xcol, shapes}}. The xcol variable is mapped to the \code{fill} aesthetic of bar graph and \code{colour} aesthetic of scatterplot.
#' Fill and colour can be changed using \code{scale_fill_brewer} (or related) and \code{scale_colour_brewer} (or related) options. The "shapes" argument is plotted to the shape of the \code{geom_point}.
#' The "shapes" variable is mapped to the aesthetic \code{shape} of the scatterplot \code{\link{geom_point}} geometry.
#'
#' Boxplot geometry uses \code{\link[ggplot2]{geom_boxplot}} with \code{position = position_dodge(width = 0.9), width = 0.6}.
#'
#' In \code{plot_4d_scatterbox}, the third variable is mapped to the boxplot and 4th to shapes. Both variables could be entered as the same as well. Up to 25 levels can be mapped to "shapes".
#'
#' All three functions can be expanded further, for example with \code{\link[ggplot2]{facet_grid}} or \code{\link[ggplot2]{facet_wrap}}.
#'
#' @param data a data table, e.g. data.frame or tibble.
#' @param xcol name of the column with the categorical factor to be plotted on X axis.
#' @param ycol name of the column with quantitative variable to plot on the Y axis.
#' @param shapes name of the column with the second categorical factor, for example from a two-way ANOVA design.
#'
#' @return This function returns a \code{ggplot2} object.
#' @export plot_3d_scatterbar
#'
#' @examples
#' #Basic usage
#' plot_3d_scatterbar(Chol, Treatment, Cholesterol, Hospital)
#'
#' #Additional layers can be used for same or additional variables
#' plot_3d_scatterbar(Chol, Treatment, Cholesterol, Hospital)+
#'    facet_wrap("Hospital")
#'


plot_3d_scatterbar <- function(data, xcol, ycol, shapes){
  ggplot2::ggplot(data, aes(x = factor({{ xcol }}),
                            y = {{ ycol }},
                            group = interaction(factor({{ xcol }}),
                                                factor({{ shapes }}))))+
    stat_summary(geom = "bar", width = .7, colour = "black",
                 fun = "mean",
                 aes(fill = {{ xcol }}),
                 alpha = 0.2,
                 position = position_dodge(width = 0.8))+
    geom_point(size = 2.5, stroke = 1.5,
               alpha = 0.9,
               position = position_jitterdodge(dodge.width = 0.8,
                                               jitter.width = 0.05),
               aes(colour = {{ xcol }},
                   shape = {{ shapes }}))+
    stat_summary(geom = "errorbar", width = 0.15,
                 fun.data = "mean_sdl",
                 fun.args = list(mult = 1),
                 position = position_dodge(width = 0.8))+
    scale_shape_manual(values = 0:25)+
    labs(x = enquo(xcol))+
    theme_classic(base_size = 14)
}
