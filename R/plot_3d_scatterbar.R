#' Plot a scatter graph with matched shapes on a bar plot using three variables.
#'
#' The functions \code{\link{plot_3d_scatterbar}}, \code{\link{plot_3d_scatterbox}}, \code{\link{plot_4d_scatterbar}}  and \code{\link{plot_4d_scatterbox}} allow 3d or 4d plots with 3 or 4 variables, respectively.
#' These four functions are useful for factorial ANOVA designs with randomised blocks or repeated-measures or for showing matched observations (e.g. depicting matched subjects or experiments). 
#' They rely on \code{\link[ggplot2]{ggplot}} with \code{\link[ggplot2]{geom_point}} and \code{\link[ggplot2]{geom_bar}} (through \code{stat_summary}) or \code{\link[ggplot2]{geom_boxplot}} geometries.
#'
#' These functions take a data table, X and Y variables, and a one or two more categorical variable for grouping together for boxes or bars, and the shape of symbols. These variables will be automatically converted to categorical variables even if numeric in the data table.
#' In \code{plot_3d_scatterbox} and \code{plot_3d_scatterbar} the third variable is called "shapes" and is mapped to both the box/bar fill colour and the shape of symbols. 
#' In \code{plot_4d_scatterbox} and \code{plot_4d_scatterbar} the third variable is called "boxes" or "bars", respectively, is mapped to fill the colour of these geometries. The fourth variable "shapes" is mapped to the shape of symbols.
#' Shapes are always plotted in black colour, with 80% opacity (to enable overlapping data points to be seen; cannot be changed).
#'
#' Scatter points are depicted using \code{\link[ggplot2]{geom_point}} with \code{position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.05)}, so shapes are jittered and dodged along the X variable.
#' Bars depict means using \code{\link[ggplot2]{stat_summary}} with \code{geom = "bar", fun = "mean"} , and bar width is set to 0.7 and cannot be changed. 
#' Error bar width can be changed with the `ewid` argument.
#' Boxplot geometry uses \code{\link[ggplot2]{geom_boxplot}} with \code{position = position_dodge(width = 0.9), width = 0.6}. The thick line depicts the median, the box the IQR and the whiskers 1.5*IQR.
#' 
#' The X and "shapes" variables are mapped with \code{group = interaction{ xcol, shapes}}. 
#' In the 3d versions, the "shapes" variable is mapped to \code{scale_fill_grafify} aesthetic of bar/boxes, and in 4d versions, it is the "boxes" or "bars" argument that is mapped to the `scale_fill_grafify` call.
#' 
#' Colour palette can be changed using `ColPal` and colours reversed with `ColRev`. 
#' 
#' Since v0.2.2, the scatter plots are jittered with `width = 0.05` to prevent overlapping and for consistency with `plot_3d_scatterbar` and `plot_3d_scatterbox`.
#' The opacity of the boxes/bars can be changed with the `alpha` argument.
#' 
#' In \code{plot_4d_scatterbox}, the third & fourth variables can be the same (e.g. for one-way ANOVA with randomised blocks). Up to 25 levels can be mapped to "shapes".
#'
#' All four functions can be expanded further, for example with \code{\link[ggplot2]{facet_grid}} or \code{\link[ggplot2]{facet_wrap}}.
#'
#' @param data a data table, e.g. data.frame or tibble.
#' @param xcol name of the column with the categorical factor to be plotted on X axis.
#' @param ycol name of the column with quantitative variable to plot on the Y axis.
#' @param shapes name of the column with the second categorical factor, for example from a two-way ANOVA design.
#' @param ewid width of error bars, default set to 0.2
#' @param symsize size of symbols, default set to 3
#' @param symthick size of outline of symbol lines (\code{stroke = 1.5}), default set to 1.5
#' @param jitter extent of jitter (scatter) of symbols, default is 0.1. Increase to reduce symbol overlap, set to 0 for aligned symbols.  
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param b_alpha fractional opacity of bars, default set to 1 (i.e. maximum opacity & zero transparency)
#' @param s_alpha fractional opacity of symbols, default set to 1 (i.e. maximum opacity & zero transparency)
#' @param ColPal grafify colour palette to apply, default "all_grafify"; alternatives: "okabe_ito", "bright", "pale", "vibrant", "contrast", "muted" "dark", "light".
#' @param ColRev whether to reverse order of colour choice, default F (FALSE); can be set to T (TRUE)
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text
#'
#' @return This function returns a \code{ggplot2} object.
#' @export plot_3d_scatterbar
#' @import ggplot2 Hmisc
#'
#' @examples
#' #Basic usage
#' plot_3d_scatterbar(data_cholesterol, Treatment, Cholesterol, Hospital)


plot_3d_scatterbar <- function(data, xcol, ycol, shapes, ewid = 0.2, symsize = 2.5, symthick = 1, jitter = 0.1, fontsize = 20, b_alpha = 1.0, s_alpha = 1, ColPal = "all_grafify", ColRev = F, TextXAngle = 0){
  ggplot2::ggplot(data, aes(x = factor({{ xcol }}),
                            y = {{ ycol }},
                            group = interaction(factor({{ xcol }}),
                                                factor({{ shapes }}))))+
    stat_summary(geom = "bar", width = .7, colour = "black",
                 fun = "mean", size = 1,
                 aes(fill = factor({{ shapes }})),
                 alpha = {{ b_alpha }},
                 position = position_dodge(width = 0.8))+
    geom_point(size = {{ symsize }}, 
               stroke = {{ symthick }},
               alpha = {{ s_alpha }}, 
               colour = "black",
               position = position_jitterdodge(dodge.width = 0.8,
                                               jitter.width = {{ jitter }}),
               aes(shape = factor({{ shapes }})))+
    stat_summary(geom = "errorbar", width = {{ ewid }},
                 fun.data = "mean_sdl", size = 1,
                 fun.args = list(mult = 1),
                 position = position_dodge(width = 0.8))+
    scale_shape_manual(values = 0:25)+
    labs(x = enquo(xcol),
         fill = enquo(shapes),
         shape = enquo(shapes))+
    theme_classic(base_size = {{ fontsize }})+
    theme(strip.background = element_blank())+
    guides(x = guide_axis(angle = {{ TextXAngle }}))+
    scale_fill_grafify(palette = {{ ColPal }}, 
                       reverse = {{ ColRev }})
}
