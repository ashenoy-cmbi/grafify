#' Plot a dot plot with matched shapes on a box plot using four variables.
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
#' @param xcol name of the column with the categorical factor to plot on X axis. If column is numeric, enter as \code{factor(col)}.
#' @param ycol name of the column to plot on quantitative variable on the Y axis.
#' @param boxes name of the column containing grouping within the factor plotted on X axis. Can be categorical or numeric X. If your table has numeric X and you want to plot as factor, enter \code{xcol = factor(name of colum)}.
#' @param shapes name of the column that contains matched observations, e.g. subject IDs, experiment number etc.
#' @param symsize size of symbols, default set to 3
#' @param symthick size of outline of symbol lines (\code{stroke = 1.0}), default set to 1.0
#' @param jitter extent of jitter (scatter) of symbols, default is 0.1. Increase to reduce symbol overlap, set to 0 for aligned symbols.  
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param b_alpha fractional opacity of boxes, default set to 1 (i.e. maximum opacity & zero transparency)
#' @param s_alpha fractional opacity of symbols, default set to 1 (i.e. maximum opacity & zero transparency)
#' @param ColPal grafify colour palette to apply, default "all_grafify"; alternatives: "okabe_ito", "bright", "pale", "vibrant", "contrast", "muted" "dark", "light".
#' @param ColRev whether to reverse order of colour choice, default F (FALSE); can be set to T (TRUE)
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text
#'
#' @return This function returns a \code{ggplot2} object
#' @export plot_4d_scatterbox
#' @import ggplot2
#'
#' @examples
#' #Basic usage can take up to 4 variables from the data table
#' plot_4d_scatterbox(data_2w_Festing, Strain, GST, Treatment, Block)
#'
#' #boxes and shapes can be the same factor if you want matched shapes for them
#' plot_4d_scatterbox(data_doubling_time, Student, Doubling_time, Student, Experiment)
#'

plot_4d_scatterbox <- function(data, xcol, ycol, boxes, shapes, symsize = 2.5, symthick = 1.0, jitter = 0.1, fontsize = 20, b_alpha = 1, s_alpha = 1, ColPal = "all_grafify", ColRev = F, TextXAngle = 0){
  ggplot2::ggplot(data, aes(x = factor({{ xcol }}),
                            y = {{ ycol }},
                            group = interaction(factor({{ boxes }}),
                                                factor({{ xcol }}))))+
    geom_boxplot(width = 0.5, alpha = {{ b_alpha }}, size = 1,
                 aes(fill = factor({{ boxes }})), 
                 outlier.alpha = 0,
                 position = position_dodge(width = 0.8))+
    geom_point(size = {{ symsize }}, 
               alpha = {{ s_alpha }}, 
               stroke = {{ symthick }}, 
               colour = "black",
               position = position_jitterdodge(jitter.width = {{ jitter }},
                                               dodge.width = 0.8),
               aes(shape = factor({{ shapes }})))+
    scale_shape_manual(values = 0:25)+
    labs(shape = enquo(shapes),
         fill = enquo(boxes),
         x = enquo(xcol))+
    theme_classic(base_size = {{ fontsize }})+
    theme(strip.background = element_blank())+
    guides(x = guide_axis(angle = {{ TextXAngle }}))+
    scale_fill_grafify(palette = {{ ColPal }}, 
                       reverse = {{ ColRev }})+
    scale_colour_grafify(palette = {{ ColPal }}, 
                         reverse = {{ ColRev }})
}
