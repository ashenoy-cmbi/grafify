#' Plot a scatter graph with matched shapes on a bar plot using three variables.
#'
#' The functions \code{\link{plot_3d_scatterbar}}, \code{\link{plot_3d_scatterbox}}, \code{\link{plot_4d_scatterbar}}  and \code{\link{plot_4d_scatterbox}} are useful for plotting one-way or two-way ANOVA designs with randomised blocks or repeated measures. The blocks or subjects can be mapped to the `shapes` argument in both functions (up to 25 levels can be mapped to `shapes`; there will be an error if this number is exceeded). The 3d versions use the categorical variable (`xcol`) for grouping (e.g. one-way ANOVA designs), and 4d versions take an additional grouping variable (e.g. two-way ANOVA designs) that is passed to either `boxes` or `bars` argument.
#' 
#' These functions rely on \code{\link[ggplot2]{ggplot}} with \code{\link[ggplot2]{geom_point}} and \code{\link[ggplot2]{geom_bar}} (through \code{stat_summary}) or \code{\link[ggplot2]{geom_boxplot}} geometries.
#'
#' Variables other than the quantitative variable (`ycol`) will be automatically converted to categorical variables even if they are numeric in the data table.
#' 
#' Shapes are always plotted in black colour, and their opacity can be changed with the `s_alpha` argument and overlap can be reduced with the `jitter` argument. Other arguments are similar to other plot functions as briefly explained below.
#'
#' Bars depict means using \code{\link[ggplot2]{stat_summary}} with \code{geom = "bar", fun = "mean"} , and bar width is set to 0.7 (cannot be changed). Error bar width can be changed with the `ewid` argument.
#' 
#' Boxplot geometry uses \code{\link[ggplot2]{geom_boxplot}} with \code{position = position_dodge(width = 0.9), width = 0.6}. The thick line within the boxplot depicts the median, the box the IQR (interquantile range) and the whiskers show 1.5*IQR.
#' 
#' In 4d versions, the two grouping variables (i.e. `xcol` and either `boxes` or `bars`) are passed to ggplot aesthetics through \code{group = interaction{ xcol, shapes}}. 
#'  
#' Colours can be changed using `ColPal`, `ColRev` or `ColSeq` arguments. 
#' `ColPal` can be one of the following: "okabe_ito", "dark", "light", "bright", "pale", "vibrant,  "muted" or "contrast".
#' `ColRev` (logical TRUE/FALSE) decides whether colours are chosen from first-to-last or last-to-first from within the chosen palette. 
#' `ColSeq` (logical TRUE/FALSE) decides whether colours are picked by respecting the order in the palette or the most distant ones using \code{\link[grDevices]{colorRampPalette}}.
#' 
#' All four functions can be expanded further, for example with \code{\link[ggplot2]{facet_grid}} or \code{\link[ggplot2]{facet_wrap}}.
#'
#' @param data a data table, e.g. data.frame or tibble.
#' @param xcol name of the column with the categorical factor to be plotted on X axis.
#' @param ycol name of the column with quantitative variable to plot on the Y axis.
#' @param shapes name of the column with the second categorical factor, for example from a two-way ANOVA design.
#' @param ewid width of error bars, default set to 0.2.
#' @param symsize size of symbols, default set to 3.
#' @param symthick size of outline of symbol lines (\code{stroke = 1.5}), default set to 1.5
#' @param jitter extent of jitter (scatter) of symbols, default is 0.1. Increase to reduce symbol overlap, set to 0 for aligned symbols.  
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param b_alpha fractional opacity of bars, default set to 1 (i.e. maximum opacity & zero transparency).
#' @param s_alpha fractional opacity of symbols, default set to 1 (i.e. maximum opacity & zero transparency).
#' @param ColSeq logical TRUE or FALSE. Default TRUE for sequential colours from chosen palette. Set to FALSE for distant colours, which will be applied using  \code{scale_fill_grafify2}.
#' @param ColPal grafify colour palette to apply, default "all_grafify"; alternatives: "okabe_ito", "bright", "pale", "vibrant", "contrast", "muted" "dark", "light".
#' @param ColRev whether to reverse order of colour choice, default F (FALSE); can be set to T (TRUE).
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text.
#'
#' @return This function returns a \code{ggplot2} object of class "gg" and "ggplot".
#' @export plot_3d_scatterbar
#' @import ggplot2 Hmisc
#'
#' @examples
#' #3d version for 1-way data with blocking
#' plot_3d_scatterbox(data = data_1w_death, 
#' xcol = Genotype, ycol = Death, shapes = Experiment)
#' #compare above graph to
#' plot_scatterbox(data = data_1w_death, xcol = Genotype, ycol = Death)
#' 
#' #4d version for 2-way data with blocking
#' plot_4d_scatterbox(data = data_2w_Tdeath, 
#' xcol = Genotype, 
#' ycol = PI, 
#' boxes = Time, 
#' shapes = Experiment)
#' 
#' plot_4d_scatterbar(data = data_2w_Festing, 
#' xcol = Strain, 
#' ycol = GST, 
#' bars = Treatment, 
#' shapes = Block)
#' 

plot_3d_scatterbar <- function(data, xcol, ycol, shapes, ewid = 0.2, symsize = 2.5, symthick = 1, jitter = 0.1, fontsize = 20, b_alpha = 1.0, s_alpha = 1, ColSeq = TRUE, ColPal = "all_grafify", ColRev = FALSE, TextXAngle = 0){
  P <- ggplot2::ggplot(data, aes(x = factor({{ xcol }}),
                            y = {{ ycol }},
                            group = factor({{ xcol }})))+
    stat_summary(geom = "bar", width = .7, colour = "black",
                 fun = "mean", size = 1,
                 aes(fill = factor({{ xcol }})),
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
    guides(x = guide_axis(angle = {{ TextXAngle }}))
    if (ColSeq) {
      P <- P + scale_fill_grafify(palette = {{ ColPal }}, reverse = {{ ColRev }})
    } else {
      P <- P + scale_fill_grafify2(palette = {{ ColPal }}, reverse = {{ ColRev }})}
    P
}
