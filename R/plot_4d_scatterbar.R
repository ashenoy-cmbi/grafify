#' Plot a dot plot with matched shapes on a box plot using four variables.
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
#' @param xcol name of the column with the categorical factor to plot on X axis. If column is numeric, enter as \code{factor(col)}.
#' @param ycol name of the column to plot on quantitative variable on the Y axis.
#' @param bars name of the column containing grouping within the factor plotted on X axis. Can be categorical or numeric X. If your table has numeric X and you want to plot as factor, enter \code{xcol = factor(name of colum)}.
#' @param shapes name of the column that contains matched observations, e.g. subject IDs, experiment ID.
#' @param symsize size of symbols, default set to 3.
#' @param symthick size of outline of symbol lines (\code{stroke = 1.0}), default set to 1.0
#' @param ewid width of error bars, default set to 0.2.
#' @param jitter extent of jitter (scatter) of symbols, default is 0.2. Increase to reduce symbol overlap, set to 0 for aligned symbols.  
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param b_alpha fractional opacity of bars, default set to 1 (i.e. maximum opacity & zero transparency).
#' @param s_alpha fractional opacity of symbols, default set to 1 (i.e. maximum opacity & zero transparency).
#' @param ColSeq logical TRUE or FALSE. Default TRUE for sequential colours from chosen palette. Set to FALSE for distant colours, which will be applied using  \code{scale_fill_grafify2}.
#' @param ColPal grafify colour palette to apply, default "okabe_ito"; see \code{\link{graf_palettes}} for available palettes.
#' @param ColRev whether to reverse order of colour within the selected palette, default F (FALSE); can be set to T (TRUE).
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text.
#' @param ... any additional arguments to pass to \code{ggplot2}[stat_summary] or \code{ggplot2}[geom_point].
#'
#' @return This function returns a \code{ggplot2} object of class "gg" and "ggplot".
#' @export plot_4d_scatterbar
#' @import ggplot2
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

plot_4d_scatterbar <- function(data, xcol, ycol, bars, shapes, symsize = 2.5, symthick = 1.0, jitter = 0.2, ewid = 0.2, fontsize = 20, b_alpha = 1, s_alpha = 1, ColPal = c("okabe_ito", "all_grafify", "bright",  "contrast",  "dark",  "fishy",  "kelly",  "light",  "muted",  "pale",  "r4",  "safe",  "vibrant"), ColRev = FALSE, ColSeq = TRUE, TextXAngle = 0, ...){
  ColPal <- match.arg(ColPal)
  P <- ggplot2::ggplot(data, aes(x = factor({{ xcol }}),
                            y = {{ ycol }},
                            group = interaction(factor({{ bars }}),
                                                factor({{ xcol }}))))+
    stat_summary(geom = "bar", 
                 colour = "black", 
                 width = 0.7, 
                 alpha = {{ b_alpha }}, size = 1,
                 aes(fill = factor({{ bars }})),
                 position = position_dodge(width = 0.8),
                 fun = "mean", ...)+
    geom_point(size = {{ symsize }}, 
               alpha = {{ s_alpha }}, 
               stroke = {{ symthick }}, 
               colour = "black",
               position = position_jitterdodge(jitter.width = {{ jitter }},
                                               dodge.width = 0.8),
               aes(shape = factor({{ shapes }})), ...)+
    stat_summary(geom = "errorbar", colour = "black", size = 1, 
                 width = {{ ewid }},
                 fun.data = "mean_sdl",
                 fun.args = list(mult = 1),
                 position = position_dodge(width = 0.8), ...)+
    scale_shape_manual(values = 0:25)+
    labs(fill = enquo(bars),
         shape = enquo(shapes),
         x = enquo(xcol))+
    theme_classic(base_size = {{ fontsize }})+
    theme(strip.background = element_blank())+
    guides(x = guide_axis(angle = {{ TextXAngle }}))+
    scale_fill_grafify(palette = {{ ColPal }}, 
                       reverse = {{ ColRev }}, 
                       ColSeq = {{ ColSeq }})
  P
}
