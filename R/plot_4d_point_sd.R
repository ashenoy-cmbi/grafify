#' Plot mean & error bars for 2-way ANOVAs with or without a blocking factor.
#'
#' There are 4 related functions for 2-way ANOVA type plots. In addition to a categorical variable along the X-axis, a grouping factor is passed to either `points`, `bars` or `boxes` argument in these functions. A blocking factor (or any other categorical variable) can be optionally passed to the `shapes` argument.
#' 1. \code{\link{plot_4d_point_sd}} (mean & SD, SEM or CI95 error bars)
#' 2. \code{\link{plot_4d_scatterbar}} (bar & SD, SEM or CI95 error bars)
#' 3. \code{\link{plot_4d_scatterbox}} (box & whiskers)
#' 4. \code{\link{plot_4d_scatterviolin}} (box & whiskers, violin)
#' 
#' 
#' These can be especially useful when the fourth variable `shapes` is a random factor or blocking factor (up to 25 levels are allowed; there will be an error with more levels). The `shapes` argument can be left blank to plot ordinary 2-way ANOVAs without blocking. 
#' 
#' In `plot_4d_point_sd` and `plot_4d_scatterbar`, the default error bar is SD (can be changed to SEM or CI95). In `plot_4d_point_sd`, a large coloured symbol is plotted at the mean, all other data are shown as smaller symbols. Boxplot uses \code{\link[ggplot2]{geom_boxplot}} to depict median (thicker line), box (interquartile range (IQR)) and the whiskers (1.5*IQR).
#'  
#' Colours can be changed using `ColPal`, `ColRev` or `ColSeq` arguments. 
#' `ColPal` can be one of the following: "okabe_ito", "dark", "light", "bright", "pale", "vibrant,  "muted" or "contrast".
#' `ColRev` (logical TRUE/FALSE) decides whether colours are chosen from first-to-last or last-to-first from within the chosen palette. 
#' `ColSeq` (logical TRUE/FALSE) decides whether colours are picked by respecting the order in the palette or the most distant ones using \code{\link[grDevices]{colorRampPalette}}.
#' 
#' The resulting `ggplot2` graph can take additional geometries or other layers. 
#'
#' @param data a data table, e.g. data.frame or tibble.
#' @param xcol name of the column (without quotes) with the variable to plot on X axis (will be converted to a factor/categorical variable).
#' @param ycol name of the column (without quotes) with the quantitative variable to plot on the Y axis.
#' @param points name of the column with grouping within the factor plotted on X-axis (will be converted to a factor/categorical variable).
#' @param shapes name of the column (without quotes) that contains matched observations (e.g. subject IDs, experiment number) or another variable to pass on to symbol shapes (will be converted to a factor/categorical variable). If not provided, the shapes for all groups is the same, and can be changed with `all_shapes`, `all_alpha`, `all_size` etc.
#' @param facet add another variable (without quotes) from the data table to create faceted graphs using \code{\link[ggplot2]{facet_wrap}}.
#' @param ErrorType select the type of error bars to display. Default is "SD" (standard deviation). Other options are "SEM" (standard error of the mean) and "CI95" (95% confidence interval based on t distributions).
#' @param symsize size of symbols, default set to 3.5.
#' @param s_alpha fractional opacity of symbols, default set to 1 (i.e. fully opaque).
#' @param all_alpha fractional opacity of all data points (default = 0.3). 
#' @param all_size size of symbols of all data points, if shown (default = 2.5).
#' @param all_jitter reduce overlap of all data points, if shown, by setting a value between 0-1 (default = 0).
#' @param all_shape all data points are shown with symbols of the shape number 0 (default, open square). Pick a number between 0-25 to pick a different type of symbol from ggplot2. This argument only has an effect if `shapes` argument is used.
#' @param ewid width of error bars, default set to 0.2.
#' @param ethick thickness of error bar lines; default `fontsize`/22.
#' @param group_wid space between the factors along X-axis, i.e., dodge width. Default `group_wid = 0.8` (range 0-1), which can be set to 0 if you'd like the two plotted as `position = position_identity()`.
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text.
#' @param LogYTrans transform Y axis into "log10" or "log2" (in quotes).
#' @param LogYBreaks argument for \code{\link[ggplot2]{scale_y_continuous}} for Y axis breaks on log scales, default is `waiver()`, or provide a vector of desired breaks.
#' @param LogYLabels argument for \code{\link[ggplot2]{scale_y_continuous}} for Y axis labels on log scales, default is `waiver()`, or provide a vector of desired labels. 
#' @param LogYLimits a vector of length two specifying the range (minimum and maximum) of the Y axis.
#' @param facet_scales whether or not to fix scales on X & Y axes for all facet facet graphs. Can be `fixed` (default), `free`, `free_y` or `free_x` (for Y and X axis one at a time, respectively).
#' @param symshape The mean is shown with symbol of the shape number 21 (default, filled circle). Pick a number between 0-25 to pick a different type of symbol from ggplot2.  
#' @param fontsize parameter of \code{base_size} of fonts in \code{\link[ggplot2]{theme_classic}}, default set to size 20.
#' @param symthick size (in 'pt' units) of outline of symbol lines (\code{stroke}), default = `fontsize`/22.
#' @param ColPal grafify colour palette to apply (in quotes), default "okabe_ito"; see \code{\link{graf_palettes}} for available palettes.
#' @param ColRev whether to reverse order of colour within the selected palette, default F (FALSE); can be set to T (TRUE).
#' @param ColSeq logical TRUE or FALSE. Default TRUE for sequential colours from chosen palette. Set to FALSE for distant colours, which will be applied using  \code{scale_fill_grafify2}.
#' @param ... any additional arguments to pass to \code{\link[ggplot2]{stat_summary}} or \code{\link[ggplot2]{geom_point}}.
#'
#' @return This function returns a \code{ggplot2} object of class "gg" and "ggplot".
#' @export plot_4d_point_sd
#' @import ggplot2
#'
#' @examples
#' #4d version for 2-way data with blocking
#' plot_4d_point_sd(data = data_2w_Tdeath, 
#' xcol = Genotype, 
#' ycol = PI, 
#' points = Time, 
#' shapes = Experiment)
#' 
#' #4d version without blocking factor
#' #`shapes` can be left blank
#' plot_4d_point_sd(data = data_2w_Festing, 
#' xcol = Strain, 
#' ycol = GST, 
#' points = Treatment)
#'
plot_4d_point_sd <- function(data, xcol, ycol, points, shapes, facet, ErrorType = "SD", symsize = 3.5, s_alpha = 1, symshape = 22, all_alpha = 0.3, all_size = 2.5, all_shape = 0, all_jitter = 0, ewid = 0.2, group_wid = 0.8, TextXAngle = 0, LogYTrans, LogYBreaks = waiver(), LogYLabels = waiver(), LogYLimits = NULL, facet_scales = "fixed", fontsize = 20, symthick, ethick, ColPal = c("okabe_ito", "all_grafify", "bright",  "contrast",  "dark",  "fishy",  "kelly",  "light",  "muted",  "pale",  "r4",  "safe",  "vibrant"), ColSeq = TRUE, ColRev = FALSE, ...){
  if(symshape < 21 | symshape > 25){
    stop("`symshape` should be between 21-25.")}
  ColPal <- match.arg(ColPal)
  if (!(ErrorType %in% c("SD", "SEM", "CI95"))) {
    stop('ErrorType should be "SD", "SEM" or "CI95".')}
  if(ErrorType == "SD") {ER <- "mean_sdl"}
  if(ErrorType == "SEM") {ER <- "mean_se"}
  if(ErrorType == "CI95") {ER <- "mean_cl_normal"}
  if (missing(ethick)) {ethick = fontsize/22}
  if (missing(symthick)) {symthick = fontsize/22}
  if (missing(shapes)){
    suppressWarnings(P <- ggplot2::ggplot(data, aes(x = factor({{ xcol }}),
                                                    y = {{ ycol }},
                                                    group = interaction(factor({{ points }}), 
                                                                        factor({{ xcol }}))))+
                       geom_point(aes(fill = factor({{ points }})),
                                  shape = all_shape, 
                                  alpha = all_alpha,
                                  size = all_size,
                                  position = position_jitterdodge(jitter.width = all_jitter,
                                                                  dodge.width = group_wid)))
  } else {
    suppressWarnings(P <- ggplot2::ggplot(data, aes(x = factor({{ xcol }}),
                                                    y = {{ ycol }},
                                                    group = interaction(factor({{ points }}), 
                                                                        factor({{ xcol }}))))+
                       geom_point(aes(fill = factor({{ points }}),
                                      shape = factor({{ shapes }})),
                                  alpha = all_alpha,
                                  size = all_size,
                                  position = position_jitterdodge(jitter.width = all_jitter,
                                                                  dodge.width = group_wid))+
                       scale_shape_manual(values = 0:25))
  }
  if (ER == "mean_cl_normal") {
    suppressWarnings(P <- P +
                       stat_summary(geom = "errorbar",
                                    fun.data = "mean_cl_normal", 
                                    size = ethick,
                                    width = ewid, 
                                    position = position_dodge(width = group_wid),
                                    ...)+
                       stat_summary(geom = "point", 
                                    shape = symshape,
                                    size = symsize, 
                                    stroke = symthick,
                                    alpha = s_alpha,
                                    fun = "mean",
                                    position = position_dodge(width = group_wid),
                                    aes(fill = factor({{ points }})), 
                                    ...))
  } else {
    suppressWarnings(P <- P +
                       stat_summary(geom = "errorbar",
                                    fun.data = ER, 
                                    size = ethick,
                                    fun.args = list(mult = 1),
                                    width = ewid, 
                                    position = position_dodge(width = group_wid),
                                    ...)+
                       stat_summary(geom = "point", 
                                    shape = symshape,
                                    size = symsize, 
                                    stroke = symthick,
                                    alpha = s_alpha,
                                    fun = "mean",
                                    position = position_dodge(width = group_wid),
                                    aes(fill = factor({{ points }})), 
                                    ...))
  }
  if(!missing(facet)) {
    P <- P + facet_wrap(vars({{ facet }}), 
                        scales = facet_scales,
                        ...)
  }
  if (!missing(LogYTrans)) {
    if (!(LogYTrans %in% c("log2", "log10"))) {
      stop("LogYTrans only allows 'log2' or 'log10' transformation.")
    }
    if (LogYTrans == "log10") {
      P <- P + 
        scale_y_continuous(trans = "log10", 
                           breaks = LogYBreaks, 
                           labels = LogYLabels, 
                           limits = LogYLimits, 
                           ...)+
        annotation_logticks(sides = "l", 
                            outside = TRUE,
                            base = 10, color = "grey20",
                            long = unit(7*fontsize/22, "pt"), size = unit(fontsize/22, "pt"),# 
                            short = unit(4*fontsize/22, "pt"), mid = unit(4*fontsize/22, "pt"),#
                            ...)+ 
        coord_cartesian(clip = "off", ...)
    }
    if (LogYTrans == "log2") {
      P <- P + 
        scale_y_continuous(trans = "log2", 
                           breaks = LogYBreaks, 
                           labels = LogYLabels, 
                           limits = LogYLimits, 
                           ...)}
  }
  P <- P + 
    labs(x = enquo(xcol),
         fill = enquo(points),
         shape = enquo(shapes))+
    theme_grafify(base_size = fontsize)+
    guides(x = guide_axis(angle = TextXAngle),
           fill = guide_legend(order = 1),
           shape = guide_legend(order = 2))+
    scale_fill_grafify(palette = ColPal,
                       ColSeq = ColSeq,
                       reverse = ColRev)
  P
}
