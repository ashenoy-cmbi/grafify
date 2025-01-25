#' Plot a point as mean with SD error bars using two variables.
#'
#' There are 4 related functions that use \code{\link[ggplot2]{geom_point}} to plot a categorical variable along the X axis. 
#' 1. \link{plot_point_sd} (mean & SD, SEM or CI95 error bars)
#' 2. \link{plot_scatterbar_sd} (bar & SD, SEM or CI95 error bars)
#' 3. \link{plot_scatterbox} (box & whiskers)
#' 4. \link{plot_scatterviolin} (box & whiskers, violin)
#' 
#' These functions take a data table, categorical X and numeric Y variables, and plot various geometries. The X variable is mapped to the \code{fill} aesthetic of symbols. 
#'
#' In \link{plot_point_sd} and \link{plot_scatterbar_sd}, default error bars are SD, which can be changed to SEM or CI95. 
#' 
#' Colours can be changed using `ColPal`, `ColRev` or `ColSeq` arguments. Colours available can be seen quickly with \code{\link{plot_grafify_palette}}.
#' `ColPal` can be one of the following: "okabe_ito", "dark", "light", "bright", "pale", "vibrant,  "muted" or "contrast".
#' `ColRev` (logical TRUE/FALSE) decides whether colours are chosen from first-to-last or last-to-first from within the chosen palette. 
#' `ColSeq` (logical TRUE/FALSE) decides whether colours are picked by respecting the order in the palette or the most distant ones using \code{\link[grDevices]{colorRampPalette}}.
#' 
#' If there are many groups along the X axis and you prefer a single colour for the graph,use the `SingleColour` argument.
#'
#' @param data a data table object, e.g. data.frame or tibble.
#' @param xcol name of the column with a X variable (will be forced to be a factor/categorical variable).
#' @param ycol name of the column with quantitative Y variable.
#' @param facet add another variable from the data table to create faceted graphs using \code{\link[ggplot2]{facet_wrap}}.
#' @param ErrorType select the type of error bars to display. Default is "SD" (standard deviation). Other options are "SEM" (standard error of the mean) and "CI95" (95% confidence interval based on t distributions).
#' @param symsize size of point symbols, default set to 3.5.
#' @param s_alpha fractional opacity of symbols, default set to 1 (i.e. maximum opacity & zero transparency).
#' @param symshape The mean is shown with symbol of the shape number 22 (default, filled square). Pick a number between 21-25 to pick a different type of symbol from ggplot2.
#' @param all_alpha fractional opacity of all data points (default = 0.3). Set to non-zero value if you would like all data points plotted in addition to the mean.
#' @param all_size size of symbols of all data points, if shown (default = 2.5).
#' @param all_jitter reduce overlap of all data points, if shown, by setting a value between 0-1 (default = 0).
#' @param all_shape all data points are shown with symbols of the shape number 1 (default, transparent circle). Pick a number between 0-25 to pick a different type of symbol from ggplot2.
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text.
#' @param LogYTrans transform Y axis into "log10" or "log2"
#' @param LogYBreaks argument for \code{ggplot2[scale_y_continuous]} for Y axis breaks on log scales, default is `waiver()`, or provide a vector of desired breaks.
#' @param LogYLabels argument for \code{ggplot2[scale_y_continuous]} for Y axis labels on log scales, default is `waiver()`, or provide a vector of desired labels. 
#' @param LogYLimits a vector of length two specifying the range (minimum and maximum) of the Y axis.
#' @param facet_scales whether or not to fix scales on X & Y axes for all facet facet graphs. Can be `fixed` (default), `free`, `free_y` or `free_x` (for Y and X axis one at a time, respectively).
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param symthick thickness of symbol border, default set to `fontsize`/22.
#' @param ewid width of error bars, default set to 0.2.
#' @param ethick thickness of error bar lines; default `fontsize`/22.
#' @param ColPal grafify colour palette to apply, default "okabe_ito"; see \code{\link{graf_palettes}} for available palettes.
#' @param ColSeq logical TRUE or FALSE. Default TRUE for sequential colours from chosen palette. Set to FALSE for distant colours, which will be applied using  \code{scale_fill_grafify2}.
#' @param ColRev whether to reverse order of colour within the selected palette, default F (FALSE); can be set to T (TRUE).
#' @param SingleColour a colour hexcode (starting with #), a number between 1-154, or names of colours from `grafify` or base R palettes to fill along X-axis aesthetic. Accepts any colour other than "black"; use `grey_lin11`, which is almost black.
#' @param ... any additional arguments to pass to \code{ggplot2}[stat_summary].
#'
#' @return This function returns a \code{ggplot2} object of class "gg" and "ggplot".
#' @export plot_point_sd
#' @import ggplot2 Hmisc
#'
#' @examples
#' #Basic usage
#' plot_point_sd(data = data_doubling_time, 
#' xcol = Student, ycol = Doubling_time)
#' 
plot_point_sd <- function(data, xcol, ycol, facet, ErrorType = "SD", symsize = 3.5, s_alpha = 1, symshape = 22, all_alpha = 0.3, all_size = 2.5, all_shape = 1, all_jitter = 0, ewid = 0.2, TextXAngle = 0, LogYTrans, LogYBreaks = waiver(), LogYLabels = waiver(), LogYLimits = NULL, facet_scales = "fixed", fontsize = 20, symthick, ethick, ColPal = c("okabe_ito", "all_grafify", "bright",  "contrast",  "dark",  "fishy",  "kelly",  "light",  "muted",  "pale",  "r4",  "safe",  "vibrant"), ColSeq = TRUE, ColRev = FALSE, SingleColour = "NULL", ...){
  ColPal <- match.arg(ColPal)
  if (!(ErrorType %in% c("SD", "SEM", "CI95"))) {
    stop('ErrorType should be "SD", "SEM" or "CI95".')}
  if(ErrorType == "SD") {ER <- "mean_sdl"}
  if(ErrorType == "SEM") {ER <- "mean_se"}
  if(ErrorType == "CI95") {ER <- "mean_cl_normal"}
  if (missing(ethick)) {ethick = fontsize/22}
  if (missing(symthick)) {symthick = fontsize/22}
  if(symshape < 21 | symshape > 25){
    stop("`symshape` should be betwee 21-25.")}
  if (ER == "mean_cl_normal") {
    suppressWarnings(P <- ggplot2::ggplot(data, aes(x = factor({{ xcol }}),
                                                    y = {{ ycol }}))+
                       geom_point(aes(fill = factor({{ xcol }})),
                                  shape = all_shape, 
                                  alpha = all_alpha,
                                  size = all_size,
                                  position = position_jitter(width = all_jitter))+
                       stat_summary(geom = "errorbar",
                                    fun.data = "mean_cl_normal", 
                                    size = ethick,
                                    width = ewid, ...)+
                       stat_summary(geom = "point", 
                                    shape = symshape,
                                    size = symsize, 
                                    stroke = symthick,
                                    alpha = s_alpha,
                                    fun = "mean",
                                    aes(fill = factor({{ xcol }})), 
                                    ...))
  } else {
    suppressWarnings(P <- ggplot2::ggplot(data, aes(x = factor({{ xcol }}),
                                                    y = {{ ycol }}))+
                       geom_point(aes(fill = factor({{ xcol }})),
                                  shape = all_shape, 
                                  alpha = all_alpha,
                                  size = all_size,
                                  position = position_jitter(width = all_jitter))+
                       stat_summary(geom = "errorbar",
                                    fun.data = ER, 
                                    size = ethick,
                                    fun.args = list(mult = 1),
                                    width = ewid, ...)+
                       stat_summary(geom = "point", 
                                    shape = symshape,
                                    size = symsize, 
                                    stroke = symthick,
                                    alpha = s_alpha,
                                    fun = "mean",
                                    aes(fill = factor({{ xcol }})), 
                                    ...))
  }
  P <- P + 
    labs(x = enquo(xcol),
         fill = enquo(xcol))+
    theme_grafify(base_size = fontsize)+
    guides(x = guide_axis(angle = TextXAngle))
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
  if (!missing(SingleColour)) {
    ifelse(grepl("#", SingleColour), 
           a <- SingleColour,
           ifelse(isTRUE(get_graf_colours(SingleColour) != 0), 
                  a <- unname(get_graf_colours(SingleColour)), 
                  a <- SingleColour))
    xcol <- deparse(substitute(xcol))
    x <- length(levels(factor(data[[xcol]])))
    P <- P +
      scale_fill_manual(values = rep(a, 
                                     times = x))+
      guides(fill = "none")
  } else {
    P <- P +
      scale_fill_grafify(palette = ColPal, 
                         reverse = ColRev, 
                         ColSeq = ColSeq)
  }
  P
}
