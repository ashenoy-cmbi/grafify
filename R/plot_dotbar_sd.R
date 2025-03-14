#' Plot a dotplot on a bar graph with SD error bars with two variables.
#'
#' There are three types of `plot_dot_` functions that plot data as "dots" using the \code{\link[ggplot2]{geom_dotplot}} geometry. They all take a data table, a categorical X variable and a numeric Y variable. 
#' 1. \link{plot_dotbar_sd} (bar & SD, SEM or CI95 error bars)
#' 2. \link{plot_dotbox} (box & whiskers)
#' 3. \link{plot_dotviolin} (box & whiskers, violin)
#' 
#' Related `plot_scatter_` variants show data symbols using the \code{\link[ggplot2]{geom_point}} geometry. These are \link{plot_scatterbar_sd} (or SEM or CI95 error bars), \link{plot_scatterbox} and \link{plot_scatterviolin}. Over plotting in `plot_scatter` variants can be reduced with the `jitter` argument.
#' 
#' The X variable is mapped to the \code{fill} aesthetic of dots, symbols, bars, boxes and violins.
#' 
#' Colours can be changed using `ColPal`, `ColRev` or `ColSeq` arguments. Colours available can be seen quickly with \code{\link{plot_grafify_palette}}. 
#' `ColPal` can be one of the following: "okabe_ito", "dark", "light", "bright", "pale", "vibrant,  "muted" or "contrast".
#' `ColRev` (logical TRUE/FALSE) decides whether colours are chosen from first-to-last or last-to-first from within the chosen palette. 
#' `ColSeq` decides whether colours are picked by respecting the order in the palette or the most distant ones using \code{\link[grDevices]{colorRampPalette}}.
#' 
#' If you prefer a single colour for the graph, use the `SingleColour` argument.
#' 
#' @param data a data table object, e.g. data.frame or tibble.
#' @param xcol name of the column (without quotes) to plot on X axis. This should be a categorical variable.
#' @param ycol name of the column (without quotes) with the quantitative variable to plot on the Y axis. This should be a quantitative variable.
#' @param facet add another variable (without quotes) from the data table to create faceted graphs using \code{\link[ggplot2]{facet_wrap}}.
#' @param ErrorType select the type of error bars to display. Default is "SD" (standard deviation). Other options are "SEM" (standard error of the mean) and "CI95" (95% confidence interval based on t distributions).
#' @param dotsize size of dots relative to binwidth used by \code{\link[ggplot2]{geom_dotplot}}. Default set to 1.5, increase/decrease as needed.
#' @param d_alpha fractional opacity of dots, default set to 0.8 (i.e., 80% opacity).
#' @param b_alpha fractional opacity of bars, default set to 1.
#' @param bwid width of bars; default 0.5.
#' @param ewid width of error bars, default set to 0.2.
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text.
#' @param LogYTrans transform Y axis into "log10" or "log2" (in quotes).
#' @param LogYBreaks argument for \code{\link[ggplot2]{scale_y_continuous}} for Y axis breaks on log scales, default is `waiver()`, or provide a vector of desired breaks.
#' @param LogYLabels argument for \code{\link[ggplot2]{scale_y_continuous}} for Y axis labels on log scales, default is `waiver()`, or provide a vector of desired labels. 
#' @param LogYLimits a vector of length two specifying the range (minimum and maximum) of the Y axis.
#' @param facet_scales whether or not to fix scales on X & Y axes for all facet facet graphs. Can be `fixed` (default), `free`, `free_y` or `free_x` (for Y and X axis one at a time, respectively).
#' @param fontsize parameter of \code{base_size} of fonts in \code{\link[ggplot2]{theme_classic}}, default set to size 20.
#' @param dotthick thickness of dot border (`stroke` parameter of \code{\link[ggplot2]{geom_dotplot}}), default set to `fontsize`/22.
#' @param bthick thickness (in 'pt' units) of bar and error bar lines; default = `fontsize`/22.
#' @param ColPal grafify colour palette to apply (in quotes), default "okabe_ito"; see \code{\link{graf_palettes}} for available palettes.
#' @param ColSeq logical TRUE or FALSE. Default TRUE for sequential colours from chosen palette. Set to FALSE for distant colours, which will be applied using  \code{scale_fill_grafify2}.
#' @param ColRev whether to reverse order of colour within the selected palette, default F (FALSE); can be set to T (TRUE).
#' @param SingleColour a colour hexcode (starting with #, e.g., "#E69F00"), a number between 1-154, or names of colours from `grafify` or base R palettes to fill along X-axis aesthetic. Accepts any colour other than "black"; use `grey_lin11`, which is almost black.
#' @param ... any additional arguments to pass to \code{\link[ggplot2]{geom_dotplot}}.
#'
#' @return This function returns a \code{ggplot2} object of class "gg" and "ggplot".
#' @export plot_dotbar_sd
#' @import ggplot2 Hmisc
#'
#' @examples
#' plot_dotbar_sd(data = data_cholesterol, 
#' xcol = Treatment,
#' ycol = Cholesterol)
#'
#' plot_dotbar_sd(data = data_1w_death, 
#' xcol = Genotype, ycol = Death, 
#' ColPal = "pale", ColSeq = FALSE, ColRev = TRUE)
#' 
#' #single colour along X
#' plot_dotbar_sd(data = data_1w_death, 
#' xcol = Genotype, ycol = Death, 
#' SingleColour = "light_orange")

plot_dotbar_sd <- function(data, xcol, ycol, facet, ErrorType = "SD", dotsize = 1.5, d_alpha = 0.8, b_alpha = 1, bwid = 0.5, ewid = 0.2, TextXAngle = 0, LogYTrans, LogYBreaks = waiver(), LogYLabels = waiver(), LogYLimits = NULL, facet_scales = "fixed", fontsize = 20, dotthick, bthick, ColPal = c("okabe_ito", "all_grafify", "bright",  "contrast",  "dark",  "fishy",  "kelly",  "light",  "muted",  "pale",  "r4",  "safe",  "vibrant"), ColSeq = TRUE, ColRev = FALSE, SingleColour = "NULL", ...){
  ColPal <- match.arg(ColPal)
  if (!(ErrorType %in% c("SD", "SEM", "CI95"))) {
    stop('ErrorType should be "SD", "SEM" or "CI95".')}
  if(ErrorType == "SD") {ER <- "mean_sdl"}
  if(ErrorType == "SEM") {ER <- "mean_se"}
  if(ErrorType == "CI95") {ER <- "mean_cl_normal"}
  if (missing(bthick)) {bthick = fontsize/22}
  if (missing(dotthick)) {dotthick = fontsize/22}
  suppressWarnings(P <- ggplot2::ggplot(data, aes(x = factor({{ xcol }}),
                                                  y = {{ ycol }}))+
                     stat_summary(geom = "bar", 
                                  colour = "black", 
                                  width = bwid,
                                  fun = "mean", 
                                  alpha = b_alpha, 
                                  linewidth = bthick,
                                  aes(fill = factor({{ xcol }})))+
                     geom_dotplot(dotsize = dotsize, 
                                  stroke = dotthick,
                                  binaxis = 'y', 
                                  alpha = d_alpha,
                                  stackdir = 'center',
                                  aes(fill = factor({{ xcol }})),
                                  ...))
  if (ER == "mean_cl_normal") {
    P <- P + stat_summary(geom = "errorbar", 
                          linewidth = bthick,
                          fun.data = "mean_cl_normal",
                          width = ewid)
  } else {
    P <- P + stat_summary(geom = "errorbar", 
                          linewidth = bthick,
                          fun.data = ER,
                          fun.args = list(mult = 1),
                          width = ewid) 
  }
  #add facets
  if(!missing(facet)) {
    P <- P + facet_wrap(vars({{ facet }}), 
                        scales = facet_scales, 
                        ...)
  }
  #add logs
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
  #add single colour
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
                                     times = x))
  } else {
    P <- P +
      scale_fill_grafify(palette = ColPal, 
                         reverse = ColRev, 
                         ColSeq = ColSeq)
  }
  P <- P  +
    labs(x = enquo(xcol),
         fill = enquo(xcol))+
    theme_grafify(base_size = fontsize)+
    guides(x = guide_axis(angle = TextXAngle))
  P
}
