#' Plot points on a quantitative X - Y plot & a grouping variable.
#'
#' This function takes a data table, quantitative X and Y variables along with a categorical grouping variable, and a and plots a graph with using \code{\link[ggplot2]{geom_point}}. The categorical `CatGroup` variable is mapped to the \code{fill} aesthetic of symbols.
#' 
#' Data summary can be shown with mean and error bars (SD, SEM or CI95) or box and whisker plot. A line joining the central value (mean or median, respectively) will also appear. The line can be removed by setting its opacity to 0 (`l_alpha = 0`). The smooth line type can be changed to linear or loess fit with the `SmoothType` argument, which can take the following options: `none`, `Linear` or `Loess`. Both options are fitted using \code{\link[ggplot2]{stat_smooth}} with options `lm` or `loess`, respectively.
#' 
#' Data summary options can be accessed with the `ErrorType` argument (default is `none`, with data scatter shown). The following options exist for this argument: `SD`, `SEM`, `CI95`, `Boxplot`, to additionally data summaries. The mean value will appear as a larger square symbol. Its opacity and size can be adjusted with `m_alpha` and `mean_size`, respectively.
#' 
#' Colours can be changed using `ColPal`, `ColRev` or `ColSeq` arguments. Colours available can be seen quickly with \code{\link{plot_grafify_palette}}.
#' `ColPal` can be one of the following: "okabe_ito", "dark", "light", "bright", "pale", "vibrant,  "muted" or "contrast".
#' `ColRev` (logical TRUE/FALSE) decides whether colours are chosen from first-to-last or last-to-first from within the chosen palette. 
#' `ColSeq` (logical TRUE/FALSE) decides whether colours are picked by respecting the order in the palette or the most distant ones using \code{\link[grDevices]{colorRampPalette}}.
#' 
#' This plot is related to  \code{\link{plot_xy_CatGroup}}  and \code{\link{plot_xy_NumGroup}}, and might eventually replace them in future updates.
#'
#' @param data A data frame containing the variables to be plotted.
#' @param xcol A column name in `data` for the x-axis (typically a factor or grouping variable).
#' @param ycol A column name in `data` for the y-axis (numeric).
#' @param Group A grouping variable used for color/fill aesthetics. Whether this variable is numeric or not will determine colour palette choice for the `ColPal` argument. 
#' @param facet An optional variable for faceting the plot using `facet_wrap()`.
#' @param ErrorType select the way to show data centrality and dispersion. The default is "none", which can be changed to "SD" (standard deviation), "SEM" (standard error of the mean) or "CI95" (95% confidence interval based on *t* distributions); all these will be displayed with a square symbol representing the mean. Choosing `Boxplot` will show a box and whiskers plot and the median. A line joining the central values will also apear. Use `l_alpha = 0` to not show the line.
#' @param SmoothType Add a smoothened average using 'Loess' or 'Linear', which will be passed on to `stat_smooth` as `method = "loess"` or `method = "lm"`, respectively. 
#' @param symsize Size of the raw data points. Default is `3`.
#' @param s_alpha Alpha transparency for raw data points. Default is `0.8`, which will reduce to `0.2` when an `ErrorType` is set.
#' @param mean_size Size of the square symbol representing the mean. Default is `symsize + 3` to prominently show the central value.
#' @param m_alpha Alpha transparency for the mean symbol. Default is `1`.
#' @param TextXAngle Angle of x-axis text labels. Default is `0`.
#' @param LogYTrans transform Y axis into "log10" or "log2" (in quotes).
#' @param LogXTrans transform X axis into "log10" or "log2"
#' @param LogYBreaks argument for \code{ggplot2[scale_y_continuous]} for Y axis breaks on log scales, default is `waiver()`, or provide a vector of desired breaks.
#' @param LogXBreaks argument for \code{ggplot2[scale_x_continuous]} for Y axis breaks on log scales, default is `waiver()`, or provide a vector of desired breaks.
#' @param LogYLabels argument for \code{ggplot2[scale_y_continuous]} for Y axis labels on log scales, default is `waiver()`, or provide a vector of desired labels. 
#' @param LogXLabels argument for \code{ggplot2[scale_x_continuous]} for Y axis labels on log scales, default is `waiver()`, or provide a vector of desired labels. 
#' @param LogYLimits a vector of length two specifying the range (minimum and maximum) of the Y axis.
#' @param LogXLimits a vector of length two specifying the range (minimum and maximum) of the X axis.
#' @param facet_scales whether or not to fix scales on X & Y axes for all graphs. Can be `fixed` (default), `free`, `free_y` or `free_x` (for Y and X axis one at a time, respectively).
#' @param fontsize parameter of \code{base_size} of fonts in \code{\link[ggplot2]{theme_classic}}, default set to size 20.
#' @param bwid width of boxplot (default = 0.3).
#' @param ewid width of errorbars (default = 0.1).
#' @param b_alpha fractional opacity of boxes (default = 0.3).
#' @param l_alpha fractional opacity of lines joining boxes, (default = 0.8).
#' @param sm_alpha fractional opacity of error range around loess or linear smooth fit (default = 0.3).
#' @param e_alpha fractional opacity of error bars (default = 1).
#' @param symthick size (in 'pt' units) of outline of symbol lines (\code{stroke}), default = `fontsize`/22.
#' @param bthick size (in 'pt' units) of outline of boxes, whisker and joining lines (\code{stroke}), default = `fontsize`/22.
#' @param ColPal Character. Name of the color palette to use from grafify. For categorical variables, one of: `"okabe_ito"`, `"all_grafify"`, `"bright"`, `"contrast"`, `"dark"`, `"fishy"`, `"kelly"`, `"light"`, `"muted"`, `"pale"`, `"r4"`, `"safe"`, `"vibrant"`. For quantitative variables, one of: `blue_conti`, `yellow_conti`, `grey_conti`, `PrGn_div`, `PrGn_div`.
#' @param ColSeq logical TRUE or FALSE. Default TRUE for sequential colours from chosen palette. Set to FALSE for distant colours, which will be applied using  \code{scale_fill_grafify2}.
#' @param ColRev whether to reverse order of colour within the selected palette, default F (FALSE); can be set to T (TRUE).
#' @param ... Additional arguments passed to `ggplot2` geoms or scales.
#'
#' @return This function returns a \code{ggplot2} object of class "gg" and "ggplot".
#' @export plot_xy_Group
#' @import ggplot2
#' @importFrom stats qt

plot_xy_Group <- function(data, xcol, ycol, Group, facet, 
         ErrorType = c("none", "SD", "SEM", "CI95", "Boxplot"), 
         SmoothType = c("none", "Loess", "Linear"),
         symsize = 3, s_alpha, TextXAngle = 0, 
         mean_size, m_alpha = 1,
         LogYTrans, LogXTrans, 
         LogYBreaks = waiver(), LogXBreaks = waiver(), 
         LogYLabels = waiver(), LogXLabels = waiver(), 
         LogYLimits = NULL, LogXLimits = NULL, 
         facet_scales = "fixed", fontsize = 20, 
         bwid = 0.3, b_alpha = 0.3, l_alpha = 0.8, sm_alpha = 0.3,
         symthick, bthick, ewid = 0.1, e_alpha = 1,
         ColPal = NULL, ColSeq = TRUE, ColRev = FALSE, ...) {
  
  # Set defaults
  if (missing(symthick)) symthick <- fontsize / 22
  if (missing(bthick)) bthick <- fontsize / 22
  if (missing(mean_size)) mean_size <- symsize + 3
  SmoothType <- match.arg(SmoothType)
  ErrorType <- match.arg(ErrorType)
  if (missing(s_alpha) & ErrorType != "none") s_alpha <- 0.2
  if (missing(s_alpha) & ErrorType == "none") s_alpha <- 0.8

  # Determine type of Group
  group_var <- data[[deparse(substitute(Group))]]
  is_numeric_group <- is.numeric(group_var)
  
  # Set default palette
  if (is.null(ColPal)) {
    ColPal <- if (is_numeric_group) "blue_conti" else "okabe_ito"
  }
  
  # Validate palette
  if (is_numeric_group && !(ColPal %in% c("blue_conti", "yellow_conti", "grey_conti", "PrGn_div", "PrGn_div"))) {
    stop("For numeric grouping variables, ColPal must be one of: 'blue_conti', 'yellow_conti', 'grey_conti', 'PrGn_div', 'PrGn_div'.")
  }
  if (!is_numeric_group && !(ColPal %in% c("okabe_ito", "all_grafify", "bright", "contrast", "dark", "fishy", "kelly", "light", "muted", "pale", "r4", "safe", "vibrant"))) {
    stop("For categorical grouping variables, ColPal must be one of the categorical palettes from grafify.")
  }
  
  # identify smoothening method
  #if(!SmoothType %in% c("Linear", "Loess")) {
  #  stop("SmoothType must be specified when Smooth is TRUE.
  #        Options are 'Linear' or 'Loess'.")
  #}
  if(SmoothType == "Linear") {sm_type <- "lm"} else 
    if (SmoothType == "Loess") {sm_type <- "loess"}

  # Define error bar function
  error_fun <- switch(ErrorType,
                      "SD" = function(x) {
                        m <- mean(x)
                        s <- sd(x)
                        c(y = m, ymin = m - s, ymax = m + s)
                      },
                      "SEM" = function(x) {
                        m <- mean(x)
                        s <- sd(x) / sqrt(length(x))
                        c(y = m, ymin = m - s, ymax = m + s)
                      },
                      "CI95" = function(x) {
                        m <- mean(x)
                        se <- sd(x) / sqrt(length(x))
                        ci <- qt(0.975, df = length(x) - 1) * se
                        c(y = m, ymin = m - ci, ymax = m + ci)
                      },
                      "none" = NULL)
  
  # Base plot
  P <- ggplot2::ggplot(data, aes(x = {{ xcol }}, y = {{ ycol }}))
  
  # Add smoothing line if requested
  if (SmoothType != "none") {
    P <- P + stat_smooth(aes(group = {{ Group }},
                             colour = {{ Group }},
                             fill = {{ Group }}),
                         method = sm_type, geom = "smooth",
                         linewidth = 0, show.legend = FALSE,
                         alpha = sm_alpha)+ 
      stat_smooth(geom = "line",
                  aes(group = {{ Group }},
                      colour = {{ Group }}),
                  method = sm_type, 
                  alpha = l_alpha, #show.legend = FALSE,
                  linewidth = bthick, 
                  se = FALSE)
  }
  #Add boxplot or points
  if (ErrorType == "Boxplot") {
    P <- P +
      geom_boxplot(aes(group = interaction({{ xcol }}, {{ Group }}),
                       fill = {{ Group }}),
                   linewidth = bthick,
                   outlier.alpha = 0,
                   width = bwid,
                   alpha = b_alpha,
                   position = position_identity(),
                   show.legend = FALSE) +
      stat_summary(geom = "line",
                   fun = median,
                   aes(colour = {{ Group }}),
                   linewidth = bthick,
                   alpha = ifelse(SmoothType != "none", 0, l_alpha))
  }
  
  P <- P +
    geom_point(aes(fill = {{ Group }}),
               shape = 21,
               size = symsize,
               stroke = symthick,
               alpha = s_alpha,
               ...)
  
  if (ErrorType %in% c("SD", "SEM", "CI95")) {
    P <- P +
      stat_summary(fun.data = error_fun,
                   geom = "errorbar",
                   show.legend = FALSE,
                   position = position_identity(),
                   width = ewid, alpha = e_alpha,
                   aes(group = {{ Group }})) +
      stat_summary(fun = mean,
                   geom = "point",
                   shape = 22,
                   size = mean_size,
                   stroke = symthick,
                   alpha = m_alpha,
                   position = position_identity(),
                   aes(group = {{ Group }}, fill = {{ Group }})) +
      stat_summary(fun = mean,
                   geom = "line", show.legend = FALSE,
                   aes(group = {{ Group }}, colour = {{ Group }}),
                   linewidth = bthick,
                   alpha = ifelse(SmoothType != "none", 0, l_alpha),
                   position = position_identity())
  }
  # add facets   
  if (!missing(facet)) {
    P <- P + facet_wrap(vars({{ facet }}), scales = facet_scales, ...)
  }
  
  # Log transformations
  if (!missing(LogYTrans)) {
    if (!(LogYTrans %in% c("log2", "log10"))) stop("LogYTrans only allows 'log2' or 'log10'.")
    P <- P + scale_y_continuous(trans = LogYTrans,
                                breaks = LogYBreaks,
                                labels = LogYLabels,
                                limits = LogYLimits,
                                ...)
    if (LogYTrans == "log10") {
      P <- P + annotation_logticks(sides = "l", outside = TRUE,
                                   base = 10, color = "grey20",
                                   long = unit(7 * fontsize / 22, "pt"),
                                   size = unit(fontsize / 22, "pt"),
                                   short = unit(4 * fontsize / 22, "pt"),
                                   mid = unit(4 * fontsize / 22, "pt"),
                                   ...) +
        coord_cartesian(clip = "off", ...)
    }
  }
  
  if (!missing(LogXTrans)) {
    if (!(LogXTrans %in% c("log2", "log10"))) stop("LogXTrans only allows 'log2' or 'log10'.")
    P <- P + scale_x_continuous(trans = LogXTrans,
                                breaks = LogXBreaks,
                                labels = LogXLabels,
                                limits = LogXLimits,
                                ...)
    if (LogXTrans == "log10") {
      P <- P + annotation_logticks(sides = "b", outside = TRUE,
                                   base = 10, color = "grey20",
                                   long = unit(7 * fontsize / 22, "pt"),
                                   size = unit(fontsize / 22, "pt"),
                                   short = unit(4 * fontsize / 22, "pt"),
                                   mid = unit(4 * fontsize / 22, "pt"),
                                   ...) +
        coord_cartesian(clip = "off", ...)
    }
  }
  
  # Apply appropriate color scales
  P <- P +
    scale_fill_grafify(palette = ColPal, reverse = ColRev, ColSeq = ColSeq) +
    scale_colour_grafify(palette = ColPal, reverse = ColRev, ColSeq = ColSeq) +
    labs(fill = enquo(Group), colour = enquo(Group)) +
    theme_grafify(base_size = fontsize) +
    guides(x = guide_axis(angle = TextXAngle))
  
  return(P)
}
