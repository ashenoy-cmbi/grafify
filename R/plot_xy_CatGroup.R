#' Plot points on a quantitative X - Y plot & a categorical grouping variable.
#'
#' This function takes a data table, quantitative X and Y variables along with a categorical grouping variable, and a and plots a graph with using \code{\link[ggplot2]{geom_point}}. The categorical `CatGroup` variable is mapped to the \code{fill} aesthetic of symbols.
#' 
#' A box and whisker plot with lines joining the medians can be plotted with `Boxplot = TRUE`. If only box plot is needed without the line, set the opacity of the line to 0 (i.e., l_alpha = 0).
#' 
#' Colours can be changed using `ColPal`, `ColRev` or `ColSeq` arguments. Colours available can be seen quickly with \code{\link{plot_grafify_palette}}.
#' `ColPal` can be one of the following: "okabe_ito", "dark", "light", "bright", "pale", "vibrant,  "muted" or "contrast".
#' `ColRev` (logical TRUE/FALSE) decides whether colours are chosen from first-to-last or last-to-first from within the chosen palette. 
#' `ColSeq` (logical TRUE/FALSE) decides whether colours are picked by respecting the order in the palette or the most distant ones using \code{\link[grDevices]{colorRampPalette}}.
#' 
#' This plot is related to \code{\link{plot_xy_NumGroup}} which requires a numeric grouping factor.
#' When summary statistics (mean/median) are required, use \code{\link{plot_3d_scatterbar}}, \code{\link{plot_3d_scatterbox}} or \code{\link{plot_4d_scatterbox}}. 
#'
#' @param data a data table object, e.g. data.frame or tibble.
#' @param xcol name of the column with quantitative X variable.
#' @param ycol name of the column with quantitative Y variable.
#' @param CatGroup a categorical variable as grouping factor for colour of data points, should be a categorical variable for default colours to work. Will be converted to `factor` if your column is numeric
#' @param facet add another variable from the data table to create faceted graphs using \code{ggplot2}[facet_wrap].
#' @param Boxplot logical TRUE/FALSE to plot box and whiskers plot (default = FALSE).
#' @param symsize size of symbols used by \code{geom_point}. Default set to 3.
#' @param s_alpha fractional opacity of symbols, default set to to 0.8 (i.e, 80% opacity).
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text.
#' @param LogYTrans transform Y axis into "log10" or "log2"
#' @param LogXTrans transform X axis into "log10" or "log2"
#' @param LogYBreaks argument for \code{ggplot2[scale_y_continuous]} for Y axis breaks on log scales, default is `waiver()`, or provide a vector of desired breaks.
#' @param LogXBreaks argument for \code{ggplot2[scale_x_continuous]} for Y axis breaks on log scales, default is `waiver()`, or provide a vector of desired breaks.
#' @param LogYLabels argument for \code{ggplot2[scale_y_continuous]} for Y axis labels on log scales, default is `waiver()`, or provide a vector of desired labels. 
#' @param LogXLabels argument for \code{ggplot2[scale_x_continuous]} for Y axis labels on log scales, default is `waiver()`, or provide a vector of desired labels. 
#' @param LogYLimits a vector of length two specifying the range (minimum and maximum) of the Y axis.
#' @param LogXLimits a vector of length two specifying the range (minimum and maximum) of the X axis.
#' @param facet_scales whether or not to fix scales on X & Y axes for all graphs. Can be `fixed` (default), `free`, `free_y` or `free_x` (for Y and X axis one at a time, respectively).
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param bwid width of boxplot (default = 0.3).
#' @param b_alpha fractional opacity of boxes, (default = 0.3).
#' @param l_alpha fractional opacity of lines joining boxes, (default = 0.8).
#' @param symthick size (in 'pt' units) of outline of symbol lines (\code{stroke}), default = `fontsize`/22.
#' @param bthick size (in 'pt' units) of outline of boxes, whisker and joining lines (\code{stroke}), default = `fontsize`/22.
#' @param ColPal grafify colour palette to apply, default "okabe_ito"; see \code{\link{graf_palettes}} for available palettes.
#' @param ColSeq logical TRUE or FALSE. Default TRUE for sequential colours from chosen palette. Set to FALSE for distant colours, which will be applied using  \code{scale_fill_grafify2}.
#' @param ColRev whether to reverse order of colour within the selected palette, default F (FALSE); can be set to T (TRUE).
#' @param ... any additional arguments to pass on.
#'
#' @return This function returns a \code{ggplot2} object of class "gg" and "ggplot".
#' @export plot_xy_CatGroup
#' @import ggplot2
#'
#' @examples
#' #The grouping factor cyl  is automatically converted to categorical variable
#' plot_xy_CatGroup(data = mtcars,
#' xcol = mpg, ycol = disp, CatGroup = cyl, 
#' ColPal = "vibrant", ColSeq = FALSE)
#' 
#' #with boxplot
#' plot_xy_CatGroup(data = mpg, 
#' xcol = cyl, ycol = cty, 
#' CatGroup = fl, Boxplot = TRUE)
#' 
#' #add another variable
#' #with boxplot
#' plot_xy_CatGroup(data = mpg, 
#' xcol = cyl, ycol = cty, 
#' CatGroup = fl, facet = drv,
#' Boxplot = TRUE)
#' 

plot_xy_CatGroup <- function(data, xcol, ycol, CatGroup, facet, Boxplot = FALSE, symsize = 3, s_alpha = 0.8, TextXAngle = 0, LogYTrans, LogXTrans, LogYBreaks = waiver(), LogXBreaks = waiver(), LogYLabels = waiver(), LogXLabels = waiver(), LogYLimits = NULL, LogXLimits = NULL, facet_scales = "fixed", fontsize = 20, bwid = 0.3, b_alpha = 0.3, l_alpha = 0.8, symthick, bthick, ColPal = c("okabe_ito", "all_grafify", "bright",  "contrast",  "dark",  "fishy",  "kelly",  "light",  "muted",  "pale",  "r4",  "safe",  "vibrant"), ColSeq = TRUE, ColRev = FALSE, ...){
  ColPal <- match.arg(ColPal)
  if (missing(symthick)) {symthick = fontsize/22}
  if (missing(bthick)) {bthick = fontsize/22}
  if (!(Boxplot)) {
    suppressWarnings(P <- ggplot2::ggplot(data, aes(x = {{ xcol }},
                                   y = {{ ycol }}))+
      geom_point(size = symsize, 
                 alpha = s_alpha,
                 aes(fill = factor({{ CatGroup }})),
                 shape = 21, 
                 stroke = symthick, 
                 ...)+
      labs(fill = enquo(CatGroup)))
  } else {
    suppressWarnings(P <- ggplot2::ggplot(data, aes(x = {{ xcol }},
                                   y = {{ ycol }}))+
      geom_boxplot(aes(group = interaction({{ xcol }},
                                           {{ CatGroup }}),
                       fill = {{ CatGroup }}),
                   size = bthick,
                   outlier.alpha = 0,
                   width = bwid,
                   alpha = b_alpha,
                   position = position_identity(),
                   show.legend = FALSE)+
      stat_summary(geom = "line",
                   size = bthick,
                   alpha = l_alpha,
                   aes(colour = {{ CatGroup }}),
                   fun = "median")+
      scale_colour_grafify(palette = ColPal,
                           reverse = ColRev,
                           ColSeq = ColSeq)+
      geom_point(size = symsize, 
                 alpha = s_alpha,
                 aes(fill = factor({{ CatGroup }})),
                 shape = 21, 
                 stroke = symthick,
                 ...)+
      labs(fill = enquo(CatGroup),
           colour = enquo(CatGroup)))
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
                           ...)
    }
  }
  if (!missing(LogXTrans)) {
    if (!(LogXTrans %in% c("log2", "log10"))) {
      stop("LogXTrans only allows 'log2' or 'log10' transformation.")
    }
    if (LogXTrans == "log10") {
      P <- P + 
        scale_x_continuous(trans = "log10", 
                           breaks = LogXBreaks, 
                           labels = LogXLabels, 
                           limits = LogXLimits, 
                           ...)+
        annotation_logticks(sides = "b", 
                            outside = TRUE,
                            base = 10, color = "grey20",
                            long = unit(7*fontsize/22, "pt"), size = unit(fontsize/22, "pt"),# 
                            short = unit(4*fontsize/22, "pt"), mid = unit(4*fontsize/22, "pt"),#
                            ...)+ 
        coord_cartesian(clip = "off", ...)
    }
    if (LogXTrans == "log2") {
      P <- P + 
        scale_x_continuous(trans = "log2", 
                           breaks = LogXBreaks, 
                           labels = LogXLabels, 
                           limits = LogXLimits,  
                           ...)
    }
  }
  P <- P +
    theme_grafify(base_size = fontsize)+
    guides(x = guide_axis(angle = TextXAngle))+
    scale_fill_grafify(palette = ColPal, 
                       reverse = ColRev, 
                       ColSeq = ColSeq)
  P
}
