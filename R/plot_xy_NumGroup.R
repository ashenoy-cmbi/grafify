#' Plot points on a quantitative X - Y plot & a numeric grouping variable.
#'
#' This function takes a data table, quantitative X and Y variables, and a numeric grouping variable, and a and plots a graph with using \code{\link[ggplot2]{geom_point}}. The numerical `NumGroup` variable is mapped to the \code{fill} aesthetic of symbols, which receives the `scale_fill_grafify` default quantitative palette (`blue_conti`). Alternatives are `yellow_conti`, `grey_conti`, `OrBl_div` and `PrGn_div`. Colour order can be reversed with `ColRev = TRUE` (default is `FALSE`).
#' 
#' This plot is related to \code{\link{plot_xy_CatGroup}} which requires a categorical grouping factor.
#' When summary statistics (mean/median) are required, use \code{\link{plot_3d_scatterbar}}, \code{\link{plot_3d_scatterbox}} or \code{\link{plot_4d_scatterbox}}. 
#'
#' @param data a data table object, e.g. data.frame or tibble.
#' @param xcol name of the column with quantitative X variable.
#' @param ycol name of the column with quantitative Y variable.
#' @param NumGroup a numeric factor for `fill` aesthetic of data points.
#' @param facet add another variable from the data table to create faceted graphs using \code{\link[ggplot2]{facet_wrap}}.
#' @param Boxplot logical TRUE/FALSE to plot box and whiskers plot (default = FALSE).
#' @param symsize size of symbols used by \code{geom_point}. Default set to 3.
#' @param s_alpha fractional opacity of symbols, default set to 0.8 (i.e, 80% opacity).
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
#' @param ColRev whether to reverse order of colour within the selected palette, default F (FALSE); can be set to T (TRUE).
#' @param ... any additional arguments to pass on.
#'
#' @return This function returns a \code{ggplot2} object of class "gg" and "ggplot".
#' @export plot_xy_NumGroup
#' @import ggplot2
#'
#' @examples
#' #The grouping factor gear is numeric 
#' plot_xy_NumGroup(data = mtcars,
#' xcol = mpg, ycol = disp, NumGroup = cyl,
#' s_alpha = 0.8)
#' #change colour palette
#' plot_xy_NumGroup(data = mtcars,
#' xcol = mpg, ycol = disp, NumGroup = cyl,
#' s_alpha = 0.8, 
#' ColPal = "grey_conti")

plot_xy_NumGroup <- function(data, xcol, ycol, NumGroup, facet, Boxplot = FALSE, symsize = 3, s_alpha = 0.8, TextXAngle = 0, LogYTrans, LogXTrans, LogYBreaks = waiver(), LogXBreaks = waiver(), LogYLabels = waiver(), LogXLabels = waiver(), LogYLimits = NULL, LogXLimits = NULL, facet_scales = "fixed", fontsize = 20, bwid = 0.3, b_alpha = 0.3, l_alpha = 0.8, symthick, bthick, ColPal = c("blue_conti", "yellow_conti", "grey_conti", "PrGn_div", "OrBl_div"), ColRev = FALSE, ...){
  ColPal <- match.arg(ColPal)
  if (missing(symthick)) {symthick = fontsize/22}
  if (missing(bthick)) {bthick = fontsize/22}
  if (!(Boxplot)) {
    suppressWarnings(P <- ggplot2::ggplot(data, aes(x = {{ xcol }},
                                   y = {{ ycol }}))+
      geom_point(size = symsize, 
                 alpha = s_alpha,
                 aes(fill = {{ NumGroup }}),
                 shape = 21, 
                 stroke = symthick, 
                 ...)+
      labs(fill = enquo(NumGroup)))
  } else {
    suppressWarnings(P <- ggplot2::ggplot(data, aes(x = {{ xcol }},
                                   y = {{ ycol }}))+
      geom_boxplot(aes(group = interaction({{ xcol }},
                                           {{ NumGroup }}),
                       fill = {{ NumGroup }}),
                   linewidth = bthick,
                   outlier.alpha = 0,
                   width = bwid,
                   alpha = b_alpha,
                   position = position_identity(),
                   show.legend = FALSE)+
      stat_summary(geom = "line",
                   linewidth = bthick,
                   alpha = l_alpha,
                   colour = "grey35",
                   fun = "median")+
      geom_point(size = symsize, 
                 alpha = s_alpha,
                 aes(fill = {{ NumGroup }}),
                 shape = 21, 
                 stroke = symthick,
                 ...)+
      labs(fill = enquo(NumGroup),
           colour = enquo(NumGroup)))}
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
                       reverse = ColRev)
  P
}
