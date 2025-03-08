#' Before-after style graph with a boxplot
#'
#' One of 3 related functions to plot matching data joined by lines. The variable containing information for matching (e.g. matched subjects or experiments etc.) is passed to the `match` argument. 
#' 1.  \code{\link{plot_befafter_colours}} or \code{\link{plot_befafter_colors}},
#' 2. \code{\link{plot_befafter_shapes}}  
#' 3. \code{\link{plot_befafter_box}}
#' 
#' In `plot_befafter_colours`/`plot_befafter_colors` and `plot_befafter_shapes` setting `Boxplot = TRUE` will also plot a box and whiskers plot.
#'
#' Note that only 25 shapes are available, and there will be errors with \code{\link{plot_befafter_shapes}} when there are fewer than 25 matched observations; instead use \code{\link{plot_befafter_colours}} instead.
#' 
#' Colours can be changed using `ColPal`, `ColRev` or `ColSeq` arguments. 
#' `ColPal` can be one of the following: "okabe_ito", "dark", "light", "bright", "pale", "vibrant,  "muted" or "contrast".
#' `ColRev` (logical TRUE/FALSE) decides whether colours are chosen from first-to-last or last-to-first from within the chosen palette. 
#' `ColSeq` decides whether colours are picked by respecting the order in the palette or the most distant ones using \code{\link[grDevices]{colorRampPalette}}.
#'
#' To plot a graph with a single colour along the X axis variable, use the `SingleColour` argument.
#' 
#' The resulting `ggplot2` graph can take additional geometries or other layers. 
#' 
#' @param data a data table object, e.g. data.frame or tibble.
#' @param xcol name of the column containing the categorical variable to be plotted on the X axis.
#' @param ycol name of the column containing the quantitative Y values.
#' @param match name of the column with the grouping variable to pass on to \code{\link[ggplot2]{geom_line}}.
#' @param facet add another variable from the data table to create faceted graphs using \code{\link[ggplot2]{facet_wrap}}.
#' @param PlotShapes logical TRUE or FALSE (default = FALSE) if the shape of the symbol is to be mapped to the `match` variable. Note that only 25 shapes allowed.
#' @param symsize size of symbols, default set to 3.
#' @param s_alpha fractional opacity of symbols, default set to 0.8 (i.e., 80% opacity).
#' @param b_alpha fractional opacity of boxes, default set to 1.
#' @param bwid width of boxplots; default 0.4.
#' @param jitter extent of jitter (scatter) of symbols, default is 0.1. Increase to reduce symbol overlap, set to 0 for aligned symbols.  
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text.
#' @param LogYTrans transform Y axis into "log10" or "log2"
#' @param LogYLabels argument for \code{\link[ggplot2]{scale_y_continuous}} for Y axis labels on log scales, default is `waiver()`, or provide a vector of desired labels. 
#' @param LogYBreaks argument for \code{\link[ggplot2]{scale_y_continuous}} for Y axis breaks on log scales, default is `waiver()`, or provide a vector of desired breaks.
#' @param LogYLimits a vector of length two specifying the range (minimum and maximum) of the Y axis.
#' @param facet_scales whether or not to fix scales on X & Y axes for all facet facet graphs. Can be `fixed` (default), `free`, `free_y` or `free_x` (for Y and X axis one at a time, respectively).
#' @param fontsize parameter of \code{base_size} of fonts in \code{\link[ggplot2]{theme_classic}}, default set to size 20.
#' @param symthick size (in 'pt' units) of outline of symbol lines (\code{stroke}), default = `fontsize`/22.
#' @param bthick thickness (in 'pt' units) of boxes; default = `(fontsize)/22`.
#' @param lthick thickness (in 'pt' units) of lines; default = `(fontsize/1.2)/22`.
#' @param ColPal grafify colour palette to apply, default "okabe_ito"; see \code{\link{graf_palettes}} for available palettes.
#' @param ColSeq logical TRUE or FALSE. Default TRUE for sequential colours from chosen palette. Set to FALSE for distant colours, which will be applied using  \code{scale_fill_grafify2}.
#' @param ColRev whether to reverse order of colour within the selected palette, default F (FALSE); can be set to T (TRUE).
#' @param SingleColour a colour hexcode (starting with #), a number between 1-154, or names of colours from `grafify` or base R palettes to fill along X-axis aesthetic. Accepts any colour other than "black"; use `grey_lin11`, which is almost black.
#' @param ... any additional arguments to pass to \code{\link[ggplot2]{geom_line}},  \code{\link[ggplot2]{geom_point}}, or \code{\link[ggplot2]{facet_wrap}}.
#'
#' @return This function returns a \code{ggplot2} object of class "gg" and "ggplot".
#' @export plot_befafter_box
#' @import ggplot2
#'
#' @examples
#' plot_befafter_box(data = data_t_pdiff, 
#' xcol = Condition, ycol = Mass, 
#' match = Subject)
#' 
#' #with PlotShapes = TRUE
#' plot_befafter_box(data = data_t_pdiff, 
#' xcol = Condition, ycol = Mass, 
#' match = Subject, PlotShapes = TRUE)

plot_befafter_box <- function(data, xcol, ycol, match, facet, PlotShapes = FALSE, symsize = 3,  s_alpha = 0.8, b_alpha = 1, bwid = 0.4, jitter = 0.1, TextXAngle = 0, LogYTrans, LogYBreaks = waiver(), LogYLabels = waiver(), LogYLimits = NULL, facet_scales = "fixed", fontsize = 20, symthick, bthick, lthick, ColPal = c("okabe_ito", "all_grafify", "bright",  "contrast",  "dark",  "fishy",  "kelly",  "light",  "muted",  "pale",  "r4",  "safe",  "vibrant"), ColSeq = TRUE, ColRev = FALSE, SingleColour = "NULL", ...){
  ColPal <- match.arg(ColPal)
  if (missing(bthick)) {bthick = (fontsize)/22}
  if (missing(lthick)) {lthick = (fontsize/1.5)/22}
  if (missing(symthick)) {symthick = (fontsize)/22}
  suppressWarnings(P <- ggplot2::ggplot(data, 
                       aes(x = factor({{ xcol }}),
                           y = {{ ycol }},
                           group = factor({{ match }})))+
    geom_boxplot(aes(group = {{ xcol }},
                     fill = factor({{ xcol }})),
                 alpha = b_alpha,
                 outlier.alpha = 0,
                 width = bwid,
                 linewidth = bthick,
                 ...)+
    geom_line(aes(group = factor({{ match }})),
              colour = "grey35", 
              alpha = 0.8, 
              size = lthick, 
              position = position_dodge(width = jitter),
              ...))
  if (!PlotShapes) {
    suppressWarnings(P <- P + 
      geom_point(size = symsize, 
                 stroke = symthick,
                 alpha = s_alpha, 
                 shape = 21, 
                 position = position_dodge(width = jitter),
                 aes(fill = factor({{ xcol }})), ...))
  } else {
    suppressWarnings(P <- P + 
      geom_point(size = symsize, 
                 stroke = symthick,
                 alpha = s_alpha,
                 colour = "black",
                 position = position_dodge(width = jitter),
                 aes(shape = factor({{ match }})),
                 ...)+
      scale_shape_manual(values = 0:25))
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
      guides(x = guide_axis(angle = TextXAngle),
             fill = "none")
  } else {
    P <- P +
      scale_fill_grafify(palette = ColPal, 
                         reverse = ColRev, 
                         ColSeq = ColSeq)+
      guides(x = guide_axis(angle = TextXAngle),
             fill = guide_legend(order = 1),
             shape = guide_legend(order = 2))
  }
  P <- P +
    labs(x = enquo(xcol),
         fill = enquo(match),
         shape = enquo(match))+
    theme_grafify(base_size = fontsize)
  P
}
