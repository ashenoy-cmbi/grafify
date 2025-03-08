#' Plot a scatter and box plot for 1-way ANOVAs with matched shapes mapped to blocking factor.  
#'
#' One of 4 related functions for plotting 1-way ANOVA designs with a blocking factor. 
#' 1. \code{\link{plot_3d_point_sd}} (mean & SD, SEM or CI95 error bars)
#' 2. \code{\link{plot_3d_scatterbar}} (bar & SD, SEM or CI95 error bars)
#' 3. \code{\link{plot_3d_scatterbox}} (box & whiskers)
#' 4. \code{\link{plot_3d_scatterviolin}} (box & whiskers, violin)
#' 
#' 
#' The blocking factor (or any other categorical variable) can be mapped to the `shapes` argument (up to 25 levels allowed). Variables passed to `xcol` and `shapes` are internally converted to factors even if they are numeric or other type of variables.
#' 
#' In `plot_3d_point_sd` and `plot_3d_scatterbar`, the default error bar is SD (can be changed to SEM or CI95). In `plot_3d_point_sd`, a large coloured symbol is plotted at the mean, all other data are shown as smaller symbols. Boxplot uses \code{\link[ggplot2]{geom_boxplot}} to depict median (thicker line), box (interquartile range (IQR)) and the whiskers (1.5*IQR).
#' 
#' Colours can be changed using `ColPal`, `ColRev` or `ColSeq` arguments. 
#' `ColPal` can be one of the following: "okabe_ito", "dark", "light", "bright", "pale", "vibrant,  "muted" or "contrast".
#' `ColRev` (logical TRUE/FALSE) decides whether colours are chosen from first-to-last or last-to-first from within the chosen palette. 
#' `ColSeq` (logical TRUE/FALSE) decides whether colours are picked by respecting the order in the palette or the most distant ones using \code{\link[grDevices]{colorRampPalette}}.
#' 
#' The resulting `ggplot2` graph can take additional geometries or other layers. 
#' 
#' @param data a data table, e.g. data.frame or tibble.
#' @param xcol name of the column with the categorical factor to be plotted on X axis. If your table has numeric X, enter \code{xcol = factor(name of colum)}.
#' @param ycol name of the column with quantitative variable to plot on the Y axis.
#' @param shapes name of the column with the second categorical factor in a two-way ANOVA design.
#' @param facet add another variable from the data table to create faceted graphs using \code{\link[ggplot2]{facet_wrap}}.
#' @param symsize size of symbols, default set to 3.
#' @param s_alpha fractional opacity of symbols, default set to 0.8 (i.e. 80% opacity). Set `s_alpha = 0` to not show scatter plot.
#' @param b_alpha fractional opacity of boxes.  Default is set to 0, which results in white boxes inside violins. Change to any value >0 up to 1 for different levels of transparency. 
#' @param bwid width of boxes; default 0.5.
#' @param jitter extent of jitter (scatter) of symbols, default is 0.1. Increase to reduce symbol overlap, set to 0 for aligned symbols.  
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text.
#' @param LogYTrans transform Y axis into "log10" or "log2"
#' @param LogYBreaks argument for \code{\link[ggplot2]{scale_y_continuous}} for Y axis breaks on log scales, default is `waiver()`, or provide a vector of desired breaks.
#' @param LogYLabels argument for \code{\link[ggplot2]{scale_y_continuous}} for Y axis labels on log scales, default is `waiver()`, or provide a vector of desired labels. 
#' @param LogYLimits a vector of length two specifying the range (minimum and maximum) of the Y axis.
#' @param facet_scales whether or not to fix scales on X & Y axes for all facet facet graphs. Can be `fixed` (default), `free`, `free_y` or `free_x` (for Y and X axis one at a time, respectively).
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param symthick size (in 'pt' units) of outline of symbol lines (\code{stroke}), default = `fontsize`/22.
#' @param bthick thickness (in 'pt' units) of lines of boxes; default = `fontsize`/22.
#' @param ColPal grafify colour palette to apply, default "okabe_ito"; see \code{\link{graf_palettes}} for available palettes.
#' @param ColRev whether to reverse order of colour within the selected palette, default F (FALSE); can be set to T (TRUE).
#' @param ColSeq logical TRUE or FALSE. Default TRUE for sequential colours from chosen palette. Set to FALSE for distant colours, which will be applied using  \code{scale_fill_grafify2}.
#' @param SingleColour a colour hexcode (starting with #), a number between 1-154, or names of colours from `grafify` or base R palettes to fill along X-axis aesthetic. Accepts any colour other than "black"; use `grey_lin11`, which is almost black.
#' @param ... any additional arguments to pass to \code{\link[ggplot2]{geom_boxplot}}.
#'
#' @return This function returns a \code{ggplot2} object of class "gg" and "ggplot".
#' @export plot_3d_scatterbox
#' @import ggplot2
#'
#' @examples
#' #3d version for 1-way data with blocking
#' plot_3d_scatterbox(data = data_1w_death, 
#' xcol = Genotype, ycol = Death, 
#' shapes = Experiment)
#' #use plot_scatterbox without a blocking factor
#' 
plot_3d_scatterbox <- function(data, xcol, ycol, shapes, facet, symsize = 3, s_alpha = 0.8, b_alpha = 1, bwid = 0.5, jitter = 0.1, TextXAngle = 0, LogYTrans, LogYBreaks = waiver(), LogYLabels = waiver(), LogYLimits = NULL, facet_scales = "fixed", fontsize = 20,  symthick, bthick, ColPal = c("okabe_ito", "all_grafify", "bright",  "contrast",  "dark",  "fishy",  "kelly",  "light",  "muted",  "pale",  "r4",  "safe",  "vibrant"), ColSeq = TRUE, ColRev = FALSE, SingleColour = "NULL", ...){
  ColPal <- match.arg(ColPal)
  if (missing(bthick)) {bthick = fontsize/22}
  if (missing(symthick)) {symthick = fontsize/22}
  #data[[deparse(substitute(xcol))]] <- factor(#data[[deparse(substitute(xcol))]])
  #data[[deparse(substitute(shapes))]] <- factor(#data[[deparse(substitute(shapes))]])
  suppressWarnings(P <- ggplot2::ggplot(data, 
                                        aes(x = factor({{ xcol }}),
                                            y = {{ ycol }},
                                            group = {{ xcol }}))+
                     geom_boxplot(aes(fill = factor({{ xcol }})), 
                                  linewidth = bthick,
                                  alpha = b_alpha,
                                  position = position_dodge(width = 0.8),
                                  width = bwid,
                                  outlier.alpha = 0, 
                                  ...)+
                     geom_point(size = symsize, 
                                stroke = symthick,
                                alpha = s_alpha, colour = "black",
                                position = position_jitterdodge(jitter.width = jitter,
                                                                dodge.width = 0.8),
                                aes(shape = factor({{ shapes }})))+
                     scale_shape_manual(values = 0:25))
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
    x1 <- deparse(substitute(xcol))
    x <- length(levels(factor(data[[x1]])))
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
  P <- P +
    labs(x = enquo(xcol),
         fill = enquo(xcol),
         shape = enquo(shapes))+
    theme_grafify(base_size = fontsize)+
    guides(x = guide_axis(angle = TextXAngle),
           fill = guide_legend(order = 1),
           shape = guide_legend(order = 2))
  P  
}
