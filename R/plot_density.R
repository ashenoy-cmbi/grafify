#' Plot density distribution of data.
#'
#' This function takes a data table, `ycol` of quantitative variable and a categorical grouping variable (`group`), if available, and plots a density graph using \code{\link[ggplot2]{geom_density}}). Alternatives are \code{\link{plot_histogram}}, or \code{\link{plot_qqline}}.
#' 
#' Note that the function requires the quantitative Y variable first, and groups them based on an X variable. The group variable is mapped to the \code{fill} and \code{colour} aesthetics in \code{\link[ggplot2]{geom_density}}.
#' Colours can be changed using `ColPal`, `ColRev` or `ColSeq` arguments. Colours available can be seen quickly with \code{\link{plot_grafify_palette}}.
#' `ColPal` can be one of the following: "okabe_ito", "dark", "light", "bright", "pale", "vibrant,  "muted" or "contrast".
#' `ColRev` (logical TRUE/FALSE) decides whether colours are chosen from first-to-last or last-to-first from within the chosen palette. 
#' `ColSeq` decides whether colours are picked by respecting the order in the palette or the most distant ones using \code{\link[grDevices]{colorRampPalette}}.
#'
#' @param data a data table e.g. data.frame or tibble.
#' @param ycol name of the column containing the quantitative variable whose density distribution is to be plotted.
#' @param group name of the column containing a categorical grouping variable (optional since v4.1.0).
#' @param facet add another variable from the data table to create faceted graphs using \code{\link[ggplot2]{facet_wrap}}.
#' @param PlotType the default (`Density`) plot will be the probability density curve, which can be changed to `Counts` or `Normalised counts`. 
#' @param c_alpha fractional opacity of filled colours under the curve, default set to 0.2 (i.e. 20% opacity).
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text.
#' @param facet_scales whether or not to fix scales on X & Y axes for all facet facet graphs. Can be `fixed` (default), `free`, `free_y` or `free_x` (for Y and X axis one at a time, respectively).
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param linethick thickness of symbol border, default set to `fontsize`/22.
#' @param ColPal grafify colour palette to apply, default "okabe_ito"; see \code{\link{graf_palettes}} for available palettes.
#' @param ColSeq logical TRUE or FALSE. Default TRUE for sequential colours from chosen palette. Set to FALSE for distant colours, which will be applied using  \code{scale_fill_grafify2}.
#' @param ColRev whether to reverse order of colour within the selected palette, default F (FALSE); can be set to T (TRUE).
#' @param SingleColour a colour hexcode (starting with #), a number between 1-154, or names of colours from `grafify` palettes or base R to fill along X-axis aesthetic. Accepts any colour other than "black"; use `grey_lin11`, which is almost black.
#' @param LogYTrans transform Y axis into "log10" or "log2"
#' @param LogYBreaks argument for \code{ggplot2[scale_y_continuous]} for Y axis breaks on log scales, default is `waiver()`, or provide a vector of desired breaks.
#' @param LogYLabels argument for \code{ggplot2[scale_y_continuous]} for Y axis labels on log scales, default is `waiver()`, or provide a vector of desired labels. 
#' @param LogYLimits a vector of length two specifying the range (minimum and maximum) of the Y axis.
#' @param ... any additional arguments to pass to \code{\link[ggplot2]{geom_density}}.
#'
#' @return This function returns a \code{ggplot2} object of class "gg" and "ggplot".
#' @export plot_density
#' @import ggplot2
#' @importFrom dplyr count
#'
#' @examples
#' plot_density(data = data_t_pratio, 
#' ycol = log(Cytokine), group = Genotype)
#' 
#' #with faceting
#' plot_density(data = data_cholesterol, 
#' ycol = Cholesterol, group = Treatment, 
#' facet = Treatment, fontsize = 12)
#' 
#' #Counts
#' plot_density(data = data_cholesterol, 
#' ycol = Cholesterol, group = Treatment,
#' PlotType = "Counts", 
#' facet = Treatment, fontsize = 12)
#' 
plot_density <- function(data, ycol, group, facet, PlotType = c("Density", "Counts", "Normalised counts"), c_alpha = 0.2, TextXAngle = 0, facet_scales = "fixed", fontsize = 20, linethick, LogYTrans, LogYBreaks = waiver(), LogYLabels = waiver(), LogYLimits = NULL, ColPal = c("okabe_ito", "all_grafify", "bright",  "contrast",  "dark",  "fishy",  "kelly",  "light",  "muted",  "pale",  "r4",  "safe",  "vibrant"), ColSeq = TRUE, ColRev = FALSE, SingleColour = NULL, ...){
  message("Showing plot of smooth kernel density (default). `PlotType` can be changed to 'Counts' or 'Normalised counts'.")
  if(missing(linethick)) {linethick = fontsize/22}
  ColPal <- match.arg(ColPal)
  PlotType <- match.arg(PlotType)
  if (!(PlotType %in% c("Density", "Counts", "Normalised counts"))) {
    stop("`PlotType` can only be NULL, count or max_counts")
  }
  if(missing(group) & missing(SingleColour)) {message("You did not provide a grouping variable, so grafify used the default colour. You can change this with the `SingleColour` argument.") }
  if(missing(group) & missing(SingleColour)) SingleColour <- "#E69F00"
  if(missing(group) & !missing(SingleColour)) {
    ifelse(grepl("#", SingleColour), 
           a <- SingleColour,
           ifelse(isTRUE(get_graf_colours(SingleColour) != 0), 
                  a <- unname(get_graf_colours(SingleColour)), 
                  a <- SingleColour))
  }
  if(missing(group)) {
    suppressWarnings(P <- ggplot2::ggplot(data, 
                                          aes(x = {{ ycol }},
                                              fill = "one",
                                              colour = "one")) + 
                       scale_fill_manual(values = a)+
                       scale_colour_manual(values = a)+
                       guides(fill = "none", colour = "none"))
  } else {
    suppressWarnings(P <- ggplot2::ggplot(data, 
                                          aes(x = {{ ycol }},
                                              fill = factor({{ group }}),
                                              colour = factor({{ group }}))) +
                       scale_fill_grafify(palette = ColPal, 
                                          reverse = ColRev, 
                                          ColSeq = ColSeq)+
                       scale_colour_grafify(palette = ColPal, 
                                            reverse = ColRev, 
                                            ColSeq = ColSeq))
  }
  if(PlotType == "Density") {
    suppressWarnings(P <- P +
                       geom_density(linewidth = linethick,
                                    alpha = c_alpha)+
                       labs(y = "Density"))}
  if(PlotType == "Counts") {
    suppressWarnings(P <- P +
                       geom_density(linewidth = linethick,
                                    aes(y = after_stat(count)),
                                    alpha = c_alpha)+
                       labs(y = "Counts"))
  }
  if(PlotType == "Normalised counts") {
    suppressWarnings(P <- P +
                       geom_density(linewidth = linethick,
                                    aes(y = after_stat(count/max(count))),
                                    alpha = c_alpha)+
                       labs(y = "Normalised counts"))
  }
  if (!missing(LogYTrans)) {
    if (!(LogYTrans %in% c("log2", "log10"))) {
      stop("LogYTrans only allows 'log2' or 'log10' transformation.")
    }
    if (LogYTrans == "log10") {
      P <- P + 
        scale_x_continuous(trans = "log10", 
                           breaks = LogYBreaks, 
                           labels = LogYLabels, 
                           limits = LogYLimits, 
                           ...)+
        annotation_logticks(sides = "b", 
                            outside = TRUE,
                            base = 10, color = "grey20",
                            long = unit(7*fontsize/22, "pt"), size = unit(fontsize/22, "pt"),# 
                            short = unit(4*fontsize/22, "pt"), mid = unit(4*fontsize/22, "pt"),#
                            ...)+ 
        coord_cartesian(clip = "off", ...)
    }
    if (LogYTrans == "log2") {
      P <- P + 
        scale_x_continuous(trans = "log2", 
                           breaks = LogYBreaks, 
                           labels = LogYLabels, 
                           limits = LogYLimits, 
                           ...)}
  }
  if(!missing(facet)) {
    P <- P + facet_wrap(vars({{ facet }}), 
                        scales = facet_scales, 
                        ...)
  }
  P <- P +
    theme_grafify(base_size = fontsize) +
    guides(x = guide_axis(angle = TextXAngle)) +
    labs(fill = enquo(group),
         colour = enquo(group))
  P
}
