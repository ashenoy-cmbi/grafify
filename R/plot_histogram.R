#' Plot data distribution as histograms.
#'
#' This function takes a data table, a quantitative variable (`ycol`) and a grouping variable (`group`), if available, and plots a histogram graph using \code{\link{geom_histogram}}).  Alternatives are \code{\link{plot_density}}, or \code{\link{plot_qqline}}. 
#' 
#' Note that the function requires the quantitative Y variable first, and groups them based on a categorical variable passed on via the `group` argument. The grouping variable is mapped to the \code{fill} aesthetics in \code{geom_histogram}.
#' 
#' ColPal & ColRev options are applied to both `fill` and `colour` scales. Colours available can be seen quickly with \code{\link{plot_grafify_palette}}.
#' Colours can be changed using `ColPal`, `ColRev` or `ColSeq` arguments. 
#' `ColPal` can be one of the following: "okabe_ito", "dark", "light", "bright", "pale", "vibrant,  "muted" or "contrast".
#' `ColRev` (logical TRUE/FALSE) decides whether colours are chosen from first-to-last or last-to-first from within the chosen palette. 
#' `ColSeq` decides whether colours are picked by respecting the order in the palette or the most distant ones using \code{\link[grDevices]{colorRampPalette}}.
#'
#' @param data a data table e.g. data.frame or tibble.
#' @param ycol name of the column containing the quantitative variable whose histogram distribution is to be plotted.
#' @param group name of the column containing a categorical grouping variable.
#' @param facet add another variable from the data table to create faceted graphs using \code{\link[ggplot2]{facet_wrap}}.
#' @param PlotType the default (`Counts`) plot will plot counts in the bins, which can be changed to `Normalised counts`. 
#' @param BinSize number of distinct bins to use on X-axis, default set to 30.
#' @param c_alpha fractional opacity of colour filled within histograms, default set to 0.8 (i.e. 80% opacity).
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text.
#' @param facet_scales whether or not to fix scales on X & Y axes for all facet facet graphs. Can be `fixed` (default), `free`, `free_y` or `free_x` (for Y and X axis one at a time, respectively).
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param linethick thickness of symbol border, default set to `fontsize`/22.
#' @param alpha deprecated old argument for `c_alpha`; retained for backward compatibility.
#' @param ColPal grafify colour palette to apply, default "okabe_ito"; see \code{\link{graf_palettes}} for available palettes.
#' @param ColSeq logical TRUE or FALSE. Default TRUE for sequential colours from chosen palette. Set to FALSE for distant colours, which will be applied using  \code{scale_fill_grafify2}.
#' @param ColRev whether to reverse order of colour within the selected palette, default F (FALSE); can be set to T (TRUE).
#' @param SingleColour a colour hexcode (starting with #), a number between 1-154, or names of colours from `grafify` palettes or base R to fill along X-axis aesthetic. Accepts any colour other than "black"; use `grey_lin11`, which is almost black.
#' @param LogYTrans transform Y axis into "log10" or "log2"
#' @param LogYBreaks argument for \code{ggplot2[scale_y_continuous]} for Y axis breaks on log scales, default is `waiver()`, or provide a vector of desired breaks.
#' @param LogYLabels argument for \code{ggplot2[scale_y_continuous]} for Y axis labels on log scales, default is `waiver()`, or provide a vector of desired labels. 
#' @param LogYLimits a vector of length two specifying the range (minimum and maximum) of the Y axis.
#' @param ... any additional arguments to pass to \code{ggplot2}[geom_histogram].
#'
#' @return This function returns a \code{ggplot2} object of class "gg" and "ggplot".
#' @export plot_histogram
#' @import ggplot2
#' @importFrom dplyr count
#'
#' @examples
#' #Basic usage
#' plot_histogram(data = data_t_pratio, 
#' ycol = Cytokine, group = Genotype, 
#' BinSize = 10)
#' #with log transformation
#' plot_histogram(data = data_t_pratio, 
#' ycol = log(Cytokine), group = Genotype, 
#' BinSize = 10)
#' #Normalised counts
#' plot_histogram(data = data_t_pratio, 
#' ycol = log(Cytokine), group = Genotype, 
#' PlotType = "Normalised counts", 
#' BinSize = 10)

plot_histogram <- function(data, ycol, group, facet, PlotType = c("Counts", "Normalised counts"), BinSize = 30, c_alpha = 0.8, TextXAngle = 0, facet_scales = "fixed", fontsize = 20, linethick, alpha, LogYTrans, LogYBreaks = waiver(), LogYLabels = waiver(), LogYLimits = NULL, ColPal = c("okabe_ito", "all_grafify", "bright",  "contrast",  "dark",  "fishy",  "kelly",  "light",  "muted",  "pale",  "r4",  "safe",  "vibrant"), ColSeq = TRUE, ColRev = FALSE, SingleColour = NULL, ...){
  if(missing(linethick)) {linethick = fontsize/22}
  if (!missing("alpha")) {
    warning("Use `c_alpha` argument instead, as `alpha` is deprecated.")
    c_alpha <- substitute(alpha)}
  ColPal <- match.arg(ColPal)
  PlotType <- match.arg(PlotType)
  if (!(PlotType %in% c("Counts", "Normalised counts"))) {
    stop("`PlotType` can only be 'Counts' or 'Normalised Counts'.")
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
                                          aes({{ ycol }},
                                              fill = "one",
                                              colour = "one")) + 
                       scale_fill_manual(values = a)+
                       scale_colour_manual(values = a)+
                       guides(fill = "none", colour = "none")) 
  } else {
    suppressWarnings(P <- ggplot2::ggplot(data, 
                                          aes({{ ycol }},
                                              fill = factor({{ group }}),
                                              colour = factor({{ group }}))) +
                       scale_fill_grafify(palette = ColPal, 
                                          reverse = ColRev, 
                                          ColSeq = ColSeq)+
                       scale_colour_grafify(palette = ColPal, 
                                            reverse = ColRev, 
                                            ColSeq = ColSeq))
  }
  if(PlotType == "Counts") {
    P <- P +                     
      geom_histogram(linewidth = linethick,
                     alpha = c_alpha,
                     bins = BinSize,
                     #colour = "black"
      )+
      labs(y = "Counts")}
  if(PlotType == "Normalised counts") {
    P <- P +                     
      geom_histogram(linewidth = linethick,
                     alpha = c_alpha,
                     bins = BinSize,
                     #colour = "black",
                     aes(y = after_stat(count/max(count))))+
      labs(y = "Normalised counts")}
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
    labs(fill = enquo(group))+
    labs(colour = enquo(group))+
    theme_grafify(base_size = fontsize)+
    theme(strip.background = element_blank())+
    guides(x = guide_axis(angle = TextXAngle))
  P
}
