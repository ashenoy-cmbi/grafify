#' Add log transformations to graphs
#' 
#' This function allows "log10" or "log2" transformation of X or Y axes. With "log10" transformation, log10 ticks are also added on the outside. 
#' 
#' Arguments allow for axes limits, breaks and labels to passed on. 
#'
#' @param Plot a ggplot2 object. 
#' @param LogYTrans transform Y axis into "log10" (default) or "log2"
#' @param LogXTrans transform X axis into "log10" or "log2"
#' @param LogYBreaks argument for \code{\link[ggplot2]{scale_y_continuous}} for Y axis breaks on log scales, default is `waiver()`, or provide a vector of desired breaks.
#' @param LogXBreaks argument for \code{\link[ggplot2]{scale_x_continuous}} for Y axis breaks on log scales, default is `waiver()`, or provide a vector of desired breaks.
#' @param LogYLimits a vector of length two specifying the range (minimum and maximum) of the Y axis.
#' @param LogXLimits a vector of length two specifying the range (minimum and maximum) of the X axis.
#' @param LogYLabels argument for \code{\link[ggplot2]{scale_y_continuous}} for Y axis labels on log scales, default is `waiver()`, or provide a vector of desired labels. 
#' @param LogXLabels argument for \code{\link[ggplot2]{scale_x_continuous}} for Y axis labels on log scales, default is `waiver()`, or provide a vector of desired labels. 
#' @param fontsize this parameter sets the linewidth of the `log10` tickmarks (`8*fontsize/22` for long ticks and `4*fontsize/22` for middle ticks). It is set to 20 as default to be consistent with rest of `grafify`. It will need to be changed to 12, which is the default fontsize for graphs produced natively with `ggplot2`.
#' @param ... any other arguments to pass to \code{\link[ggplot2]{scale_y_continuous}} or \code{\link[ggplot2]{scale_x_continuous}}
#'
#' @return This function returns a \code{ggplot2} object of class "gg" and "ggplot".
#' @importFrom ggplot2 scale_y_continuous scale_x_continuous annotation_logticks coord_cartesian
#' @export plot_logscale
#'
#' @examples
#' #save a ggplot object
#' P <- ggplot(data_t_pratio, 
#' aes(Genotype,Cytokine))+
#' geom_jitter(shape = 21, 
#' size = 5, width = .2, 
#' aes(fill = Genotype), 
#' alpha = .7)
#' #transform Y axis
#' plot_logscale(Plot = P)
#' 
#' #or in one go 
#' plot_logscale(ggplot(data_t_pratio, 
#' aes(Genotype,Cytokine))+
#' geom_jitter(shape = 21, 
#' size = 5, width = .2, 
#' aes(fill = Genotype), 
#' alpha = .7))
#' 
plot_logscale <- function(Plot, LogYTrans = "log10", LogXTrans, LogYBreaks = waiver(), LogXBreaks = waiver(), LogYLimits = NULL, LogXLimits = NULL, LogYLabels = waiver(), LogXLabels = waiver(), fontsize = 22, ...){
  P <- Plot + #add margins
    theme(axis.text.x.bottom = element_text(margin = margin(t = fontsize/4,
                                                            unit = "pt")),
          axis.text.y.left = element_text(margin = margin(r = fontsize/4,
                                                          unit = "pt")))
  if (!(LogYTrans %in% c("log2", "log10"))) {
    stop("LogYTrans only allows 'log2' or 'log10' transformation.")
  }
  if (LogYTrans == "log10") {
    suppressWarnings(P <- P + 
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
                       coord_cartesian(clip = "off", ...))
  }
  if (LogYTrans == "log2") {
    suppressWarnings(P <- P + 
                       scale_y_continuous(trans = "log2", 
                                          breaks = LogYBreaks, 
                                          labels = LogYLabels, 
                                          limits = LogYLimits,
                                          ...))}
  if(!missing(LogXTrans)){
    if (!(LogXTrans %in% c("log2", "log10"))) {
      stop("LogXTrans only allows 'log2' or 'log10' transformation.")
    }
    if (LogXTrans == "log10") {
      suppressWarnings(P <- P + 
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
                         coord_cartesian(clip = "off", ...))
    }
    if (LogXTrans == "log2") {
      suppressWarnings(P <- P + 
                         scale_x_continuous(trans = "log2", 
                                            breaks = LogXBreaks, 
                                            labels = LogXLabels, 
                                            limits = LogXLimits,
                                            ...))
    }
  }
  P
}
