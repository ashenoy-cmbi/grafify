#' Plot a dotplot on a violin plot with two variables.
#'
#' This function takes a data table, X and Y variables, and plots a graph with a dotplot, box-whiskers and violinplot using \code{\link[ggplot2]{geom_violin}}, \code{\link[ggplot2]{geom_boxplot}}  \code{\link[ggplot2]{geom_dotplot}} geometries. 
#' 
#' Note that the \code{\link{geom_violin}} options are set as follows: \code{scale = "width"}. The \code{trim = T} set by default can be changed when calling the function.
#' The boxplot shows IQR, and whiskers show 1.5*IQR; the median is marked with a thicker horizontal line.
#' 
#' The X variable is mapped to the \code{fill} aesthetic in both violinplot and dotplot.
#' Colours can be changed using `ColPal`, `ColRev` or `ColSeq` arguments. Colours available can be seen quickly with \code{\link{plot_grafify_palette}}.
#' `ColPal` can be one of the following: "okabe_ito", "dark", "light", "bright", "pale", "vibrant,  "muted" or "contrast".
#' `ColRev` (logical TRUE/FALSE) decides whether colours are chosen from first-to-last or last-to-first from within the chosen palette. 
#' `ColSeq` (logical TRUE/FALSE) decides whether colours are picked by respecting the order in the palette or the most distant ones using \code{\link[grDevices]{colorRampPalette}}.
#' 
#' If there are many groups along the X axis and you prefer a single colour for the graph,use the `SingleColour` argument.
#' 
#' The size of dots can be adjusted using the parameter, which is \code{dotsize = 1} by default.
#'
#' This function is related to \code{\link{plot_scatterbar_sd}}, \code{\link{plot_dotbar_sd}} and \code{\link{plot_dotviolin}}.
#'
#' @param data a data table object, e.g. data.frame or tibble.
#' @param xcol name of the column to plot on X axis. This should be a categorical variable.
#' @param ycol name of the column to plot on quantitative Y axis. This should be a quantitative variable.
#' @param dotsize size of dots relative to \code{binwidth} used by \code{geom_dotplot}. Default set to 1.5, increase/decrease as needed.
#' @param dotthick thickness of dot border (`stroke` parameter of `geom_dotplot`), default set to 1.
#' @param bvthick thickness of violin and boxplot lines; default 1.
#' @param bwid width of boxplots; default 0.2
#' @param b_alpha fractional opacity of boxplots.  Default is set to 0, which results in white boxes inside violins. Change to any value >0 up to 1 for different levels of transparency. 
#' @param v_alpha fractional opacity of violins, default set to 1 (i.e. maximum opacity & zero transparency).
#' @param d_alpha fractional opacity of dots, default set to 1 (i.e. maximum opacity & zero transparency).
#' @param ColPal grafify colour palette to apply, default "okabe_ito"; see \code{\link{graf_palettes}} for available palettes.
#' @param ColRev whether to reverse order of colour within the selected palette, default F (FALSE); can be set to T (TRUE).
#' @param ColSeq logical TRUE or FALSE. Default TRUE for sequential colours from chosen palette. Set to FALSE for distant colours, which will be applied using  \code{scale_fill_grafify2}.
#' @param SingleColour a colour hexcode (starting with #), a number between 1-154, or names of colours from `grafify` colour palettes to fill along X-axis aesthetic. Accepts any colour other than "black"; use `grey_lin11`, which is almost black.
#' @param trim set whether tips of violin plot should be trimmed at high/low data. Default \code{trim = T}, can be changed to F.
#' @param scale set to "area" by default, can be changed to "count" or "width".
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text.
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param ... any additional arguments to pass to \code{ggplot2}[geom_boxplot], \code{ggplot2}[geom_dotplot] or \code{ggplot2}[geom_violin].
#'
#' @return This function returns a \code{ggplot2} object of class "gg" and "ggplot".
#' @export plot_dotviolin
#' @import ggplot2
#'
#' @examples
#'
#' #plot with trim = FALSE
#' plot_dotviolin(data = data_t_pdiff, 
#' xcol = Condition, ycol = Mass, 
#' dotsize = 2, trim = FALSE)
#' 
#' plot_dotviolin(data = data_t_pdiff, 
#' xcol = Condition, ycol = Mass,
#' trim = FALSE, b_alpha = 0.5, 
#' ColPal = "pale", ColSeq = FALSE)
#' 
#' #single colour along X
#' plot_dotviolin(data = data_t_pdiff, 
#' xcol = Condition, ycol = Mass,
#' trim = FALSE, b_alpha = 0.5, 
#' SingleColour = "pale_cyan")


plot_dotviolin <- function(data, xcol, ycol, dotsize = 1.5, dotthick = 1, bvthick = 1, bwid = 0.2, trim = TRUE, scale = "width", b_alpha = 0, v_alpha = 1, d_alpha = 1, ColPal = c("okabe_ito", "all_grafify", "bright",  "contrast",  "dark",  "fishy",  "kelly",  "light",  "muted",  "pale",  "r4",  "safe",  "vibrant"), ColRev = FALSE, ColSeq = TRUE, SingleColour = "NULL", TextXAngle = 0, fontsize = 20, ...){
  ColPal <- match.arg(ColPal)
  if (missing(SingleColour)){
    if (b_alpha == 0) {
      suppressWarnings(P <- ggplot2::ggplot(data, aes(x = factor({{ xcol }}),
                                                      y = {{ ycol }}))+
                         geom_violin(aes(fill = factor({{ xcol }})),
                                     alpha = {{ v_alpha }},
                                     trim = {{ trim }},
                                     scale = {{ scale }},
                                     colour = "black", 
                                     size = {{ bvthick }},
                                     ...)+
                         geom_boxplot(fill = "white",
                                      colour = "black", 
                                      size = {{ bvthick }},
                                      outlier.alpha = 0,
                                      width = {{ bwid }},
                                      ...)+
                         geom_dotplot(stackdir = "center", 
                                      stroke = {{ dotthick }}, 
                                      alpha = {{ d_alpha }},
                                      dotsize = {{ dotsize }},
                                      binaxis = 'y',
                                      aes(fill = factor({{ xcol }})),
                                      ...)+
                         labs(x = enquo(xcol),
                              fill = enquo(xcol))+
                         theme_classic(base_size = {{ fontsize }})+
                         theme(strip.background = element_blank())+
                         guides(x = guide_axis(angle = {{ TextXAngle }})))
    } else {
      suppressWarnings(P <- ggplot2::ggplot(data, aes(x = factor({{ xcol }}),
                                                      y = {{ ycol }}))+
                         geom_violin(aes(fill = factor({{ xcol }})),
                                     alpha = {{ v_alpha }},
                                     trim = {{ trim }},
                                     scale = {{ scale }},
                                     colour = "black", 
                                     size = {{ bvthick }},
                                     ...)+
                         geom_boxplot(aes(fill = factor({{ xcol }})),
                                      alpha = {{ b_alpha }},
                                      colour = "black", 
                                      size = {{ bvthick }},
                                      outlier.alpha = 0,
                                      width = {{ bwid }},
                                      ...)+
                         geom_dotplot(stackdir = "center", 
                                      stroke = {{ dotthick }}, 
                                      alpha = {{ d_alpha }},
                                      dotsize = {{ dotsize }},
                                      binaxis = 'y',
                                      aes(fill = factor({{ xcol }})),
                                      ...)+
                         labs(x = enquo(xcol),
                              fill = enquo(xcol))+
                         theme_classic(base_size = {{ fontsize }})+
                         theme(strip.background = element_blank())+
                         guides(x = guide_axis(angle = {{ TextXAngle }}))) 
    }
    P <- P + scale_fill_grafify(palette = {{ ColPal }}, 
                                reverse = {{ ColRev }}, 
                                ColSeq = {{ ColSeq }})
  } else {
    ifelse(grepl("#", SingleColour), 
           a <- SingleColour,
           a <- get_graf_colours({{ SingleColour }}))
    if (b_alpha == 0){
      suppressWarnings(P <- ggplot2::ggplot(data, aes(x = factor({{ xcol }}),
                                                      y = {{ ycol }}))+
                         geom_violin(fill = a,
                                     alpha = {{ v_alpha }},
                                     trim = {{ trim }},
                                     scale = {{ scale }},
                                     colour = "black", 
                                     size = {{ bvthick }},
                                     ...)+
                         geom_boxplot(fill = "white",
                                      colour = "black", 
                                      size = {{ bvthick }},
                                      outlier.alpha = 0,
                                      width = {{ bwid }},
                                      ...)+
                         geom_dotplot(stackdir = "center", 
                                      stroke = {{ dotthick }}, 
                                      alpha = {{ d_alpha }},
                                      dotsize = {{ dotsize }},
                                      binaxis = 'y',
                                      fill = a,
                                      ...)+
                         labs(x = enquo(xcol))+
                         theme_classic(base_size = {{ fontsize }})+
                         theme(strip.background = element_blank())+
                         guides(x = guide_axis(angle = {{ TextXAngle }})))
    } else {
      suppressWarnings(P <- ggplot2::ggplot(data, aes(x = factor({{ xcol }}),
                                                      y = {{ ycol }}))+
                         geom_violin(fill = a,
                                     alpha = {{ v_alpha }},
                                     trim = {{ trim }},
                                     scale = {{ scale }},
                                     colour = "black", 
                                     size = {{ bvthick }},
                                     ...)+
                         geom_boxplot(fill = a,
                                      alpha = {{ b_alpha }},
                                      colour = "black", 
                                      size = {{ bvthick }},
                                      outlier.alpha = 0,
                                      width = {{ bwid }},
                                      ...)+
                         geom_dotplot(stackdir = "center", 
                                      stroke = {{ dotthick }}, 
                                      alpha = {{ d_alpha }},
                                      dotsize = {{ dotsize }},
                                      binaxis = 'y',
                                      fill = a,
                                      ...)+
                         labs(x = enquo(xcol))+
                         theme_classic(base_size = {{ fontsize }})+
                         theme(strip.background = element_blank())+
                         guides(x = guide_axis(angle = {{ TextXAngle }})))
    }
  }
  P
}
