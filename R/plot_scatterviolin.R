#' Plot a scatter plot on a violin plot with two variables.
#'
#' This function takes a data table, X and Y variables, and plots a graph with a scatter plot and violinplot using \code{ggplot}.
#'
#' The function uses \code{\link[ggplot2]{geom_violin}}, \code{\link[ggplot2]{geom_boxplot}}  and \code{\link[ggplot2]{geom_point}} geometries.
#' Note that the \code{\link{geom_violin}} options are set as follows: \code{scale = "width"}. The \code{trim = T} set by default can be changed when calling the function.
#' The boxplot shows IQR and the median is marked with a thicker horizontal line, and whisker depicts 1.5*IQR.
#' The X variable is mapped to the \code{fill} aesthetic in both violin and symbols, and its colour can be changed using `ColPal` option. 
#' Colours can be changed using `ColPal`, `ColRev` or `ColSeq` arguments. Colours available can be seen quickly with \code{\link{plot_grafify_palette}}.
#' `ColPal` can be one of the following: "okabe_ito", "dark", "light", "bright", "pale", "vibrant,  "muted" or "contrast".
#' `ColRev` (logical TRUE/FALSE) decides whether colours are chosen from first-to-last or last-to-first from within the chosen palette. 
#' `ColSeq` (logical TRUE/FALSE) decides whether colours are picked by respecting the order in the palette or the most distant ones using \code{\link[grDevices]{colorRampPalette}}.
#' 
#' The size of symbols can be adjusted using \code{symsize} set to 1 by default.
#' Transparency of violins and symbols can be set independently with `v_alpha` and `s_alpha`, respectively.
#'
#' Three types of plots are available for scatter/jitter symbols and either bars+SD, boxplot or violin plots: \code{\link{plot_scatterbar_sd}}, \code{\link{plot_scatterbox}} and \code{\link{plot_scatterviolin}}.
#' These are related to the three "dot" versions that use a different geometry for symbols: \code{\link{plot_dotbox}}, \code{\link{plot_dotbar_sd}} and \code{\link{plot_dotviolin}}.
#'
#' @param data a data table object, e.g. data.frame or tibble
#' @param xcol name of the column to plot on X axis. This should be a categorical variable.
#' @param ycol name of the column to plot on quantitative Y axis. This should be a quantitative variable.
#' @param symsize size of dots relative to \code{binwidth} used by \code{geom_point}. Default set to 2.5, increase/decrease as needed.
#' @param symthick thickness of dot border (`stroke` parameter of `geom_point`), default set to 1
#' @param bvthick thickness of both violin and boxplot lines; default 1
#' @param bwid width of boxplots; default 0.2
#' @param b_alpha fractional opacity of boxplots, default set to 1 (i.e. maximum opacity & zero transparency). For white boxplots inside violins, set `b_alpha = 0`.
#' @param v_alpha fractional opacity of violins, default set to 1 (i.e. maximum opacity & zero transparency)
#' @param s_alpha fractional opacity of symbols, default set to 1 (i.e. maximum opacity & zero transparency). Set `s_alpha = 0` to not show scatter plot.
#' @param ColPal grafify colour palette to apply, default "all_grafify"; alternatives: "okabe_ito", "bright", "pale", "vibrant", "contrast", "muted" "dark", "light".
#' @param ColRev whether to reverse order of colour choice, default F (FALSE); can be set to T (TRUE)
#' @param ColSeq logical TRUE or FALSE. Default TRUE for sequential colours from chosen palette. Set to FALSE for distant colours, which will be applied using  \code{scale_fill_grafify2}.
#' @param jitter extent of jitter (scatter) of symbols, default is 0 (i.e. aligned symbols). To reduce symbol overlap, try 0.1-0.3 or higher.  
#' @param trim set whether tips of violin plot should be trimmed at high/low data. Default \code{trim = T}, can be changed to F.
#' @param scale set to "area" by default, can be changed to "count" or "width".
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param ... any additional arguments to pass to \code{ggplot2}[geom_boxplot], \code{ggplot2}[geom_point] or \code{ggplot2}[geom_violin].
#'
#' @return This function returns a \code{ggplot2} object on which additional geometries etc. can be added.
#' @export plot_scatterviolin
#' @import ggplot2
#'
#' @examples
#'
#' #plot without jitter
#' plot_scatterviolin(data = data_t_pdiff, 
#' xcol = Condition, ycol = Mass, 
#' symsize = 2, trim = FALSE)
#' 
#' #with jitter
#' plot_scatterviolin(data = data_t_pdiff, 
#' xcol = Condition, ycol = Mass, 
#' symsize = 2, trim = FALSE, jitter = 0.1)
#' 
#' #white boxplot and no symbols
#' plot_scatterviolin(data = data_t_pdiff, 
#' xcol = Condition, ycol = Mass, 
#' b_alpha = 0, s_alpha = 0,
#' symsize = 2, trim = FALSE, jitter = 0.1)
#'
#'

plot_scatterviolin <- function(data, xcol, ycol, symsize = 2.5, symthick = 1, bwid = 0.1, bvthick = 1, b_alpha = 1, s_alpha = 1, v_alpha = 1, ColPal = "all_grafify", ColSeq = TRUE, ColRev = FALSE, jitter = 0, trim = TRUE, scale = "width", TextXAngle = 0, fontsize = 20, ...){
  if (b_alpha == 0){
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
      geom_point(shape = 21,
                 position = position_jitter(width = {{ jitter }}),
                 alpha = {{ s_alpha }},
                 stroke = {{ symthick }},
                 size = {{ symsize }},
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
      geom_point(shape = 21,
                 position = position_jitter(width = {{ jitter }}),
                 alpha = {{ s_alpha }},
                 stroke = {{ symthick }},
                 size = {{ symsize }},
                 aes(fill = factor({{ xcol }})),
                 ...)+
      labs(x = enquo(xcol),
           fill = enquo(xcol))+
      theme_classic(base_size = {{ fontsize }})+
      theme(strip.background = element_blank())+
      guides(x = guide_axis(angle = {{ TextXAngle }})))
  }
  if (ColSeq) {
    P <- P + scale_fill_grafify(palette = {{ ColPal }}, reverse = {{ ColRev }})
  } else {
    P <- P + scale_fill_grafify2(palette = {{ ColPal }}, reverse = {{ ColRev }})}
  P
}
