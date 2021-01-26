#' Plot a dotplot on a boxplot with two variables.
#'
#' This function takes a data table, X and Y variables, and plots a graph with a dotplot and boxplot using \code{ggplot}.
#'
#' The function uses \code{\link[ggplot2]{geom_boxplot}} and \code{\link[ggplot2]{geom_dotplot}} geometries.
#' Note that \code{\link{geom_boxplot}} option for outliers is set to \code{outlier.alpha = 0}.
#' The X variable is mapped to the \code{fill} aesthetic in both boxplot and dotplot, and its colour can be changed using \code{scale_fill_brewer} or any \code{scale_fill...} option. The size of dots can be adjusted using the parameter, which is \code{dotsize = 1} by default.
#'
#' This function is related to \code{\link{plot_scatterbar_sd}}, \code{\link{plot_dotbar_sd}} and \code{\link{plot_dotviolin}}.
#'
#' @param data a data table object, e.g. data.frame or tibble.
#' @param xcol name of the column to plot on X axis. This should be a categorical variable.
#' @param ycol name of the column to plot on quantitative Y axis. This should be a quantitative variable.
#' @param dotsize size of dots relative to binwidth used by \code{geom_dotplot}. Default set to 1.5, increase/decrease as needed.
#' @param dotthick thickness of dot border (`stroke` parameter of `geom_dotplot`), default set to 1
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param alpha fractional opacity of boxplot, default set to 1 (i.e. maximum opacity & zero transparency)
#' @param ColPal grafify colour palette to apply, default "all_grafify"; alternatives: "okabe_ito", "bright", "pale", "vibrant", "contrast", "muted" "dark", "light".
#' @param ColRev whether to reverse order of colour choice, default F (FALSE); can be set to T (TRUE)
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text
#'
#' @return This function returns a \code{ggplot2} object on which additional geometries etc. can be added.
#' @export plot_dotbox
#' @import ggplot2
#'
#' @examples
#'
#' #Basic usage requires a data table and X & Y variables#'
#' plot_dotbox(data_cholesterol, Treatment, Cholesterol)
#'
#' #Transformations of Y variable are possible as follows
#' #' plot_dotbox(data_cholesterol, Treatment, log(Cholesterol))
#'
#' #Additional ggplot layering is possible
#' plot_dotbox(data_cholesterol, Treatment, Cholesterol, dotsize = 2)+
#'    labs(title = "Plot with scatter dots & boxplot")+
#'    scale_colour_grafify()+
#'    facet_wrap("Hospital")

plot_dotbox <- function(data, xcol, ycol, dotsize = 1.5, dotthick = 1, fontsize = 20, alpha = 1, ColPal = "all_grafify", ColRev = F, TextXAngle = 0){
  ggplot2::ggplot(data, aes(x = factor({{ xcol }}),
                            y = {{ ycol }}))+
    geom_boxplot(aes(fill = factor({{ xcol }})), size = 1,
                 alpha = {{ alpha }},
                 outlier.alpha = 0,
                 width = 0.7)+
    geom_dotplot(stackdir = "center", 
                 stroke = {{ dotthick }},
                 binaxis = 'y', 
                 dotsize = {{ dotsize }},
                 aes(fill = factor({{ xcol }})))+
    labs(x = enquo(xcol),
         fill = enquo(xcol))+
    theme_classic(base_size = {{ fontsize }})+
    theme(strip.background = element_blank())+
    guides(x = guide_axis(angle = {{ TextXAngle }}))+
    scale_fill_grafify(palette = {{ ColPal }}, 
                       reverse = {{ ColRev }})
}
