#' Plot points on a quantitative X - Y plot & a numeric grouping variable.
#'
#' This function takes a data table, quantitative X and Y variables, and a numeric grouping variable, and a and plots a graph with using \code{\link[ggplot2]{geom_point}}. The numerical `NumGroup` variable is mapped to the \code{fill} aesthetic of symbols, which receives the `scale_fill_grafify_c` default palette.
#' 
#' This plot is related to \code{\link{plot_xy_CatGroup}} which requires a categorical grouping factor.
#' When summary statistics (mean/median) are required, use \code{\link{plot_3d_scatterbar}}, \code{\link{plot_3d_scatterbox}} or \code{\link{plot_4d_scatterbox}}. 
#'
#' @param data a data table object, e.g. data.frame or tibble.
#' @param xcol name of the column with quantitative X variable.
#' @param ycol name of the column with quantitative Y variable.
#' @param NumGroup a numeric factor for `fill` aesthetic of data points.
#' @param symsize size of symbols used by \code{geom_point}. Default set to 2.5, increase/decrease as needed.
#' @param symthick thickness of symbol border, default set to 1.
#' @param s_alpha fractional opacity of symbols, default set to 1 (i.e. maximum opacity & zero transparency).
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text.
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
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

plot_xy_NumGroup <- function(data, xcol, ycol, NumGroup, symsize = 2.5, symthick = 1, s_alpha = 1, TextXAngle = 0, fontsize = 20){
  ggplot2::ggplot(data, aes(x = {{ xcol }},
                            y = {{ ycol }}))+
    geom_point(size = {{ symsize }}, 
               alpha = {{ s_alpha }},
               aes(fill = {{ NumGroup }}),
               shape = 21, 
               stroke = {{ symthick }})+
    theme_classic(base_size = {{ fontsize }})+
    theme(strip.background = element_blank())+
    guides(x = guide_axis(angle = {{ TextXAngle }}))+
    scale_fill_grafify_c()
}
