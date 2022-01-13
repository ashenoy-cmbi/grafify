#' Plot points on a quantitative X - Y plot & a categorical grouping variable.
#'
#' This function takes a data table, quantitative X and Y variables along with a categorical grouping variable, and a and plots a graph with using \code{\link[ggplot2]{geom_point}}. The categorical `CatGroup` variable is mapped to the \code{fill} aesthetic of symbols.
#' 
#' Colours can be changed using `ColPal`, `ColRev` or `ColSeq` arguments. Colours available can be seen quickly with \code{\link{plot_grafify_palette}}.
#' `ColPal` can be one of the following: "okabe_ito", "dark", "light", "bright", "pale", "vibrant,  "muted" or "contrast".
#' `ColRev` (logical TRUE/FALSE) decides whether colours are chosen from first-to-last or last-to-first from within the chosen palette. 
#' `ColSeq` (logical TRUE/FALSE) decides whether colours are picked by respecting the order in the palette or the most distant ones using \code{\link[grDevices]{colorRampPalette}}.
#' 
#' This plot is related to \code{\link{plot_xy_NumGroup}} which requires a numeric grouping factor.
#' When summary statistics (mean/median) are required, use \code{\link{plot_3d_scatterbar}}, \code{\link{plot_3d_scatterbox}} or \code{\link{plot_4d_scatterbox}}. 
#'
#' @param data a data table object, e.g. data.frame or tibble.
#' @param xcol name of the column with quantitative X variable
#' @param ycol name of the column with quantitative Y variable
#' @param CatGroup a categorical variable as grouping factor for colour of data points, should be a categorical variable for default colours to work. 
#' Will be converted to `factor` if your column is numeric
#' @param symsize size of symbols used by \code{geom_point}. Default set to 2.5, increase/decrease as needed.
#' @param symthick thickness of symbol border, default set to 1
#' @param s_alpha fractional opacity of symbols, default set to 1 (i.e. maximum opacity & zero transparency)
#' @param ColPal grafify colour palette to apply, default "all_grafify"; alternatives: "okabe_ito", "bright", "pale", "vibrant", "contrast", "muted" "dark", "light".
#' @param ColSeq logical TRUE or FALSE. Default TRUE for sequential colours from chosen palette. Set to FALSE for distant colours, which will be applied using  \code{scale_fill_grafify2}.
#' @param ColRev whether to reverse order of colour choice, default F (FALSE); can be set to T (TRUE)
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#'
#' @return This function returns a \code{ggplot2} object.
#' @export plot_xy_CatGroup
#' @import ggplot2
#'
#' @examples
#' #The grouping factor cyl  is automatically converted to categorical variable
#' plot_xy_CatGroup(data = mtcars,
#' xcol = mpg, ycol = disp, CatGroup = cyl, 
#' ColPal = "vibrant", ColSeq = FALSE)

plot_xy_CatGroup <- function(data, xcol, ycol, CatGroup, symsize = 2.5, symthick = 1, s_alpha = 1, ColPal = "all_grafify", ColSeq = TRUE, ColRev = FALSE, TextXAngle = 0, fontsize = 20){
  P <- ggplot2::ggplot(data, aes(x = {{ xcol }},
                            y = {{ ycol }}))+
    geom_point(size = {{ symsize }}, 
               alpha = {{ s_alpha }},
               aes(fill = factor({{ CatGroup }})),
               shape = 21, 
               stroke = {{ symthick }})+
    labs(fill = enquo(CatGroup))+
    theme_classic(base_size = {{ fontsize }})+
    theme(strip.background = element_blank())+
    guides(x = guide_axis(angle = {{ TextXAngle }}))
  if (ColSeq) {
    P <- P + scale_fill_grafify(palette = {{ ColPal }}, reverse = {{ ColRev }})
  } else {
    P <- P + scale_fill_grafify2(palette = {{ ColPal }}, reverse = {{ ColRev }})}
  P
}
