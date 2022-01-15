#' Plot a dotplot on a boxplot with two variables.
#'
#' This function is related to \code{plot_dotbox} which maps the X variable to different fill colours, but this one maps a single or same colour, therefore `_sc`. The only new argument is `colour`, which can be any hexcode or name of colours in the `all_grafify` [palette](https://grafify-vignettes.netlify.app/colour_palettes.html#full-list-of-hexcodes).
#' 
#' The default colour is `ok_orange`. `ColPal` and `ColRev` arguments are not available. Colours available can be seen quickly with \code{\link{plot_grafify_palette}}.
#'
#' @param data a data table object, e.g. data.frame or tibble.
#' @param xcol name of the column to plot on X axis. This should be a categorical variable.
#' @param ycol name of the column to plot on quantitative Y axis. This should be a quantitative variable.
#' @param colour colour of boxes and dots; a number between 1-64, any hexcode or names from `grafify` colour palettes. Default is `ok_orange`.
#' @param dotsize size of dots relative to binwidth used by \code{geom_dotplot}. Default set to 1.5, increase/decrease as needed.
#' @param dotthick thickness of dot border (`stroke` parameter of `geom_dotplot`), default set to 1.
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param b_alpha fractional opacity of boxes, default set to 1 (i.e. maximum opacity & zero transparency).
#' @param d_alpha fractional opacity of dots, default set to 1 (i.e. maximum opacity & zero transparency).
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text.
#' @param ... any additional arguments to pass to \code{ggplot2}[geom_boxplot] or \code{ggplot2}[geom_dotplot].
#'
#' @return This function returns a \code{ggplot2} object of class "gg" and "ggplot".
#' @export plot_dotbox_sc
#' @import ggplot2
#'
#' @examples
#'
#' #with default colour ("okabe_ito")
#' plot_dotbox_sc(data = data_doubling_time, 
#' xcol = Student, ycol = Doubling_time)
#' #set a different colour
#' plot_dotbox_sc(data = data_doubling_time, 
#' xcol = Student, ycol = Doubling_time, 
#' colour = "pale_blue")
#'


plot_dotbox_sc <- function(data, xcol, ycol, colour = "ok_orange", dotsize = 1.5, dotthick = 1,  b_alpha = 1, d_alpha = 1, TextXAngle = 0, fontsize = 20, ...){

ifelse(grepl("#", colour), 
       a <- colour,
       a <- get_graf_colours({{ colour }}))
 suppressWarnings(P <- ggplot2::ggplot(data, aes(x = factor({{ xcol }}),
                            y = {{ ycol }}))+
    geom_boxplot(size = 1,
                 alpha = {{ b_alpha }},
                 outlier.alpha = 0,
                 width = 0.7,
                 fill = a,
                 ...)+
    geom_dotplot(stackdir = "center", 
                 stroke = {{ dotthick }},
                 alpha = {{ d_alpha }},
                 binaxis = 'y', 
                 dotsize = {{ dotsize }},
                 fill = a,
                 ...)+
    labs(x = enquo(xcol))+
    theme_classic(base_size = {{ fontsize }})+
    theme(strip.background = element_blank())+
    guides(x = guide_axis(angle = {{ TextXAngle }})))
 P
}
