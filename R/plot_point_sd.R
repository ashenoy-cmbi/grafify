#' Plot a point as mean with SD error bars using two variables.
#'
#' This function takes a data table, X and Y variables, and plots a graph with a summary point showing the mean of data using \code{\link[ggplot2]{ggplot}}.
#'
#' The function uses \code{\link[ggplot2]{stat_summary}} with \code{geom = "point"} with \code{size = 3}.
#' Standard deviation (SD) is plotted through \code{\link[ggplot2]{stat_summary}} calculated using \code{mean_sdl} from the \code{ggplot2} package (get help with \code{?mean_sdl}), and 1x SD is plotted (\code{fun.arg = list(mult = 1)}.
#' The categorical X variable is mapped to the \code{fill} aesthetic in the point geometry, and by default receives `all_grafify` palette. 
#'
#' You are instead encouraged to show all data using the following functions: \code{\link{plot_scatterbar_sd}}, \code{\link{plot_dotbox}}, \code{\link{plot_dotbar_sd}} and \code{\link{plot_dotviolin}}.
#'
#' @param data a data table object, e.g. data.frame or tibble.
#' @param xcol name of the column with a **categorical** X variable.
#' @param ycol name of the column with quantitative Y variable
#' @param symsize size of point symbols, default set to 3.5
#' @param symthick thickness of symbol border, default set to 1
#' @param ewid width of error bars, default set to 0.2
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param ColPal grafify colour palette to apply, default "all_grafify"; alternatives: "okabe_ito", "bright", "pale", "vibrant", "contrast", "muted" "dark", "light".
#' @param ColRev whether to reverse order of colour choice, default F (FALSE); can be set to T (TRUE)
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text
#'
#' @return This function returns a \code{ggplot2} object.
#' @export plot_point_sd
#' @import ggplot2 Hmisc
#'
#' @examples
#' #Basic usage
#' plot_point_sd(data_doubling_time, Student, Doubling_time)
#'

plot_point_sd <- function(data, xcol, ycol, symsize = 3.5, symthick = 1, ewid = 0.2, fontsize = 20, ColPal = "all_grafify", ColRev = F, TextXAngle = 0){
  ggplot2::ggplot(data, aes(x = factor({{ xcol }}),
                            y = {{ ycol }}))+
    stat_summary(geom = "errorbar",
                 fun.data = "mean_sdl", size = 1,
                 fun.args = list(mult = 1),
                 width = {{ ewid }})+
    stat_summary(geom = "point", shape = 21,
                 size = {{ symsize }}, 
                 stroke = {{ symthick }},
                 fun = "mean",
                 aes(fill = factor({{ xcol }})))+
    labs(x = enquo(xcol),
         fill = enquo(xcol))+
    theme_classic(base_size = {{ fontsize }})+
    theme(strip.background = element_blank())+
    guides(x = guide_axis(angle = {{ TextXAngle }}))+
    scale_fill_grafify(palette = {{ ColPal }}, 
                       reverse = {{ ColRev }})
}
