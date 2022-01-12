#' Plot a scatter plot on a violin plot with two variables.
#'
#' This function is related to \code{plot_scatterviolin}, but this one maps a single or same colour, therefore `_sc`.
#' The only new argument is `colour`, which can be any hexcode or name of colours in the `all_grafify` [palette](https://grafify-vignettes.netlify.app/colour_palettes.html#full-list-of-hexcodes).
#' The default colour is `ok_orange`. `ColPal` and `ColRev` arguments are not available.
#'
#' @param data a data table object, e.g. data.frame or tibble
#' @param xcol name of the column to plot on X axis. This should be a categorical variable.
#' @param ycol name of the column to plot on quantitative Y axis. This should be a quantitative variable.
#' @param colour colour of boxes and dots; a number between 1-64, any hexcode or names from `grafify` colour palettes. Default is `ok_orange`.
#' @param symsize size of dots relative to \code{binwidth} used by \code{geom_point}. Default set to 2.5, increase/decrease as needed.
#' @param symthick thickness of dot border (`stroke` parameter of `geom_point`), default set to 1
#' @param bvthick thickeness of both violin and box plot lines; default 1
#' @param bwid width of boxplots; default 0.2
#' @param b_alpha fractional opacity of boxplots, default set to 1 (i.e. maximum opacity & zero transparency). For white boxplots inside violins, set `b_alpha = 0`.
#' @param v_alpha fractional opacity of violins, default set to 1 (i.e. maximum opacity & zero transparency).  Set `s_alpha = 0` to not show scatter plot.
#' @param s_alpha fractional opacity of symbols, default set to 1 (i.e. maximum opacity & zero transparency)
#' @param jitter extent of jitter (scatter) of symbols, default is 0 (i.e. aligned symbols). To reduce symbol overlap, try 0.1-0.3 or higher.  
#' @param trim set whether tips of violin plot should be trimmed at high/low data. Default \code{trim = T}, can be changed to F.
#' @param scale set to "area" by default, can be changed to "count" or "width".
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param ... any additional arguments to pass to \code{ggplot2}[geom_boxplot], \code{ggplot2}[geom_point] or \code{ggplot2}[geom_violin].
#'
#' @return This function returns a \code{ggplot2} object on which additional geometries etc. can be added.
#' @export plot_scatterviolin_sc
#' @import ggplot2
#'
#' @examples
#' plot_scatterviolin_sc(data = data_doubling_time, 
#' xcol = Student, ycol = Doubling_time, 
#' colour = "ok_grey", 
#' symsize = 2, trim = FALSE, scale = "width")
#' 
#' #white boxplots and no symbols
#' plot_scatterviolin_sc(data = data_1w_death, 
#' xcol = Genotype, ycol = Death, 
#' colour = "pale_blue", b_alpha = 0, s_alpha = 0,
#' symsize = 2, trim = FALSE, scale = "width")


plot_scatterviolin_sc <- function(data, xcol, ycol, colour = "ok_orange", symsize = 2.5, symthick = 1, bwid = 0.2, bvthick = 1, b_alpha = 1, v_alpha = 1, s_alpha = 1, jitter = 0, trim = TRUE, scale = "width", TextXAngle = 0, fontsize = 20, ...){
  ifelse(grepl("#", colour), 
         a <- colour,
         a <- get_graf_colours({{ colour }}))
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
      geom_point(shape = 21,
                 position = position_jitter(width = {{ jitter }}),
                 alpha = {{ s_alpha }},
                 stroke = {{ symthick }},
                 size = {{ symsize }},
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
    geom_point(shape = 21,
               position = position_jitter(width = {{ jitter }}),
               alpha = {{ s_alpha }},
               stroke = {{ symthick }},
               size = {{ symsize }},
               fill = a,
               ...)+
    labs(x = enquo(xcol))+
    theme_classic(base_size = {{ fontsize }})+
    theme(strip.background = element_blank())+
    guides(x = guide_axis(angle = {{ TextXAngle }})))
  }
  P
}
