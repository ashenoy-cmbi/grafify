#' Plot a dotplot on a violin plot with two variables.
#'
#' This function is related to \code{plot_dotviolin}, but this one maps a single or same colour, therefore `_sc`.
#' The only new argument is `colour`, which can be any hexcode or name of colours in the `all_grafify` [palette](https://grafify-vignettes.netlify.app/colour_palettes.html#full-list-of-hexcodes).
#' The default colour is `ok_orange`. `ColPal` and `ColRev` arguments are not available. Colours available can be seen quickly with \code{\link{plot_grafify_palette}}.
#'
#' @param data a data table object, e.g. data.frame or tibble.
#' @param xcol name of the column to plot on X axis. This should be a categorical variable.
#' @param ycol name of the column to plot on quantitative Y axis. This should be a quantitative variable.
#' @param colour colour of boxes and dots; a number between 1-64, any hexcode or names from `grafify` colour palettes. Default is `ok_orange`.
#' @param dotsize size of dots relative to \code{binwidth} used by \code{geom_dotplot}. Default set to 1.5, increase/decrease as needed.
#' @param b_alpha fractional opacity of violins, default set to 1 (i.e. maximum opacity & zero transparency). For white boxplots inside violins, set `b_alpha = 0`.
#' @param d_alpha fractional opacity of dots, default set to 1 (i.e. maximum opacity & zero transparency).
#' @param v_alpha fractional opacity of violins, default set to 1 (i.e. maximum opacity & zero transparency)
#' @param bvthick thickness of violin an boxplot lines; default 1.
#' @param dotthick thickness of dot border (`stroke` parameter of `geom_dotplot`), default set to 1.
#' @param bwid width of boxplots; default 0.2
#' @param trim set whether tips of violin plot should be trimmed at high/low data. Default \code{trim = T}, can be changed to F.
#' @param scale set to "area" by default, can be changed to "count" or "width".
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text.
#' @param ... any additional arguments to pass to \code{ggplot2}[geom_boxplot], \code{ggplot2}[geom_dotplot] or \code{ggplot2}[geom_violin].
#'
#' @return This function returns a \code{ggplot2} object of class "gg" and "ggplot".
#' @export plot_dotviolin_sc
#' @import ggplot2
#'
#' @examples
#'
#' #plot with trim = FALSE
#' plot_dotviolin_sc(data = data_1w_death, 
#' xcol = Genotype, ycol = Death, 
#' scale = "width", trim = FALSE)
#' #white boxplots
#' plot_dotviolin_sc(data = data_1w_death, 
#' xcol = Genotype, ycol = Death, colour = "light_orange", 
#' scale = "width", trim = FALSE, b_alpha = 0)
#' 

plot_dotviolin_sc <- function(data, xcol, ycol, colour = "ok_orange", dotsize = 1.5,  dotthick = 1, bvthick = 1, bwid = 0.2, b_alpha = 1, d_alpha = 1, v_alpha = 1,  trim = TRUE, scale = "width", TextXAngle = 0, fontsize = 20, ...){
  
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
  P
}
