#' Plot data distribution as histograms.
#'
#' This function takes a data table, X and a Grouping variable (if available) and plots a histogram graph using \code{\link[ggplot2]{ggplot}}.
#'
#' The function uses \code{\link{geom_histogram}}).
#' Note that the function requires the quantitative Y variable first, and groups them based on an X variable.
#' The Group variable is mapped to the \code{fill} and \code{colour} aesthetics in \code{geom_histogram}.
#' ColPal & ColRev options are applied to both `fill` and `colour` scales.
#'
#' @param data a data table e.g. data.frame or tibble.
#' @param ycol name of the column containing the quantitative variable whose histogram distribution is to be plotted
#' @param Group name of the column containing a categorical grouping variable
#' @param BinSize bins to use on X-axis, default set to 30. 
#' @param linethick thickness of symbol border, default set to 1
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param alpha fractional opacity of symbols, default set to 0.2 (i.e. 20% opacity)
#' @param ColPal grafify colour palette to apply, default "all_grafify"; alternatives: "okabe_ito", "bright", "pale", "vibrant", "contrast", "muted" "dark", "light".
#' @param ColRev whether to reverse order of colour choice, default F (FALSE); can be set to T (TRUE)
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text
#'
#' @return This function returns a \code{ggplot2} object.
#' @export plot_histogram
#' @import ggplot2
#'
#' @examples
#' #Basic usage
#' plot_histogram(data_t_pratio, Cytokine, Genotype, BinSize = 10)+scale_x_log10()

plot_histogram <- function(data, ycol, Group, BinSize = 30, linethick = 1, fontsize = 20, alpha = 0.2, ColPal = "all_grafify", ColRev = F, TextXAngle = 0){
  ggplot2::ggplot(data, aes(sample = {{ ycol }}))+
    geom_histogram(size = {{ linethick }},
                   alpha = {{ alpha }},
                   bins = {{ BinSize }}, 
                   aes(x = {{ ycol }},
                       fill = {{ Group }},
                       colour = {{ Group }}))+
    labs(fill = enquo(Group),
         colour = enquo(Group),
         y = "Count")+
    theme_classic(base_size = {{ fontsize }})+
    theme(strip.background = element_blank())+
    guides(x = guide_axis(angle = {{ TextXAngle }}))+
    scale_fill_grafify(palette = {{ ColPal }},
                       reverse = {{ ColRev }})+
    scale_colour_grafify(palette = {{ ColPal }}, 
                         reverse = {{ ColRev }})
}
