#' See grafify colour palettes
#' 
#' This simple function allows quick visualisation of colours in grafify palettes and their hex codes. It uses \code{plot_bar_sd} and some arguments are similar and can be adjusted.
#'
#' @param palette name of grafify palettes: "okabe_ito", "vibrant, "bright", "pale", "muted", "dark", "light", "contrast" or "all_grafify".
#' @param bthick thickness of bars; passed on \code{plot_bar_sd}.
#' @param fontsize font size.
#' @param ... any additional parameters to pass to \code{plot_bar_sd}
#'
#' @return This function returns a \code{ggplot2} object of class "gg" and "ggplot".
#' @export plot_grafify_palette
#' @import ggplot2
#' @importFrom stats reorder
#'
#' @examples
#' plot_grafify_palette("pale")
#' plot_grafify_palette("contrast")
#' 
plot_grafify_palette <- function(palette = "okabe_ito", bthick = 0, fontsize = 14, ...) {
  t1 <- data.frame(graf_palettes[palette])
  t1$colour_name <- rownames(t1)
  t1$number <- -1*seq(from = 1, to = nrow(t1))
  t1$names <- paste0(t1[,2], "_", t1[,1])
  suppressWarnings(P <- plot_bar_sd(data = t1, xcol = reorder(t1$names, t1$number),
                     ycol = .1, ColPal = {{ palette }},
                     TextXAngle = 45, ColRev = T, ColSeq = T,
                     fontsize = fontsize, bthick = bthick, ...)+ guides(fill = "none")+
    theme(axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())+
    labs(title = paste0("grafify palette: ", palette))+
    coord_flip())+scale_y_reverse()
    P
}
