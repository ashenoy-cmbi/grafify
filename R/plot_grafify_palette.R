#' See grafify colour palettes
#' 
#' This simple function allows quick visualisation of colours in grafify palettes and their hex codes. It uses \code{plot_scatterbar_sd} and some arguments are similar and can be adjusted.
#'
#' @param palette name of grafify palettes: "okabe_ito", "vibrant, "bright", "pale", "muted", "dark", "light", "contrast" or "all_grafify".
#' @param fontsize font size.
#' @param ... any additional parameters to pass to \code{\link{plot_scatterbar_sd}}
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
plot_grafify_palette <- function(palette = c("okabe_ito", "all_grafify", "bright",  "contrast",  "dark",  "fishy",  "kelly",  "light",  "muted",  "pale",  "r4",  "safe",  "vibrant", "OrBl_div", "PrGn_div", "blue_conti", "grey_conti", "yellow_conti"), fontsize = 14, ...) {
  palette <- match.arg(palette)
  names <- as.vector(names(as.list(graf_palettes[[palette]])))
  names <- factor(names, 
                  levels = rev(names))
  colours <- as.vector(graf_palettes[[palette]])
  t1 <- data.frame(names = names, 
                   colours = colours)
  names2 <- paste0(t1[,1], "_", t1[,2])
  t1$names <- factor(t1$names,
                     levels = names,
                     labels = rev(names2))
  #t1[order(t1$number),]
  suppressWarnings(P <- plot_scatterbar_sd(data = t1, 
                                           xcol = names,
                                           ycol = .1, s_alpha = 0,
                                           ColPal = palette,
                                           ColRev = TRUE,
                                           fontsize = fontsize, 
                                           b_alpha = 1,
                                           ...)+ 
                     guides(fill = "none")+
                     theme(axis.line.x = element_blank(),
                           axis.line.y = element_blank(),
                           axis.text.x = element_blank(),
                           axis.ticks.x = element_blank(),
                           axis.ticks.y = element_blank(),
                           axis.title.x = element_blank(),
                           axis.title.y = element_blank())+
                     labs(title = paste0("grafify palette: ", palette))+
                     coord_flip())
  P
}
