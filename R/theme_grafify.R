#' A modified `theme_classic()` for `grafify`-like graphs.
#' 
#' This is a slightly modified \code{\link{theme_classic}[ggplot2]} with two key differences: no border & background for facet panel labels, and font size of text on axes is 0.85 times that of the axes titles. The size of text legend title is also same as base font. 
#'
#' @param base_size base font size for all text (default is 20). Other text is relative to this.
#' @param base_family default font family 
#' @param base_line_size default line size (default is base font size/22)
#' @param base_rect_size default size of rectangles (default is base font size/22)
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text. 
#' @param ... for any other arguments to pass to `theme`. A useful one is `aspect.ratio = 1` for square plots.
#'
#' @return this returns an output with class "theme" and "gg". 
#' @export theme_grafify
#' @importFrom ggplot2 theme_classic
#'
#' @examples
#' 
#' plot(mpg, aes(drv, cty ))+
#' geom_jitter(width = 0.2)+
#' theme_grafify()
#' 
theme_grafify <- function (base_size = 20, base_family = "", base_line_size = base_size/22, base_rect_size = base_size/22, TextXAngle = 0, ...) {
  theme_classic(base_size = base_size, 
                base_family = base_family, 
                base_line_size = base_line_size, 
                base_rect_size = base_rect_size) %+replace% 
    theme(strip.background = element_blank(), 
          complete = TRUE,
          axis.text = element_text(size = rel(.85),
                                   colour = "grey30"),
          axis.text.x.bottom = element_text(margin = margin(t = base_size/4,
                                                            unit = "pt")),
          axis.text.y.left = element_text(margin = margin(r = base_size/4,
                                                          unit = "pt")),
          legend.title = element_text(size = rel(1)),
          legend.text = element_text(size = rel(.85)),
          legend.justification = "top",
          strip.text = element_text(size = rel(1)),
          axis.text.x = element_text(angle = {{ TextXAngle }}),
          ...)
}
