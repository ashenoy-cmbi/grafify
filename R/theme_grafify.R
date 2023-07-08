#' A modified `theme_classic()` for `grafify`-like graphs.
#' 
#' This is a slightly modified \code{\link{theme_classic}[ggplot2]} with two key differences: no border & background for facet panel labels, and font size of text on axes is the same as that of the axes titles (prior to v3.2.0, this was 0.85 times the base font size). The size of text legend title is also same as base font. 
#' 
#' Since v3.2.0, `theme_grafify` produces transparent backgrounds.
#'
#' @param base_size base font size for all text (default is 20). Other text is relative to this.
#' @param base_family default font family 
#' @param base_line_size default line size (default is base font size/22)
#' @param base_rect_size default size of rectangles (default is base font size/22)
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text. 
#' @param vjust vertical adjustment of X-axis text alignment (between 0 and 1). Set `hjust` and `vjust` to 1 if `TextXAngle = 45`. Try other options if using other angles.
#' @param hjust horizontal adjustment of X-axis text alignment (between 0 and 1).  Set `hjust` and `vjust` to 1 if `TextXAngle = 45`. Try other options if using other angles.
#' @param ... for any other arguments to pass to `theme`. A useful one is `aspect.ratio = 1` for square plots.
#'
#' @return this returns an output with class "theme" and "gg". 
#' @export theme_grafify
#' @importFrom ggplot2 theme_classic
#'
#' @examples
#' 
#' ggplot(mpg, aes(drv, cty, colour = fl))+
#' geom_jitter(width = 0.2, 
#' size = 3, alpha = .7)+
#' theme_grafify()
#' 
theme_grafify <- function(base_size = 20, base_family = "", base_line_size = base_size/22, base_rect_size = base_size/22, TextXAngle = 0, vjust = 0, hjust = 0, ...) {
  theme_classic(base_size = base_size, 
                base_family = base_family, 
                base_line_size = base_line_size, 
                base_rect_size = base_rect_size) %+replace% 
    theme(strip.background = element_blank(), 
          line = element_line(lineend = "square",
                              colour = "black", 
                              linewidth = base_line_size,
                              linetype = 1),
          complete = TRUE,
          axis.text = element_text(size = rel(1),
                                   colour = "grey30"),
          axis.text.x.bottom = element_text(margin = margin(t = base_size/4,
                                                            unit = "pt")),
          axis.text.y.left = element_text(margin = margin(r = base_size/4,
                                                          unit = "pt")),
          legend.title = element_text(size = rel(1)),
          legend.text = element_text(size = rel(1)),
          legend.justification = "top",
          strip.text = element_text(size = rel(1)),
          axis.text.x = element_text(angle = TextXAngle,
                                     vjust = vjust, 
                                     hjust = hjust),
          rect = element_rect(fill = "transparent"),
          panel.background = element_rect(fill = 'transparent',
                                          color = NA_character_),
          plot.background = element_rect(fill = 'transparent',
                                         color = NA_character_),
          ...)
}
