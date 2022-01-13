#' Scale_fill colour scheme
#'
#' `grafify` internally includes colour-blind compatible schemes for fill and colour/color aesthetics.
#' Note that these **only** work for categorical variables. Use the brewer or viridis packages for numeric gradient scales.
#'
#' The default for \code{scale_fill_grafify()}, \code{scale_colour_grafify()} or \code{scale_color_grafify()} is a list of 55 colours as part of \code{palette = "all_grafify"}.
#'
#' Obviously, it is not recommended to use so many colours, but implementing this was easiest to prevent errors when using a lot of categorical variables.
#' 
#' Colours available can be seen quickly with \code{\link{plot_grafify_palette}}. There are eight palettes with 5-10 colours each, which are recommended. These can be called by naming the colour scheme using \code{palette = } argument.
#' Additional options include "okabe_ito", "vibrant, "bright", "pale", "muted", "dark", "light", and "contrast". These are taken from [Paul Taul](https://personal.sron.nl/~pault/#sec:qualitative),  [Mike Mol](https://mikemol.github.io/technique/colorblind/2018/02/11/color-safe-palette.html) and [Okabe Ito](https://jfly.uni-koeln.de/color/).
#' `scale_fill_grafify2` and `scale_colour_grafify2` are identical except that when the number of categorical variables is fewer than the total number of colour shades in the palette (e.g. if you have 3 groups and the "okabe_ito" palette has 7 colours), these functions will pick the most 'distant' colours from the scheme than going sequentially. If you want colours assigned sequentially use `scale_fill_grafify` or `scale_colour_grafify`.
#'
#' @param palette Name of the colour scheme. Default set to \code{palette = "all_grafify"}. Provide names as above in quotes.
#' @param reverse Whether the colour order should be reversed.
#' @param ... Additional parameters for `scale_fill` or `scale_colour`
#'
#' @return ggplot scale_fill function
#' @export scale_fill_grafify2
#' @import ggplot2
#'
#' @examples
#' #add a grafify fill scheme to ggplot
#' ggplot(emmeans::neuralgia, aes(x = Treatment, y = Duration))+
#' geom_point(aes(fill = Treatment), shape = 21, size = 3,
#' position = position_jitter(0.15), alpha = 0.8)+
#' scale_fill_grafify2(palette = "muted")+facet_wrap("Sex")
#' #reverse colour order
#' ggplot(emmeans::neuralgia, aes(x = Treatment, y = Duration))+
#' geom_point(aes(fill = Treatment), shape = 21, size = 3,
#' position = position_jitter(0.15), alpha = 0.8)+
#' scale_fill_grafify2(palette = "muted", reverse = TRUE)+facet_wrap("Sex")
#'
scale_fill_grafify2 <- function(palette = "all_grafify", reverse = FALSE, ...){
  pal <- graf_col_palette_default(palette = palette, reverse = reverse)
  discrete_scale("fill", paste0("graf_", palette), palette = pal, ...)
}
