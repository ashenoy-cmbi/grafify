#' Scale_fill colour scheme
#'
#' `grafify` internally includes colour-blind compatible schemes for fill and colour/color aesthetics. Note that these **only** work for categorical variables. Use the brewer or viridis packages for numeric gradient scales.
#'
#' The default for \code{scale_fill_grafify()}, \code{scale_colour_grafify()} or \code{scale_color_grafify()} is a list of 46 colours as part of \code{palette = "all_grafify"}.
#'
#' Obviously, it is not recommended to use so many colours, but implementing this was easiest to prevent errors when using a lot of categorical variables.
#'
#' There are six other schemes with 6-10 colours each, which are recommended. These can be called by naming the colour scheme using \code{palette = } argument.
#' Additional options include "okabe_ito", "bright", "pale", "muted", "dark" and "light". These are taken from [Paul Taul](https://personal.sron.nl/~pault/#sec:qualitative),  [Mike Mol](https://mikemol.github.io/technique/colorblind/2018/02/11/color-safe-palette.html) and [Okabe Ito](http://jfly.iam.u-tokyo.ac.jp/color/#pallet).
#'
#' @param palette Name of the colour scheme. Default set to \code{palette = "all_grafify"}. Provide names as above in quotes.
#' @param reverse Whether the colour order should be reversed.
#' @param ... Additional parameters for `scale_fill` or `scale_colour`
#'
#' @return ggplot scale_fill function
#' @export scale_fill_grafify
#'
#' @examples
#' #if p is a ggplot object produced by any of the `plot_` functions or `ggplot2`
#' p + scale_colour_grafify() #for point and scatter graphs
#' p + scale_colour_grafify() + scale_fill_graf() #for dotbar, dotbox etc
#'
#' #specify colour palette & reverse order
#' p + scale_colour_graf(palette = "okabe_ito", reverse = T)
#'
scale_fill_grafify <- function(palette = "all_grafify", reverse = FALSE, ...){
  pal <- graf_col_palette(palette = palette, reverse = reverse)
  discrete_scale("fill", paste0("graf_", palette), palette = pal, ...)
}
