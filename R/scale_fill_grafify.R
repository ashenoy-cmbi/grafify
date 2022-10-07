#' `scale_colour_` and `scale_fill_` functions
#'
#' These let you apply `grafify` discrete or continuous palettes as `fill` or `colour` aesthetics to any `ggplot2` (`scale_color_` spelling is also accepted).
#' 
#' The default is `palette = "okabe_ito"`. The `discrete` argument is not used at present. The following discrete and quantitative palettes can be used.
#'  
#' Categorical/discreet palettes:
#' - `okabe_ito` (default)
#' - `bright`
#' - `contrast`
#' - `dark`
#' - `kelly`
#' - `light`
#' - `muted`
#' - `pale`
#' - `r4`
#' - `safe`
#' - `vibrant`
#' 
#' By default, sequential colours from above palettes will be chosen. To choose the most distant colours set `ColSeq = TRUE`.
#' 
#' Sequential quantitative palettes:
#' - `grey_conti`
#' - `blue_conti`
#' - `yellow_conti`
#' 
#' Divergent quantitative palettes:
#' - `OrBl_div`
#' - `PrGn_div`
#' 
#' @param palette Name of the `grafify` palettes from above, provide within quotes. Default discrete palette is `okabe_ito`. For quantitative palette, set `discrete = FALSE` (which will apply `blue_conti` unless another palette is chosen).
#' @param reverse Whether the colour order should be reversed.
#' @param ColSeq logical TRUE or FALSE. Default TRUE for sequential colours from chosen palette. Set to FALSE for distant colours.
#' @param discrete not used.
#' @param ... Additional parameters for `scale_fill` or `scale_colour`.
#'
#' @return ggplot scale_fill function for discrete colours.
#' @export scale_fill_grafify
#' @import ggplot2
#'
#' @examples
#' #add a grafify fill scheme to ggplot
#' ggplot(emmeans::neuralgia, aes(x = Treatment, 
#'                                y = Duration))+
#'   geom_boxplot(aes(fill = Treatment), 
#'                alpha = .6)+
#'   geom_point(aes(colour = Treatment,
#'                  shape = Treatment), 
#'              size = 3)+
#'   scale_fill_grafify(palette = "bright")+
#'   scale_colour_grafify(palette = "bright")+
#'   facet_wrap("Sex")+
#'   theme_classic()
#' #distant colours `ColSeq = FALSE`   
#' ggplot(emmeans::neuralgia, aes(x = Treatment, 
#'                                y = Duration))+
#'   geom_boxplot(aes(fill = Treatment), 
#'                alpha = .6)+
#'   geom_point(aes(colour = Treatment,
#'                  shape = Treatment), 
#'              size = 3)+
#'   scale_fill_grafify(palette = "bright",
#'                      ColSeq = FALSE)+
#'   scale_colour_grafify(palette = "bright",
#'                        ColSeq = FALSE)+
#'   facet_wrap("Sex")+
#'   theme_classic()
#' #quantitative colour schemes
#' ggplot(mtcars, aes(x = disp,
#'                    y = mpg))+
#'   geom_point(aes(colour = cyl), 
#'              size = 3)+
#'   scale_colour_grafify(palette = "blue_conti")
#'
scale_fill_grafify <- function(palette = "okabe_ito", ColSeq = TRUE, reverse = FALSE, discrete = TRUE, ...){
  palette <- match.arg(palette, 
                       choices = c("blue_conti", "yellow_conti", "grey_conti", "PrGn_div", "OrBl_div", "okabe_ito", "all_grafify", "bright",  "contrast",  "dark",  "fishy",  "kelly",  "light",  "muted",  "pale",  "r4",  "safe",  "vibrant"))
  if (palette %in% c("blue_conti", "yellow_conti", "grey_conti", "PrGn_div", "OrBl_div")) {
    pal <- graf_col_palette(palette = palette, 
                            reverse = reverse)
    scale_fill_gradientn(colours = pal(255), ...)
  } else {
    if (ColSeq) {
      pal <- graf_col_palette(palette = palette, 
                              reverse = reverse)
    } else {
      pal <- graf_col_palette_default(palette = palette, 
                                      reverse = reverse)
    }
    discrete_scale("fill", paste0("graf_", palette), 
                   palette = pal, ...)}
  
  #  if (palette == "okabe_ito" & discrete == FALSE) {palette <- #"blue_conti"}
  #  if (discrete == FALSE) {palette <- "blue_conti"}
  #  if (palette %in% c("blue_conti", "yellow_conti", "grey_conti", #"PrGn_div", "OrBl_div")) {discrete <- FALSE}
  #  if (discrete == FALSE) {
  #    palette <- match.arg(palette, 
  #                         choices = c("blue_conti", "yellow_conti", #"grey_conti", "PrGn_div", "OrBl_div"))
  #    if (palette %in% c("okabe_ito", "all_grafify", "bright",  #"contrast",  "dark",  "fishy",  "kelly",  "light",  "muted",  "pale", # "r4",  "safe",  "vibrant")) {
  #      stop("Discrete palette chosen with `discrete = FALSE`.")
  #    } else {
  #      pal <- graf_col_palette(palette = palette, 
  #                              reverse = reverse)
  #      scale_fill_gradientn(colours = pal(255), ...)
  #    } 
  #  } else {
  #    palette <- match.arg(palette, 
  #                         choices = c("okabe_ito", "all_grafify", #"bright",  "contrast",  "dark",  "fishy",  "kelly",  "light",  #"muted",  "pale",  "r4",  "safe",  "vibrant"))
  #    if (palette %in% c("blue_conti", "yellow_conti", "grey_conti", #"PrGn_div", "OrBl_div")) {
  #      stop("Quantitative palette chosen `discrete=TRUE`")
  #    } else {
  #      if (ColSeq) {
  #        pal <- graf_col_palette(palette = palette, 
  #                                reverse = reverse)
  #      } else {
  #        pal <- graf_col_palette_default(palette = palette, 
  #                                        reverse = reverse)
  #      }
  #      discrete_scale("fill", paste0("graf_", palette), 
  #                     palette = pal, ...)
  #    } 
  #  }
}
