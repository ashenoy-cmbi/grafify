#' `scale_colour_` and `scale_fill_` functions
#'
#' `grafify` palettes for fill and colour/color aesthetics can be applied to any `ggplot2` object using `scale_colour_` and `scale_fill_` functions. `scale_color_` spelling is also accepted.
#' 
#' Note that `scale_fill_grafify` and `scale_colour_grafify` **only** work for categorical variables. Use `scale_fill_grafify_c` or `scale_colour_grafify_c` for continuous or divergent palettes in `grafify`.
#' 
#' Names of palettes available are as follows:
#' 
#' Categorical/discreet palettes: 
#' - `okabe_ito`
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
#' Sequential quantitative palettes:
#' - `grey_conti`
#' - `blue_conti`
#' - `yellow_conti`
#' 
#' Divergent quantitative palettes:
#' - `OrBl_div`
#' - `PrGn_div`

#' `scale_fill_grafify2` and `scale_colour_grafify2` will choose the most distant colours from selected palettes instead of choosing colours sequentially from the palette (e.g. if you have 3 groups and the "okabe_ito" palette has 7 colours), these functions will pick the most 'distant' colours from the scheme than going sequentially. 
#' 
#' Both these will be deprecated soon. Instead use `ColSeq = FALSE` to implement distant colours. Default is `ColSeq = TRUE`.
#' 
#' @param palette Name of the colour scheme. Default set to \code{palette = "all_grafify"}. Provide names as above in quotes.
#' @param reverse Whether the colour order should be reversed.
#' @param ColSeq logical TRUE or FALSE. Default TRUE for sequential colours from chosen palette. Set to FALSE for distant colours.
#' @param ... Additional parameters for `scale_fill` or `scale_colour`.
#'
#' @return ggplot scale_fill function for discrete colours.
#' @export scale_colour_grafify
#' @import ggplot2
#'
#' @examples
#' #add a grafify fill scheme to ggplot
#' ggplot(emmeans::neuralgia, aes(x = Treatment, 
#'                                y = Duration))+
#'   geom_boxplot(aes(fill = Treatment), 
#'                alpha = .4)+
#'   geom_point(aes(colour = Treatment), 
#'              size = 3,
#'              position = position_jitter(0.15), 
#'              alpha = 0.8)+
#'   scale_fill_grafify(palette = "vibrant")+
#'   scale_colour_grafify(palette = "vibrant")+
#'   facet_wrap("Sex")+
#'   theme_classic()
#' #distant colours   
#' ggplot(emmeans::neuralgia, aes(x = Treatment, 
#'                                y = Duration))+
#'   geom_boxplot(aes(fill = Treatment), 
#'                alpha = .4)+
#'   geom_point(aes(colour = Treatment), 
#'              size = 3,
#'              position = position_jitter(0.15), 
#'              alpha = 0.8)+
#'   scale_fill_grafify(palette = "vibrant", 
#'                      ColSeq = FALSE)+
#'   scale_colour_grafify(palette = "vibrant", 
#'                        ColSeq = FALSE)+
#'                        facet_wrap("Sex")+
#'                        theme_classic()
#' #reverse colour order
#' ggplot(emmeans::neuralgia, aes(x = Treatment, 
#'                                y = Duration))+
#'   geom_boxplot(aes(fill = Treatment), 
#'                alpha = .4)+
#'   geom_point(aes(colour = Treatment), 
#'              size = 3,
#'              position = position_jitter(0.15), 
#'              alpha = 0.8)+
#'   scale_fill_grafify(palette = "vibrant", 
#'                      reverse = TRUE)+
#'   scale_colour_grafify(palette = "vibrant", 
#'                        reverse = TRUE)+
#'                        facet_wrap("Sex")+
#'                        theme_classic()
#'                        
scale_colour_grafify <- function(palette = c("okabe_ito", "all_grafify", "bright",  "contrast",  "dark",  "fishy",  "kelly",  "light",  "muted",  "pale",  "r4",  "safe",  "vibrant"), ColSeq = TRUE, reverse = FALSE, ...){
  palette <- match.arg(palette)
  if (ColSeq) {
    pal <- graf_col_palette(palette = palette, reverse = reverse)} else {
      pal <- graf_col_palette_default(palette = palette, reverse = reverse)
    }
  discrete_scale("colour", paste0("graf_", palette), palette = pal, ...)
}


