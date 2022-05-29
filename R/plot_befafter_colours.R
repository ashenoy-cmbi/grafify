#' Plot a before-after plot with lines joining colour-matched symbols.
#'
#' The \code{\link{plot_befafter_colours}}, \code{\link{plot_befafter_colors}} and \code{\link{plot_befafter_shapes}} are for plotting matched data joined by lines. These functions take X and Y variables along with a data column with matching information (e.g. matched subjects or experiments etc.) and plot symbols matched by colour or shape.
#'
#' Note that only 25 shapes are available, and there will be errors with \code{\link{plot_befafter_shapes}} when there are fewer than 25 matched observations; instead use \code{\link{plot_befafter_colours}} instead.
#' 
#' Colours can be changed using `ColPal`, `ColRev` or `ColSeq` arguments. 
#' `ColPal` can be one of the following: "okabe_ito", "dark", "light", "bright", "pale", "vibrant,  "muted" or "contrast".
#' `ColRev` (logical TRUE/FALSE) decides whether colours are chosen from first-to-last or last-to-first from within the chosen palette. 
#' `ColSeq` decides whether colours are picked by respecting the order in the palette or the most distant ones using \code{\link[grDevices]{colorRampPalette}}.
#'
#' To plot a graph with a single colour along the X axis variable, use the `SingleColour` argument.
#' 
#' More complex designs can also be plotted when used with \code{\link[ggplot2]{facet_wrap}} or \code{\link[ggplot2]{facet_grid}}.
#' @param data a data table object, e.g. data.frame or tibble.
#' @param xcol name of the column containing the categorical variable to be plotted on the X axis.
#' @param ycol name of the column containing the quantitative Y values.
#' @param match name of the column with the matching variable to pass on to \code{geom_line}.
#' @param symsize size of symbols, default set to 3.
#' @param symthick thickness of symbol border, default set to 1.
#' @param s_alpha fractional opacity of symbols, default set to 1 (i.e. maximum opacity & zero transparency).
#' @param ColPal grafify colour palette to apply, default "okabe_ito"; see \code{\link{graf_palettes}} for available palettes.
#' @param ColSeq logical TRUE or FALSE. Default TRUE for sequential colours from chosen palette. Set to FALSE for distant colours, which will be applied using  \code{scale_fill_grafify2}.
#' @param ColRev whether to reverse order of colour within the selected palette, default F (FALSE); can be set to T (TRUE).
#' @param SingleColour a colour hexcode (starting with #), a number between 1-154, or names of colours from `grafify` colour palettes to fill along X-axis aesthetic. Accepts any colour other than "black"; use `grey_lin11`, which is almost black.
#' @param TextXAngle orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text.
#' @param fontsize parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.
#' @param groups old argument name for `match`; retained for backward compatibility.
#' @param ... any additional arguments to pass to \code{ggplot2}[geom_line] or \code{ggplot2}[geom_point].
#'
#' @return This function returns a \code{ggplot2} object of class "gg" and "ggplot".
#' @export plot_befafter_colours
#' @import ggplot2
#'
#' @examples
#' #plot without legends if necessary
#' plot_befafter_colors(data = data_t_pdiff, 
#' xcol = Condition, ycol = Mass, 
#' match = Subject, s_alpha = .9, ColSeq = FALSE)+
#' guides(fill = "none", 
#' colour = "none") #remove guides
#' #2way ANOVA design with randomised blocks
#' plot_befafter_colors(data = data_2w_Tdeath, 
#' xcol = Genotype, ycol = PI, 
#' match = Experiment) + facet_wrap("Time")

plot_befafter_colours <- function(data, xcol, ycol, match, symsize = 3, symthick = 1, s_alpha = 1, ColPal = c("okabe_ito", "all_grafify", "bright",  "contrast",  "dark",  "fishy",  "kelly",  "light",  "muted",  "pale",  "r4",  "safe",  "vibrant"), ColSeq = TRUE, ColRev = FALSE, SingleColour = "NULL", TextXAngle = 0, fontsize = 20, groups, ...){
  ColPal <- match.arg(ColPal)
  if (!missing("groups")) {
    warning("Use `match` argument instead, as `groups` is deprecated.")
    match <- substitute(groups)}
  if (missing(SingleColour)) {
    P <- ggplot2::ggplot({{ data }}, aes(x = factor({{ xcol }}),
                                         y = {{ ycol }},
                                         group = factor({{ match }})))+
      geom_line(aes(group = factor({{ match }})),
                colour = "grey35", alpha = 0.8, 
                ...)+
      geom_point(size = {{ symsize }}, 
                 stroke = {{ symthick }},
                 alpha = {{ s_alpha }}, 
                 shape = 21,
                 aes(fill = factor({{ match }})), ...)+
      labs(x = enquo(xcol),
           fill = enquo(match))+
      theme_classic(base_size = {{ fontsize }})+
      theme(strip.background = element_blank())+
      guides(x = guide_axis(angle = {{ TextXAngle }}))+
      scale_fill_grafify(palette = {{ ColPal }}, 
                         reverse = {{ ColRev }}, 
                         ColSeq = {{ ColSeq }})
  } else {
    ifelse(grepl("#", SingleColour),
           a <- SingleColour,
           a <- get_graf_colours({{ SingleColour }}))
    P <- ggplot2::ggplot({{ data }}, 
                         aes(x = factor({{ xcol }}),
                             y = {{ ycol }},
                             group = factor({{ match }})))+
      geom_line(aes(group = factor({{ match }})),
                colour = "grey35", 
                alpha = 0.8,
                ...)+
      geom_point(size = {{ symsize }}, 
                 stroke = {{ symthick }},
                 alpha = {{ s_alpha }}, 
                 shape = 21,
                 fill = a, ...)+
      labs(x = enquo(xcol))+
      theme_classic(base_size = {{ fontsize }})+
      theme(strip.background = element_blank())+
      guides(x = guide_axis(angle = {{ TextXAngle }}))
  }
  P
}

