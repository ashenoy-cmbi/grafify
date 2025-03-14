% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_dotviolin.R
\name{plot_dotviolin}
\alias{plot_dotviolin}
\title{Plot a dotplot on a violin plot with two variables.}
\usage{
plot_dotviolin(
  data,
  xcol,
  ycol,
  facet,
  dotsize = 1.5,
  d_alpha = 0.8,
  b_alpha = 0,
  v_alpha = 1,
  bwid = 0.3,
  vadjust = 1,
  trim = TRUE,
  scale = "width",
  TextXAngle = 0,
  LogYTrans,
  LogYBreaks = waiver(),
  LogYLabels = waiver(),
  LogYLimits = NULL,
  facet_scales = "fixed",
  fontsize = 20,
  dotthick,
  bthick,
  vthick,
  bvthick,
  ColPal = c("okabe_ito", "all_grafify", "bright", "contrast", "dark", "fishy", "kelly",
    "light", "muted", "pale", "r4", "safe", "vibrant"),
  ColSeq = TRUE,
  ColRev = FALSE,
  SingleColour = "NULL",
  ...
)
}
\arguments{
\item{data}{a data table object, e.g. data.frame or tibble.}

\item{xcol}{name of the column (without quotes) to plot on X axis. This should be a categorical variable.}

\item{ycol}{name of the column (without quotes) with the quantitative variable to plot on the Y axis. This should be a quantitative variable.}

\item{facet}{add another variable (without quotes) from the data table to create faceted graphs using \code{\link[ggplot2]{facet_wrap}}.}

\item{dotsize}{size of dots relative to \code{binwidth} used by \code{\link[ggplot2]{geom_dotplot}}. Default set to 1.5, increase/decrease as needed.}

\item{d_alpha}{fractional opacity of dots, default set to 0.8 (i.e., 80\% opacity).}

\item{b_alpha}{fractional opacity of boxplots.  Default is set to 0, which results in white boxes inside violins. Change to any value >0 up to 1 for different levels of transparency.}

\item{v_alpha}{fractional opacity of violins, default set to 1.}

\item{bwid}{width of boxplots; default 0.3.}

\item{vadjust}{number to adjust the smooth/wigglyness of violin plot (default set to 1).}

\item{trim}{set whether tips of violin plot should be trimmed at high/low data. Default \code{trim = TRUE}, can be changed to FALSE.}

\item{scale}{set to "area" by default, can be changed to "count" or "width".}

\item{TextXAngle}{orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text.}

\item{LogYTrans}{transform Y axis into "log10" or "log2" (in quotes).}

\item{LogYBreaks}{argument for \code{\link[ggplot2]{scale_y_continuous}} for Y axis breaks on log scales, default is \code{waiver()}, or provide a vector of desired breaks.}

\item{LogYLabels}{argument for \code{\link[ggplot2]{scale_y_continuous}} for Y axis labels on log scales, default is \code{waiver()}, or provide a vector of desired labels.}

\item{LogYLimits}{a vector of length two specifying the range (minimum and maximum) of the Y axis.}

\item{facet_scales}{whether or not to fix scales on X & Y axes for all facet facet graphs. Can be \code{fixed} (default), \code{free}, \code{free_y} or \code{free_x} (for Y and X axis one at a time, respectively).}

\item{fontsize}{parameter of \code{base_size} of fonts in \code{\link[ggplot2]{theme_classic}}, default set to size 20.}

\item{dotthick}{thickness of dot border (\code{stroke} parameter of \code{\link[ggplot2]{geom_dotplot}}), default set to \code{fontsize}/22.}

\item{bthick}{thickness (in 'pt' units) of boxplots; default = \code{fontsize}/22.}

\item{vthick}{thickness (in 'pt' units) of violins; default = \code{fontsize}/22.}

\item{bvthick}{thickness (in 'pt' units) of both violins and boxplots; default = \code{fontsize}/22.}

\item{ColPal}{grafify colour palette to apply (in quotes), default "okabe_ito"; see \code{\link{graf_palettes}} for available palettes.}

\item{ColSeq}{logical TRUE or FALSE. Default TRUE for sequential colours from chosen palette. Set to FALSE for distant colours, which will be applied using  \code{scale_fill_grafify2}.}

\item{ColRev}{whether to reverse order of colour within the selected palette, default F (FALSE); can be set to T (TRUE).}

\item{SingleColour}{a colour hexcode (starting with #, e.g., "#E69F00"), a number between 1-154, or names of colours from \code{grafify} or base R palettes to fill along X-axis aesthetic. Accepts any colour other than "black"; use \code{grey_lin11}, which is almost black.}

\item{...}{any additional arguments to pass to \code{\link[ggplot2]{geom_boxplot}}, \code{\link[ggplot2]{geom_dotplot}} or \code{\link[ggplot2]{geom_violin}}.}
}
\value{
This function returns a \code{ggplot2} object of class "gg" and "ggplot".
}
\description{
There are three types of \code{plot_dot_} functions that plot data as "dots" using the \code{\link[ggplot2]{geom_dotplot}} geometry. They all take a data table, a categorical X variable and a numeric Y variable.
\enumerate{
\item \link{plot_dotbar_sd} (bar & SD, SEM or CI95 error bars)
\item \link{plot_dotbox} (box & whiskers)
\item \link{plot_dotviolin} (box & whiskers, violin)
}
}
\details{
Related \code{plot_scatter_} variants show data symbols using the \code{\link[ggplot2]{geom_point}} geometry. These are \link{plot_scatterbar_sd} (or SEM or CI95 error bars), \link{plot_scatterbox} and \link{plot_scatterviolin}. Over plotting in \code{plot_scatter} variants can be reduced with the \code{jitter} argument.

The X variable is mapped to the \code{fill} aesthetic of dots, symbols, bars, boxes and violins.

Colours can be changed using \code{ColPal}, \code{ColRev} or \code{ColSeq} arguments. Colours available can be seen quickly with \code{\link{plot_grafify_palette}}.
\code{ColPal} can be one of the following: "okabe_ito", "dark", "light", "bright", "pale", "vibrant,  "muted" or "contrast".
\code{ColRev} (logical TRUE/FALSE) decides whether colours are chosen from first-to-last or last-to-first from within the chosen palette.
\code{ColSeq} decides whether colours are picked by respecting the order in the palette or the most distant ones using \code{\link[grDevices]{colorRampPalette}}.

If you prefer a single colour for the graph, use the \code{SingleColour} argument.
}
\examples{

#plot with trim = FALSE
plot_dotviolin(data = data_t_pdiff, 
xcol = Condition, ycol = Mass, 
dotsize = 2, trim = FALSE)

plot_dotviolin(data = data_t_pdiff, 
xcol = Condition, ycol = Mass,
trim = FALSE, b_alpha = 0.5, 
ColPal = "pale", ColSeq = FALSE)

#single colour along X
plot_dotviolin(data = data_t_pdiff, 
xcol = Condition, ycol = Mass,
trim = FALSE, b_alpha = 0.5, 
SingleColour = "pale_cyan")
}
