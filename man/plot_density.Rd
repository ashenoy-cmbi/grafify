% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_density.R
\name{plot_density}
\alias{plot_density}
\title{Plot density distribution of data.}
\usage{
plot_density(
  data,
  ycol,
  group,
  facet,
  PlotType = c("Density", "Counts", "Normalised counts"),
  c_alpha = 0.2,
  TextXAngle = 0,
  facet_scales = "fixed",
  fontsize = 20,
  linethick,
  LogYTrans,
  LogYBreaks = waiver(),
  LogYLabels = waiver(),
  LogYLimits = NULL,
  ColPal = c("okabe_ito", "all_grafify", "bright", "contrast", "dark", "fishy", "kelly",
    "light", "muted", "pale", "r4", "safe", "vibrant"),
  ColSeq = TRUE,
  ColRev = FALSE,
  SingleColour = NULL,
  ...
)
}
\arguments{
\item{data}{a data table e.g. data.frame or tibble.}

\item{ycol}{name of the column (without quotes) with the quantitative variable whose density distribution is to be plotted.}

\item{group}{name of the column containing a categorical grouping variable (optional Since v5.0.0).}

\item{facet}{add another variable (without quotes) from the data table to create faceted graphs using \code{\link[ggplot2]{facet_wrap}}.}

\item{PlotType}{the default (\code{Density}) plot will be the probability density curve, which can be changed to \code{Counts} or \verb{Normalised counts}.}

\item{c_alpha}{fractional opacity of filled colours under the curve, default set to 0.2 (i.e. 20\% opacity).}

\item{TextXAngle}{orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text.}

\item{facet_scales}{whether or not to fix scales on X & Y axes for all facet facet graphs. Can be \code{fixed} (default), \code{free}, \code{free_y} or \code{free_x} (for Y and X axis one at a time, respectively).}

\item{fontsize}{parameter of \code{base_size} of fonts in \code{\link[ggplot2]{theme_classic}}, default set to size 20.}

\item{linethick}{thickness of symbol border, default set to \code{fontsize}/22.}

\item{LogYTrans}{transform Y axis into "log10" or "log2" (in quotes).}

\item{LogYBreaks}{argument for \code{\link[ggplot2]{scale_y_continuous}} for Y axis breaks on log scales, default is \code{waiver()}, or provide a vector of desired breaks.}

\item{LogYLabels}{argument for \code{\link[ggplot2]{scale_y_continuous}} for Y axis labels on log scales, default is \code{waiver()}, or provide a vector of desired labels.}

\item{LogYLimits}{a vector of length two specifying the range (minimum and maximum) of the Y axis.}

\item{ColPal}{grafify colour palette to apply (in quotes), default "okabe_ito"; see \code{\link{graf_palettes}} for available palettes.}

\item{ColSeq}{logical TRUE or FALSE. Default TRUE for sequential colours from chosen palette. Set to FALSE for distant colours, which will be applied using  \code{scale_fill_grafify2}.}

\item{ColRev}{whether to reverse order of colour within the selected palette, default F (FALSE); can be set to T (TRUE).}

\item{SingleColour}{a colour hexcode (starting with #, e.g., "#E69F00"), a number between 1-154, or names of colours from \code{grafify} palettes or base R to fill along X-axis aesthetic. Accepts any colour other than "black"; use \code{grey_lin11}, which is almost black.}

\item{...}{any additional arguments to pass to \code{\link[ggplot2]{geom_density}}.}
}
\value{
This function returns a \code{ggplot2} object of class "gg" and "ggplot".
}
\description{
This function takes a data table, \code{ycol} of quantitative variable and a categorical grouping variable (\code{group}), if available, and plots a density graph using \code{\link[ggplot2]{geom_density}}). Alternatives are \code{\link{plot_histogram}}, or \code{\link{plot_qqline}}.
}
\details{
Note that the function requires the quantitative Y variable first, and groups them based on an X variable. The group variable is mapped to the \code{fill} and \code{colour} aesthetics in \code{\link[ggplot2]{geom_density}}.
Colours can be changed using \code{ColPal}, \code{ColRev} or \code{ColSeq} arguments. Colours available can be seen quickly with \code{\link{plot_grafify_palette}}.
\code{ColPal} can be one of the following: "okabe_ito", "dark", "light", "bright", "pale", "vibrant,  "muted" or "contrast".
\code{ColRev} (logical TRUE/FALSE) decides whether colours are chosen from first-to-last or last-to-first from within the chosen palette.
\code{ColSeq} decides whether colours are picked by respecting the order in the palette or the most distant ones using \code{\link[grDevices]{colorRampPalette}}.
}
\examples{
plot_density(data = data_t_pratio, 
ycol = log(Cytokine), group = Genotype)

#with faceting
plot_density(data = data_cholesterol, 
ycol = Cholesterol, group = Treatment, 
facet = Treatment, fontsize = 12)

#Counts
plot_density(data = data_cholesterol, 
ycol = Cholesterol, group = Treatment,
PlotType = "Counts", 
facet = Treatment, fontsize = 12)

}
