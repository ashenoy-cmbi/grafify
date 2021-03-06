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
  dotsize = 1.5,
  dotthick = 1,
  trim = T,
  scale = "area",
  fontsize = 20,
  v_alpha = 1,
  d_alpha = 1,
  ColPal = "all_grafify",
  ColRev = F,
  TextXAngle = 0
)
}
\arguments{
\item{data}{a data table object, e.g. data.frame or tibble}

\item{xcol}{name of the column to plot on X axis. This should be a categorical variable.}

\item{ycol}{name of the column to plot on quantitative Y axis. This should be a quantitative variable.}

\item{dotsize}{size of dots relative to \code{binwidth} used by \code{geom_dotplot}. Default set to 1.5, increase/decrease as needed.}

\item{dotthick}{thickness of dot border (\code{stroke} parameter of \code{geom_dotplot}), default set to 1}

\item{trim}{set whether tips of violin plot should be trimmed at high/low data. Default \code{trim = T}, can be changed to F.}

\item{scale}{set to "area" by default, can be changed to "count" or "width".}

\item{fontsize}{parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.}

\item{v_alpha}{fractional opacity of violins, default set to 1 (i.e. maximum opacity & zero transparency)}

\item{d_alpha}{fractional opacity of dots, default set to 1 (i.e. maximum opacity & zero transparency)}

\item{ColPal}{grafify colour palette to apply, default "all_grafify"; alternatives: "okabe_ito", "bright", "pale", "vibrant", "contrast", "muted" "dark", "light".}

\item{ColRev}{whether to reverse order of colour choice, default F (FALSE); can be set to T (TRUE)}

\item{TextXAngle}{orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text}
}
\value{
This function returns a \code{ggplot2} object on which additional geometries etc. can be added.
}
\description{
This function takes a data table, X and Y variables, and plots a graph with a dotplot and violinplot using \code{ggplot}.
}
\details{
The function uses \code{\link[ggplot2]{geom_violin}} and \code{\link[ggplot2]{geom_dotplot}} geometries.
Note that the \code{\link{geom_violin}} options are set as follows: \code{scale = "area", draw_quantiles = c(0.25, .5, .75)}. The \code{trim = T} set by default can be changed when calling the function.
The X variable is mapped to the \code{fill} aesthetic in both violinplot and dotplot, and its colour can be changed using \code{scale_fill_brewer} or any \code{scale_fill...} option. The size of dots can be adjusted using the parameter, which is \code{dotsize = 1} by default.

This function is related to \code{\link{plot_scatterbar_sd}}, \code{\link{plot_dotbar_sd}} and \code{\link{plot_dotviolin}}.
}
\examples{

#plot with trim = F
plot_dotviolin(data_t_pdiff, Condition, Mass, dotsize = 2)

#without trimming
plot_dotviolin(data_t_pdiff, Condition, Mass, dotsize = 2, trim = FALSE)

}
