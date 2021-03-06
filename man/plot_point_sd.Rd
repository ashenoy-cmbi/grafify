% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_point_sd.R
\name{plot_point_sd}
\alias{plot_point_sd}
\title{Plot a point as mean with SD error bars using two variables.}
\usage{
plot_point_sd(
  data,
  xcol,
  ycol,
  symsize = 3.5,
  symthick = 1,
  ewid = 0.2,
  fontsize = 20,
  ColPal = "all_grafify",
  ColRev = F,
  TextXAngle = 0
)
}
\arguments{
\item{data}{a data table object, e.g. data.frame or tibble.}

\item{xcol}{name of the column with a \strong{categorical} X variable.}

\item{ycol}{name of the column with quantitative Y variable}

\item{symsize}{size of point symbols, default set to 3.5}

\item{symthick}{thickness of symbol border, default set to 1}

\item{ewid}{width of error bars, default set to 0.2}

\item{fontsize}{parameter of \code{base_size} of fonts in \code{theme_classic}, default set to size 20.}

\item{ColPal}{grafify colour palette to apply, default "all_grafify"; alternatives: "okabe_ito", "bright", "pale", "vibrant", "contrast", "muted" "dark", "light".}

\item{ColRev}{whether to reverse order of colour choice, default F (FALSE); can be set to T (TRUE)}

\item{TextXAngle}{orientation of text on X-axis; default 0 degrees. Change to 45 or 90 to remove overlapping text}
}
\value{
This function returns a \code{ggplot2} object.
}
\description{
This function takes a data table, X and Y variables, and plots a graph with a summary point showing the mean of data using \code{\link[ggplot2]{ggplot}}.
}
\details{
The function uses \code{\link[ggplot2]{stat_summary}} with \code{geom = "point"} with \code{size = 3}.
Standard deviation (SD) is plotted through \code{\link[ggplot2]{stat_summary}} calculated using \code{mean_sdl} from the \code{ggplot2} package (get help with \code{?mean_sdl}), and 1x SD is plotted (\code{fun.arg = list(mult = 1)}.
The categorical X variable is mapped to the \code{fill} aesthetic in the point geometry, and by default receives \code{all_grafify} palette.

You are instead encouraged to show all data using the following functions: \code{\link{plot_scatterbar_sd}}, \code{\link{plot_dotbox}}, \code{\link{plot_dotbar_sd}} and \code{\link{plot_dotviolin}}.
}
\examples{
#Basic usage
plot_point_sd(data_doubling_time, Student, Doubling_time)

}
