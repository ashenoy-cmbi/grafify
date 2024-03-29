% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_qq_gam.R
\name{plot_qq_gam}
\alias{plot_qq_gam}
\title{Plot model diagnostics for generalised additive models}
\usage{
plot_qq_gam(
  Model,
  symsize = 2,
  s_colour = "#E69F00",
  s_alpha = 0.6,
  line_col = "black",
  base_size = 12,
  linethick,
  n_bins = c("sturges", "scott", "fd")
)
}
\arguments{
\item{Model}{a model of class \code{gam} fitted with \code{ga_model} or the \code{mgcv} package.}

\item{symsize}{size of symbols (default = 2)}

\item{s_colour}{colour of symbols (default = \code{ok_orange})}

\item{s_alpha}{opacity of symbols (default = 0.8)}

\item{line_col}{colour of lines (default = \code{black})}

\item{base_size}{font size for theme (default = 12)}

\item{linethick}{thickness in 'pt' units of lines and symbol orders (default = base_size/22)}

\item{n_bins}{one of either "sturges", "scott", "fd"}
}
\value{
This function returns an object of classes "ggplot" and "gg".

This function returns a \code{ggplot2} object of class "gg" and "ggplot".
}
\description{
This is a clone of the \code{\link[gratia]{appraise}} function in the \code{gratia} package (rewritten to avoid depending on \code{gratia} package for these plots). This function will plot 4 diagnostic plots when given a generalised additive model fitted with \code{\link{ga_model}} or \code{mgcv}. It creates graphs that use \code{grafify} colours and \code{theme_grafify()}.
}
