#' Plot fitted smooths of generalised additive models
#' 
#' This is a clone of the \code{\link[gratia]{draw}} function in the `gratia` package. It will plot smooths for all levels of all fixed factors in a generalised additive model fitted with \code{\link{ga_model}}. It creates graphs that use `grafify` colours and `theme_classic()`.
#'
#' @param Model a model of class `gam` fitted with `ga_model` or the `mgcv` package.
#' @param ... additional arguments to pass to \code{\link[gratia]{draw}}.
#'
#' @return This function returns an object of classes "ggplot", "gg" and "patchwork 
#' @export plot_gam_smooth
#' @importFrom gratia draw
#'
plot_gam_smooth <- function(Model, ...){
  p <- gratia::draw(object = Model,
               smooth_col = unname(get_graf_colours("ok_orange")), 
               ci_col =  unname(get_graf_colours("pale_grey")), 
               ci_alpha = .8, ...)&theme_classic()
  p
}

#' Plot model diagnostics for generalised additive models
#' 
#' This is a clone of the \code{\link[gratia]{appraise}} function in the `gratia` package and will plot 4 diagnostic plots when given a generalised additive model fitted with \code{\link{ga_model}}. It creates graphs that use `grafify` colours and `theme_classic()`.
#' 
#' @param Model a model of class `gam` fitted with `ga_model` or the `mgcv` package.
#' @param ... additional arguments to pass to \code{\link[gratia]{appraise}}.
#'
#' @return This function returns an object of classes "ggplot", "gg" and "patchwork".
#' @export plot_gam_qq
#' @importFrom gratia appraise
#'
plot_gam_qq <- function(Model, ...){
  p <- gratia::appraise(model = Model,
                        line_col = "#E69F00",
                        point_col = "#676767",
                        point_alpha = 0.6,
                        ci_alpha = 1,
                        ...)&theme_classic()
  p
}
