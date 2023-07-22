#' Plot prediction of `gam` model
#' 
#' @param Model a generalised additive model (`gam`) fitted with `ga_model` or `mgcv`
#' @param xcol the smooth in the `gam` (should match variable in the model exactly)
#' @param ycol the dependent variable in `gam` (should match variable in the model exactly)
#' @param ByFactor the `by` factor used in `gam` (should match variable in the model exactly)
#' @param symsize size of symbols (default = 1)
#' @param s_alpha opacity of symbols (default = 0.1)
#' @param smooth_alpha opacity of the predicted CI interval (default = 0.7)
#' @param linethick thickness of symbol lines (default = fontsize/22)
#' @param fontsize base font size for graph
#' @param ... additional arguments to pass to \code{\link{plot_xy_CatGroup}}.
#'
#' @return This function returns a \code{ggplot2} object of class "gg" and "ggplot".
#' @export plot_gam_predict
#' @importFrom stats model.frame predict
#' @examples 
#' #fit zooplankton data
#' z1 <- ga_model(data = data_zooplankton,
#' Y_value = "log(density_adj)",
#' Fixed_Factor = "taxon",
#' Smooth_Factor = "day")
#' 
#' #plot fitted data
#' plot_gam_predict(Model = z1,
#' xcol = day, 
#' ycol = `log(density_adj)`,
#' ByFactor = taxon)
#' 
#'
plot_gam_predict <- function(Model, xcol, ycol, ByFactor, symsize = 1, s_alpha = 0.1, smooth_alpha = 0.7, linethick, fontsize = 20, ...) {
  if(missing(linethick)){linethick <- fontsize/22}
  high <- pred <- low <- NULL
  modeldf <- model.frame(Model)
  predmod <- predict(Model, 
                     se.fit = TRUE)
  modeldf$pred <- predmod$fit
  modeldf$pred.se <- predmod$se
  modeldf$high <- modeldf$pred+modeldf$pred.se*1.96
  modeldf$low <- modeldf$pred-modeldf$pred.se*1.96
  
  #plot_xy_CatGroup(modeldf, 
  #                 xcol = {{ xcol }}, 
  #                 ycol = {{ ycol }},
  #                 CatGroup = {{ ByFactor }},
  #                 facet = {{ ByFactor }},
  #                 symsize = symsize,
  #                 s_alpha = s_alpha,
  #                 ...)
  suppressWarnings(ggplot(modeldf,
                          aes(x = {{ xcol }},
                              y = {{ ycol }},
                              fill = {{ ByFactor }}))+
                     geom_point(size = symsize,
                                alpha = s_alpha,
                                shape = 21,
                                stroke = fontsize/22)+
                     facet_wrap(vars({{ ByFactor }}))+
                     geom_ribbon(aes(ymin = low,
                                     ymax = high,
                                     fill = {{ ByFactor }}),
                                 alpha = smooth_alpha)+
                     theme_grafify(base_size = fontsize)+
                     scale_fill_grafify())
}
