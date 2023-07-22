#' Plot data and predictions from linear model
#' 
#' This function takes a linear model, and up to three variables and plots observe data (circles) and model predictions (squares). If the X-variable is categorical, a box and whiskers plot is overlaid. A variable (`ByFactor`) can be used for faceting.  
#'
#' @param Model a linear model saved with `simple_model`, `mixed_model` or `ga_model`. 
#' @param xcol variable along the X axis (should match one of the dependent variables in model exactly).
#' @param ycol independent variable along the Y axis (should match independent variable in model exactly). 
#' @param ByFactor optional faceting variable (should match one of the variables in model exactly).
#' @param obs_size size of symbols for observed data (default = 2).
#' @param obs_alpha opacity of symbols for observed data (default = 0.3).
#' @param pred_size size of symbols for predicted data (default = 2). 
#' @param pred_alpha opacity of symbols for predicted data (default = 0.8).
#' @param linethick thickness of border lines for boxes and symbols (default is base_size/20).
#' @param base_size base fontsize for `theme_grafify`
#' @param ... any other parameters to be passed to `theme_grafify`
#'
#' @return This function returns a \code{ggplot2} object of class "gg" and "ggplot".
#' @export plot_lm_predict
#' @importFrom ggplot2 ggplot
#' @importFrom stats predict model.frame
#'
#' @examples
#' #fit a model
#' deathm1 <- mixed_model(data_2w_Tdeath, 
#' "PI", c("Genotype", "Time"), 
#' "Experiment")
#' #plot model
#' plot_lm_predict(deathm1,
#' Genotype, PI, Time)
#' #fit zooplankton data
#' z1 <- ga_model(data = data_zooplankton,
#' Y_value = "log(density_adj)",
#' Fixed_Factor = "taxon",
#' Smooth_Factor = "day")
#' 
#' #plot fitted data
#' plot_lm_predict(Model = z1,
#' xcol = day, 
#' ycol = `log(density_adj)`,
#' ByFactor = taxon)
#' 
plot_lm_predict <- function(Model, xcol, ycol, ByFactor, obs_size = 2, obs_alpha = 0.3, pred_size = 2, pred_alpha = 0.8, linethick, base_size = 15, ...) {
  if(missing(linethick)){linethick <- base_size/22}
  x <- substitute(xcol)
  high <- low <- pred <- NULL
  modeldf <- model.frame(Model)
  modeldf$pred <- predict(Model)
  if(isTRUE(is.numeric(modeldf[[deparse1(x)]]))){
    suppressMessages(P <- ggplot(modeldf, 
                                 aes(x = factor({{ xcol }}),
                                     fill = {{ ByFactor }}))+
                       geom_point(aes(y = {{ ycol }},
                                      colour = {{ ByFactor }}),
                                  alpha = obs_alpha,
                                  size = obs_size,
                                  stroke = linethick)+
                       geom_point(aes(y = .data$pred,
                                      fill = {{ ByFactor }}),
                                  alpha = pred_alpha,
                                  size = pred_size,
                                  stroke = linethick,
                                  shape = 22)+
                       guides(fill = guide_legend(title = "predicted",
                                                  order = 1),
                              colour = guide_legend(title = "observed",
                                                    order = 2))+
                       theme_grafify(base_size = base_size,
                                     ...)+
                       labs(x = enquo(xcol))+
                       scale_fill_grafify()+
                       scale_colour_grafify())
  } else {
    suppressMessages(P <- ggplot(modeldf, 
                                 aes(x = factor({{ xcol }}),
                                     fill = factor({{ xcol }})))+
                       geom_boxplot(aes(colour = {{ xcol }},
                                        y = pred),
                                    alpha = 0,
                                    outlier.alpha = 0,
                                    size = linethick,
                                    width = 0.3,
                                    show.legend = FALSE)+
                       geom_point(aes(y = {{ ycol }},
                                      colour = {{ xcol }}),
                                  alpha = obs_alpha,
                                  size = obs_size,
                                  stroke = linethick)+
                       geom_point(aes(y = .data$pred,
                                      fill = factor({{ xcol }})),
                                  alpha = pred_alpha,
                                  size = pred_size,
                                  stroke = linethick,
                                  shape = 22)+
                       guides(fill = guide_legend(title = "predicted",
                                                  order = 1),
                              colour = guide_legend(title = "observed",
                                                    order = 2))+
                       theme_grafify(base_size = base_size,
                                     ...)+
                       labs(x = enquo(xcol),
                            fill = enquo(xcol),
                            colour = enquo(xcol))+
                       scale_fill_grafify()+
                       scale_colour_grafify())
  }
  if(!missing(ByFactor)){
    P <- P +
      facet_wrap(vars({{ ByFactor }}))
  }  
  P
}
