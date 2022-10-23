#' Plot model diagnostics for generalised additive models
#' 
#' This is a clone of the \code{\link[gratia]{appraise}} function in the `gratia` package (rewritten to avoid depending on `gratia` package for these plots). This function will plot 4 diagnostic plots when given a generalised additive model fitted with \code{\link{ga_model}} or `mgcv`. It creates graphs that use `grafify` colours and `theme_grafify()`.
#' 
#' @return This function returns an object of classes "ggplot" and "gg".
#' @param Model a model of class `gam` fitted with `ga_model` or the `mgcv` package.
#' @param symsize size of symbols (default = 2)
#' @param s_colour colour of symbols (default = `ok_orange`)
#' @param s_alpha opacity of symbols (default = 0.8)
#' @param line_col colour of lines (default = `black`)
#' @param base_size font size for theme (default = 12)
#' @param linethick thickness in 'pt' units of lines and symbol orders (default = base_size/22)
#' @param n_bins one of either "sturges", "scott", "fd"
#' 
#' @export plot_qq_gam
#' @importFrom stats residuals na.action napredict fitted
#' @importFrom grDevices nclass.FD nclass.Sturges nclass.scott
#' @importFrom ggplot2 ggplot
#' @importFrom patchwork wrap_plots
#' @return This function returns a \code{ggplot2} object of class "gg" and "ggplot".
#' 
plot_qq_gam <- function (Model, symsize = 2, s_colour = "#E69F00", s_alpha = 0.6, line_col = "black", base_size = 12, linethick, n_bins = c("sturges", "scott", "fd")) {
  if(missing(linethick)) {linethick <- base_size/22}
  type = "pearson"
  #get data from Model
  r <- residuals(Model, type = type)
  eta <- Model[["linear.predictors"]]
  na_action <- na.action(Model)
  if (is.matrix(eta) && !is.matrix(r)) { eta <- eta[, 1] }
  eta <- napredict(na_action, eta)
  fit <- fitted(Model)
  if (NCOL(fit) > 1L) { fit <- fit[, 1] }
  obs <- Model[["y"]]
  df <- data.frame(eta = eta, 
                   residuals = r, 
                   observed = obs, 
                   fitted = fit)
  #plots
  plt1 <- plot_qqmodel(Model,  #qqplot using grafify
                       symsize = symsize,
                       s_alpha = s_alpha,
                       SingleColour = s_colour)+
    theme_grafify(base_size = base_size)+
    labs(title = "QQ plot of residuals",
         x = "theoretical quantiles",
         y = "pearson residuals")
  #residual plot function from gratia
  residuals_linpred_plot <- function (Model){
    plt <- ggplot(df, aes(x = .data$eta, 
                          y = .data$residuals)) + 
      geom_hline(yintercept = 0, 
                 col = line_col) +
      geom_point(fill = s_colour, 
                 alpha = s_alpha,
                 shape = 21,
                 stroke = linethick,
                 size = symsize)+
      labs(x = "linear predictor", 
           y = "pearson residuals", 
           title = "Residuals vs linear predictor")+
      theme_grafify(base_size = base_size)
    plt
  }
  plt2 <- residuals_linpred_plot(Model)
  #plot function from gratia
  residuals_hist_plot <- function (Model, 
                                   n_bins = n_bins) {
    if (is.character(n_bins)) {
      n_bins <- match.arg(n_bins)
      n_bins <- switch(n_bins, 
                       sturges = nclass.Sturges(df[["residuals"]]), 
                       scott = nclass.scott(df[["residuals"]]), 
                       fd = nclass.FD(df[["residuals"]]))
      n_bins <- n_bins + 2
    }
    if (!is.numeric(n_bins)) {
      stop("'n_bins' should be a number or one of: ", paste(dQuote(c("sturges", "scott", "fd")), collapse = ", "))
    }
    plt <- ggplot(df, aes(x = .data$residuals)) + 
      geom_histogram(bins = n_bins, 
                     alpha = s_alpha,
                     size = linethick,
                     colour = "black", 
                     fill = s_colour)+
      labs(x = "pearson residuals", 
           y = "frequency", 
           title = "Histogram of residuals")+
      theme_grafify(base_size = base_size)
    plt
  }
  plt3 <- residuals_hist_plot(Model,
                              n_bins = n_bins)
  #last plot
  observed_fitted_plot <- function (Model) 
  {
    plt <- ggplot(df, aes(x = .data$fitted, 
                          y = .data$observed)) + 
      geom_point(fill = s_colour, 
                 alpha = s_alpha, 
                 shape = 21,
                 size = symsize,
                 stroke = linethick) + 
      labs(x = "fitted values", 
           y = "response", 
           title = "Observed vs fitted values")+
      theme_grafify(base_size = base_size)
    plt
  }
  
  plt4 <- observed_fitted_plot(Model)
  patchwork::wrap_plots(plt1, plt2, plt3, plt4,
                        nrow = 2)
}

#plot_qq_gam <- function(Model, ...){
#  p <- gratia::appraise(model = Model,
#                        line_col = "#E69F00",
#                        point_col = "#676767",
#                        point_alpha = 0.6,
#                        ci_alpha = 1,
#                        ...)&theme_classic()
#  p
#}
