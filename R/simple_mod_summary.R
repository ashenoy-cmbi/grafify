#' Model summary from fit of a linear model to data.
#'
#' This function uses \code{\link{lm}} to fit a linear model to data and provides the full \code{\link{summary}}. It requires a data table, one quantitative dependent variable and one or more independent variables. The model output can be used to extract coefficients and other information. If your experiment design has random factors, use the related function \code{\link{mixed_mod_summary}}.
#'
#' This function is related to \code{link{simple_anova}} and \code{\link{simple_model}}.
#'
#' @param data a data table object, e.g. data.frame or tibble
#' @param Y_value name of column containing quantitative (dependent) variable, provided within "quotes".
#' @param Fixed_Factor name(s) of categorical fixed factors (independent variables) provided as a vector if more than one or within "quotes".
#' @param ... any additional arguments to pass on to \code{lm} or \code{anova}.
#'
#' @return This function returns the output of from \code{summary(lm())}.
#' @export simple_mod_summary
#'
#' @examples
#' #Basic usage where the table Chol is passed with names of one variable within quotes
#'
#' simple_lmod_summary(Chol, "Cholesterol", "Treatment")
#'
#' #two way ANOVA with Treatment & Hospital as fixed factors
#' simple_lmod_summary(Chol, "Cholesterol", c("Treatment", "Hospital"))
#'

simple_mod_summary <- function(data, Y_value, Fixed_Factor, ...){
  Y <- substitute(Y_value)

  ifelse(length(Fixed_Factor) == 1,
         Facs <- paste0(Fixed_Factor, collapse = ""),
         Facs <- paste0(Fixed_Factor, collapse = "*"))
  fo <- as.formula(paste(Y, "~", Facs))
  mod <- lm(fo, data)
  mod$call$formula <-fo
  summary(mod)
}
