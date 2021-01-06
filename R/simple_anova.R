#' ANOVA table from a linear model fit to data.
#'
#' This function uses \code{\link{lm}} to fit a linear model to data, passes it on to \code{\link{anova}}, and outputs the ANOVA table. It requires a data table, one quantitative dependent variable and one or more independent variables. If your experiment design has random factors, use the related function \code{\link{mixed_anova}}.
#'
#' This function is related to \code{link{simple_model}}.
#'
#' @param data a data table object, e.g. data.frame or tibble.
#' @param Y_value name of column containing quantitative (dependent) variable, provided within "quotes".
#' @param Fixed_Factor name(s) of categorical fixed factors (independent variables) provided as a vector if more than one or within "quotes".
#' @param ... any additional argument to ass on to \code{\link{lm}} if required.
#'
#' @return ANOVA table output by \code{anova}.
#' @export simple_anova
#'
#' @examples
#' #Basic usage where the table data_cholesterol is passed with names of one variable within quotes
#'
#' simple_anova(data_cholesterol, "Cholesterol", "Treatment")
#'
#' #two way ANOVA with Treatment & Hospital as fixed factors
#' simple_anova(data_cholesterol, "Cholesterol", c("Treatment", "Hospital"))

simple_anova <- function(data, Y_value, Fixed_Factor, ...){
  Y <- substitute(Y_value)

  ifelse(length(Fixed_Factor) == 1,
         Facs <- paste0(Fixed_Factor, collapse = ""),
         Facs <- paste0(Fixed_Factor, collapse = "*"))
  fo <- as.formula(paste(Y, "~", Facs))
  mod <- lm(fo, data, ...)
  mod$call$formula <-fo
  anova(mod)
}
