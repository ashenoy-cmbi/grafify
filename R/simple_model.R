#' Model from a linear model fit to data.
#'
#' One of two functions for simple ANOVA tables and linear models without random effects, which use \code{\link{lm}} to fit a linear models. 
#' 1. \code{link{simple_anova}}
#' 2. \code{link{simple_model}}
#' 
#' Update in v0.2.1: This function uses \code{\link{lm}} to fit a linear model to data, passes it on to \code{\link[car]{Anova}}, and outputs the ANOVA table with type II sum of squares with F statistics and _P_ values. 
#' 
#' (Previous versions produced type I sum of squares using \code{\link{anova}} call.)
#' It requires a data table, one quantitative dependent variable and one or more independent variables. 
#' 
#' The model output can be used to extract coefficients and other information, including post-hoc comparisons. If your experiment design has random factors, use the related function \code{\link{mixed_model}}.
#'
#' This function is related to \code{link{simple_anova}}. 
#' Output of this function can be used with \code{\link{posthoc_Pairwise}}, \code{\link{posthoc_Levelwise}} and \code{\link{posthoc_vsRef}}, or with \code{\link[emmeans]{emmeans}}.
#'
#' @param data a data table object, e.g. data.frame or tibble.
#' @param Y_value name of column containing quantitative (dependent) variable, provided within "quotes". Data transformations, such as "log(Y_value)" or "logit(Y_value)" are accepted (note quotes outside the log or logit calls).
#' @param Fixed_Factor name(s) of categorical fixed factors (independent variables) provided as a vector if more than one or within "quotes".
#' @param ... any additional arguments to pass on to \code{\link{lm}} if required.
#'
#' @return This function returns an object of class "lm".
#' @export simple_model
#' @importFrom stats as.formula lm
#' @importFrom car logit
#'
#' @examples
#' #fixed factors provided as a vector
#' Doubmodel <- simple_model(data = data_doubling_time,
#' Y_value =  "Doubling_time", 
#' Fixed_Factor = "Student")
#' #get summary
#' summary(Doubmodel)

simple_model <- function(data, Y_value, Fixed_Factor, ...){
  Y <- substitute(Y_value)
  d <- substitute(data)
  ifelse(length(Fixed_Factor) == 1,
         Facs <- paste0(Fixed_Factor, collapse = ""),
         Facs <- paste0(Fixed_Factor, collapse = "*"))
  fo <- as.formula(paste(Y, Facs, sep = "~"))
  call1 <- paste0("lm(formula = ", 
                  deparse1(fo), 
                  ", data = ", 
                  deparse1(d), 
                  ", ...)")
  mod <- eval(parse(text = call1))
  mod
}
