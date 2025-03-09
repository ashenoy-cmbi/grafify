#' ANOVA table from a linear model fit to data.
#'
#' One of two functions for simple ANOVA tables and linear models without random effects, which use \code{\link[stats]{lm}} to fit a linear models. 
#' 1. \code{link{simple_anova}}
#' 2. \code{link{simple_model}}
#' 
#' 
#' Update in v0.2.1: This function uses \code{\link[stats]{lm}} to fit a linear model to data, passes it on to \code{\link[car]{Anova}}, and outputs the ANOVA table with type II sum of squares with F statistics and _P_ values. 
#' (Previous versions produced type I sum of squares using \code{\link[stats]{anova}} call.)
#' 
#' It requires a data table, one quantitative dependent variable and one or more independent variables. If your experiment design has random factors, use the related function \code{\link{mixed_anova}}.
#'
#' This function is related to \code{link{simple_model}}.
#'
#' @param data a data table object, e.g. data.frame or tibble.
#' @param Y_value name of column containing quantitative (dependent) variable, provided within "quotes". The following transformations are permitted: "log(Y_value)", "log(Y_value + c)" where c a positive number, "logit(Y_value)" or "logit(Y_value/100)" which may be useful when `Y_value` are percentages  (note quotes outside the log or logit calls); "sqrt(Y_value)" or "(Y_value)^2" should also work. During posthoc-comparisons, log and logit transformations will be back-transformed to the original scale. Other transformations, e.g., "sqrt(Y_value)" will not be back-transformed. Check out the \code{\link[emmeans]{regrid}} and \code{\link[emmeans]{ref_grid}} for details if you need back-transformation to the response scale. 
#' @param Fixed_Factor name(s) of categorical fixed factors (independent variables) provided within quotes (e.g., "A") or as a vector if more than one (e.g., c("A", "B"). If a numeric variable(s) is used, transformations similar to `Y_value` are permitted.
#' @param ... any additional argument to pass on to \code{\link[stats]{lm}} if required.
#'
#' @return ANOVA table of class "anova" and "data.frame".
#' @export simple_anova
#' @importFrom car Anova logit
#' @importFrom stats as.formula lm
#'
#' @examples
#' #Basic usage 
#' simple_anova(data = data_doubling_time, 
#' Y_value = "Doubling_time", 
#' Fixed_Factor = "Student")
#'

simple_anova <- function(data, Y_value, Fixed_Factor, ...){
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
  t1 <- car::Anova(mod)
  h1 <- attr(t1, "heading")
  t1$`Mean sq` <- t1$`Sum Sq`/t1$Df
  t1 <- t1[, c(1, 5, 2,3,4)]
  attr(t1, "heading") <- h1
  t1
}
