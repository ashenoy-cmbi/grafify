#' ANOVA table from a linear model fit to data.
#'
#' Update in v0.2.1: This function uses \code{\link{lm}} to fit a linear model to data, passes it on to \code{\link[car]{Anova}}, and outputs the ANOVA table with type II sum of squares with F statistics and _P_ values. 
#' (Previous versions produced type I sum of squares using \code{\link{anova}} call.)
#' 
#' It requires a data table, one quantitative dependent variable and one or more independent variables. If your experiment design has random factors, use the related function \code{\link{mixed_anova}}.
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
#' @importFrom car Anova
#'
#' @examples
#' #Basic usage where the table data_doubling_time is passed with names of the fixed factor within quotes
#'
#' simple_anova(data_doubling_time, "Doubling_time", "Student")
#'

simple_anova <- function(data, Y_value, Fixed_Factor, ...){
  Y <- substitute(Y_value)

  ifelse(length(Fixed_Factor) == 1,
         Facs <- paste0(Fixed_Factor, collapse = ""),
         Facs <- paste0(Fixed_Factor, collapse = "*"))
  fo <- as.formula(paste(Y, "~", Facs))
  mod <- lm(fo, data, ...)
  mod$call$formula <-fo
  t1 <- car::Anova(mod)
  h1 <- attr(t1, "heading")
  t1$`Mean sq` <- t1$`Sum Sq`/t1$Df
  t1 <- t1[, c(1, 5, 2,3,4)]
  attr(t1, "heading") <- h1
  t1
}
