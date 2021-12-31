#' Model from a linear mixed effects model
#'
#' This function uses \code{\link[lmerTest]{lmer}} to fit a linear mixed effect model and provides the model object, which could be used for post-hoc comparisons. 
#' It requires a data table, one dependent variable (Y_value), one or more independent variables (Fixed_Factor), and at least one random factor (Random_Factor).
#' This function is related to \code{\link{mixed_anova}}. 
#' Output of this function can be used with \code{\link{posthoc_Pairwise}}, \code{\link{posthoc_Levelwise}} and \code{\link{posthoc_vsRef}}, or with \code{\link[emmeans]{emmeans}}.
#'
#' When more than one fixed factors are entered in the argument, a full model with interaction term is fitted. 
#' This means when \code{Y_value = Y, Fixed_factor = c("A", "B"), Random_factor = "R"} are entered as arguments, these are passed on as \code{Y ~ A*B + (1|R)} (which is equivalent to \code{Y ~ A + B + A:B + (1|R)}).
#' For simplicity, only random intercepts are fitted (\code{(1|R)}). For factorial ANOVAs the default sum of squares is Type II and degrees of freedom are calculated using the Kenward-Roger approximation. 
#' All other settings are \code{\link[lmerTest]{lmer}} and \code{\link{anova}} defaults.
#'
#' @param data a data table object, e.g. data.frame or tibble.
#' @param Y_value name of column containing quantitative (dependent) variable, provided within "quotes".
#' @param Fixed_Factor name(s) of categorical fixed factors (independent variables) provided as a vector if more than one or within "quotes".
#' @param Random_Factor name(s) of random factors to allow random intercepts; to be provided as a vector when more than one or within "quotes".
#' @param ... any additional arguments to pass on to \code{\link[lmerTest]{lmer}} if required.
#'
#' @return This function returns the output of \code{lmerTest::lmer()}.
#' @export mixed_model
#' @importFrom lme4 lmer
#' @importFrom lmerTest as_lmerModLmerTest
#' @importFrom stats as.formula
#'
#' @examples
#' #Basic usage where the table Chol is passed with names of variables within quotes
#' mixed_model(data_cholesterol, "Cholesterol", c("Treatment", "Hospital"), "Subject")
#'
#' mixed_model(data_doubling_time, "Doubling_time", "Student", "Experiment")
#'
#' #save model
#' model <- mixed_model(data_doubling_time, "Doubling_time", "Student", "Experiment")
#'
#' #get model summary
#' summary(model)

mixed_model <- function(data, Y_value, Fixed_Factor, Random_Factor, ...){
  Y <- substitute(Y_value)
  d <- substitute(data)
  ifelse(length(Fixed_Factor) == 1,
         Facs <- paste0(Fixed_Factor, collapse = ""),
         Facs <- paste0(Fixed_Factor, collapse = "*"))

  ifelse((length(Random_Factor) == 1),
         RFacs <- paste0("(1|", Random_Factor, ")"),
         RFacs <- paste0("(1|", Random_Factor, ")", collapse = "+"))

  fo <- as.formula(paste(Y,
                         paste(paste(Facs, collapse = ""),
                               paste(RFacs, collapse = ""),
                               sep = "+"),
                         sep = " ~ "))
  call1 <- paste0("lmer(formula = ", 
                  deparse1(fo), 
                  ", data = ", 
                  deparse1(d), 
                  ", ...)")
  mod1 <- eval(parse(text = call1))
  mod1 <- as_lmerModLmerTest(mod1)
  mod1
}
