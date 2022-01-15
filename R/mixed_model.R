#' Model from a linear mixed effects model
#'
#' There are four related functions for mixed effects analyses: \code{mixed_model}, \code{mixed_anova}, \code{mixed_model_slopes}, and \code{mixed_anova_slopes}.
#'
#'
#' This function uses \code{\link[lme4]{lmer}} to fit a linear mixed effect model and provides the model object, which could be used for post-hoc comparisons. The model object is converted to class `lmerModLmerTest` object by \code{\link[lmerTest]{as_lmerModLmerTest}}.
#' 
#' It requires a data table, one dependent variable (Y_value), one or more independent variables (Fixed_Factor), and at least one random factor (Random_Factor). These should match names of variables in the long-format data table exactly.
#' This function is related to \code{\link{mixed_anova}}. 
#' Output of this function can be used with \code{\link{posthoc_Pairwise}}, \code{\link{posthoc_Levelwise}} and \code{\link{posthoc_vsRef}}, or with \code{\link[emmeans]{emmeans}}.
#'
#' More than one fixed factors can be provided as a vector (e.g. c("A", "B")). A full model with interaction term is fitted. 
#' This means when \code{Y_value = Y, Fixed_factor = c("A", "B"), Random_factor = "R"} are entered as arguments, these are passed on as \code{Y ~ A*B + (1|R)} (which is equivalent to \code{Y ~ A + B + A:B + (1|R)}).
#' For simplicity, only random intercepts are fitted (\code{(1|R)}). 
#' 
#' Also see \code{mixed_anova_slopes} and \code{mixed_model_slopes} for similar functions where variable slopes and intercept models are fit.
#'
#' @param data a data table object, e.g. data.frame or tibble.
#' @param Y_value name of column containing quantitative (dependent) variable, provided within "quotes".
#' @param Fixed_Factor name(s) of categorical fixed factors (independent variables) provided as a vector if more than one or within "quotes".
#' @param Random_Factor name(s) of random factors to allow random intercepts; to be provided as a vector when more than one or within "quotes".
#' @param ... any additional arguments to pass on to \code{\link[lme4]{lmer}} if required.
#'
#' @return This function returns an S4 object of class "lmerModLmerTest".
#' @export mixed_model
#' @importFrom lme4 lmer
#' @importFrom lmerTest as_lmerModLmerTest
#' @importFrom stats as.formula
#'
#' @examples
#' #one fixed factor and random factor
#' mixed_model(data = data_doubling_time, 
#' Y_value = "Doubling_time", 
#' Fixed_Factor = "Student", 
#' Random_Factor = "Experiment")
#' 
#' #two fixed factors as a vector, one random factor
#' mixed_model(data = data_cholesterol, 
#' Y_value = "Cholesterol", 
#' Fixed_Factor = c("Treatment", "Hospital"), 
#' Random_Factor = "Subject")
#'
#' #save model
#' model <- mixed_model(data = data_doubling_time, 
#' Y_value =  "Doubling_time", 
#' Fixed_Factor = "Student", 
#' Random_Factor = "Experiment")
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
