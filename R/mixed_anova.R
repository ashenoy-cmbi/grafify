#' ANOVA table from linear mixed effects analysis.
#' 
#' There are four related functions for mixed effects analyses: \code{mixed_model}, \code{mixed_anova}, \code{mixed_model_slopes}, and \code{mixed_anova_slopes}.
#'
#' This function uses \code{\link[lme4]{lmer}} to fit a linear mixed effect model and provides the model object, which could be used for post-hoc comparisons. The model object is converted to class `lmerModLmerTest` object by \code{\link[lmerTest]{as_lmerModLmerTest}}. This is then passes on the model to \code{\link{anova}} and provides the ANOVA table with F and P values.
#' It produces a type II sum of squares ANOVA table with Kenward-Roger approximation for degrees of freedom (as implemented in \code{lmerTest}) package.
#' It requires a data table, one dependent variable (Y_value), one or more independent variables (Fixed_Factor), and at least one random factor (Random_Factor). These should match names of variables in the long-format data table exactly.
#' This function is related to \code{\link{mixed_model}}.
#'
#' More than one fixed factors can be provided as a vector (e.g. c("A", "B")). A full model with interaction term is fitted. 
#' This means when \code{Y_value = Y, Fixed_factor = c("A", "B"), Random_factor = "R"} are entered as arguments, these are passed on as \code{Y ~ A*B + (1|R)} (which is equivalent to \code{Y ~ A + B + A:B + (1|R)}).
#' For simplicity, only random intercepts are fitted (\code{(1|R)}). 
#'
#' @param data a data table object, e.g. data.frame or tibble.
#' @param Y_value name of column containing quantitative (dependent) variable, provided within "quotes".
#' @param Fixed_Factor name(s) of categorical fixed factors (independent variables) provided as a vector if more than one or within "quotes".
#' @param Random_Factor name(s) of random factors to allow random intercepts; to be provided as a vector when more than one or within "quotes".
#' @param Df_method method for calculating degrees of freedom. Default is Kenward-Roger, can be changed to "Satterthwaite".
#' @param SS_method type of sum of square, default is type II, can be changed to "I", "III", "1" or "2", or others.
#' @param ... any additional arguments to pass on to \code{\link[lme4]{lmer}} if required.
#'
#' @return This function returns the output of \code{anova}.
#' @export mixed_anova
#' @importFrom lmerTest as_lmerModLmerTest
#' @importFrom stats as.formula anova
#' @importFrom lme4 lmer
#'
#' @examples
#' #Usage with one fixed (Student) and random factor (Experiment), each within quotes
#' mixed_anova(data = data_doubling_time, 
#' Y_value = "Doubling_time", 
#' Fixed_Factor = "Student", 
#' Random_Factor = "Experiment")
#' 
#' #two fixed factors provided as a vector
#' mixed_anova(data = data_cholesterol, 
#' Y_value = "Cholesterol", 
#' Fixed_Factor = c("Treatment", "Hospital"), 
#' Random_Factor = "Subject")
#'

mixed_anova <- function(data, Y_value, Fixed_Factor, Random_Factor, Df_method = "Kenward-Roger", SS_method = "II", ...){
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
  anova(mod1,
        type = SS_method,
        ddf = Df_method)
}
