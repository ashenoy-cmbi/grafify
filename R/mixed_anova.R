#' ANOVA table from linear mixed effects analysis.
#'
#' This function uses \code{\link[lmerTest]{lmer}} to fit a linear mixed effect model, passes on the model to \code{\link{anova}} and provides the ANOVA table. 
#' It produces a type II sum of squares ANOVA table with Kenward-Roger approximation for degrees of freedom (as implemented in \code{lmerTest}) package.
#' It requires a data table, one dependent variable (Y_value), one or more independent variables (Fixed_Factor), and at least one random factor (Random_Factor).
#' This function is related to \code{\link{mixed_model}}.
#'
#' When more than one fixed factors are entered in the argument, a full model with interaction term is fitted. 
#' This means when \code{Y_value = Y, Fixed_factor = c("A", "B"), Random_factor = "R"} are entered as arguments, these are passed on as \code{Y ~ A*B + (1|R)} (which is equivalent to \code{Y ~ A + B + A:B + (1|R)}).
#' For simplicity, only random intercepts are fitted (\code{(1|R)}). For factorial ANOVAs the default sum of squares is Type II and degrees of freedom are calculated using the Kenward-Roger approximation. 
#' Check \code{\link[lmerTest]{lmer}} for more details.
#'
#' @param data a data table object, e.g. data.frame or tibble.
#' @param Y_value name of column containing quantitative (dependent) variable, provided within "quotes".
#' @param Fixed_Factor name(s) of categorical fixed factors (independent variables) provided as a vector if more than one or within "quotes".
#' @param Random_Factor name(s) of random factors to allow random intercepts; to be provided as a vector when more than one or within "quotes".
#' @param Df_method method for calculating degrees of freedom. Default is Kenward-Roger, can be changed to "Satterthwaite".
#' @param SS_method type of sum of square, default is type II, can be changed to "I", "III", "1" or "2", or others.
#' @param ... any additional arguments to pass on to \code{\link[lmerTest]{lmer}} if required.
#'
#' @return This function returns the output of \code{anova}.
#' @export mixed_anova
#' @import lmerTest
#'
#' @examples
#' #Basic usage where the table data_cholesterol is passed with names of two fixed factors as a vector
#' mixed_anova(data_cholesterol, "Cholesterol", c("Treatment", "Hospital"), "Subject")
#'
#' #Usages with one fixed (Student) and random factor (Experiment), each within quotes
#' mixed_anova(data_doubling_time, "Doubling_time", "Student", "Experiment")

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
  mod1 <- lmer(fo, data, ...)
  mod1@call$formula <- fo
  mod1@call$data <- d
  mod1
  lmerTest:::single_anova(mod1, 
        type = SS_method, 
        ddf = Df_method)
}
