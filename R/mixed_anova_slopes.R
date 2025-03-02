#' ANOVA table from linear mixed effects analysis.
#'
#' One of four related functions for mixed effects analyses (based on \code{\link[lme4]{lmer}} and \code{\link[lmerTest]{as_lmerModLmerTest}}) to get a linear model for downstream steps, or an ANOVA table.
#' 1. \code{mixed_model}
#' 2. \code{mixed_anova}
#' 3. \code{mixed_model_slopes}
#' 4. \code{mixed_anova_slopes}.
#'
#' 
#' These functions require a data table, one dependent variable (Y_value), one or more independent variables (Fixed_Factor), and at least one random factor (Random_Factor). These should match names of variables in the long-format data table exactly. Since v4.1.0, if `AvgRF = TRUE`, the response variable is averaged over levels of the fixed and random factors (to collapse replicate observations) and reduce the number of denominator degrees of freedom. If you do not want to do this, set `AvgRF = FALSE`.
#' 
#' Outputs of `mixed_model` and `mixed_model_slopes` can be used for post-hoc comparisons with \code{\link{posthoc_Pairwise}}, \code{\link{posthoc_Levelwise}}, \code{\link{posthoc_vsRef}}, \code{\link{posthoc_Trends_Pairwise}}, \code{\link{posthoc_Trends_Levelwise}} and \code{\link{posthoc_Trends_vsRef}}or with \code{\link[emmeans]{emmeans}}.
#'
#' More than one fixed factors can be provided as a vector (e.g. c("A", "B")). A full model with interaction term is fitted. 
#' This means when \code{Y_value = Y, Fixed_factor = c("A", "B"), Random_factor = "R"} are entered as arguments, these are passed on as \code{Y ~ A*B + (1|R)} (which is equivalent to \code{Y ~ A + B + A:B + (1|R)}).
#' 
#' In `mixed_model_slopes` and `mixed_anova_slopes`, the following kind of formula is used: \code{Y ~ A*B + (S|R)} (which is equivalent to \code{Y ~ A + B + A:B + (S|R)}). 
#' In this experimental implementation, random slopes and intercepts are fitted (\code{(Slopes_Factor|Random_Factor)}). Only one term each is allowed for `Slopes_Factor` and `Random_Factor`.
#'
#' @param data a data table object, e.g. data.frame or tibble.
#' @param Y_value name of column containing quantitative (dependent) variable, provided within "quotes". The following transformations are permitted: "log(Y_value)", "log(Y_value + c)" where c a positive number, "logit(Y_value)" or "logit(Y_value/100)" which may be useful when `Y_value` are percentages  (note quotes outside the log or logit calls). During posthoc-comparisons, the quantitative variable will back-transform to its original scale.
#' @param Fixed_Factor name(s) of categorical fixed factors (independent variables) provided as a vector if more than one or within "quotes".
#' @param Slopes_Factor name of factor to allow varying slopes on. 
#' @param Random_Factor name(s) of random factors to allow random intercepts; to be provided as a vector when more than one or within "quotes".
#' @param Df_method method for calculating degrees of freedom. Default is Kenward-Roger, can be changed to "Satterthwaite".
#' @param SS_method type of sum of square, default is type II, can be changed to "I", "III", "1" or "2", or others.
#' @param AvgRF this is a new argument since v4.1.0. The default `AvgRF = TRUE` will use the mean of `Y_value` (the response variable) grouped by levels of the `Fixed_Factor` and `Random_Factor`  (using \code{\link{table_summary}}). This ensures that replicates within `Random_Factor` (or any other unused variable) are averaged (e.g., technical replicates nested within experimental blocks) before fitting a linear model and the denominator Df values are sensible. Setting `AvgRF = FALSE` will lead to behaviour like versions <4.1.0.
#' @param ... any additional arguments to pass on to \code{\link[lme4]{lmer}} if required.
#'
#' @return ANOVA table of class "anova" and "data.frame".
#' @export mixed_anova_slopes
#' @importFrom lmerTest as_lmerModLmerTest
#' @importFrom stats as.formula anova
#' @importFrom lme4 lmer
#' @importFrom car logit
#' 
#' @examples
#' mixed_anova_slopes(data = data_2w_Tdeath,
#' Y_value = "PI",
#' Fixed_Factor = c("Genotype", "Time"),
#' Slopes_Factor = "Time",
#' Random_Factor = "Experiment")
#'

mixed_anova_slopes <- function(data, Y_value, Fixed_Factor, Slopes_Factor, Random_Factor, Df_method = "Kenward-Roger", SS_method = "II", AvgRF = TRUE, ...){
  if(AvgRF == TRUE){
    message("The new argument `AvgRF` is set to TRUE by default in >=v4.1.0). See help for details.")}  
  df <- data
  var_name <- Y_value
  lx1r = length(Fixed_Factor)+length(Random_Factor)
  # Check if the Y_value contains a transformation
  if (grepl("\\(|\\^", Y_value)) {
    # Extract the base variable name from the Y_value input
    var_name <- sub("^.*\\(([^\\)]+)\\).*|\\^.*$", "\\1", Y_value)
    # Further extract the variable name if it includes an addition or division (e.g., log(y + 0.1) or logit(y / 100))
    var_name <- sub("^(.*?)(\\s*\\+\\s*.*|\\s*/\\s*100)?$", "\\1", var_name)
  } else {
    # If no transformation, use the Y_value as is
    var_name <- Y_value
  }
  if(AvgRF == TRUE){
    avgdf <- table_summary(df,
                           var_name,
                           c(Fixed_Factor, 
                             Random_Factor))
    avgdf <- avgdf[, c(1:(lx1r + 1))]
    colnames(avgdf) <- c(Fixed_Factor, Random_Factor, var_name)
    df <- avgdf
  }
  if(AvgRF == FALSE){
    df <- data
  }
  ########## old formula code
  ifelse(length(Fixed_Factor) == 1,
         Facs <- paste0(Fixed_Factor, collapse = ""),
         Facs <- paste0(Fixed_Factor, collapse = "*"))
  if (length(Slopes_Factor) > 1) stop("Only one term for Slopes_Factor allowed; more than 1 supplied")
  ifelse((length(Random_Factor) == 1),
         RFacs <- paste0("(", Slopes_Factor, "|", Random_Factor, ")"),
         stop("Only 1 random factor allowed; more than 1 supplied"))
  
  fo <- as.formula(paste(Y_value,
                         paste(paste(Facs, collapse = ""),
                               paste(RFacs, collapse = ""),
                               sep = "+"),
                         sep = " ~ "))
  
  ########## like grafify online
  mod1 <- lmer(formula = fo,
               data = df)
  mod1@call$formula <- fo
  mod1@call$data <- substitute(data)
  mod1 <- lmerTest::as_lmerModLmerTest(mod1)
  mod1
  anova(mod1,
        type = SS_method,
        ddf = Df_method)
  ######################
  
#  Y <- substitute(Y_value)
#  d <- substitute(data)
#  ifelse(length(Fixed_Factor) == 1,
#         Facs <- paste0(Fixed_Factor, collapse = ""),
#         Facs <- paste0(Fixed_Factor, collapse = "*"))
#  if (length(Slopes_Factor) > 1) stop("Only one term for Slopes_Factor allowed; more than 1 supplied")
#  ifelse((length(Random_Factor) == 1),
#         RFacs <- paste0("(", Slopes_Factor, "|", Random_Factor, ")"),
#         stop("Only 1 random factor allowed; more than 1 supplied"))
#  
#  fo <- as.formula(paste(Y,
#                         paste(paste(Facs, collapse = ""),
#                               paste(RFacs, collapse = ""),
#                               sep = "+"),
#                         sep = " ~ "))
#  call1 <- paste0("lmer(formula = ", 
#                  deparse1(fo), 
#                  ", data = ", 
#                  deparse1(d), 
#                  ", ...)")
#  mod1 <- eval(parse(text = call1))
#  mod1 <- as_lmerModLmerTest(mod1)
#  anova(mod1,
#        type = SS_method,
#        ddf = Df_method)
}
