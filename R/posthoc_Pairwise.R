#' Pairwise post-hoc comparisons from a linear or linear mixed effects model.
#'
#' This function is a wrapper based on \code{\link[emmeans]{emmeans}}, and needs a model produced by \code{\link{simple_model}} (which calls \code{\link{lm}}) or a linear mixed effects model produced by \code{\link{mixed_model}} (which calls \code{\link[lmerTest]{lmer}}). It also needs to know the fixed factor(s), which should match those in the model and data table. 
#'
#' The function will generate [pairwise comparisons](https://CRAN.R-project.org/package=emmeans) of every level of every factor (as described in Comparisons and contrasts in emmeans). Too many comparisons will be generated and only use this when necessary.
#' By default, P values are corrected by the FDR method (which can be changed). If the model was fit by transforming the quantitative response variable using "log", "logit", "sqrt" etc., results will still be on the original scale, i.e. \code{type = "response"} is the default; data will be back-transformed (check results to confirm this), and for log or logit see Transformations and link functions in emmeans, [ratios will be compared](https://CRAN.R-project.org/package=emmeans).
#' The first part of the \code{emmeans} results has the estimated marginal means, SE and CI (\code{$emmeans}), which are generated from the fitted model, and **not** the original data table. The second part has the results of the comparisons (\code{$contrasts}).
#'
#' @param Model a model object fit using \code{\link{simple_model}} or \code{\link{mixed_model}} or related.
#' @param Fixed_Factor one or  more categorical variables, provided as a vector (see Examples), whose levels you wish to compare pairwise. Names of Fixed_Factor should match Fixed_Factor used to fit the model. When more than one factor is provided e.g. \code{Fixed_factor = c("A", "B")}, this function passes this on as \code{specs = A:B} (note the colon between the two Fixed_Factor) to \code{\link[emmeans]{emmeans}} to produce pairwise comparisons.
#' @param P_Adj method for correcting P values for multiple comparisons. Default is set to false discovery rate ("fdr"), can be changed to "none", "tukey", "bonferroni", "sidak". See Interaction analysis in emmeans in the [manual](https://CRAN.R-project.org/package=emmeans) for \code{emmeans}.
#' @param ... additional arguments for \code{\link[emmeans]{emmeans}} such as \code{lmer.df} or others. See help for sophisticated models in [emmeans](https://CRAN.R-project.org/package=emmeans).
#' @param Factor old argument name for `Fixed_Factor`; retained for backward compatibility.
#'
#' @return returns results produced by \code{\link[emmeans]{emmeans}}.
#' @export posthoc_Pairwise
#' @importFrom emmeans emmeans
#' @importFrom stats as.formula
#'
#' @examples
#' #make linear models first
#' DoublMod <- simple_model(data = data_doubling_time, 
#' Y_value =  "Doubling_time", Fixed_Factor = "Student") 
#' CholMod <- mixed_model(data = data_cholesterol, 
#' Y_value = "Cholesterol",
#' Fixed_Factor = c("Hospital", "Treatment"), 
#' Random_Factor = "Subject") 
#' 
#' posthoc_Pairwise(Model = DoublMod, 
#' Fixed_Factor = "Student")
#'
#' #basic use with two Fixed_Factor provided as a vector
#' posthoc_Pairwise(Model = CholMod, 
#' Fixed_Factor = c("Treatment", "Hospital"))
#'
#' #same call with "tukey" adjustment
#' posthoc_Pairwise(Model = CholMod, 
#' Fixed_Factor = c("Treatment", "Hospital"), 
#' P_adj = "tukey")
#'


posthoc_Pairwise <- function(Model, Fixed_Factor, P_Adj = "fdr", Factor, ...){
  if (!missing("Factor")) {
    warning("Use `Fixed_Factor` argument instead, as `Factor` is deprecated.")
    Fixed_Factor <- Factor}
  ifelse(length(Fixed_Factor) > 1,
         comp <- paste0(Fixed_Factor, collapse = ":"),
         comp <- paste0(Fixed_Factor))
  sp <- as.formula(paste("pairwise ~",
                         comp,
                         collapse = ""))
  pc <- emmeans::emmeans(object = Model,
                specs = sp,
                type = "response",
                adjust = P_Adj,
                ...)
  pc
}
