#' Level-wise post-hoc comparisons from a linear or linear mixed effects model.
#'
#' This function is a wrapper based on \code{\link[emmeans]{emmeans}}, and needs a ordinary linear model produced by \code{\link{simple_model}} or a mixed effects model produced by \code{\link{mixed_model}} or \code{\link{mixed_model_slopes}} (or generated directly with `lm`,  `lme4` or `lmerTest` calls). It also needs to know the fixed factor(s), which should match those in the model and data table. 
#'
#' The function will generate [level-wise comparisons](https://CRAN.R-project.org/package=emmeans) (as described in Comparisons and contrasts in emmeans), i.e. comparison between of every level of one factor separately at each level of the other factor.
#' By default, P values are corrected by the FDR method (which can be changed). If the model was fit by transforming the quantitative response variable using "log", "logit", "sqrt" etc., results will still be on the original scale, i.e. \code{type = "response"} is the default; data will be back-transformed (check results to confirm this), and for log or logit see Transformations and link functions in emmeans, [ratios will be compared](https://CRAN.R-project.org/package=emmeans).
#' The first part of the \code{\link{emmeans}} results has the estimated marginal means, SE and CI (\code{$emmeans}), which are generated from the fitted model, and **not** the original data table. The second part has the results of the comparisons (\code{$contrasts}).
#'
#' @param Model a model object fit using \code{\link{simple_model}} or \code{\link{mixed_model}} or related.
#' @param Fixed_Factor one or  more categorical variables, provided as a vector (see Examples), whose levels you wish to compare pairwise. Names of Fixed_Factor should match Fixed_Factor used to fit the model. When more than one factor is provided e.g. \code{Fixed_factor = c("A", "B")}, this function passes this on as \code{specs = A|B} (note the vertical | between the two Fixed_Factor) to \code{\link[emmeans]{emmeans}} to produce comparisons between each level A with each other listed separately at each level of B.
#' @param P_Adj method for correcting P values for multiple comparisons. Default is set to false discovery rate ("fdr"), can be changed to "none", "tukey", "bonferroni", "sidak". See Interaction analysis in emmeans in the [manual](https://CRAN.R-project.org/package=emmeans) for \code{emmeans}.
#' @param ... additional arguments for \code{\link[emmeans]{emmeans}} such as \code{lmer.df} or others. See help for sophisticated models in [emmeans](https://CRAN.R-project.org/package=emmeans).
#' @param Factor old argument name for `Fixed_Factor`; retained for backward compatibility.
#'
#' @return returns an "emm_list" object containing contrasts and emmeans through \code{\link[emmeans]{emmeans}}.
#' @export posthoc_Levelwise
#' @importFrom emmeans emmeans
#' @importFrom stats as.formula
#'
#' @examples
#' #make a linear model first
#' CholMod <- mixed_model(data = data_cholesterol, 
#' Y_value =  "Cholesterol", 
#' Fixed_Factor = c("Hospital", "Treatment"),
#' Random_Factor =  "Subject") 
#' 
#' #note quotes used only for fixed Fixed_Factor
#' #to get comparisons between different hospitals separately for each level of Treatment
#' posthoc_Levelwise(Model = CholMod, 
#' Fixed_Factor = c("Hospital", "Treatment"))
#'
#' #get comparisons between treatments separately at each hospital
#' posthoc_Levelwise(Model = CholMod, 
#' Fixed_Factor = c("Treatment", "Hospital"))
#'

posthoc_Levelwise <- function(Model, Fixed_Factor, P_Adj = "fdr", Factor, ...){
  if (!missing("Factor")) {
    warning("Use `Fixed_Factor` argument instead, as `Factor` is deprecated.")
    Fixed_Factor <- Factor}
  ifelse(length(Fixed_Factor) > 1,
         comp <- paste0(Fixed_Factor, collapse = "|"),
         comp <- paste0(Fixed_Factor))
  sp <- as.formula(paste0("pairwise ~",
                          comp,
                          collapse = ""))
  pc <- emmeans::emmeans(object = Model,
                specs = sp,
                type = "response",
                adjust = P_Adj,
                ...)
  pc
}
