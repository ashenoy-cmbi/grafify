#' Pairwise post-hoc comparisons from a linear or linear mixed effects model.
#'
#' This function is a wrapper based on \code{\link[emmeans]{emmeans}}, and needs a model produced by \code{\link{simple_model}} (which calls \code{\link{lm}}) or a linear mixed effects model produced by \code{\link{mixed_model}} (which calls \code{\link[lmerTest]{lmer}}). It also needs to know the fixed factors, which should match those in the model.
#'
#' The function will generate [pairwise comparisons](https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html#pairwise) of every level of every factor. Too many comparisons will be generated and only use this when necessary.
#' By default, P values are corrected by the FDR method (which can be changed). If the model was fit by transforming the quantitative response variable using "log", "logit", "sqrt" etc., results will still be on the [original scale](https://cran.r-project.org/web/packages/emmeans/vignettes/transformations.html#overview), i.e. \code{type = "response"} is the default; data will be back-transformed (check results to confirm this), and for log or logit, [ratios will be compared](https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html#logs).
#' The first part of the \code{emmeans} results has the estimated marginal means, SE and CI (\code{$emmeans}), which are generated from the fitted model, and **not** the original data table. The second part has the results of the comparisons (\code{$contrasts}).
#'
#' @param Model a model object fit using \code{\link{simple_model}} or \code{\link{mixed_model}} (or \code{\link{lm}} or \code{\link[lmerTest]{lmer}}).
#' @param Factors one or  more categorical variables, provided as a vector (see Examples), whose levels you wish to compare pairwise. Names of factors should match factors used to fit the model. When more than one factor is provided e.g. \code{Fixed_factor = c("A", "B")}, this function passes this on as \code{specs = A:B} (note the colon between the two factors) to \code{\link[emmeans]{emmeans}} to produce pairwise comparisons.
#' @param P_Adj method for correcting P values for multiple comparisons. Default is set to false discovery rate ("fdr"), can be changed to "none", "tukey", "bonferroni", "sidak". See the [manual](https://cran.r-project.org/web/packages/emmeans/vignettes/confidence-intervals.html#adjust) for \code{emmeans}.
#' @param ... additional arguments for \code{\link[emmeans]{emmeans}} such as \code{lmer.df} or others. See help for sophisticated models in [emmeans](https://cran.r-project.org/web/packages/emmeans/vignettes/sophisticated.html).
#'
#' @return returns results produced by \code{\link[emmeans]{emmeans}}.
#' @export posthoc_Pairwise
#' @importFrom emmeans emmeans
#' @importFrom stats as.formula
#'
#' @examples
#' #make linear models first
#' DoublMod <- simple_model(data_doubling_time, "Doubling_time", "Student") 
#' CholMod <- mixed_model(data_cholesterol, "Cholesterol", c("Hospital", "Treatment"), "Subject") 
#' 
#' posthoc_Pairwise(DoublMod, "Student")
#'
#' #basic use with two factors provided as a vector
#' posthoc_Pairwise(CholMod, c("Treatment", "Hospital"))
#'
#' #same call with "tukey" adjustment
#' posthoc_Pairwise(CholMod, c("Treatment", "Hospital"), P_adj = "tukey")
#'


posthoc_Pairwise <- function(Model, Factors, P_Adj = "fdr", ...){
  ifelse(length(Factors) > 1,
         comp <- paste0(Factors, collapse = ":"),
         comp <- paste0(Factors))
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
