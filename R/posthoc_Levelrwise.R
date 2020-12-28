#' Level-wise post-hoc comparisons from a linear or linear mixed effects model.
#'
#' This function is a wrapper based on \code{\link[emmeans]{emmeans}}, and needs a model produced by \code{\link{simple_model}} (which calls \code{\link{lm}}) or a linear mixed effects model produced by \code{\link{mixed_model}} (which calls \code{\link[lmerTest]{lmer}}). It also needs to know the fixed factors, which should match those in the model.
#'
#' The function will generate [level-wise comparisons](https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html#pairwise), i.e. comparison between of every level of one factor separately at each level of the other factor.
#' By default, P values are corrected by the FDR method (which can be changed). If the model was fit by transforming the quantitative response variable using "log", "logit", "sqrt" etc., results will still be on the [original scale](https://cran.r-project.org/web/packages/emmeans/vignettes/transformations.html#overview), i.e. \code{type = "response"} is the default; data will be back-transformed (check results to confirm this), and for log or logit, [ratios will be compared](https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html#logs).
#'
#' @param Model a model object fit using \code{\link{simple_model}} or \code{\link{mixed_model}} (or \code{\link{lm}} or \code{\link[lmerTest]{lmer}}).
#' @param Factors one or  more categorical variables, provided as a vector (see Examples), whose levels you wish to compare pairwise. Names of factors should match factors used to fit the model. When more than one factor is provided e.g. \code{Fixed_factor = c("A", "B")}, this function passes this on as \code{specs = A|B} (note the vertical | between the two factors) to \code{\link[emmeans]{emmeans}} to produce comparisons between each level A with each other listed separately at each level of B.
#' @param P_Adj method for correcting P values for multiple comparisons. Default is set to false discovery rate ("fdr"), can be changed to "none", "tukey", "bonferroni", "sidak". See the [manual](https://cran.r-project.org/web/packages/emmeans/vignettes/confidence-intervals.html#adjust) for \code{emmeans}.
#'
#' @return returns results produced by \code{\link[emmeans]{emmeans}}.
#' @export posthoc_Levelwise
#'
#' @examples
#' #basic use with one factor
#' #note quotes used only for fixed factors
#' #to get comparisons between different hospitals separately for each level of Treatment
#' posthoc_Levelwise(CholMod, c("Hospital", "Treatment"))
#'
#' #get comparisons between treatments separately at each hospital
#' posthoc_Levelwise(CholMod, c("Treatment", "Hospital"))
#'

posthoc_Levelwise <- function(Model, Factors, P_Adj = "fdr", ...){
  ifelse(length(Factors) > 1,
         comp <- paste0(Factors, collapse = "|"),
         comp <- paste0(Factors))
  sp <- as.formula(paste0("~",
                          comp,
                          collapse = ""))
  pc <- emmeans(object = Model,
                specs = sp,
                type = "response")
  pairs(pc, adjust = P_Adj)
}
