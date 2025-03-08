#' Use emtrends to get pairwise comparison of slopes from a linear model.
#'
#' This function is a wrapper based on \code{\link[emmeans]{emmeans}}, and needs a ordinary linear model produced by \code{\link{simple_model}} or a mixed effects model produced by \code{\link{mixed_model}} or \code{\link{mixed_model_slopes}} (or generated directly with `lm`,  `lme4` or `lmerTest` calls). At least one of the factors should be a numeric covariate whose slopes you wish to find. It also needs to know the fixed factor(s), which should match those in the model and data table. 
#' 
#' Checkout the Interactions with covariates section in the [emmeans](https://CRAN.R-project.org/package=emmeans) vignette for more details. One of the independent variables should  be a quantitative (e.g. time points) variable whose slope (trend) you want to find at levels of the other factor.
#'
#' @param Model a model object fit using \code{\link{simple_model}} or \code{\link{mixed_model}} (or \code{\link[stats]{lm}} or \code{\link[lmerTest]{lmer}}).
#' @param Fixed_Factor one or  more categorical variables, provided as a vector (see Examples), whose levels you wish to compare pairwise. Names of Fixed_Factor should match Fixed_Factor used to fit the model. When more than one factor is provided e.g. \code{Fixed_factor = c("A", "B")}, this function passes this on as \code{specs = A:B} (note the colon between the two Fixed_Factor) to \code{\link[emmeans]{emmeans}} to produce pairwise comparisons.
#' @param Trend_Factor a quantitative variable that interacts with a factor and whose slope (trend) is to be compared 
#' @param P_Adj method for correcting P values for multiple comparisons. Default is "sidak", can be changed to "bonferroni". See Interaction analysis in emmeans in the [manual](https://CRAN.R-project.org/package=emmeans) for \code{emmeans}.
#' @param ... additional arguments for \code{\link[emmeans]{emmeans}} such as \code{lmer.df} or others. See help for sophisticated models in [emmeans](https://CRAN.R-project.org/package=emmeans).
#'
#' @return returns an "emm_list" object containing slopes and their contrasts calculated through  \code{\link[emmeans]{emtrends}}.
#' @export posthoc_Trends_Pairwise
#' @importFrom emmeans emtrends
#' @importFrom stats as.formula model.frame
#'
#' @examples
#' #create an lm model 
#' #Time2 is numeric (time points)
#' m1 <- simple_model(data = data_2w_Tdeath, 
#' Y_value = "PI", Fixed_Factor = c("Genotype", "Time2"))
#' posthoc_Trends_Pairwise(Model = m1, 
#' Fixed_Factor = "Genotype", 
#' Trend_Factor = "Time2")
#'

posthoc_Trends_Pairwise <- function(Model, Fixed_Factor, Trend_Factor, P_Adj = "sidak", ...){
  t1 <- stats::model.frame(Model)
  if (!is.numeric(t1[, Trend_Factor])) stop("Trend_Factor should be numeric.")
  ifelse(length(Fixed_Factor) > 1,
         comp <- paste0(Fixed_Factor, collapse = ":"),
         comp <- paste0(Fixed_Factor))
  sp <- as.formula(paste("pairwise ~",
                         comp,
                         collapse = ""))
  pc <- emmeans::emtrends(object = Model,
                specs = sp,
                var = Trend_Factor,
                type = "response",
                adjust = P_Adj,
                ...)
  pc
}
