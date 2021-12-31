#' Make one-way or two-way independent group or randomised block design data.
#'
#' The \code{\link{make_1way_data}}, \code{\link{make_1way_rb_data}}, \code{\link{make_2way_data}} and \code{\link{make_2way_rb_data}} functions generate independent or randomised block (rb) design data of one-way or two-way designs.
#'
#' Random variates from the normal distribution based on user provided mean and SD provided are generated. For independent designs, the `Residual_SD` argument is used to set expected residual SD from the linear model. Exp_SD is used to set experiment-to-experiment SD, that will be assigned to the random factor for rb designs.
#'
#' Num_exp sets the number of independent measurements per group.
#'
#' For one-way designs, the user provides Group_means as a vector. Number of levels are recognised based on number of means.
#' For two-way designs, two vectors are to be provided by the user containing means of levels of a second factor. Number of means in both vectors should be the same. These functions can only handle balanced designs, i.e. same number of observations in all groups.
#'
#' The output is a data frame with one or two columns denoting the fixed factor with levels that match the number of means entered. For rb data, the column for RandFac denotes levels of the blocking factor.
#' The quantitative response variables are in the numeric Values column.
#'
#' @param Group_means a vector with means of each level of the first fixed factor (FixFac_X1) measured within Group 1
#' @param Num_obs a single numeric value indicating the number of independent measurements, i.e. levels within the random factor Experiment
#' @param Residual_SD a single numeric value indicating residual SD in the model
#'
#' @return This function produces a \code{data.frame} object
#' @export make_1way_data
#' @importFrom purrr pmap_dfc map_df set_names
#' @importFrom tidyr pivot_longer
#' @importFrom stats rnorm
#' @importFrom magrittr %>% 
#' @importFrom utils globalVariables
#'
#' @examples
#' #Basic usage with three levels within Factor_X, 
#' #20 observations in each group, with residual SD 15
#'
#' one_independent_tab <- make_1way_data(c(350, 250, 100), 15, 20)
#'
#' str(one_independent_tab)
#' head(one_independent_tab)

#globalVariables(".")

make_1way_data <- function(Group_means, Num_obs, Residual_SD) {
  suppressMessages(ymce <- function(m, c, e){
    y = m + c + rnorm(1, mean = 0, sd = e)
    y})
  Group_slopes <- Group_means - Group_means[[1]]
  suppressMessages(df1 <- pmap_dfc(list(m = Group_slopes,
                                        c = Group_means[[1]],
                                        e = Residual_SD),
                                   ymce) %>%
                     set_names(paste0("Lev_",
                                      rep(1:length(Group_means))))) 
                     df1 <- map_df(df1, function(x) rnorm(n = Num_obs, 
                                                 mean = x, 
                                                 sd = Residual_SD)) %>%
                     pivot_longer(cols = 1:length(Group_means),
                                  names_to = "FixFac_1",
                                  values_to = "Values")
  df1 <- as.data.frame(df1)
  df1$FixFac_1 <- as.factor(df1$FixFac_1)
  as.data.frame(df1)
}
