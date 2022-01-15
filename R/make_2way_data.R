#' Make one-way or two-way independent group or randomised block design data.
#'
#' The \code{\link{make_1way_data}}, \code{\link{make_1way_rb_data}}, \code{\link{make_2way_data}} and \code{\link{make_2way_rb_data}} functions generate independent or randomised block (rb) design data of one-way or two-way designs.
#'
#' Random variates from the normal distribution based on user provided mean and SD provided are generated. For independent designs, the `Residual_SD` argument is used to set expected residual SD from the linear model. Exp_SD is used to set experiment-to-experiment SD, that will be assigned to the random factor for rb designs.
#'
#' Num_obs sets the number of independent measurements per group.
#'
#' For one-way designs, the user provides Group_means as a vector. Number of levels are recognised based on number of means.
#' For two-way designs, two vectors are to be provided by the user containing means of levels of a second factor. Number of means in both vectors should be the same. These functions can only handle balanced designs, i.e. same number of observations in all groups.
#'
#' The output is a data frame with one or two columns denoting the fixed factor with levels that match the number of means entered. For rb data, the column for RandFac denotes levels of the blocking factor.
#' The quantitative response variables are in the numeric Values column.
#'
#' @param Group_1_means a vector with means of each level of the first fixed factor (FixFac_X1) measured within Group 1.
#' @param Group_2_means only for \code{make_2way_data} and \code{make_2way_rb_data}: a vector with mean(s) of each level of FactorX2 measured within Group 2.
#' @param Num_obs a single numeric value indicating the number of independent measurements, i.e. levels within the random factor Experiment.
#' @param Residual_SD a single numeric value indicating residual SD in the model.
#'
#' @return This function produces a \code{data.frame} object containing simulated data.
#' @export make_2way_data
#' @importFrom purrr map_dfc set_names
#' @importFrom tidyr pivot_longer
#' @importFrom stats rnorm
#' @importFrom magrittr %>% 
#'
#' @examples
#' #Basic usage with two levels within FactorX2, 20 observations in each group, with residual SD 15
#'
#' two_independent_tab <- make_2way_data(c(100, 20), c(200, 300), 20, 15)
#'
#' #Four levels with 5 observations and residual SD 5
#' two_independent_tab <- make_2way_data(c(100, 20, 1500, 20), c(150, 5, 1450, 25), 5, 5)

make_2way_data <- function(Group_1_means, Group_2_means, Num_obs, Residual_SD) {
  {
    if(!length(Group_1_means) == length(Group_2_means)){
      stop("Both groups should have same number of means")
    } else{
      suppressMessages(df1 <- map_dfc(c(Group_1_means, Group_2_means),
                                      function(x) rnorm(Num_obs, x, Residual_SD)) %>%
                         set_names(paste0("Fac_", rep(1:2,
                                                      each = length(Group_1_means),
                                                      length = length(Group_1_means)*2),
                                          "x",
                                          paste0("Lev_",
                                                 rep(1:length(Group_2_means),
                                                     length = length(Group_2_means)*2)))) %>%
                         pivot_longer(names_to = c("FixFac_1", "FixFac_2"),
                                      names_sep = "x",
                                      cols = 1:(length(Group_1_means)+
                                                  length(Group_2_means)),
                                      values_to = "Values"))
                         df1 <- as.data.frame(df1)
      df1$FixFac_1 <- as.factor(df1$FixFac_1)
      df1$FixFac_2 <- as.factor(df1$FixFac_2)
      df1
    }
  }
}
