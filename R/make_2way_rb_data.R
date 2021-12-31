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
#' @param Group1_means a vector with means of each level of the first fixed factor (FixFac_X1) measured within Group 1
#' @param Group2_means only for \code{make_2way_data} and \code{make_2way_rb_data}: a vector with mean(s) of each level of FactorX2 measured within Group 2
#' @param Num_exp a single numeric value indicating the number of independent measurements, i.e. levels within the random factor Experiment
#' @param Exp_SD a single numeric value indicating the standard deviation (SD) between experiments
#' @param Residual_SD a single numeric value indicating residual SD in the model

#' @return This function produces a \code{data.frame} object
#' @export make_2way_rb_data
#' @importFrom purrr map_dfc set_names
#' @importFrom tidyr pivot_longer
#' @importFrom stats rnorm
#' @importFrom magrittr %>% 
#'
#' @examples
#' #Basic usage with two levels within FactorX2, 
#' #20 experiments with inter-experiment SD 20, and residual SD 15
#'
#' two_rb_tab <- make_2way_rb_data(c(100, 20), c(200, 300), 20, 20, 15)
#'
#' str(two_rb_tab)
#' head(two_rb_tab)

make_2way_rb_data <- function(Group1_means, Group2_means, Num_exp, Exp_SD, Residual_SD) {
  if(!length(Group1_means) == length(Group2_means)) {
    stop("Number of group means should be the same")
  } else{
    All_means <- c(Group1_means, Group2_means)
    Group_slopes <- All_means - All_means[[1]]
    Base_val <- rnorm(n = Num_exp,
                      Group1_means[[1]],
                      sd = Exp_SD)

    suppressMessages(df1 <- map_dfc(Group_slopes,
                                    function(x) {x +
                                        Base_val +
                                        rnorm(Num_exp,
                                              0,
                                              Residual_SD)}) %>%
                       set_names(paste0("Fac_",
                                        rep(1:2,
                                            each = length(Group1_means),
                                            length = length(Group1_means)*2),
                                        "x",
                                        paste0("Lev_",
                                               rep(1:length(Group2_means),
                                                   length = length(Group2_means)*2)))))


    suppressMessages(Group_names <- paste0("Exp_",
                                           rep(1:Num_exp)))

    df1$Experiment <- Group_names
    suppressMessages(df1 <- as.data.frame(df1) %>%
                       pivot_longer(cols = 1:length(All_means),
                                    names_to = c("FixFac_1", "FixFac_2"),
                                    names_sep="x",
                                    values_to = "Values"))

    df1$FixFac_1 <- as.factor(df1$FixFac_1)
    df1$FixFac_2 <- as.factor(df1$FixFac_2)
    df1$Experiment <- as.factor(df1$Experiment)
    as.data.frame(df1)
  }
}
