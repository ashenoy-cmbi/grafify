#' Get numeric summary grouped by factors
#' 
#' This is a wrapper around \code{\link{aggregate}} function in base R to obtain mean, median, standard deviation and count for quantiative variable(s) grouped by one or more factors.
#'
#' @param Ycol name of one column or a vector of column names containing the numerical variable to be summarised.
#' @param ByXcol name of one column or a vector of column names containing the grouping factors 
#' @param data name of the data table.
#'
#' @return this function takes in a data.frame or tibble and returns a data.frame or tibble.
#' @export get_summary
#' @importFrom stats aggregate median sd
#'
#' @examples
#' get_summary(Ycol = "cty",
#' ByXcol = c("fl", "drv"),
#' data = mpg)
#' 
get_summary <- function(Ycol, ByXcol, data) {
  aggregate(data[Ycol],
            by = data[ByXcol], 
            FUN = function(x){
              c(Mean = mean(x, na.rm = TRUE), 
                Median = median(x, na.rm = TRUE),
                SD = sd(x, na.rm = TRUE),
                Count = length(x))})
}
