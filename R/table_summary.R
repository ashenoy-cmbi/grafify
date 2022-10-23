#' Get numeric summary grouped by factors
#' 
#' This is a wrapper around \code{\link{aggregate}} function in base R to obtain mean, median, standard deviation and count for quantitative variable(s) grouped by one or more factors. More than one column containing of quantitative variables can be passed on, and summaries for each is provided with column names with a `.`. 
#'
#' @param data name of the data table.
#' @param Ycol name of one column (in quotes) or a vector of column names containing the numerical variable to be summarised.
#' @param ByGroup name of one column (in quotes) or a vector of column names containing the grouping factors 
#'
#' @return this function takes in a data.frame or tibble and returns a data.frame or tibble.
#' @export table_summary
#' @importFrom stats aggregate median sd
#'
#' @examples
#' table_summary(Ycol = "cty",
#' ByGroup = c("fl", "drv"),
#' data = mpg)
#' 
table_summary <- function(data, Ycol, ByGroup) {
  table <- aggregate(data[Ycol],
            by = data[ByGroup], 
            FUN = function(x){
              c(Mean = mean(x, na.rm = TRUE), 
                Median = median(x, na.rm = TRUE),
                SD = sd(x, na.rm = TRUE),
                Count = length(x))})
  t2 <- data.frame(lapply(table[Ycol], 
                          data.frame))
  t2 <- cbind(table[c(1:length(ByGroup))], t2)
  t2
}
