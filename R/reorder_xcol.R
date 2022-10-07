#' Reordering groups along X-axis
#' 
#' This simple function takes in a data table and reorders groups (categorical variables or factors) to be plotted along the X-axis in a user-defined order. 
#' 
#' It uses two base R functions: `as.factor` to first force the user-selected column into a factor, and `factor` that reorders levels based on a user-provided vector. 
#'
#' @param data a data table
#' @param xcol name of column in above data table (provided within quotes) whose levels are to be reordered
#' @param OrderX a vector of group names in the desired order
#' @param ... any additional arguments for `factor` call.
#'
#' @return This function returns a data frame with a selected column converted into factor with reordered levels.
#' @export reorder_xcol
#'
#' @examples
#' #reorder levels within Genotype
#' new_data <- reorder_xcol(data_t_pratio, 
#' xcol = "Genotype", 
#' OrderX = c("KO", "WT"))
#' #compare 
#' plot_scatterbox(data_t_pratio, 
#' Genotype, 
#' Cytokine)
#' #with
#' plot_scatterbox(new_data, 
#' Genotype, 
#' Cytokine)
#' #also works within the plot call
#' plot_scatterbox(data = reorder_xcol(data_t_pratio, 
#' xcol = "Genotype", 
#' OrderX = c("KO", "WT")), 
#' xcol = Genotype, 
#' ycol = Cytokine)
#' 
reorder_xcol <- function(data, xcol, OrderX, ...) {
  data[[xcol]] <- sapply(data[[xcol]], as.factor)
  data[[xcol]] <- factor(data[[xcol]], levels = OrderX, ...)
  data
}
