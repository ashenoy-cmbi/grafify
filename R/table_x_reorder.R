#' Reordering groups along X-axis
#' 
#' This simple function takes in a data table and reorders groups (categorical variables or factors) to be plotted along the X-axis in a user-defined order. 
#' 
#' It uses two base R functions: `as.factor` to first force the user-selected column into a factor, and `factor` that reorders levels based on a user-provided vector. 
#'
#' @param data a data table or tibble.
#' @param xcol name of column in above data table (in quotes), e.g., "A", whose levels are to be reordered.
#' @param OrderX a vector of group names within the column selected in `xcol` in the desired order, .e.g., c("D", "A", "C").
#' @param ... any additional arguments for `factor` call.
#'
#' @return This function returns a data frame with a selected column converted into factor with reordered levels.
#' @export table_x_reorder
#'
#' @examples
#' #reorder levels within Genotype
#' new_data <- table_x_reorder(data_t_pratio, 
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
#' plot_scatterbox(data = table_x_reorder(data_t_pratio, 
#' xcol = "Genotype", 
#' OrderX = c("KO", "WT")), 
#' xcol = Genotype, 
#' ycol = Cytokine)
#' 
table_x_reorder <- function(data, xcol, OrderX, ...) {
  data[[xcol]] <- sapply(data[[xcol]], as.factor)
  data[[xcol]] <- factor(data[[xcol]], levels = OrderX, ...)
  data
}
