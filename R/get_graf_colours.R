#' Get graf internal
#'
#' Function to make grafify colour scheme. [Thank you Dr Simon](https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2).
#' 
#' To visualise grafify colours use \code{plot_grafify_palette}.
#'
#' @param ... internal
#' @export get_graf_colours

get_graf_colours <- function(...){
  cols <- c(...)
  if(is.null(cols))
    return(graf_colours)
  graf_colours[cols]
}
