#' Get graf internal
#'
#' Function to make grafify colour scheme. [Thank you Dr Simon](https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2).
#' 
#' Colour palettes available are as follows:
#' 
#' \if{html}{\out{<div style="text-align: center">}\figure{grafify_palettesv020.jpg}{options: style="width:750px;max-width:70\%;"}\out{</div>}}
#'
#' 
#'
#' @param ... internal
#' @export get_graf_colours

get_graf_colours <- function(...){
  cols <- c(...)
  if(is.null(cols))
    return(graf_colours)
  graf_colours[cols]
}
