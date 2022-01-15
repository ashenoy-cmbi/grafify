#' colorRamPalette_d
#'
#' Variant of \code{colorRampPalette} for sequential use of colours for discrete scales. [Thank you linog](https://stackoverflow.com/questions/61674217/custom-discrete-color-scale-in-ggplot-does-not-respect-order). Called by other functions in grafify and not generally meant to be called by user. 
#'
#' @param colors internal
#' @param ... internal
#' @return This is a variant of \code{colourRampPalette} that generates sequential colours from chosen grafify palettes when called by \code{graf_col_palette_d}.
#' @export colorRampPalette_d

colorRampPalette_d <- function (colors, ...){
  # n: number of classes
  function(n) {
    ramp <- colorRamp_d(colors, n, ...)
    x <- ramp(seq.int(0, 1, length.out = n))
    if (ncol(x) == 4L)
      rgb(x[, 1L], x[, 2L], x[, 3L], x[, 4L], maxColorValue = 255)
    else rgb(x[, 1L], x[, 2L], x[, 3L], maxColorValue = 255)
  }
}
