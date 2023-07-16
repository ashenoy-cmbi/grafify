#' Fit a generalised additive model (`gam`)
#' 
#' One of two functions for fitting generalised additive models (`gam`) with the [`mgcv` package](https://CRAN.R-project.org/package=mgcv). It will use the `gam()` function in `mgcv` for ANOVA designs with **up to two categorical fixed factors** (with two or more levels; `Fixed_Factor`), and **exactly one factor is a continuous variable** (e.g. time), which is called `Smooth_Factor`. 
#'  1. \code{\link{ga_model}}
#'  2. \code{\link{ga_anova}} 
#'  
#'  
#' A smooth function is fitted with factor-wise smooth basis function (`by = `). A default value for number of nodes (the argument `k` in `gam`) may work, but a specific number can be provided using the `Nodes` argument. The model is fit using the `REML` method. When two categorical fixed factors are provided, an interaction term is included for main effects and smooth basis functions. 
#' 
#' If a `Random_Factor` is also provided, it is fitted using `bs = "re"` smooth. 
#'
#' @param data a data frame where categorical independent variables are converted to factors using `as.factor()` first. The function will throw errors without this.
#' @param Y_value name of column containing quantitative (dependent) variable, provided within "quotes".
#' @param Fixed_Factor name(s) of categorical fixed factors (independent variables) provided as a vector if more than one or within "quotes". Convert to factors first with `as.factor`.
#' @param Smooth_Factor the continuous variable to fit smoothly with a basis function, provided within "quotes" (only 1 Smooth_Factor allowed).
#' @param Random_Factor name(s) of random factors to be provided in "quotes" (only 1 Random_Factor allowed). Convert to factor with `as.factor` first.
#' @param Nodes number of nodes (the parameter `k` in `gam`). 
#' @param ... any additional variables to pass on to `gam` or `anova`
#'
#' @return This function gives a generalised additive model object of class "gam", "lm" and "glm".
#' @export ga_model
#' @importFrom stats as.formula
#' @importFrom mgcv gam
#' 
#' @examples 
#' #fit a model with zooplankton data
#' z1 <- ga_model(data = data_zooplankton,
#' Y_value = "log(density_adj)",
#' Fixed_Factor = "taxon",
#' Smooth_Factor = "day")
#'
ga_model <- function(data, Y_value, Fixed_Factor, Smooth_Factor, Random_Factor = NULL, Nodes = "NULL", ...){
  #confirm only one smooth factor
  if (length(Smooth_Factor) >1) {
    stop("Only one smoothening factor allowed.")
  }
  if (length(Fixed_Factor) >2) {
    stop("Only one or two fixed factors allowed.")
  }
  #force Fixed_Factor as factor
  data[Fixed_Factor] <- lapply(data[Fixed_Factor], factor)
  #force Random_Factor as factor
  if(!missing(Random_Factor)) {
    data[[Random_Factor]] <- sapply(data[[Random_Factor]], as.factor)
  }
  Y <- substitute(Y_value)
  d <- substitute(data)
  #with default number for Nodes
  if(missing(Nodes)) {
    #if two fixed factors
    if(length(Fixed_Factor) == 2){
      Facs <- paste0(Fixed_Factor[1], "*",
                     Fixed_Factor[2], "+",  
                     " s(", Smooth_Factor, 
                     ", by = ", Fixed_Factor[1], ")", 
                     "+ s(", Smooth_Factor,
                     ", by = ", Fixed_Factor[2],")")
    } else {
      Facs <- paste0(Fixed_Factor, "+ s(", Smooth_Factor, 
                     ", by = ", Fixed_Factor, ")")
    }
    #if no random factor
    if(missing(Random_Factor)) { 
      f2 <- paste0(Facs)
    } else {
      #when random factor present
      rf <- paste0("s(", Random_Factor, 
                   ", bs = 're')")
      f2 <- paste(Facs, rf, sep = "+")
    }
    #formula
    fo <- as.formula(paste(Y, f2, sep = "~"))
  } else {
    #when Nodes provided
    if(length(Fixed_Factor) == 2){
      #2 fixed factors
      Facs <- paste0(Fixed_Factor[1], "*",
                     Fixed_Factor[2], "+",  
                     " s(", Smooth_Factor, 
                     ", by = ", Fixed_Factor[1], ", k = ", 
                     Nodes, ")", 
                     "+ s(", Smooth_Factor,
                     ", by = ", Fixed_Factor[2], ", k = ",
                     Nodes, ")")
    } else {
      Facs <- paste0(Fixed_Factor, "+ s(", Smooth_Factor, 
                     ", by = ", Fixed_Factor, ", k = ", Nodes, ")")
    }
    if(missing(Random_Factor)) { 
      #no random factor
      f2 <- paste0(Facs)
    } else {
      rf <- paste0("s(", Random_Factor, 
                   ", bs = 're')")
      f2 <- paste(Facs, rf, sep = "+")
    }
    #formula
    fo <- as.formula(paste(Y, f2, sep = "~"))
  }
  #set up call
  call1 <- paste0("gam(formula = ", deparse1(fo),
                  ", data = ", deparse1(d),
                  ", method = 'REML', ...)")
  #execute call
  mod1 <- eval(parse(text = call1))
  mod1
}