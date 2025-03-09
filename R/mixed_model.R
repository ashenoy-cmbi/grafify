#' Model from a linear mixed effects model
#'
#' One of four related functions for mixed effects analyses (based on \code{\link[lme4]{lmer}} and \code{\link[lmerTest]{as_lmerModLmerTest}}) to get a linear model for downstream steps, or an ANOVA table.
#' 1. \code{mixed_model}
#' 2. \code{mixed_anova}
#' 3. \code{mixed_model_slopes}
#' 4. \code{mixed_anova_slopes}.
#'
#' 
#' These functions require a data table, one dependent variable (Y_value), one or more independent variables (Fixed_Factor), and at least one random factor (Random_Factor). These should match names of variables in the long-format data table exactly. Since v5.0.0, if `AvgRF = TRUE`, the response variable is averaged over levels of the fixed and random factors (to collapse replicate observations) and reduce the number of denominator degrees of freedom. If you do not want to do this, set `AvgRF = FALSE`.
#' 
#' Outputs of `mixed_model` and `mixed_model_slopes` can be used for post-hoc comparisons with \code{\link{posthoc_Pairwise}}, \code{\link{posthoc_Levelwise}}, \code{\link{posthoc_vsRef}}, \code{\link{posthoc_Trends_Pairwise}}, \code{\link{posthoc_Trends_Levelwise}} and \code{\link{posthoc_Trends_vsRef}}or with \code{\link[emmeans]{emmeans}}.
#'
#' More than one fixed factors can be provided as a vector (e.g. c("A", "B")). A full model with interaction term is fitted. 
#' This means when \code{Y_value = Y, Fixed_factor = c("A", "B"), Random_factor = "R"} are entered as arguments, these are passed on as \code{Y ~ A*B + (1|R)} (which is equivalent to \code{Y ~ A + B + A:B + (1|R)}).
#' 
#' In `mixed_model_slopes` and `mixed_anova_slopes`, the following kind of formula is used: \code{Y ~ A*B + (S|R)} (which is equivalent to \code{Y ~ A + B + A:B + (S|R)}). 
#' In this experimental implementation, random slopes and intercepts are fitted (\code{(Slopes_Factor|Random_Factor)}). Only one term each is allowed for `Slopes_Factor` and `Random_Factor`.
#'
#' @param data a data table object, e.g. data.frame or tibble.
#' @param Y_value name of column containing quantitative (dependent) variable, provided within "quotes". The following transformations are permitted: "log(Y_value)", "log(Y_value + c)" where c a positive number, "logit(Y_value)" or "logit(Y_value/100)" which may be useful when `Y_value` are percentages  (note quotes outside the log or logit calls); "sqrt(Y_value)" or "(Y_value)^2" should also work. During posthoc-comparisons, log and logit transformations will be back-transformed to the original scale. Other transformations, e.g., "sqrt(Y_value)" will not be back-transformed. Check out the \code{\link[emmeans]{regrid}} and \code{\link[emmeans]{ref_grid}} for details if you need back-transformation to the response scale. 
#' @param Fixed_Factor name(s) of categorical fixed factors (independent variables) provided within quotes (e.g., "A") or as a vector if more than one (e.g., c("A", "B"). If a numeric variable is used, transformations similar to `Y_value` are permitted.
#' @param Random_Factor name(s) of random factors to allow random intercepts; to be provided within quotes (e.g., "R") or as a vector when more than one (e.g., c("R1", "R2")).
#' @param AvgRF this is a new argument since v5.0.0. The default `AvgRF = TRUE` will use the mean of `Y_value` (the response variable) grouped by levels of the `Fixed_Factor` and `Random_Factor`  (using \code{\link{table_summary}}). This ensures that replicates within `Random_Factor` (or any other unused variable) are averaged (e.g., technical replicates nested within experimental blocks) before fitting a linear model and the denominator Df values are sensible. The name of the data frame in the model object will have `(AvgRF)` appended to it to indicate the averaging within levels of the `Random_Factor`. Using `AvgRF = FALSE` will lead to behaviour like versions <5.0.0.
#' @param Formula directly provide an a formula (within quotes) as you would if you were using \code{\link[lme4]{lmer}}. If `Y_value`, `Fixed_Factor` and `Random_Factor` are provided, they will be ignored. This is basically a wrapper, which may be useful if fitting more complex random factor structures.
#' @param ... any additional arguments to pass on to \code{\link[lme4]{lmer}} if required.
#'
#' @return This function returns an S4 object of class "lmerModLmerTest".
#' @export mixed_model
#' @importFrom lme4 lmer
#' @importFrom lmerTest as_lmerModLmerTest
#' @importFrom stats as.formula
#' @importFrom car logit
#' 
#' @examples
#' #one fixed factor and random factor
#' mixed_model(data = data_doubling_time, 
#' Y_value = "Doubling_time", 
#' Fixed_Factor = "Student", 
#' Random_Factor = "Experiment")
#' 
#' #with formula
#' mixed_anova(data = data_doubling_time, 
#' Formula = "Doubling_time ~ Student +(1|Experiment)")
#' 
#' #' #save model
#' model <- mixed_model(data = data_doubling_time, 
#' Y_value =  "Doubling_time", 
#' Fixed_Factor = "Student", 
#' Random_Factor = "Experiment")
#'
#' #get model summary
#' summary(model)

mixed_model <- function(data, Y_value, Fixed_Factor, Random_Factor, AvgRF = TRUE, Formula = NULL, ...) {
  if (!is.null(Formula)) {
    # If Formula is provided, use it directly
    fo <- as.formula(Formula)
    df <- data
  } else {
    if (AvgRF == TRUE) {
      # Check if Y_value, Fixed_Factor, and Random_Factor are provided when AvgRF is TRUE
      if (is.null(Y_value) ||
          is.null(Fixed_Factor) || is.null(Random_Factor)) {
        stop("Y_value, Fixed_Factor, and Random_Factor must be provided when AvgRF is TRUE.")
      }
      message(
        "The new argument `AvgRF` is set to TRUE by default in >=5.0.0). Response variable is averaged over levels of Fixed and Random factors. Use help for details."
      )
    }
    df <- data
    data_name <- deparse(substitute(data))  # Get the name of the data frame
    new_data_name <- if (AvgRF)
      paste0(data_name, " (AvgRF)")
    else
      data_name  # Create a new name for the data frame if AvgRF is TRUE
    
    lx1r <- length(Fixed_Factor) + length(Random_Factor)  # Calculate the total number of factors
    
    # Function to extract variable names from input strings
    extract_var_name <- function(input) {
      gsub(".*\\((.*)\\).*", "\\1", gsub("\\s+|/.*|\\+.*|\\-.*|\\*.*|\\^.*|\\).*|.*\\(", "", input))
    }
    
    res_var <- extract_var_name(Y_value)  # Extract response variable name
    dep_var <- extract_var_name(Fixed_Factor)  # Extract fixed factor variable name
    
    if (AvgRF == TRUE) {
      # Summarize the data frame by averaging over levels of fixed and random factors
      avgdf <- table_summary(df, res_var, c(dep_var, Random_Factor))
      avgdf <- avgdf[, c(1:(lx1r + 1))]
      colnames(avgdf) <- c(dep_var, Random_Factor, res_var)
      df <- avgdf
    }
    
    if (AvgRF == FALSE) {
      df <- data
    }
    #env <- new.env()  # Create a new environment to store the filtered data frame
    assign(new_data_name, df, envir = environment())  # Assign the new data frame to the environment
    
    # Construct the formula for the model
    ifelse(
      length(Fixed_Factor) == 1,
      Facs <- paste0(Fixed_Factor, collapse = ""),
      Facs <- paste0(Fixed_Factor, collapse = "*")
    )
    ifelse((length(Random_Factor) == 1),
           RFacs <- paste0("(1|", Random_Factor, ")"),
           RFacs <- paste0("(1|", Random_Factor, ")", collapse = "+")
    )
    fo <- as.formula(paste(Y_value, paste(
      paste(Facs, collapse = ""), paste(RFacs, collapse = ""), sep = "+"
    ), sep = " ~ "))
  }
  mod1 <- do.call("lmer", list(formula = fo, data = df, ...))
  #mod1 <- eval(parse(text = call1))
  mod1 <- as_lmerModLmerTest(mod1)
  if (is.null(Formula)) {
    mod1@call$data <- as.name(new_data_name)  # Update the data name in the model object
    #rm(list = ls(envir = env), envir = env)  # Clean up the environment
  } else {
    mod1@call$data <- as.name(deparse(substitute(data)))  # Use the original data name if Formula is provided
  }
  mod1
}
