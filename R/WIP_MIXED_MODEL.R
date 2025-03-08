avgMmod <- function(data, Y_value, Fixed_Factor, Random_Factor, AvgRF = TRUE, ...){
  if(AvgRF == TRUE){
    message("The new argument `AvgRF` is set to TRUE by default in >=v5.0.0). Response variable is averaged over levels of Fixed and Random factors. Use help for details.")}
  df <- data
  #rename dataframe with _AvgRF so it is not identical to original if AvgRF=TRUE
  data_name <- deparse(substitute(data))
  new_data_name <- if (AvgRF) paste0(data_name, " (AvgRF)") else data_name
  
  var_name <- Y_value
  lx1r = length(Fixed_Factor)+length(Random_Factor)
  
  #check transformations of variables
  extract_var_name <- function(input) {
    # Remove spaces and transformations, then extract the variable name
    input <- gsub("\\s+", "", input)  # Remove all spaces
    var_name <- gsub(".*\\((.*)\\).*", "\\1", input)
    var_name <- gsub("/.*", "", var_name)  # Remove division if present
    var_name <- gsub("\\+.*", "", var_name)  # Remove addition if present
    var_name <- gsub("\\-.*", "", var_name)  # Remove subtraction if present
    var_name <- gsub("\\*.*", "", var_name)  # Remove multiplication if present
    var_name <- gsub("\\^.*", "", var_name)  # Remove power if present
    var_name <- gsub("\\).*", "", var_name)  # Remove closing parenthesis if present
    var_name <- gsub(".*\\(", "", var_name)  # Remove opening parenthesis if present
    return(var_name)
  }
  res_var <- extract_var_name(Y_value)
  dep_var <- extract_var_name(Fixed_Factor)
  if(AvgRF == TRUE){
    avgdf <- table_summary(df,
                           res_var,
                           c(dep_var, 
                           Random_Factor))
    avgdf <- avgdf[, c(1:(lx1r + 1))]
    colnames(avgdf) <- c(dep_var, Random_Factor, res_var)
    df <- avgdf
  }
  if(AvgRF == FALSE){
    df <- data
  }
  # Create a new environment to store the filtered data frame
  env <- new.env()
  assign(new_data_name, df, envir = env)
  
  ifelse(length(Fixed_Factor) == 1, 
         Facs <- paste0(Fixed_Factor, 
                        collapse = ""), 
         Facs <- paste0(Fixed_Factor, 
                        collapse = "*"))
  ifelse((length(Random_Factor) == 1), 
         RFacs <- paste0("(1|", 
                         Random_Factor, ")"), 
         RFacs <- paste0("(1|", Random_Factor, 
                         ")", collapse = "+"))
  fo <- as.formula(paste(Y_value, paste(paste(Facs, collapse = ""), 
                                        paste(RFacs, collapse = ""), 
                                        sep = "+"), 
                         sep = " ~ "))
  ########## like grafify online
  mod1 <- lmer(formula = fo,
               data = get(new_data_name, envir = env))
  mod1@call$formula <- fo
  mod1 <- lmerTest::as_lmerModLmerTest(mod1)
  mod1@call$data <- as.name(new_data_name)
  # Clean up the environment
  rm(list = ls(envir = env), envir = env)
  mod1
  ################### 
  
  
  #Y <- substitute(Y_value)
  #d <- substitute(data)
  #ifelse(length(Fixed_Factor) == 1,
  #       Facs <- paste0(Fixed_Factor, collapse = ""),
  #       Facs <- paste0(Fixed_Factor, collapse = "*"))
  #
  #ifelse((length(Random_Factor) == 1),
  #       RFacs <- paste0("(1|", Random_Factor, ")"),
  #       RFacs <- paste0("(1|", Random_Factor, ")", collapse = "+"))
  #
  #fo <- as.formula(paste(Y,
  #                       paste(paste(Facs, collapse = ""),
  #                             paste(RFacs, collapse = ""),
  #                             sep = "+"),
  #                       sep = " ~ "))
  #call1 <- paste0("lmer(formula = ", 
  #                deparse1(fo), 
  #                ", data = ", 
  #                deparse1(d), 
  #                ", ...)")
  #mod1 <- eval(parse(text = call1))
  #mod1 <- as_lmerModLmerTest(mod1)
  #mod1
  
}
