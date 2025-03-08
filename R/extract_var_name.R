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


extract_var_names2 <- function(inputs, valid_vars) {
  # Helper function to extract a single variable name
  extract_var_name <- function(input) {
    input <- gsub("\\s+", "", input)  # Remove all spaces
    var_name <- gsub(".*\\(([^\\)]+)\\).*", "\\1", input)  # Extract content within parentheses
    var_name <- gsub("([*/+-]).*", "", var_name)  # Remove operations and following content
    var_name <- gsub("\\).*", "", var_name)  # Remove closing parenthesis if present
    var_name <- gsub(".*\\(", "", var_name)  # Remove opening parenthesis if present
    
    return(var_name)
  }
  
  # Apply the extraction function to each input
  var_names <- sapply(inputs, extract_var_name, USE.NAMES = FALSE)
  
  # Check if each extracted variable name is in the list of valid variables
  invalid_vars <- var_names[!var_names %in% valid_vars]
  if (length(invalid_vars) > 0) {
    stop(paste("Invalid variable name(s):", paste(invalid_vars, collapse = ", ")))
  }
  
  return(var_names)
}