###########################################################################
##  standardize_covars.R: function for standardizing climate covariates  ##
##  for ridge regression. Takes leave out year into consideration.       ##
##  Returns the covariate matrix and climate scalers as a list.          ##
###########################################################################

# Created by ATT on July 21, 2016
# Updated by ... on .............

standardize_covars <- function(covariate_dataframe, leave_out_year){
  # Check to make sure year is a column
  if(length(grep("year", colnames(covariate_dataframe))) != 1) {
    stop("dataframe must contain a column for year")
  }
  
  # Check if leave_out_year exists in dataframe
  if(length(which(covariate_dataframe$year == leave_out_year)) == 0) {
    stop("leave_out_year not found in dataframe")
  }
  
  # Remove observations for leave_out_year
  covariate_dataframe_sub <- subset(covariate_dataframe, year!=leave_out_year)
  
  # Standaradize covariates
  drop_col <- which(colnames(covariate_dataframe_sub)=="year")
  covar_matrix <- covariate_dataframe_sub[ ,-drop_col]
  standarized_covariate_matrix <- scale(covar_matrix, center = TRUE, scale = TRUE)
  
  # Calculate climate scalers for data without leave_out_year
  scalers <- data.frame(mean = apply(covar_matrix, MARGIN = 2, FUN = "mean"),
                        sd = apply(covar_matrix, MARGIN = 2, FUN = "sd"),
                        leave_out_year = leave_out_year)
  
  # Combine as output list
  list(covars=standarized_covariate_matrix,
       scalers=scalers)
}
