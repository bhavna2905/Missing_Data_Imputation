library(tidyverse)
library(dplyr)
library(mice)
library(missMethods)
library(Amelia)
library(caret)
library(ggplot2)
library(gridExtra)


employee_data <- read.csv("Employee Attrition.csv")
clean_data <- employee_data[complete.cases(employee_data), ]

clean_data_excluded <- subset(clean_data, select = -c(Emp.ID))

#clean_data_excluded <- subset(clean_data, select = -c(salary, dept, Work_accident, promotion_last_5years)) 
MCAR <- delete_MCAR(clean_data_excluded, 0.2, cols_mis=c("satisfaction_level", "last_evaluation", "number_project", 
                                                         "average_montly_hours", "time_spend_company", "Work_accident", 
                                                         "promotion_last_5years", "dept", "salary"))

# Ensure that binary variables are treated as categorical
MCAR$Work_accident <- as.factor(MCAR$Work_accident)
MCAR$promotion_last_5years <- as.factor(MCAR$promotion_last_5years)
MCAR$dept <- as.factor(MCAR$dept)
MCAR$salary <- as.factor(MCAR$salary)
MCAR$number_project <- as.factor(MCAR$number_project)

na_indices <- which(is.na(MCAR), arr.ind = TRUE)

# Create a dataframe with row and column indices
na_indices_df <- data.frame(row=na_indices[,1], col=na_indices[,2])

imputation_performance <- function(missingness_data, original, na_indices_df, methods) {
  # Initialize a list to store evaluation metric values for each method
  evaluation_results <- list()
  
  # Iterate over each imputation method
  for (method in methods) {
    # Impute missing values using the current method
    imputed_data <- impute_data(missingness_data, method)
    
    # Initialize a list to store evaluation metric values for each imputation iteration
    mse_results <- list()
    accuracy_results <- list()
    
    # Iterate over each imputation iteration
    for (i in 1:5) {
      completed_dataset <- complete(imputed_data, action = i)
      
      # Initialize empty lists for temporary evaluation metric values
      mse_temp <- list()
      accuracy_temp <- list()
      
      # Iterate over each missing value index
      for (j in 1:nrow(na_indices_df)) {
        row <- na_indices_df[j, 1]
        col <- na_indices_df[j, 2]
        
        # Check if the variable is numeric
        if (is.numeric(completed_dataset[row, col])) {
          # Compute squared error for numeric variables
          mse_temp[[j]] <- (as.numeric(completed_dataset[row, col]) - as.numeric(original[row, col]))^2
        } else {
          # Compute accuracy for non-numeric variables
          accuracy_temp[[j]] <- as.numeric(completed_dataset[row, col] == original[row, col])
        }
      }
      
      # Compute mean evaluation metric for the imputed dataset
      # Calculate MSE for numeric variables
      mse_metric <- mean(unlist(mse_temp), na.rm = TRUE)
      # Calculate accuracy for non-numeric variables
      accuracy_metric <- sum(unlist(accuracy_temp)) / length(unlist(accuracy_temp))
      
      # Store evaluation metric values for the current imputation iteration
      mse_results[[i]] <- mse_metric
      accuracy_results[[i]] <- accuracy_metric
    }
    
    # Store the list of evaluation metric values for each method
    evaluation_results[[paste(method, "MSE", sep = "_")]] <- mse_results
    evaluation_results[[paste(method, "accuracy", sep = "_")]] <- accuracy_results
  }
  
  return(evaluation_results)
}
impute_data <- function(missingness_data, method) {
  # Perform imputation based on the specified method
  if (method == "mean") {
    imputed_data <- mice(missingness_data, method = "mean", m = 5)
  } else if (method == "tree") {
    # Implement tree imputation
    imputed_data <- mice(missingness_data, method = "cart", m = 5)
  } else if (method == "random forest") {
    # Implement random forest imputation
    imputed_data <- mice(missingness_data, method = "rf", m = 5)
  } else if (method == 'pmm'){
    # Implement PMM imputation for categorical variables
    imputed_data <- mice(missingness_data, method = "pmm", m = 5)
  } else if (method == "linear regression") {
    #columns_to_include <- c("satisfaction_level", "last_evaluation", "average_montly_hours", "time_spend_company")
    imputed_data <- mice(missingness_data, method = 'norm', m = 5)
  } 
  else {
    stop("Invalid imputation method specified.")
  }
  
  return(imputed_data)
}

imputation_distribution <- function(missingness_data, clean_employee_data, na_indices_df, methods) {
  distribution_plots <- list()
  
  # Get column names
  cols <- colnames(clean_employee_data)
  
  # Iterate over each imputation method
  for (method in methods) {
    # Initialize a list to store plots for the current method
    method_plots <- list()
    
    # Impute missing values using the current method
    imputed_data <- impute_data(missingness_data, method)
    imputed_data <- complete(imputed_data)
    
    # Iterate over each column
    for (col in cols) {
      # Create dataframes for plotting
      df1 <- data.frame(Value = clean_employee_data[[col]], Dataset = 'clean_employee_data')
      df2 <- data.frame(Value = imputed_data[[col]], Dataset = 'imputed_data')
      df <- rbind(df1, df2)
      if (is.numeric(df$Value)) {
        # Create the density plot with faceting for numeric columns
        plot <- ggplot(df, aes(x = Value, color = Dataset)) +
          geom_density() +
          ggtitle(paste("Density Plot of", col, "in each dataset")) +
          xlab(col) +
          ylab("Density") +
          scale_color_manual(values = c("clean_employee_data" = "blue", "imputed_data" = "red"))
      } else {
        # Create the count plot with faceting for categorical columns
        plot <- ggplot(df, aes(x = Value, fill = Dataset)) +
          geom_bar(position = "dodge") +
          labs(title = paste("Barplot of", col, "in each dataset"),
               x = col,
               y = "Count") +
          theme_minimal() +
          scale_fill_manual(values = c("blue", "red"))
      }
      method_plots[[col]] <- plot
    }
    # Store the plots for the current method
    distribution_plots[[method]] <- method_plots
  }
  return(distribution_plots)
}



# Define a list of MICE imputation methods to evaluate
methods <- c("tree", "pmm", 'random forest', 'linear regression')

# Calculate MSE/accuracy for each imputation method
mse_results <- imputation_performance(MCAR, clean_data_excluded, na_indices_df, methods)

# Calculate the distribution of imputed data
distribution_graphs <- imputation_distribution(MCAR, clean_data_excluded, na_indices_df, methods)
distribution_graphs

# Print MSE results for each method
for (method in methods) {
  cat("MSE for", method, "imputation method:\n")
  cat(paste("  Iteration", 1:5, ":", mse_results[[method]], "\n"))
}

for (method in methods) {
  cat("MSE for", method, "imputation method:")
  cat(paste(mean(unlist(mse_results[[method]]))), "\n")
}