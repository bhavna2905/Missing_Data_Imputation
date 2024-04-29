install.packages("Amelia")
install.packages("energy")
library(Amelia)
library(missMethods)
library(corrplot)
library(QuantPsyc)
library(energy)
library(gridExtra)

employee_data <- read.csv("~/Library/CloudStorage/OneDrive-Personal/stat_mod_computing/final_project/archive/Employee Attrition.csv")
head(employee_data)
summary(employee_data)

clean_data <- employee_data[complete.cases(employee_data), ]
head(clean_data)
summary(clean_data)

clean_data_excluded <- subset(clean_data, select = -c(salary, dept, Work_accident, promotion_last_5years)) 
head(clean_data_excluded)
mvnorm.etest(clean_data_excluded, R=100)

summary(clean_data_excluded)

set.seed(123)
MCAR <- delete_MCAR(clean_data, 0.2, cols_mis=c('satisfaction_level', 'last_evaluation', 
                                                'number_project', 'average_montly_hours',
                                                'time_spend_company', 'Work_accident', 
                                                'promotion_last_5years', 'dept', 'salary'))

na_indices <- which(is.na(MCAR), arr.ind = TRUE)
head(MCAR)
# Create a dataframe with row and column indices
na_indices_df <- data.frame(row=na_indices[,1], col=na_indices[,2])

cols_to_impute <- c('satisfaction_level', 'last_evaluation', 
                    'number_project', 'average_montly_hours',
                    'time_spend_company', 'Work_accident', 
                    'promotion_last_5years', 'dept', 'salary')

imputed_data <- amelia(MCAR, m=5, ind=cols_to_impute, noms=c('dept', 'salary'))
summary(imputed_data)

# Assuming clean_data_excluded is the original dataset without missing values
true_values <- clean_data

# Calculate the MSE for each variable and each iteration
mse_values <- sapply(1:5, function(iter) {
  sapply(names(imputed_data$imputations[[iter]]), function(col) {
    # Check if column is numeric
    if (is.numeric(true_values[, col])) {
      mean((imputed_data$imputations[[iter]][, col] - true_values[, col])^2, na.rm = TRUE)
    } else {
      # For non-numeric columns, check if values match
      sum(imputed_data$imputations[[iter]][, col] != true_values[, col], na.rm = TRUE)
    }
  })
})


mse_values 
# Calculate the mean MSE for each variable across all iterations
mean_mse_values <- apply(mse_values, 1, mean, na.rm = TRUE)
mean_mse_values

# Calculate the overall MSE (average of mean MSE values for each variable)
overall_mse <- mean(mean_mse_values, na.rm = TRUE)

# Print the overall MSE
print(overall_mse)

matplot(1:5, t(mse_values), type="l", col=2:ncol(mse_values), lty=1, 
        xlab="Iteration", ylab="MSE", main="MSE Values Over Iterations",
        ylim=range(0, mse_values) * 1.1)
legend("right", legend=names(mean_mse_values), col=1:ncol(mse_values), lty=1, cex=0.5)


plot(imputed_data)

colnames(imputed_data$imputations[[1]])

# Extract the imputed values for satisfaction_level
satisfaction_imputed <- imputed_data$imputations[[1]]$satisfaction_level
satisfaction_imputed

# Plot the histogram of satisfaction_level before and after imputation
par(mfrow=c(1,2))
hist(clean_data$satisfaction_level, main="Original", xlab="Satisfaction Level", col="seagreen")
hist(satisfaction_imputed, main="Imputed", xlab="Satisfaction Level", col="lightpink")

# Extract the imputed values for satisfaction_level
evaluation_imputed <- imputed_data$imputations[[1]]$last_evaluation
hist(clean_data$last_evaluation, main="Original", xlab="Last Evaluation", col="seagreen")
hist(evaluation_imputed, main="Imputed", xlab="Last Evaluation", col="lightpink")


# Extract the imputed values for satisfaction_level
number_project_imputed <- imputed_data$imputations[[1]]$number_project
hist(clean_data$number_project, main="Original", xlab="Number of Projects", col="seagreen")
hist(number_project_imputed, main="Imputed", xlab="Number of Projects", col="lightpink")

# Extract the imputed values for satisfaction_level
average_montly_hours_imputed <- imputed_data$imputations[[1]]$average_montly_hours
hist(clean_data$average_montly_hours, main="Original", xlab="Average Monthly Hours", col="seagreen")
hist(average_montly_hours_imputed, main="Imputed", xlab="Average Montly Hours", col="lightpink")

time_spend_company_imputed <- imputed_data$imputations[[1]]$time_spend_company
hist(clean_data$time_spend_company, main="Original", xlab="Time Spend Company", col="seagreen")
hist(time_spend_company_imputed, main="Imputed", xlab="Time Spend Company", col="lightpink")

work_accident_imputed <- imputed_data$imputations[[1]]$Work_accident
hist(clean_data$Work_accident, main="Original", xlab="Work Accident", col="seagreen")
hist(work_accident_imputed, main="Imputed", xlab="Work Accident", col="lightpink")

promotion_imputed <- imputed_data$imputations[[1]]$promotion_last_5years
hist(clean_data$promotion_last_5years, main="Original", xlab="Promotion in Last Five Years", col="seagreen")
hist(promotion_imputed, main="Imputed", xlab="Promotion in Last Five Years", col="lightpink")

dept_imputed <- imputed_data$imputations[[1]]

#viewing number of people in each department
plot1 <- ggplot(data = clean_data) + geom_bar(mapping = aes(x = dept), fill="pink")
plot1 <- plot1 + ggtitle("Original") +
  xlab("Department") + ylab("Count")

#viewing number of people in each department
plot2 <- ggplot(data = imputed_data$imputations[[1]]) + geom_bar(mapping = aes(x = dept), fill="seagreen")
plot2 <- plot2 + ggtitle("Imputed") +
  xlab("Department") + ylab("Count")
grid.arrange(plot1, plot2, nrow = 1)

#viewing number of people in each department
plot1 <- ggplot(data = clean_data) + geom_bar(mapping = aes(x = salary), fill="pink")
plot1 <- plot1 + ggtitle("Original") +
  xlab("Salary") + ylab("Count")

#viewing number of people in each department
plot2 <- ggplot(data = imputed_data$imputations[[1]]) + geom_bar(mapping = aes(x = salary), fill="seagreen")
plot2 <- plot2 + ggtitle("Imputed") +
  xlab("Salary") + ylab("Count")
grid.arrange(plot1, plot2, nrow = 1)

grid.arrange(plot1, plot2, nrow = 1)
par(mfrow=c(1,1))

# Select numeric and non-numeric columns
selected_cols <- c('satisfaction_level', 'last_evaluation', 'number_project', 
                   'average_montly_hours', 'time_spend_company', 'Work_accident', 
                   'promotion_last_5years', 'dept', 'salary')

# Convert non-numeric columns to factors
factor_cols <- c('dept', 'salary')
for (col in factor_cols) {
  imputed_data$imputations[[1]][, col] <- factor(imputed_data$imputations[[1]][, col])
}

# Create pairs plot
pairs(imputed_data$imputations[[1]][, selected_cols])

# Select numeric columns for correlation calculation
numeric_cols <- c('satisfaction_level', 'last_evaluation', 'number_project', 
                  'average_montly_hours', 'time_spend_company', 'Work_accident', 
                  'promotion_last_5years')

# Convert non-numeric columns to factors
factor_cols <- c('dept', 'salary')
for (col in factor_cols) {
  imputed_data$imputations[[1]][, col] <- as.factor(imputed_data$imputations[[1]][, col])
}

# Convert factor columns to numeric using dummy encoding
numeric_factors <- model.matrix(~ . - 1, data = imputed_data$imputations[[1]][, factor_cols])

# Combine numeric and factor columns
data_for_cor <- cbind(imputed_data$imputations[[1]][, numeric_cols], numeric_factors)

# Calculate correlation matrix
cor_matrix <- cor(data_for_cor)

# Print the correlation matrix
print(cor_matrix)

corrplot(cor_matrix, method="circle")
