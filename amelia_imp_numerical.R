install.packages("Amelia")
install.packages("energy")
library(Amelia)
library(missMethods)
library(corrplot)
library(QuantPsyc)
library(energy)

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

MCAR <- delete_MCAR(clean_data_excluded, 0.2, cols_mis=c('satisfaction_level', 'last_evaluation', 
                                                         'number_project', 'average_montly_hours',
                                                         'time_spend_company'))

na_indices <- which(is.na(MCAR), arr.ind = TRUE)
head(MCAR)
# Create a dataframe with row and column indices
na_indices_df <- data.frame(row=na_indices[,1], col=na_indices[,2])

cols_to_impute <- c('satisfaction_level', 'last_evaluation', 'average_montly_hours', 'time_spend_company')

imputed_data <- amelia(MCAR, m=5, ind=cols_to_impute)
summary(imputed_data)

# Assuming clean_data_excluded is the original dataset without missing values
true_values <- clean_data_excluded

# Calculate the MSE for each variable and each iteration
mse_values <- sapply(1:5, function(iter) {
  sapply(names(imputed_data$imputations[[iter]]), function(col) {
    mean((imputed_data$imputations[[iter]][, col] - true_values[, col])^2, na.rm = TRUE)
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
hist(clean_data_excluded$satisfaction_level, main="Original", xlab="Satisfaction Level", col="seagreen")
hist(satisfaction_imputed, main="Imputed", xlab="Satisfaction Level", col="lightpink")

# Extract the imputed values for satisfaction_level
evaluation_imputed <- imputed_data$imputations[[1]]$last_evaluation
hist(clean_data_excluded$last_evaluation, main="Original", xlab="Last Evaluation", col="seagreen")
hist(evaluation_imputed, main="Imputed", xlab="Last Evaluation", col="lightpink")


# Extract the imputed values for satisfaction_level
number_project_imputed <- imputed_data$imputations[[1]]$number_project
hist(clean_data_excluded$number_project, main="Original", xlab="Number of Projects", col="seagreen")
hist(number_project_imputed, main="Imputed", xlab="Number of Projects", col="lightpink")

# Extract the imputed values for satisfaction_level
average_montly_hours_imputed <- imputed_data$imputations[[1]]$average_montly_hours
hist(clean_data_excluded$average_montly_hours, main="Original", xlab="Average Monthly Hours", col="seagreen")
hist(average_montly_hours_imputed, main="Imputed", xlab="Average Montly Hours", col="lightpink")

time_spend_company_imputed <- imputed_data$imputations[[1]]$time_spend_company
hist(clean_data_excluded$time_spend_company, main="Original", xlab="Time Spend Company", col="seagreen")
hist(time_spend_company_imputed, main="Imputed", xlab="Time Spend Company", col="lightpink")
par(mfrow=c(1,1))

pairs(imputed_data$imputations[[1]][, c('satisfaction_level', 'last_evaluation', 'number_project', 'average_montly_hours', 'time_spend_company')])

cor_matrix <- cor(imputed_data$imputations[[1]][, c('satisfaction_level', 'last_evaluation', 'number_project', 'average_montly_hours', 'time_spend_company')])
corrplot(cor_matrix, method="circle")
