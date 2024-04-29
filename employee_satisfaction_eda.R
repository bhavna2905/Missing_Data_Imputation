#loading libraries
library(ggplot2)
library(tidyverse)
library(dplyr)
library(car)

#Loading the data
employee_data <- read.csv("~/Library/CloudStorage/OneDrive-Personal/stat_mod_computing/final_project/archive/Employee Attrition.csv")
class(employee_data)
View(employee_data)
#inspecting the data
head(employee_data)

#summary of data
summary(employee_data)
#we should only look at satisfaction_level, last_evaluation, number_project, average_monthly_hours, time_spend_company
#There are 788 NAs in Emp.ID, satisfaction_level, last_evaluation, number_project, average_monthly_hours, time_spend company,
#Work_accident, and promotion_last_5_years

#EDA
#viewing number of people in each department
ggplot(data = employee_data) + geom_bar(mapping = aes(x = dept), fill="pink")
employee_data %>% count(dept)
unique(employee_data$dept)

#viewing number of people in each salary class
ggplot(data = employee_data) + geom_bar(mapping = aes(x = salary), fill="pink")
employee_data %>% count(salary)
#note here: there are quite a few missing salaries. perhaps we could use this as one of our missing value target variables
unique(employee_data$salary)

#viewing distribution of satisfaction level
hist(employee_data$satisfaction_level, col = "pink", probability = TRUE, main="Histogram of Satisfaction Level", xlab="Employee Satisfaction Level")
lines(density(employee_data$satisfaction_level, na.rm = TRUE), col = "red")
employee_data %>% count(satisfaction_level)
boxplot(employee_data$satisfaction_level, main="Boxplot of Employee Satisfaction Level", ylab="Satisfaction Level", col="pink")
boxplot.stats(employee_data$satisfaction_level)$out

#viewing distribution of average_monthly_hours
hist(employee_data$average_montly_hours, col = "pink", probability = TRUE, main="Histogram of Average Monthly Hours", xlab="Average Monthly Hours")
lines(density(employee_data$average_montly_hours, na.rm = TRUE), col = "red")
employee_data %>% count(average_montly_hours)
boxplot(employee_data$average_montly_hours, main="Boxplot of Average Monthly Hours", ylab="Average Monthly Hours", col="pink")
boxplot.stats(employee_data$average_montly_hours)$out

#viewing distribution of number_project
hist(employee_data$number_project, col = "pink", probability = TRUE, main="Histogram of Number of Projects", xlab="Number of Project")
employee_data %>% count(number_project)
boxplot(employee_data$number_project, main="Boxplot of Average Monthly Hours", ylab="Average Monthly Hours", col="pink")
boxplot.stats(employee_data$number_project)$out

#viewing distribution of last evaluation
hist(employee_data$last_evaluation, col = "pink", probability = TRUE, main="Histogram of Last Evaluation", xlab="Last Evaluation")
lines(density(employee_data$last_evaluation, na.rm = TRUE), col = "red")
employee_data %>% count(last_evaluation)
boxplot(employee_data$last_evaluation, main="Boxplot of Last Evaluation", ylab="Last Evaluation", col="pink")
boxplot.stats(employee_data$last_evaluation)$out

#Lmod
lmod <- lm(satisfaction_level ~ last_evaluation + number_project + average_montly_hours + time_spend_company + factor(Work_accident) + factor(promotion_last_5years) + factor(dept) + factor(salary), data=employee_data)
summary(lmod)
#We see that last_evaluation, number_project, time_spend_company, Work_accident, promotion_last_5years, dept for everything except
#hr, and all salaries. Also, important to note that some categorical values have been dropped. This is because the model is 
#is using one of the categorical variables as a reference

#bivariate analysis
plot(x=employee_data$last_evaluation, y=employee_data$satisfaction_level, main="Last Evaluation vs. Current Evaluation", xlab="Last Evaluation", ylab="Current Satisfaction")
#There is no trend
plot(x=employee_data$number_project, y=employee_data$satisfaction_level, main="Number of Projects vs. Current Evaluation", xlab="Number of Projects", ylab="Current Satisfaction")
#No visible trend until 7 -- tends to be on the lower side
plot(x=employee_data$average_montly_hours, y=employee_data$satisfaction_level, main="Average Monthly Hours vs. Current Evaluation", xlab="Average Monthly Hours", ylab="Current Satisfaction")
#Seems to be that between 150 and 275 average monthly hours, it tends to have a higher satisfaction level and between 250 and 
#300 hours have very low ratings
plot(x=employee_data$time_spend_company, y=employee_data$satisfaction_level, main="Time Spend Company vs. Current Evaluation", xlab="Time Spend Company", ylab="Current Satisfaction")

ggplot(employee_data, aes(x = salary, y = satisfaction_level, fill = salary)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Satisfaction Level by Salary Class", x = "Salary Class", y = "Satisfaction Level")

ggplot(employee_data, aes(x = salary, y = satisfaction_level, fill = salary)) +
  geom_violin() +
  theme_minimal() +
  labs(title = "Satisfaction Level by Salary Class", x = "Salary Class", y = "Satisfaction Level")

# Summarize the data to calculate mean satisfaction levels for each salary class
employee_summary <- employee_data %>%
  group_by(salary) %>%
  summarise(mean_satisfaction = mean(satisfaction_level, na.rm = TRUE))

# Plot
ggplot(employee_summary, aes(x = salary, y = mean_satisfaction, fill = salary)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Mean Satisfaction Level by Salary Class", x = "Salary Class", y = "Mean Satisfaction Level")

###not as many people in high salary class so they could bring up/affect the mean more

###
ggplot(employee_data, aes(x = dept, y = satisfaction_level, fill = dept)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Satisfaction Level by Department", x = "Department", y = "Satisfaction Level")

ggplot(employee_data, aes(x = dept, y = satisfaction_level, fill = dept)) +
  geom_violin() +
  theme_minimal() +
  labs(title = "Satisfaction Level by Department", x = "Department", y = "Satisfaction Level")

# Summarize the data to calculate mean satisfaction levels for each salary class
employee_summary <- employee_data %>%
  group_by(dept) %>%
  summarise(mean_satisfaction = mean(satisfaction_level, na.rm = TRUE))

# Plot
ggplot(employee_summary, aes(x = dept, y = mean_satisfaction, fill = dept)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Mean Satisfaction Level by Department", x = "Department", y = "Mean Satisfaction Level")

ggplot(employee_data, aes(x = as.factor(Work_accident), y = satisfaction_level, fill = as.factor(Work_accident))) +
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "deeppink", "1" = "lightgreen")) +
  labs(title = "Satisfaction Level by Work Accident Occurrence",
       x = "Work Accident (0 = No, 1 = Yes)",
       y = "Satisfaction Level",
       fill = "Work Accident") +
  theme_minimal()

ggplot(employee_data, aes(x = as.factor(Work_accident), y = satisfaction_level, fill = as.factor(Work_accident))) +
  geom_violin() +
  scale_fill_manual(values = c("0" = "deeppink", "1" = "lightgreen")) +
  labs(title = "Satisfaction Level by Work Accident Occurrence",
       x = "Work Accident (0 = No, 1 = Yes)",
       y = "Satisfaction Level",
       fill = "Work Accident") +
  theme_minimal()

ggplot(employee_data, aes(x = satisfaction_level, fill = as.factor(Work_accident))) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c("0" = "deeppink", "1" = "lightgreen")) +
  labs(title = "Satisfaction Level Density by Work Accident Occurrence",
       x = "Satisfaction Level",
       y = "Density",
       fill = "Work Accident") +
  theme_minimal()

ggplot(employee_data, aes(x = satisfaction_level, fill = as.factor(Work_accident))) +
  geom_histogram(aes(y = ..density..), binwidth = 0.05, alpha = 0.7, position = "identity") +
  facet_wrap(~Work_accident) +
  scale_fill_manual(values = c("0" = "deeppink", "1" = "lightgreen")) +
  labs(title = "Satisfaction Level Distribution by Work Accident Occurrence",
       x = "Satisfaction Level",
       y = "Density",
       fill = "Work Accident") +
  theme_minimal()

###
ggplot(employee_data, aes(x = as.factor(promotion_last_5years), y = satisfaction_level, fill = as.factor(promotion_last_5years))) +
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "deeppink", "1" = "lightgreen")) +
  labs(title = "Satisfaction Level by if an Employee Was Promoted in the Last Five Years",
       x = "Promotion in Last 5 Years (0 = No, 1 = Yes)",
       y = "Satisfaction Level",
       fill = "Promotion in Last 5 Years") +
  theme_minimal()

ggplot(employee_data, aes(x = as.factor(promotion_last_5years), y = satisfaction_level, fill = as.factor(promotion_last_5years))) +
  geom_violin() +
  scale_fill_manual(values = c("0" = "deeppink", "1" = "lightgreen")) +
  labs(title = "Satisfaction Level by if an Employee Was Promoted in the Last Five Years",
       x = "Promotion in Last 5 Years (0 = No, 1 = Yes)",
       y = "Satisfaction Level",
       fill = "Promotion in Last 5 Years") +
  theme_minimal()

ggplot(employee_data, aes(x = satisfaction_level, fill = as.factor(promotion_last_5years))) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c("0" = "deeppink", "1" = "lightgreen")) +
  labs(title = "Satisfaction Level Density by Promotion in Last 5 Years",
       x = "Satisfaction Level",
       y = "Density",
       fill = "Promotion in Last 5 Years") +
  theme_minimal()

ggplot(employee_data, aes(x = satisfaction_level, fill = as.factor(promotion_last_5years))) +
  geom_histogram(aes(y = ..density..), binwidth = 0.05, alpha = 0.7, position = "identity") +
  facet_wrap(~promotion_last_5years) +
  scale_fill_manual(values = c("0" = "deeppink", "1" = "lightgreen")) +
  labs(title = "Satisfaction Level Distribution by Promotion in Last 5 Years",
       x = "Satisfaction Level",
       y = "Density",
       fill = "Promotion in Last 5 Years") +
  theme_minimal()

#Testing statistical significance of salary class vs satisfaction level
# Example for a Q-Q plot for one of the salary groups
qqnorm(employee_data$satisfaction_level[employee_data$salary == "low"])
qqline(employee_data$satisfaction_level[employee_data$salary == "low"])
qqnorm(employee_data$satisfaction_level[employee_data$salary == "medium"])
qqline(employee_data$satisfaction_level[employee_data$salary == "medium"])
qqnorm(employee_data$satisfaction_level[employee_data$salary == "high"])
qqline(employee_data$satisfaction_level[employee_data$salary == "high"])
leveneTest(satisfaction_level ~ factor(salary), data = employee_data)
#Does not pass normality or homogeneity of variances so we use kruksal.wallis as non-parametric test
kruskal.test(satisfaction_level ~ factor(salary), data = employee_data)
kruskal.test(satisfaction_level ~ salary, data = employee_data)
#p-value is less than 0.05, therefore there is a statistical significance between salary and satisfaction level

#Testing statistical significance of salary class vs satisfaction level
unique(employee_data$dept)
# Example for a Q-Q plot for one of the salary groups
qqnorm(employee_data$satisfaction_level[employee_data$dept == "sales"])
qqline(employee_data$satisfaction_level[employee_data$dept == "sales"])
qqnorm(employee_data$satisfaction_level[employee_data$dept == "accounting"])
qqline(employee_data$satisfaction_level[employee_data$dept == "accounting"])
qqnorm(employee_data$satisfaction_level[employee_data$dept == "hr"])
qqline(employee_data$satisfaction_level[employee_data$dept == "hr"])
qqnorm(employee_data$satisfaction_level[employee_data$dept == "technical"])
qqline(employee_data$satisfaction_level[employee_data$dept == "technical"])
qqnorm(employee_data$satisfaction_level[employee_data$dept == "support"])
qqline(employee_data$satisfaction_level[employee_data$dept == "support"])
qqnorm(employee_data$satisfaction_level[employee_data$dept == "management"])
qqline(employee_data$satisfaction_level[employee_data$dept == "management"])
qqnorm(employee_data$satisfaction_level[employee_data$dept == "IT"])
qqline(employee_data$satisfaction_level[employee_data$dept == "IT"])
qqnorm(employee_data$satisfaction_level[employee_data$dept == "product_mng"])
qqline(employee_data$satisfaction_level[employee_data$dept == "product_mng"])
qqnorm(employee_data$satisfaction_level[employee_data$dept == "marketing"])
qqline(employee_data$satisfaction_level[employee_data$dept == "marketing"])
qqnorm(employee_data$satisfaction_level[employee_data$dept == "RandD"])
qqline(employee_data$satisfaction_level[employee_data$dept == "RandD"])
leveneTest(satisfaction_level ~ factor(dept), data = employee_data)
#Does not pass normality or homogeneity of variances so we use kruksal.wallis as non-parametric test
kruskal.test(satisfaction_level ~ factor(dept), data = employee_data)
#p-value is less than 0.05, therefore there is a statistical significance between salary and satisfaction level


# Q-Q plot for last_year_evaluation
ggplot(employee_data, aes(sample = last_evaluation)) +
  geom_qq() +
  geom_abline(intercept = mean(employee_data$last_evaluation), slope = sd(employee_data$last_evaluation)) +
  labs(title = "Q-Q Plot for Last Year's Evaluation")

# Q-Q plot for satisfaction_level
ggplot(employee_data, aes(sample = satisfaction_level)) +
  geom_qq() +
  geom_abline(intercept = mean(employee_data$satisfaction_level), slope = sd(employee_data$satisfaction_level)) +
  labs(title = "Q-Q Plot for Satisfaction Level")

kruskal.test(satisfaction_level ~ last_evaluation, data = employee_data)
#Statistically significant

kruskal.test(satisfaction_level ~ number_project, data = employee_data)
#Statistically significant

kruskal.test(satisfaction_level ~ average_montly_hours, data = employee_data)
#Statistically significant

kruskal.test(satisfaction_level ~ time_spend_company, data = employee_data)
#Statistically significant

kruskal.test(satisfaction_level ~ factor(Work_accident), data = employee_data)
#Statistically significant

kruskal.test(satisfaction_level ~ factor(promotion_last_5years), data = employee_data)
#Statistically significant

