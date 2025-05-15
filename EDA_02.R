
install.packages("ggplot2")
library(ggplot2)


# 01. DATA CLEANING

# Load the dataset
academic5 <- read.csv("C:/Users/Asus/Desktop/MSC/academic5.csv")

# View the structure of the dataset to understand data types
str(academic5)

# Check for missing values in each column
colSums(is.na(academic5))

names(academic5)


## avg_grade vs total_approved faceted by Target -- 10

library(ggplot2)

ggplot(academic5, aes(x = avg_grade, y = total_approved, color = Target)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~Target) +
  labs(title = "Academic Performance: Grade vs Approved Units by Target")


## Correlation within group
performance_vars <- academic5[, c("avg_grade", "Admission.grade", "total_approved", "total_credited", "units_with_evaluation")]
cor(performance_vars, use = "complete.obs")




#  Boxplot GDP & Unemployment faceted by Target - 11


library(tidyr)

academic5_long <- academic5 %>%
  pivot_longer(cols = c(GDP, `Unemployment.rate`, `Inflation.rate`), names_to = "Economic", values_to = "Value")

ggplot(academic5_long, aes(x = Target, y = Value, fill = Target)) +
  geom_boxplot() +
  facet_wrap(~Economic, scales = "free") +
  labs(title = "Socioeconomic Indicators by Target")



#  Stacked Bar: Application Mode + Application Order 
ggplot(academic5, aes(x = factor(`Application.mode`), fill = Target)) +
  geom_bar(position = "fill") +
  facet_wrap(~cut(`Application.order`, breaks = 3)) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Motivation Patterns by Application Mode and Order")



# Scatter: total_enrolled vs units_with_evaluation colored by Target  -- 12
ggplot(academic5, aes(x = total_enrolled, y = units_with_evaluation, color = Target)) +
  geom_point(alpha = 0.6) +
  labs(title = "Enrollment vs Evaluated Units by Target")


# Grouped Bar Chart: Gender × Special Needs × Target 
ggplot(academic5, aes(x = factor(`Gender`), fill = Target)) +
  geom_bar(position = "dodge") +
  facet_wrap(~`Educational.special.needs`) +
  labs(title = "Target Outcome by Gender and Special Needs")


# Scatter Plot of Average Grade vs Total Approved Unit -- 13
library(ggplot2)

ggplot(academic5, aes(x = avg_grade, y = total_approved, color = Target)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_manual(values = c("Dropout" = "red", "Graduate" = "green", "Enrolled" = "blue")) +
  labs(title = "Scatter Plot of Average Grade vs Total Approved Units",
       x = "Average Grade",
       y = "Total Approved Units",
       color = "Student Status") +
  theme_minimal()


# Age at Enrollment Density Plot by Target
library(ggplot2)

ggplot(academic5, aes(x = `Age.at.enrollment`, fill = Target)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("Dropout" = "red", "Graduate" = "green", "Enrolled" = "blue")) +
  labs(title = "Age at Enrollment Distribution by Target",
       x = "Age at Enrollment",
       y = "Density",
       fill = "Student Status") +
  theme_minimal()

library(dplyr)

# Calculate summary statistics by Target
summary_stats <- academic5 %>%
  group_by(Target) %>%
  summarise(
    Mean_Age = mean(`Age at enrollment`, na.rm = TRUE),
    Median_Age = median(`Age at enrollment`, na.rm = TRUE),
    SD_Age = sd(`Age at enrollment`, na.rm = TRUE),
    Min_Age = min(`Age at enrollment`, na.rm = TRUE),
    Max_Age = max(`Age at enrollment`, na.rm = TRUE),
    Q1_Age = quantile(`Age at enrollment`, 0.25, na.rm = TRUE),
    Q3_Age = quantile(`Age at enrollment`, 0.75, na.rm = TRUE),
    IQR_Age = IQR(`Age at enrollment`, na.rm = TRUE)
  )

# Print the summary statistics
print(summary_stats)



# Scatter Plot for Total Approved vs Age at Enrollment by Target - 14
library(ggplot2)

ggplot(academic5, aes(x = `Age.at.enrollment`, y = total_approved, color = Target)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_manual(values = c("Dropout" = "red", "Graduate" = "green", "Enrolled" = "blue")) +
  labs(title = "Scatter Plot of Total Approved Units vs Age at Enrollment",
       x = "Age at Enrollment",
       y = "Total Approved Units",
       color = "Student Status") +
  theme_minimal()



# Scatter Plot for Average Grade vs Age at Enrollment by Target - 15
library(ggplot2)

ggplot(academic5, aes(x = `Age.at.enrollment`, y = avg_grade, color = Target)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_manual(values = c("Dropout" = "red", "Graduate" = "green", "Enrolled" = "blue")) +
  labs(title = "Scatter Plot of Average Grade vs Age at Enrollment",
       x = "Age at Enrollment",
       y = "Average Grade",
       color = "Student Status") +
  theme_minimal()



# Overlayed Histograms for avg_grade and total_approved by Target - 16
library(ggplot2)
library(tidyr)

# Reshape the data to long format for separate histograms
academic5_long <- academic5 %>%
  pivot_longer(cols = c(avg_grade, total_approved), names_to = "Variable", values_to = "Value")

# Create the histograms with separate plots for avg_grade and total_approved by Target
ggplot(academic5_long, aes(x = Value, fill = Target)) +
  geom_histogram(binwidth = 1, alpha = 0.6, position = "dodge", color = "black") +
  facet_grid(Target ~ Variable) +  # Facet by Target and Variable
  scale_fill_manual(values = c("Dropout" = "red", "Graduate" = "green", "Enrolled" = "blue")) +
  labs(title = "Histograms of Average Grade and Total Approved Units by Target",
       x = "Value",
       y = "Frequency",
       fill = "Target") +
  theme_minimal()






# Predictive Multivariate Visualization
library(GGally)
ggpairs(academic5[, c("avg_grade", "total_approved", "GDP", "Age at enrollment", "Target")], 
        aes(color = Target, alpha = 0.6))







































































