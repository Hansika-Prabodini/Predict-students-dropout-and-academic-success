
install.packages("ggplot2")
library(ggplot2)


# 01. DATA CLEANING

# Load the dataset
academic5 <- read.csv("C:/Users/Asus/Desktop/MSC/academic5.csv")

# View the structure of the dataset to understand data types
str(academic5)

# Check for missing values in each column
colSums(is.na(academic5))


# Check duplicates
# Check for duplicate rows in the entire dataset
duplicates <- duplicated(academic5)

# Display the rows that are duplicates
academic5[duplicates, ]


# There are no any missing values in this data set - so no further action need 
# there are no duplicate rows in this dataset

# 02. Descriptive Statistics

# Summary statistics for all columns
summary(academic5)

# For specific numerical columns
summary(academic5$`Admission grade`)
summary(academic5$`avg_grade`)


# 03. Data Visualization
# Check the column names
names(academic5)


# Plot a histogram for the `Admission grade` column
ggplot(academic5, aes(x = `Admission.grade`)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Admission Grade", x = "Admission Grade", y = "Frequency")





































































