
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
write.csv(academic5, file = "C:\\Users\\USER\\Desktop\\MSC\\academic5.csv", row.names = FALSE)



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

## Univariate analysis

# Distribution of Y variable - Distribution of Student Status  - 01 analysis

library(dplyr)
library(ggplot2)

# Calculate total count and percentage per Target category
target_summary <- academic5 %>%
  group_by(Target) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = round((count / sum(count)) * 100, 1),
         label = paste0(count, " (", percentage, "%)"))

# Plot
ggplot(target_summary, aes(x = Target, y = count, fill = Target)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = label), vjust = +0.2) +
  scale_fill_manual(values = c("Dropout" = "red", "Graduate" = "green", "Enrolled" = "blue")) +
  labs(title = "Distribution of Student Status",
       x = "Target Category",
       y = "Number of Students") +
  theme_minimal()


## distribution of age enrollment - 01.1 histogram 
# Load necessary libraries
library(ggplot2)


# Plot histogram of Age at Enrollment
ggplot(academic5, aes(x = Age.at.enrollment)) +
  geom_histogram(binwidth = 2, fill = "orange", color = "black") +
  labs(title = "Histogram of Age at Enrollment",
       x = "Age at Enrollment",
       y = "Frequency") +
  theme_minimal()


# bar graph of nationality
df = academic5

library(ggplot2)
library(dplyr)



# Map numeric codes to nationality names
nationality_map <- c(
  "1" = "Portuguese",
  "2" = "German",
  "6" = "Spanish",
  "11" = "Italian",
  "13" = "Dutch",
  "14" = "English",
  "17" = "Lithuanian",
  "21" = "Angolan",
  "22" = "Cape Verdean",
  "24" = "Guinean",
  "25" = "Mozambican",
  "26" = "Santomean",
  "32" = "Turkish",
  "41" = "Brazilian",
  "62" = "Romanian",
  "100" = "Moldova (Republic of)",
  "101" = "Mexican",
  "103" = "Ukrainian",
  "105" = "Russian",
  "108" = "Cuban",
  "109" = "Colombian"
)


df$Nacionality <- as.character(df$Nacionality)
df$Nacionality <- recode(df$Nacionality, !!!nationality_map, .default = df$Nacionality)
df$Nacionality <- factor(df$Nacionality, levels = unique(df$Nacionality))

# Prepare counts for labels
counts <- df %>% count(Nacionality)

# Plot with count labels on top
ggplot(df, aes(x = Nacionality)) +
  geom_bar(fill = "steelblue") +
  geom_text(data = counts, aes(x = Nacionality, y = n, label = n), vjust = -0.4) +
  labs(title = "Bar Graph of Nationality",
       x = "Nationality",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# pie chart to Gender - 01.3
df = academic5

# Recode Gender: assuming 1 = Male, 0 = Female
df$Gender <- factor(df$Gender, levels = c(0,1), labels = c("Female", "Male"))

# Prepare data for pie chart
gender_counts <- df %>%
  count(Gender) %>%
  mutate(percentage = n / sum(n) * 100,
         label = paste0(Gender, ": ", n, " (", round(percentage, 1), "%)"))

# Pie chart with counts and percentages on labels
ggplot(gender_counts, aes(x = "", y = n, fill = Gender)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 5) +
  labs(title = "Gender Distribution") +
  theme_void() +
  theme(legend.position = "none")

names(academic5)

library(ggplot2)

# Create a histogram of parental_scores - 01.4
ggplot(academic5, aes(x = parental_scores)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Parental Scores",
       x = "Parental Scores",
       y = "Frequency") +
  theme_minimal()


## Target and Gender - 02
unique(academic5$Target)
# Create bar plots for each category of 'Target'
## percentage = women graduate count / all students

library(dplyr)
library(ggplot2)

# Calculate the total number of students
total_students <- nrow(academic5)

# Count by Gender and Target and compute percentage (for labels)
gender_target_count <- academic5 %>%
  group_by(Target, Gender) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = paste0(round((count / total_students) * 100, 1), "%"))

# Plot with frequencies on y-axis and percentages on top of bars
ggplot(gender_target_count, aes(x = factor(Gender, labels = c("Female", "Male")), 
                                y = count, fill = Target)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = percentage), 
            position = position_dodge(width = 0.9), 
            vjust = +0.2, size = 3.5) +
  labs(title = "Gender Distribution by Target Category",
       x = "Gender",
       y = "Frequency (Count)",
       fill = "Target") +
  scale_fill_manual(values = c("red", "blue", "green")) +
  theme_minimal()



## four plots together - 03
library(dplyr)
library(ggplot2)
library(tidyr)

# Step 1: Select relevant columns
selected_data <- academic5 %>%
  select(Target, `Educational.special.needs`, Debtor, `Tuition.fees.up.to.date`, `Scholarship.holder`)

# Step 2: Reshape to long format
long_data <- selected_data %>%
  pivot_longer(cols = -Target,
               names_to = "Variable",
               values_to = "Value")

# Step 3: Summarize count and compute percentage
summary_data <- long_data %>%
  group_by(Variable, Value, Target) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Variable) %>%
  mutate(percentage = round((count / sum(count)) * 100, 1),
         label = paste0(count, " (", percentage, "%)"))

# Step 4: Plot
ggplot(summary_data, aes(x = factor(Value), y = count, fill = Target)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = label),
            position = position_dodge(width = 0.9),
            vjust = -0.1, size = 2.2) +
  facet_wrap(~ Variable, scales = "free_x") +
  scale_fill_manual(values = c("Dropout" = "red", "Graduate" = "green", "Enrolled" = "blue")) +
  labs(title = "Student Status by Target and Key Variables",
       x = "Value (0 = No, 1 = Yes)",
       y = "Count",
       fill = "Target") +
  theme_minimal()






## Distribution of Displaced and International Students by Target  - 04


library(dplyr)
library(tidyr)
library(ggplot2)

# Step 1: Select relevant columns
selected_data <- academic5 %>%
  select(Target, Displaced, International)

# Step 2: Reshape to long format
long_data <- selected_data %>%
  pivot_longer(cols = -Target,
               names_to = "Variable",
               values_to = "Value")

# Step 3: Summarize counts and calculate percentage
summary_data <- long_data %>%
  group_by(Variable, Value, Target) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Variable) %>%
  mutate(percentage = round((count / sum(count)) * 100, 1),
         label = paste0(count, " (", percentage, "%)"))

# Step 4: Plot
ggplot(summary_data, aes(x = factor(Value), y = count, fill = Target)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = label),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 2.7) +
  facet_wrap(~ Variable, scales = "free_x") +
  scale_fill_manual(values = c("Dropout" = "red", "Graduate" = "green", "Enrolled" = "blue")) +
  labs(title = "Distribution of Displaced and International Students by Target",
       x = "Value (0 = No, 1 = Yes)",
       y = "Count",
       fill = "Target") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )




# Boxplot of Admission Grade by Target - 05 
library(ggplot2)
library(dplyr)

# Basic boxplot with mean and median
ggplot(academic5, aes(x = Target, y = `Admission.grade`, fill = Target)) +
  geom_boxplot(outlier.colour = "black", outlier.shape = 16, outlier.size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "white", fill = "black") +  # Mean dot
  labs(title = "Admission Grade Distribution by Target",
       subtitle = "Boxplot with Mean (●), Median (line), and Outliers (dots)",
       x = "Target (Student Status)",
       y = "Admission Grade") +
  scale_fill_manual(values = c("Dropout" = "red", "Graduate" = "green", "Enrolled" = "blue")) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "none"
  )

library(dplyr)

# Custom mode function
get_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# Summary statistics
summary_stats <- academic5 %>%
  group_by(Target) %>%
  summarise(
    Count = n(),
    Mean = round(mean(`Admission.grade`, na.rm = TRUE), 2),
    Median = round(median(`Admission.grade`, na.rm = TRUE), 2),
    Mode = get_mode(`Admission.grade`),
    Q1 = round(quantile(`Admission.grade`, 0.25, na.rm = TRUE), 2),
    Q3 = round(quantile(`Admission.grade`, 0.75, na.rm = TRUE), 2),
    IQR = round(IQR(`Admission.grade`, na.rm = TRUE), 2),
    Min = min(`Admission.grade`, na.rm = TRUE),
    Max = max(`Admission.grade`, na.rm = TRUE),
    Lower_Outlier_Threshold = round(Q1 - 1.5 * IQR, 2),
    Upper_Outlier_Threshold = round(Q3 + 1.5 * IQR, 2)
  )




## GDP Distribution by Student Target Status - 06

library(ggplot2)
library(dplyr)

# Custom mode function
get_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# Calculate the mean, median, and mode for each Target category
summary_stats <- academic5 %>%
  group_by(Target) %>%
  summarise(
    Mean = round(mean(GDP, na.rm = TRUE), 2),
    Median = round(median(GDP, na.rm = TRUE), 2),
    Mode = get_mode(GDP)
  )

# Add the summary stats as a new column to the original dataset to use in the plot
academic5 <- academic5 %>%
  left_join(summary_stats, by = "Target")

# Plot
ggplot(academic5, aes(x = Target, y = GDP, fill = Target)) +
  geom_boxplot(outlier.colour = "black", outlier.shape = 16, outlier.size = 2) +
  
  # Add mean, median, and mode as separate points for the legend
  geom_point(aes(y = Mean, shape = "Mean"), size = 4, color = "black", show.legend = TRUE) +
  geom_point(aes(y = Median, shape = "Median"), size = 4, color = "white", fill = "black", show.legend = TRUE) +
  geom_point(aes(y = Mode, shape = "Mode"), size = 4, color = "blue", show.legend = TRUE) +
  
  # Custom legend labels
  scale_shape_manual(values = c(16, 17, 18), 
                     labels = c("Mean", "Median", "Mode")) +
  
  # Aesthetic settings for the plot
  labs(title = "GDP Distribution by Student Target Status",
       x = "Target",
       y = "GDP",
       shape = "Statistics") +
  scale_fill_manual(values = c("Dropout" = "red", "Graduate" = "green", "Enrolled" = "blue")) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "top",  # Position legend at the top
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )





## Unemployment rate and Inflation rate vs Target - 07
library(ggplot2)
library(dplyr)
library(tidyr)

# Reshape data to long format for plotting both variables
academic5_long <- academic5 %>%
  select(Target, `Unemployment.rate`, `Inflation.rate`) %>%
  pivot_longer(cols = c(`Unemployment.rate`, `Inflation.rate`), 
               names_to = "Variable", 
               values_to = "Value")

# Plot
ggplot(academic5_long, aes(x = Target, y = Value, fill = Target)) +
  geom_boxplot() +
  facet_wrap(~ Variable, scales = "free_y") +  # Facet by variable
  labs(title = "Unemployment and Inflation Rates by Target Status",
       x = "Target",
       y = "Rate") +
  scale_fill_manual(values = c("Dropout" = "red", "Graduate" = "green", "Enrolled" = "blue")) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "bottom"  # Position the legend below the plot
  )


# Distribution of Average Grades by Target Status - 08

library(ggplot2)

# Plot boxplot of avg_grade by Target
ggplot(academic5, aes(x = Target, y = avg_grade, fill = Target)) +
  geom_boxplot() +
  labs(title = "Distribution of Average Grades by Target Status",
       x = "Target Category",
       y = "Average Grade") +
  scale_fill_manual(values = c("Dropout" = "red", "Graduate" = "green", "Enrolled" = "blue")) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "none"  # Remove legend
  )


library(dplyr)

# Function to calculate mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Summary statistics for avg_grade by Target
summary_stats <- academic5 %>%
  group_by(Target) %>%
  summarise(
    Mean = mean(avg_grade, na.rm = TRUE),
    Median = median(avg_grade, na.rm = TRUE),
    Mode = getmode(avg_grade),
    Q1 = quantile(avg_grade, 0.25, na.rm = TRUE),
    Q3 = quantile(avg_grade, 0.75, na.rm = TRUE),
    IQR = IQR(avg_grade, na.rm = TRUE),
    Min = min(avg_grade, na.rm = TRUE),
    Max = max(avg_grade, na.rm = TRUE),
    Outliers = list(
      c(
        min(avg_grade[avg_grade < (Q1 - 1.5 * IQR)], na.rm = TRUE),
        max(avg_grade[avg_grade > (Q3 + 1.5 * IQR)], na.rm = TRUE)
      )
    )
  )

# Print summary statistics
print(summary_stats)



## Distribution of Total Approved by Target Status - 09
library(ggplot2)
library(dplyr)

# Function to calculate mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Plot: Boxplot of total_approved by Target
ggplot(academic5, aes(x = Target, y = total_approved, fill = Target)) +
  geom_boxplot() +
  labs(title = "Distribution of Total Approved by Target Status",
       x = "Target Category",
       y = "Total Approved") +
  scale_fill_manual(values = c("Dropout" = "red", "Graduate" = "green", "Enrolled" = "blue")) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "none"  # Remove legend
  )

# Summary statistics for total_approved by Target
summary_stats_total_approved <- academic5 %>%
  group_by(Target) %>%
  summarise(
    Mean = mean(total_approved, na.rm = TRUE),
    Median = median(total_approved, na.rm = TRUE),
    Mode = getmode(total_approved),
    Q1 = quantile(total_approved, 0.25, na.rm = TRUE),
    Q3 = quantile(total_approved, 0.75, na.rm = TRUE),
    IQR = IQR(total_approved, na.rm = TRUE),
    Min = min(total_approved, na.rm = TRUE),
    Max = max(total_approved, na.rm = TRUE),
    Outliers = list(
      c(
        min(total_approved[total_approved < (Q1 - 1.5 * IQR)], na.rm = TRUE),
        max(total_approved[total_approved > (Q3 + 1.5 * IQR)], na.rm = TRUE)
      )
    )
  )

# Print summary statistics
print(summary_stats_total_approved)



# Three Boxplots of Parental Scores by Target 09.1
library(ggplot2)

# Create boxplot for parental_scores by Target
ggplot(academic5, aes(x = Target, y = parental_scores, fill = Target)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Dropout" = "red", "Graduate" = "green", "Enrolled" = "blue")) +  # Custom colors
  labs(title = "Parental Scores by Target",
       x = "Target Status",
       y = "Parental Scores") +
  theme_minimal()


## Heatmaps

# Select only the numeric columns from academic5
numeric_data <- academic5[sapply(academic5, is.numeric)]

# Now calculate the correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Display the correlation matrix
print(cor_matrix)


# Install pheatmap if not already installed
install.packages("pheatmap")

# Load the pheatmap library
library(pheatmap)

# Select numeric columns
numeric_data <- academic5[sapply(academic5, is.numeric)]

# Compute the correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Create the heatmap using pheatmap
pheatmap(cor_matrix, 
         color = colorRampPalette(c("blue", "white", "red"))(100), 
         main = "Correlation Heatmap")



# Remove the mean, median, and mode columns from both datasets
academic5 <- academic5 %>% select(-Mean, -Median, -Mode)
academic6 <- academic6 %>% select(-Mean, -Median, -Mode)



# heatmap with Target
library(pheatmap)

# Step 1: Create a new dataframe called academic6
academic6 <- academic5

# Step 2: Encode 'Target' as numeric values
academic6$Target <- as.numeric(factor(academic6$Target, levels = c("Dropout", "Graduate", "Enrolled")))

# Step 3: Select numeric columns including the newly encoded 'Target'
numeric_data <- academic6[sapply(academic6, is.numeric)]

# Step 4: Compute the correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Step 5: Create the heatmap using pheatmap
pheatmap(cor_matrix, 
         color = colorRampPalette(c("blue", "white", "red"))(100), 
         main = "Correlation Heatmap Including Target")




















































