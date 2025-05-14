academic <- read_excel("C:/Users/Asus/Desktop/MSC/students.xlsx")
View(academic)

# Load necessary libraries
library(readxl)
library(dplyr)

# Step 1: Read the Excel file into 'academic'
academic <- read_excel("C:/Users/Asus/Desktop/MSC/students.xlsx")
View(academic)

# Step 2: Clean and transform the data, store result in 'academic2'
academic2 <- academic %>%
  mutate(
    total_enrolled = `Curricular units 1st sem (enrolled)` + `Curricular units 2nd sem (enrolled)`,
    total_approved = `Curricular units 1st sem (approved)` + `Curricular units 2nd sem (approved)`,
    avg_grade = (`Curricular units 1st sem (grade)` + `Curricular units 2nd sem (grade)`) / 2,
    units_without_evaluation = `Curricular units 1st sem (without evaluations)` + `Curricular units 2nd sem (without evaluations)`,
    units_with_evaluation = `Curricular units 1st sem (evaluations)` + `Curricular units 2nd sem (evaluations)`
  )

# Step 3: View the cleaned dataset
View(academic2)

# Optional: Save the cleaned dataset to CSV (if needed)
write.csv(academic2, "C:/Users/Asus/Desktop/MSC/cleaned_students.csv", row.names = FALSE)

## Remove/Unselected used columns
academic2 <- academic2 %>%
  select(
    -`Curricular units 1st sem (enrolled)`,
    -`Curricular units 2nd sem (enrolled)`,
    -`Curricular units 1st sem (approved)`,
    -`Curricular units 2nd sem (approved)`,
    -`Curricular units 1st sem (grade)`,
    -`Curricular units 2nd sem (grade)`,
    -`Curricular units 1st sem (without evaluations)`,
    -`Curricular units 2nd sem (without evaluations)`,
    -`Curricular units 1st sem (evaluations)`,
    -`Curricular units 2nd sem (evaluations)`
  )

View(academic2)

# Load dplyr
library(dplyr)

# Ensure occupation columns are numeric
academic2$`Mother's occupation` <- as.numeric(academic2$`Mother's occupation`)
academic2$`Father's occupation` <- as.numeric(academic2$`Father's occupation`)

# Add categorized mother's occupation
academic2$mother_occ_category <- case_when(
  academic2$`Mother's occupation` == 0 ~ "Student",
  academic2$`Mother's occupation` == 1 ~ "Managers",
  academic2$`Mother's occupation` == 2 ~ "Professionals",
  academic2$`Mother's occupation` %in% c(122, 123, 125) ~ "Professionals",
  academic2$`Mother's occupation` %in% c(3, 131, 132, 134) ~ "Technicians",
  academic2$`Mother's occupation` %in% c(4, 141, 143, 144) ~ "Clerical Support",
  academic2$`Mother's occupation` %in% c(5, 151, 152, 153) ~ "Services & Sales",
  academic2$`Mother's occupation` == 6 ~ "Agriculture & Fisheries",
  academic2$`Mother's occupation` %in% c(7, 171, 173, 175) ~ "Skilled Manual",
  academic2$`Mother's occupation` == 8 ~ "Machine Operators",
  academic2$`Mother's occupation` %in% c(9, 191, 192, 193, 194) ~ "Unskilled Labor",
  academic2$`Mother's occupation` == 10 ~ "Military",
  academic2$`Mother's occupation` %in% c(90, 99) ~ "Other/Unknown",
  TRUE ~ "Other/Unknown"
)

# Add categorized father's occupation
academic2$father_occ_category <- case_when(
  academic2$`Father's occupation` == 0 ~ "Student",
  academic2$`Father's occupation` %in% c(1, 112, 114) ~ "Managers",
  academic2$`Father's occupation` %in% c(2, 121, 122, 123, 124) ~ "Professionals",
  academic2$`Father's occupation` %in% c(3, 131, 132, 134, 135) ~ "Technicians",
  academic2$`Father's occupation` %in% c(4, 141, 143, 144) ~ "Clerical Support",
  academic2$`Father's occupation` %in% c(5, 151, 152, 153, 154) ~ "Services & Sales",
  academic2$`Father's occupation` %in% c(6, 161, 163) ~ "Agriculture & Fisheries",
  academic2$`Father's occupation` %in% c(7, 171, 172, 174, 175) ~ "Skilled Manual",
  academic2$`Father's occupation` %in% c(8, 181, 182, 183) ~ "Machine Operators",
  academic2$`Father's occupation` %in% c(9, 192, 193, 194, 195) ~ "Unskilled Labor",
  academic2$`Father's occupation` %in% c(10, 101, 102, 103) ~ "Military",
  academic2$`Father's occupation` %in% c(90, 99) ~ "Other/Unknown",
  TRUE ~ "Other/Unknown"
)


View(academic2)


# Create academic3 dataset as a copy of academic2
academic3 <- academic2

# Assign weighted scores to Mother's occupation in academic3
academic3$mother_occ_weighted <- case_when(
  academic3$`Mother's occupation` == 0 ~ 1,                # Student
  academic3$`Mother's occupation` == 1 ~ 5,                # Managers
  academic3$`Mother's occupation` == 2 ~ 4,                # Professionals
  academic3$`Mother's occupation` %in% c(122, 123, 125) ~ 4,  # Professionals
  academic3$`Mother's occupation` %in% c(3, 131, 132, 134) ~ 3, # Technicians
  academic3$`Mother's occupation` %in% c(4, 141, 143, 144) ~ 2, # Clerical Support
  academic3$`Mother's occupation` %in% c(5, 151, 152, 153) ~ 2, # Services & Sales
  academic3$`Mother's occupation` == 6 ~ 3,                # Agriculture & Fisheries
  academic3$`Mother's occupation` %in% c(7, 171, 173, 175) ~ 3, # Skilled Manual
  academic3$`Mother's occupation` == 8 ~ 2,                # Machine Operators
  academic3$`Mother's occupation` %in% c(9, 191, 192, 193, 194) ~ 1, # Unskilled Labor
  academic3$`Mother's occupation` == 10 ~ 3,               # Military
  academic3$`Mother's occupation` %in% c(90, 99) ~ 0,      # Other/Unknown
  TRUE ~ 0                                              # Other/Unknown
)

# Assign weighted scores to Father's occupation in academic3
academic3$father_occ_weighted <- case_when(
  academic3$`Father's occupation` == 0 ~ 1,                # Student
  academic3$`Father's occupation` %in% c(1, 112, 114) ~ 5,  # Managers
  academic3$`Father's occupation` %in% c(2, 121, 122, 123, 124) ~ 4, # Professionals
  academic3$`Father's occupation` %in% c(3, 131, 132, 134, 135) ~ 3, # Technicians
  academic3$`Father's occupation` %in% c(4, 141, 143, 144) ~ 2, # Clerical Support
  academic3$`Father's occupation` %in% c(5, 151, 152, 153, 154) ~ 2, # Services & Sales
  academic3$`Father's occupation` %in% c(6, 161, 163) ~ 3,  # Agriculture & Fisheries
  academic3$`Father's occupation` %in% c(7, 171, 172, 174, 175) ~ 3, # Skilled Manual
  academic3$`Father's occupation` %in% c(8, 181, 182, 183) ~ 2, # Machine Operators
  academic3$`Father's occupation` %in% c(9, 192, 193, 194, 195) ~ 1, # Unskilled Labor
  academic3$`Father's occupation` %in% c(10, 101, 102, 103) ~ 3, # Military
  academic3$`Father's occupation` %in% c(90, 99) ~ 0,      # Other/Unknown
  TRUE ~ 0                                              # Other/Unknown
)


# Assign weighted scores to Father's qualification
academic3$father_qualification_weighted <- case_when(
  # Assign weight of 0 for no education or if the father cannot read or write
  academic3$`Father's qualification` %in% c(35, 36) ~ 0,  # Can't read/write, Unknown
  
  # Assign weight of 1 for secondary education or high school (incomplete or completed)
  academic3$`Father's qualification` %in% c(9, 10, 11, 12, 30) ~ 1,  # Secondary Education / High School
  
  # Assign weight of 2 for vocational or technical courses (e.g., General Commerce Course, Technical-professional course)
  academic3$`Father's qualification` %in% c(19, 22, 40) ~ 2,  # Vocational or Technical Course
  
  # Assign weight of 3 for a Bachelor's Degree
  academic3$`Father's qualification` == 2 ~ 3,  # Bachelor's Degree
  
  # Assign weight of 4 for Master's Degree
  academic3$`Father's qualification` == 4 ~ 4,  # Master's Degree
  
  # Assign weight of 5 for Doctorate degree
  academic3$`Father's qualification` == 6 ~ 5,  # Doctorate
  
  # If none of the above conditions are met, assign a default weight of 0
  TRUE ~ 0  # Default to 0 if it doesn't match any of the cases
)



# Assign weighted scores to Mother's qualification
academic3$mother_qualification_weighted <- case_when(
  # Assign weight of 0 for no education or if the mother cannot read or write
  academic3$`Mother's qualification` %in% c(35, 36) ~ 0,  # Can't read/write, Unknown
  
  # Assign weight of 1 for secondary education or high school (incomplete or completed)
  academic3$`Mother's qualification` %in% c(9, 10, 11, 12, 30) ~ 1,  # Secondary Education / High School
  
  # Assign weight of 2 for vocational or technical courses (e.g., General Commerce Course, Technical-professional course)
  academic3$`Mother's qualification` %in% c(19, 22, 40) ~ 2,  # Vocational or Technical Course
  
  # Assign weight of 3 for a Bachelor's Degree
  academic3$`Mother's qualification` == 2 ~ 3,  # Bachelor's Degree
  
  # Assign weight of 4 for Master's Degree
  academic3$`Mother's qualification` == 4 ~ 4,  # Master's Degree
  
  # Assign weight of 5 for Doctorate degree
  academic3$`Mother's qualification` == 6 ~ 5,  # Doctorate
  
  # If none of the above conditions are met, assign a default weight of 0
  TRUE ~ 0  # Default to 0 if it doesn't match any of the cases
)



View(academic3)


academic4 <- academic3

head(academic4)

## remove used data columns


academic4 <- academic4 %>%
  select(-mother_occ_category, -`Mother's qualification`, -`Father's qualification`, 
         -`Mother's occupation`, -`Father's occupation`, -father_occ_category)

View(academic4)


# Combine and calculate parental scores by summing the weights of the mother's and father's occupation and qualification
academic4$parental_scores <- rowSums(academic4[, c("mother_occ_weighted", "father_occ_weighted", 
                                                   "father_qualification_weighted", "mother_qualification_weighted")], na.rm = TRUE)

## used Min-Max 
# Normalize the score to the range 1-5
# Assuming the maximum possible score (i.e., the highest possible sum) is 20 (if all weights were at their max, 5)
academic4$parental_scores <- 1 + 4 * (academic4$parental_scores - min(academic4$parental_scores, na.rm = TRUE)) / 
  (max(academic4$parental_scores, na.rm = TRUE) - min(academic4$parental_scores, na.rm = TRUE))


View(academic4)

academic5 <- academic4

## remove unnessary data columns 


academic5 <- academic5 %>%
  select(-mother_occ_weighted, -father_occ_weighted, 
         -father_qualification_weighted, -mother_qualification_weighted)



View(academic5)























