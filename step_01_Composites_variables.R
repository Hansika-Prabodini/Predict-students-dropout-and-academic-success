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





















































