
library(ggplot2)



# Course vs Target - 9.6
View(academic5)

ggplot(academic5, aes(x = factor(Course), fill = Target)) +
  geom_bar(position = "dodge") +
  labs(title = "Course vs Target",
       x = "Course Code",
       y = "Count") +
  scale_fill_manual(values = c("Dropout" = "red", "Graduate" = "green", "Enrolled" = "blue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


# Application Mode vs Target (Bar Plot) - 9.7

ggplot(academic5, aes(x = factor(`Application mode`), fill = Target)) +
  geom_bar(position = "dodge") +
  labs(title = "Application Mode vs Target",
       x = "Application Mode",
       y = "Count") +
  scale_fill_manual(values = c("Dropout" = "red", "Graduate" = "green", "Enrolled" = "blue")) +
  theme_minimal()

application_mode_labels = c(
  "1" = "1st phase - general contingent",
  "2" = "Ordinance No. 612/93",
  "5" = "1st phase - special (Azores)",
  "7" = "Holders of other higher courses",
  "10" = "Ordinance No. 854B/99",
  "15" = "International student (bachelor)",
  "16" = "1st phase - special (Madeira)",
  "17" = "2nd phase - general contingent",
  "18" = "3rd phase - general contingent",
  "26" = "Ord. 533-A/99, b2 (Diff Plan)",
  "27" = "Ord. 533-A/99, b3 (Other Inst)",
  "39" = "Over 23 years old",
  "42" = "Transfer",
  "43" = "Change of course",
  "44" = "Tech specialization diploma",
  "51" = "Change of inst/course",
  "53" = "Short cycle diploma holders",
  "57" = "Change of inst/course (Int'l)"
)


library(dplyr)

academic7 = academic5 %>%
  mutate(`Application mode label` = recode(as.character(`Application mode`), !!!application_mode_labels))

ggplot(academic7, aes(x = `Application mode label`, fill = Target)) +
  geom_bar(position = "dodge") +
  labs(title = "Application Mode vs Target (Labeled)",
       x = "Application Mode",
       y = "Count") +
  scale_fill_manual(values = c("Dropout" = "red", "Graduate" = "green", "Enrolled" = "blue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


names(academic5)


# Proportion of Target by Application Order - 9.8

ggplot(academic5, aes(x = factor(`Application order`), fill = Target)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Target by Application Order",
       x = "Application Order (0 = first choice)",
       y = "Proportion") +
  scale_fill_manual(values = c("Dropout" = "red", "Graduate" = "green", "Enrolled" = "blue")) +
  theme_minimal()















