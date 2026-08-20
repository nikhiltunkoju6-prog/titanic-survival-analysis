# ============================================================
# ============================================================
#
# Dataset: Titanic Passenger Dataset
# Input file: titanic.csv
# Output: 11 PNG visualization files
#
# ============================================================


# ============================================================
# 1. LOAD REQUIRED LIBRARIES
# ============================================================

library(ggplot2)
library(dplyr)


# ============================================================
# 2. LOAD DATASET
# ============================================================

data <- read.csv("titanic.csv")

# View first few rows
head(data)

# Check structure
str(data)

# Check missing values
colSums(is.na(data))


# ============================================================
# 3. BASIC DATA PREPARATION
# ============================================================

# Convert important categorical variables to factors
data$Survived <- as.numeric(data$Survived)

data$Pclass <- factor(
  data$Pclass,
  levels = c(1, 2, 3)
)

data$Sex <- factor(data$Sex)

data$Embarked <- factor(data$Embarked)


# ============================================================
# 3.1 CREATE FAMILY SIZE
# ============================================================

data$FamilySize <- data$SibSp + data$Parch + 1


# ============================================================
# 3.2 CREATE FAMILY TYPE
# ============================================================

data$FamilyType <- ifelse(
  data$FamilySize == 1,
  "Alone",
  ifelse(
    data$FamilySize <= 4,
    "Small Family",
    "Large Family"
  )
)

data$FamilyType <- factor(
  data$FamilyType,
  levels = c(
    "Alone",
    "Small Family",
    "Large Family"
  )
)


# ============================================================
# 3.3 REMOVE MISSING VALUES FOR AGE/Fare VISUALIZATIONS
# ============================================================

age_data <- data %>%
  filter(!is.na(Age))

fare_data <- data %>%
  filter(!is.na(Fare))


# ============================================================
# 3.4 DUPLICATE CHECK
# ============================================================

duplicate_count <- sum(duplicated(data))

cat("Number of duplicate rows:", duplicate_count, "\n")


# ============================================================
# 4. BASIC SUMMARY STATISTICS
# ============================================================

# Embarkation counts
table(data$Embarked)

# Age summary
summary(data$Age)

# Fare summary
summary(data$Fare)

# Correlation between Age and Fare
cor(
  data$Age,
  data$Fare,
  use = "complete.obs"
)

# Survival table by gender
table(data$Sex, data$Survived)

# Survival table by passenger class
table(data$Pclass, data$Survived)

# Survival table by family type
table(data$FamilyType, data$Survived)

# Overall survival
table(data$Survived)


# ============================================================
# 5. VISUALIZATION 1
# OVERALL PASSENGER SURVIVAL
# Figure 2
# Output: 01_overall_passenger_survival.png
# ============================================================

survival_plot <- ggplot(
  data,
  aes(x = factor(Survived))
) +
  geom_bar() +
  labs(
    title = "Titanic Passenger Survival",
    x = "Survival Status",
    y = "Number of Passengers"
  ) +
  scale_x_discrete(
    labels = c(
      "0" = "Did Not Survive",
      "1" = "Survived"
    )
  ) +
  theme_minimal()

print(survival_plot)

ggsave(
  "01_overall_passenger_survival.png",
  plot = survival_plot,
  width = 7,
  height = 5,
  dpi = 300
)


# ============================================================
# 6. VISUALIZATION 2
# SURVIVAL BY GENDER
# Figure 3
# Output: 02_survival_by_gender.png
# ============================================================

gender_plot <- ggplot(
  data,
  aes(
    x = Sex,
    fill = factor(Survived)
  )
) +
  geom_bar(position = "dodge") +
  labs(
    title = "Titanic Passenger Survival by Gender",
    x = "Gender",
    y = "Number of Passengers",
    fill = "Survival Status"
  ) +
  scale_fill_discrete(
    labels = c(
      "0" = "Did Not Survive",
      "1" = "Survived"
    )
  ) +
  theme_minimal()

print(gender_plot)

ggsave(
  "02_survival_by_gender.png",
  plot = gender_plot,
  width = 7,
  height = 5,
  dpi = 300
)


# ============================================================
# 7. VISUALIZATION 3
# SURVIVAL BY PASSENGER CLASS
# Figure 4
# Output: 03_survival_by_class.png
# ============================================================

class_plot <- ggplot(
  data,
  aes(
    x = Pclass,
    fill = factor(Survived)
  )
) +
  geom_bar(position = "dodge") +
  labs(
    title = "Titanic Passenger Survival by Passenger Class",
    x = "Passenger Class",
    y = "Number of Passengers",
    fill = "Survival Status"
  ) +
  scale_fill_discrete(
    labels = c(
      "0" = "Did Not Survive",
      "1" = "Survived"
    )
  ) +
  theme_minimal()

print(class_plot)

ggsave(
  "03_survival_by_class.png",
  plot = class_plot,
  width = 7,
  height = 5,
  dpi = 300
)


# ============================================================
# 8. VISUALIZATION 4
# AGE DISTRIBUTION
# Figure 5
# Output: 04_age_distribution.png
# ============================================================

age_plot <- ggplot(
  age_data,
  aes(x = Age)
) +
  geom_histogram(
    bins = 30
  ) +
  labs(
    title = "Age Distribution of Titanic Passengers",
    x = "Age",
    y = "Number of Passengers"
  ) +
  theme_minimal()

print(age_plot)

ggsave(
  "04_age_distribution.png",
  plot = age_plot,
  width = 7,
  height = 5,
  dpi = 300
)


# ============================================================
# 9. VISUALIZATION 5
# AGE DISTRIBUTION BY SURVIVAL STATUS
# Figure 6
# Output: 05_age_by_survival.png
# ============================================================

age_survival_plot <- ggplot(
  age_data,
  aes(
    x = factor(Survived),
    y = Age
  )
) +
  geom_boxplot() +
  labs(
    title = "Age Distribution by Survival Status",
    x = "Survival Status",
    y = "Age"
  ) +
  scale_x_discrete(
    labels = c(
      "0" = "Did Not Survive",
      "1" = "Survived"
    )
  ) +
  theme_minimal()

print(age_survival_plot)

ggsave(
  "05_age_by_survival.png",
  plot = age_survival_plot,
  width = 7,
  height = 5,
  dpi = 300
)


# ============================================================
# 10. VISUALIZATION 6
# TICKET FARE DISTRIBUTION
# Figure 7
# Output: 06_fare_distribution.png
# ============================================================

fare_plot <- ggplot(
  fare_data,
  aes(x = Fare)
) +
  geom_histogram(
    bins = 30
  ) +
  labs(
    title = "Distribution of Titanic Ticket Fares",
    x = "Fare",
    y = "Number of Passengers"
  ) +
  theme_minimal()

print(fare_plot)

ggsave(
  "06_fare_distribution.png",
  plot = fare_plot,
  width = 7,
  height = 5,
  dpi = 300
)


# ============================================================
# 11. VISUALIZATION 7
# RELATIONSHIP BETWEEN AGE AND FARE
# Figure 8
# Output: 07_age_vs_fare.png
# ============================================================

age_fare_plot <- ggplot(
  data,
  aes(
    x = Age,
    y = Fare
  )
) +
  geom_point(
    na.rm = TRUE
  ) +
  labs(
    title = "Relationship Between Age and Ticket Fare",
    x = "Age",
    y = "Fare"
  ) +
  theme_minimal()

print(age_fare_plot)

ggsave(
  "07_age_vs_fare.png",
  plot = age_fare_plot,
  width = 7,
  height = 5,
  dpi = 300
)


# ============================================================
# 12. VISUALIZATION 8
# TICKET FARE DISTRIBUTION BY PASSENGER CLASS
# Figure 9
# Output: 08_fare_by_class.png
# ============================================================

fare_class_plot <- ggplot(
  fare_data,
  aes(
    x = Pclass,
    y = Fare
  )
) +
  geom_boxplot() +
  labs(
    title = "Ticket Fare Distribution by Passenger Class",
    x = "Passenger Class",
    y = "Fare"
  ) +
  theme_minimal()

print(fare_class_plot)

ggsave(
  "08_fare_by_class.png",
  plot = fare_class_plot,
  width = 7,
  height = 5,
  dpi = 300
)


# ============================================================
# 13. VISUALIZATION 9
# SURVIVAL BY FAMILY TYPE
# Figure 10
# Output: 09_survival_by_family_type.png
# ============================================================

family_plot <- ggplot(
  data,
  aes(
    x = FamilyType,
    fill = factor(Survived)
  )
) +
  geom_bar(position = "dodge") +
  labs(
    title = "Titanic Passenger Survival by Family Type",
    x = "Family Type",
    y = "Number of Passengers",
    fill = "Survival Status"
  ) +
  scale_fill_discrete(
    labels = c(
      "0" = "Did Not Survive",
      "1" = "Survived"
    )
  ) +
  theme_minimal()

print(family_plot)

ggsave(
  "09_survival_by_family_type.png",
  plot = family_plot,
  width = 7,
  height = 5,
  dpi = 300
)


# ============================================================
# 14. VISUALIZATION 10
# PASSENGERS BY EMBARKATION PORT
# Figure 11
# Output: 10_passengers_by_embarkation.png
# ============================================================

embarked_plot <- ggplot(
  data,
  aes(x = Embarked)
) +
  geom_bar() +
  labs(
    title = "Titanic Passengers by Embarkation Port",
    x = "Port of Embarkation",
    y = "Number of Passengers"
  ) +
  theme_minimal()

print(embarked_plot)

ggsave(
  "10_passengers_by_embarkation.png",
  plot = embarked_plot,
  width = 7,
  height = 5,
  dpi = 300
)


# ============================================================
# 15. CREATE AGE GROUPS
# ============================================================

data$AgeGroup <- cut(
  data$Age,
  breaks = c(
    -Inf,
    12,
    19,
    29,
    59,
    Inf
  ),
  labels = c(
    "Child",
    "Teenager",
    "Young Adult",
    "Adult",
    "Senior"
  )
)


# ============================================================
# 16. CALCULATE SURVIVAL RATE BY AGE GROUP
# ============================================================

age_survival <- data %>%
  filter(!is.na(AgeGroup)) %>%
  group_by(AgeGroup) %>%
  summarise(
    SurvivalRate = mean(Survived) * 100
  )

print(age_survival)


# ============================================================
# 17. VISUALIZATION 11
# SURVIVAL RATE ACROSS AGE GROUPS
# Figure 12
# Output: 11_survival_by_age_group.png
# ============================================================

age_group_plot <- ggplot(
  age_survival,
  aes(
    x = AgeGroup,
    y = SurvivalRate,
    group = 1
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Survival Rate Across Age Groups",
    x = "Age Group",
    y = "Survival Rate (%)"
  ) +
  theme_minimal()

print(age_group_plot)

ggsave(
  "11_survival_by_age_group.png",
  plot = age_group_plot,
  width = 7,
  height = 5,
  dpi = 300
)


# ============================================================
# 18. FINAL OUTPUT
# ============================================================

print("All 11 visualizations have been generated successfully.")
