# ============================================================
# TITANIC PASSENGER SURVIVAL ANALYSIS
# File: titanic_analysis.R
# ============================================================

# ------------------------------------------------------------
# 1. INSTALL / LOAD REQUIRED PACKAGES
# ------------------------------------------------------------

required_packages <- c(
  "tidyverse",
  "ggplot2",
  "dplyr"
)

for (package in required_packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

# ------------------------------------------------------------
# 2. CREATE OUTPUT FOLDER
# ------------------------------------------------------------

if (!dir.exists("plots")) {
  dir.create("plots")
}

# ------------------------------------------------------------
# 3. LOAD DATASET
# ------------------------------------------------------------

# The original Titanic dataset should be placed in:
# data/titanic.csv

titanic <- read.csv(
  "data/titanic.csv",
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 4. STANDARDIZE COLUMN NAMES
# ------------------------------------------------------------

names(titanic) <- tolower(names(titanic))
names(titanic) <- gsub("\\.", "_", names(titanic))
names(titanic) <- gsub(" ", "_", names(titanic))

# ------------------------------------------------------------
# 5. CHECK DATASET
# ------------------------------------------------------------

cat("========================================\n")
cat("TITANIC DATASET INFORMATION\n")
cat("========================================\n")

cat("Number of rows:", nrow(titanic), "\n")
cat("Number of columns:", ncol(titanic), "\n\n")

cat("Column names:\n")
print(names(titanic))

cat("\nFirst six rows:\n")
print(head(titanic))

cat("\nSummary:\n")
print(summary(titanic))

cat("\nMissing values:\n")
print(colSums(is.na(titanic)))

# ------------------------------------------------------------
# 6. IDENTIFY COMMON TITANIC COLUMN NAMES
# ------------------------------------------------------------

# This section allows the script to work with common versions
# of the Titanic dataset.

find_column <- function(possible_names) {
  available <- names(titanic)
  match <- possible_names[possible_names %in% available]
  
  if (length(match) > 0) {
    return(match[1])
  }
  
  return(NA)
}

survived_col <- find_column(
  c("survived", "survival", "survival_status")
)

sex_col <- find_column(
  c("sex", "gender")
)

pclass_col <- find_column(
  c("pclass", "passenger_class", "class")
)

age_col <- find_column(
  c("age")
)

fare_col <- find_column(
  c("fare", "ticket_fare")
)

sibsp_col <- find_column(
  c("sibsp", "siblings_spouses")
)

parch_col <- find_column(
  c("parch", "parents_children")
)

embarked_col <- find_column(
  c("embarked", "embarkation_port", "port")
)

# ------------------------------------------------------------
# 7. CHECK REQUIRED COLUMNS
# ------------------------------------------------------------

required_columns <- c(
  survived_col,
  sex_col,
  pclass_col,
  age_col,
  fare_col
)

if (any(is.na(required_columns))) {
  
  cat("\nERROR: Required columns could not be found.\n")
  cat("Available columns are:\n")
  print(names(titanic))
  
  stop(
    "Please check the column names in data/titanic.csv."
  )
}

# ------------------------------------------------------------
# 8. RENAME IMPORTANT COLUMNS TO STANDARD NAMES
# ------------------------------------------------------------

titanic <- titanic %>%
  rename(
    survived = all_of(survived_col),
    sex = all_of(sex_col),
    pclass = all_of(pclass_col),
    age = all_of(age_col),
    fare = all_of(fare_col)
  )

if (!is.na(sibsp_col)) {
  titanic <- titanic %>%
    rename(sibsp = all_of(sibsp_col))
}

if (!is.na(parch_col)) {
  titanic <- titanic %>%
    rename(parch = all_of(parch_col))
}

if (!is.na(embarked_col)) {
  titanic <- titanic %>%
    rename(embarked = all_of(embarked_col))
}

# ------------------------------------------------------------
# 9. CONVERT DATA TYPES
# ------------------------------------------------------------

titanic$survived <- as.numeric(as.character(titanic$survived))
titanic$pclass <- as.numeric(as.character(titanic$pclass))
titanic$age <- as.numeric(as.character(titanic$age))
titanic$fare <- as.numeric(as.character(titanic$fare))

titanic$sex <- tolower(trimws(as.character(titanic$sex)))

if ("embarked" %in% names(titanic)) {
  titanic$embarked <- toupper(
    trimws(as.character(titanic$embarked))
  )
}

# ------------------------------------------------------------
# 10. DATA CLEANING
# ------------------------------------------------------------

# Remove invalid survival values
titanic <- titanic %>%
  filter(survived %in% c(0, 1))

# Remove invalid passenger class values
titanic <- titanic %>%
  filter(pclass %in% c(1, 2, 3))

# Replace missing Age values with median age
# This keeps the observations available for age analysis.

median_age <- median(
  titanic$age,
  na.rm = TRUE
)

titanic$age[is.na(titanic$age)] <- median_age

# Replace missing fare values with median fare

median_fare <- median(
  titanic$fare,
  na.rm = TRUE
)

titanic$fare[is.na(titanic$fare)] <- median_fare

# ------------------------------------------------------------
# 11. CREATE FAMILY SIZE
# ------------------------------------------------------------

if ("sibsp" %in% names(titanic) &&
    "parch" %in% names(titanic)) {
  
  titanic$family_size <-
    titanic$sibsp +
    titanic$parch +
    1
  
} else {
  
  titanic$family_size <- 1
}

# ------------------------------------------------------------
# 12. CREATE FAMILY TYPE
# ------------------------------------------------------------

titanic$family_type <- case_when(
  
  titanic$family_size == 1 ~ "Alone",
  
  titanic$family_size >= 2 &
    titanic$family_size <= 4 ~ "Small Family",
  
  titanic$family_size >= 5 ~ "Large Family",
  
  TRUE ~ "Alone"
)

# ------------------------------------------------------------
# 13. CREATE AGE GROUPS
# ------------------------------------------------------------

titanic$age_group <- case_when(
  
  titanic$age <= 12 ~ "Child",
  
  titanic$age > 12 &
    titanic$age <= 19 ~ "Teenager",
  
  titanic$age > 19 &
    titanic$age <= 29 ~ "Young Adult",
  
  titanic$age > 29 &
    titanic$age <= 59 ~ "Adult",
  
  titanic$age >= 60 ~ "Senior",
  
  TRUE ~ "Adult"
)

titanic$age_group <- factor(
  titanic$age_group,
  levels = c(
    "Child",
    "Teenager",
    "Young Adult",
    "Adult",
    "Senior"
  )
)

# ------------------------------------------------------------
# 14. SAVE CLEANED DATASET
# ------------------------------------------------------------

write.csv(
  titanic,
  "data/titanic_cleaned.csv",
  row.names = FALSE
)

cat("\nCleaned dataset saved to:")
cat(" data/titanic_cleaned.csv\n")

# ============================================================
# PLOT 1
# TITANIC PASSENGER SURVIVAL
# ============================================================

survival_data <- titanic %>%
  group_by(survived) %>%
  summarise(
    passengers = n()
  )

plot1 <- ggplot(
  survival_data,
  aes(
    x = factor(survived),
    y = passengers
  )
) +
  geom_col(fill = "gray40") +
  labs(
    title = "Titanic Passenger Survival",
    x = "Survival Status",
    y = "Number of Passengers"
  ) +
  theme_minimal(base_size = 14)

print(plot1)

ggsave(
  "plots/01_titanic_passenger_survival.png",
  plot1,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# PLOT 2
# TITANIC SURVIVAL BY GENDER
# ============================================================

gender_data <- titanic %>%
  group_by(sex) %>%
  summarise(
    passengers = n()
  )

plot2 <- ggplot(
  gender_data,
  aes(
    x = sex,
    y = passengers
  )
) +
  geom_col(fill = "gray40") +
  labs(
    title = "Titanic Survival by Gender",
    x = "Gender",
    y = "Number of Passengers"
  ) +
  theme_minimal(base_size = 14)

print(plot2)

ggsave(
  "plots/02_titanic_survival_by_gender.png",
  plot2,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# PLOT 3
# TITANIC SURVIVAL BY PASSENGER CLASS
# ============================================================

class_data <- titanic %>%
  group_by(pclass) %>%
  summarise(
    passengers = n()
  )

plot3 <- ggplot(
  class_data,
  aes(
    x = factor(pclass),
    y = passengers
  )
) +
  geom_col(fill = "gray40") +
  labs(
    title = "Titanic Survival by Passenger Class",
    x = "Passenger Class",
    y = "Number of Passengers"
  ) +
  theme_minimal(base_size = 14)

print(plot3)

ggsave(
  "plots/03_titanic_survival_by_passenger_class.png",
  plot3,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# PLOT 4
# AGE DISTRIBUTION
# ============================================================

plot4 <- ggplot(
  titanic,
  aes(x = age)
) +
  geom_histogram(
    bins = 30,
    fill = "steelblue",
    color = "white"
  ) +
  labs(
    title = "Age Distribution of Titanic Passengers",
    x = "Age",
    y = "Number of Passengers"
  ) +
  theme_minimal(base_size = 14)

print(plot4)

ggsave(
  "plots/04_age_distribution.png",
  plot4,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# PLOT 5
# AGE DISTRIBUTION BY SURVIVAL STATUS
# ============================================================

plot5 <- ggplot(
  titanic,
  aes(
    x = factor(survived),
    y = age
  )
) +
  geom_boxplot(
    fill = "white",
    color = "gray20"
  ) +
  labs(
    title = "Age Distribution by Survival Status",
    x = "Survival Status",
    y = "Age"
  ) +
  theme_minimal(base_size = 14)

print(plot5)

ggsave(
  "plots/05_age_distribution_by_survival_status.png",
  plot5,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# PLOT 6
# TITANIC TICKET FARE DISTRIBUTION
# ============================================================

plot6 <- ggplot(
  titanic,
  aes(x = fare)
) +
  geom_histogram(
    bins = 30,
    fill = "darkorange",
    color = "white"
  ) +
  labs(
    title = "Distribution of Titanic Ticket Fares",
    x = "Fare",
    y = "Number of Passengers"
  ) +
  theme_minimal(base_size = 14)

print(plot6)

ggsave(
  "plots/06_ticket_fare_distribution.png",
  plot6,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# PLOT 7
# RELATIONSHIP BETWEEN AGE AND TICKET FARE
# ============================================================

plot7 <- ggplot(
  titanic,
  aes(
    x = age,
    y = fare
  )
) +
  geom_point(
    alpha = 0.6,
    color = "gray30"
  ) +
  labs(
    title = "Relationship Between Age and Ticket Fare",
    x = "Age",
    y = "Fare"
  ) +
  theme_minimal(base_size = 14)

print(plot7)

ggsave(
  "plots/07_age_vs_ticket_fare.png",
  plot7,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# PLOT 8
# TICKET FARE DISTRIBUTION BY PASSENGER CLASS
# ============================================================

plot8 <- ggplot(
  titanic,
  aes(
    x = factor(pclass),
    y = fare
  )
) +
  geom_boxplot(
    fill = "white",
    color = "gray20"
  ) +
  labs(
    title = "Ticket Fare Distribution by Passenger Class",
    x = "Passenger Class",
    y = "Fare"
  ) +
  theme_minimal(base_size = 14)

print(plot8)

ggsave(
  "plots/08_ticket_fare_by_passenger_class.png",
  plot8,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# PLOT 9
# TITANIC SURVIVAL BY FAMILY TYPE
# ============================================================

family_data <- titanic %>%
  group_by(family_type) %>%
  summarise(
    passengers = n()
  )

family_data$family_type <- factor(
  family_data$family_type,
  levels = c(
    "Alone",
    "Small Family",
    "Large Family"
  )
)

plot9 <- ggplot(
  family_data,
  aes(
    x = family_type,
    y = passengers
  )
) +
  geom_col(fill = "gray40") +
  labs(
    title = "Titanic Survival by Family Type",
    x = "Family Type",
    y = "Number of Passengers"
  ) +
  theme_minimal(base_size = 14)

print(plot9)

ggsave(
  "plots/09_titanic_survival_by_family_type.png",
  plot9,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# PLOT 10
# TITANIC PASSENGERS BY EMBARKATION PORT
# ============================================================

if ("embarked" %in% names(titanic)) {
  
  embarkation_data <- titanic %>%
    filter(!is.na(embarked), embarked != "") %>%
    group_by(embarked) %>%
    summarise(
      passengers = n()
    )
  
  plot10 <- ggplot(
    embarkation_data,
    aes(
      x = embarked,
      y = passengers
    )
  ) +
    geom_col(fill = "gray40") +
    labs(
      title = "Titanic Passengers by Embarkation Port",
      x = "Port of Embarkation",
      y = "Number of Passengers"
    ) +
    theme_minimal(base_size = 14)
  
  print(plot10)
  
  ggsave(
    "plots/10_titanic_passengers_by_embarkation_port.png",
    plot10,
    width = 10,
    height = 6,
    dpi = 300
  )
}

# ============================================================
# PLOT 11
# SURVIVAL RATE ACROSS AGE GROUPS
# ============================================================

age_survival <- titanic %>%
  group_by(age_group) %>%
  summarise(
    survival_rate = mean(survived, na.rm = TRUE) * 100,
    passengers = n()
  )

plot11 <- ggplot(
  age_survival,
  aes(
    x = age_group,
    y = survival_rate,
    group = 1
  )
) +
  geom_line(
    color = "black",
    linewidth = 0.8
  ) +
  geom_point(
    color = "black",
    size = 4
  ) +
  labs(
    title = "Survival Rate Across Age Groups",
    x = "Age Group",
    y = "Survival Rate (%)"
  ) +
  theme_minimal(base_size = 14)

print(plot11)

ggsave(
  "plots/11_survival_rate_across_age_groups.png",
  plot11,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# 15. ADDITIONAL STATISTICAL SUMMARY
# ============================================================

cat("\n")
cat("========================================\n")
cat("ANALYSIS RESULTS\n")
cat("========================================\n")

# Overall survival rate

overall_survival <- mean(
  titanic$survived,
  na.rm = TRUE
) * 100

cat(
  "\nOverall survival rate:",
  round(overall_survival, 2),
  "%\n"
)

# Survival by gender

gender_survival <- titanic %>%
  group_by(sex) %>%
  summarise(
    passengers = n(),
    survivors = sum(survived),
    survival_rate = mean(survived) * 100
  )

cat("\nSurvival by gender:\n")
print(gender_survival)

# Survival by passenger class

class_survival <- titanic %>%
  group_by(pclass) %>%
  summarise(
    passengers = n(),
    survivors = sum(survived),
    survival_rate = mean(survived) * 100
  )

cat("\nSurvival by passenger class:\n")
print(class_survival)

# Survival by age group

cat("\nSurvival by age group:\n")
print(age_survival)

# Survival by family type

family_survival <- titanic %>%
  group_by(family_type) %>%
  summarise(
    passengers = n(),
    survivors = sum(survived),
    survival_rate = mean(survived) * 100
  )

cat("\nSurvival by family type:\n")
print(family_survival)

# ------------------------------------------------------------
# 16. SAVE STATISTICAL SUMMARIES
# ------------------------------------------------------------

write.csv(
  gender_survival,
  "gender_survival_summary.csv",
  row.names = FALSE
)

write.csv(
  class_survival,
  "class_survival_summary.csv",
  row.names = FALSE
)

write.csv(
  age_survival,
  "age_group_survival_summary.csv",
  row.names = FALSE
)

write.csv(
  family_survival,
  "family_survival_summary.csv",
  row.names = FALSE
)

# ============================================================
# 17. FINAL MESSAGE
# ============================================================

cat("\n")
cat("========================================\n")
cat("ANALYSIS COMPLETED SUCCESSFULLY\n")
cat("========================================\n")

cat("\nGenerated plots:\n")

cat("01_titanic_passenger_survival.png\n")
cat("02_titanic_survival_by_gender.png\n")
cat("03_titanic_survival_by_passenger_class.png\n")
cat("04_age_distribution.png\n")
cat("05_age_distribution_by_survival_status.png\n")
cat("06_ticket_fare_distribution.png\n")
cat("07_age_vs_ticket_fare.png\n")
cat("08_ticket_fare_by_passenger_class.png\n")
cat("09_titanic_survival_by_family_type.png\n")
cat("10_titanic_passengers_by_embarkation_port.png\n")
cat("11_survival_rate_across_age_groups.png\n")

cat("\nCleaned dataset:\n")
cat("data/titanic_cleaned.csv\n")

cat("\nSummary files:\n")
cat("gender_survival_summary.csv\n")
cat("class_survival_summary.csv\n")
cat("age_group_survival_summary.csv\n")
cat("family_survival_summary.csv\n")

cat("\nProject analysis finished.\n")