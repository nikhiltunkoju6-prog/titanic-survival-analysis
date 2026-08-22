# Titanic Data Visualization and Insight Communication Using R

## Project Overview

This project analyzes the Titanic passenger dataset using the R programming language and RStudio.

The main objective is to transform the Titanic passenger data into meaningful visual information and identify important patterns related to passenger survival, gender, passenger class, age, ticket fare, family structure, and embarkation port.

The project uses descriptive data analysis and visualization techniques including bar charts, histograms, boxplots, and scatter plots.

This project was completed as part of the Virtual R Data Analyst Internship.

---

## Internship Information

- **Program:** Virtual R Data Analyst Internship
- **Task:** Data Visualization and Insight Communication using R
- **Internship Duration:** 23 July 2026 – 23 August 2026
- **Submitted By:** Nikhil Tunkoju
- **Submission Date:** 20 August 2026

---

## Objectives

The main objectives of this project are:

- Load and prepare the Titanic dataset in RStudio.
- Understand the important variables used in the analysis.
- Create informative visualizations using R.
- Select appropriate chart types for different types of data.
- Analyze passenger survival patterns.
- Compare survival patterns across gender and passenger class.
- Examine passenger age and ticket fare distributions.
- Analyze the relationship between passenger age and ticket fare.
- Examine passenger distribution according to family structure.
- Analyze passenger distribution by embarkation port.
- Examine survival rates across different age groups.
- Communicate analytical findings using clear visualizations.

---

## Dataset

The Titanic dataset contains demographic and travel information about passengers aboard the RMS Titanic.

### Dataset Information

| Attribute | Value |
|---|---|
| Dataset Name | Titanic Dataset |
| Number of Rows | 891 |
| Number of Columns | 12 |
| Dataset Type | Structured CSV File |
| Missing Values | Present |

### Numerical Variables

- PassengerId
- Age
- Fare
- SibSp
- Parch

### Categorical Variables

- Survived
- Pclass
- Name
- Sex
- Ticket
- Cabin
- Embarked

The dataset was obtained from the Kaggle Titanic dataset repository.

---

## Variables Used in the Analysis

| Variable | Description | Purpose |
|---|---|---|
| Survived | Passenger survival status | Main outcome variable |
| Sex | Passenger gender | Gender analysis |
| Pclass | Passenger class | Class comparison |
| Age | Passenger age | Age distribution |
| Fare | Ticket fare | Fare distribution |
| SibSp | Siblings/spouses aboard | Family classification |
| Parch | Parents/children aboard | Family classification |
| Embarked | Port of embarkation | Passenger distribution |
| FamilySize | Derived family size | Family analysis |
| FamilyType | Derived family category | Family analysis |
| AgeGroup | Derived age category | Age-group survival analysis |

---

## Tools and Technologies

### Programming Language

- R

### Development Environment

- RStudio

### R Packages

- `ggplot2`
- `dplyr`

### Other Tools

- Microsoft Word for report documentation
- GitHub for project version control and submission

---

## Project Workflow

The project follows these main steps:

1. Load the Titanic dataset.
2. Inspect the dataset structure.
3. Check missing values.
4. Prepare categorical variables.
5. Create derived variables.
6. Calculate basic summary statistics.
7. Create visualizations.
8. Save plots as PNG files.
9. Interpret the visualizations.
10. Identify key findings and limitations.

---
## Data Preparation

The dataset was loaded into R using:

```r
data <- read.csv("titanic.csv")
