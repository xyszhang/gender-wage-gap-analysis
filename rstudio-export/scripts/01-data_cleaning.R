#### Preamble ####
# Purpose: Clean the data downloaded from opendatatoronto
# Author: Sarah Zhang
# Data: 1 February 2023
# Contact: xys.zhang@mail.utoronto.ca
# Pre-requisites: 
# - Need to have downloaded the wage by education data and saved it to inputs/data
# - Don't forget to gitignore it!
# - Change these to yours
# Any other information needed?


#### Workspace setup ####
# Use R Projects, not setwd().
library(haven)
library(tidyverse)

raw_wage_data <-
  read_csv(
    file =
      "https://data.ontario.ca/dataset/1f14addd-e4fc-4a07-9982-ad98db07ef86/resource/7b325fa1-e9d6-4329-a501-08cdc22a79df/download/v0913_05.csv",
    show_col_types = FALSE
  )

write_csv(
  x = raw_wage_data,
  file = "wage_by_education.csv"
)

head(raw_wage_data)


names(raw_wage_data)

cleaned_wage_data <-
  clean_names(raw_wage_data) |>
  filter (
    geography == "Canada",
    type_of_work == "Both full- and part-time",
    wages == "Average hourly wage rate",
    education_level == "Total, all education levels",
    age_group == "15 years and over"
  ) |>
  select(
    year,
    male,
    female
  ) 

head(cleaned_wage_data)
tail(cleaned_wage_data)

cleaned_wage_data <-
  cleaned_wage_data |>
  rename(
    avg_male_wage = male,
    avg_female_wage = female
  )

head(cleaned_wage_data)

cleaned_wage_data$year |>
  unique() |>
  length() == 23

cleaned_wage_data$avg_male_wage |> min() >= 0

cleaned_wage_data$avg_male_wage |> max() <= 50

cleaned_wage_data$avg_female_wage |> max() <= 50

cleaned_wage_data$avg_female_wage |> min() >= 0

write_csv(
  x = cleaned_wage_data,
  file = "cleaned_wage_data.csv"
)

#### Explore ####
cleaned_wage_data <-
  read_csv(
    file = "cleaned_wage_data.csv",
    show_col_types = FALSE
  )         

cleaned_wage_data |>
  ggplot(aes(x = year)) +
  geom_line(aes(y = avg_male_wage), color = "blue") +
  geom_line(aes(y = avg_female_wage), color = "red") +
  labs(
    x = "Year",
    y = "Average Hourly Wage",
    color = "Gender"
  ) 

cleaned_employee_data <-
  clean_names(raw_wage_data) |>
  filter (
    geography == "Canada",
    type_of_work == "Both full- and part-time",
    wages == "Total employees",
    education_level == "Total, all education levels",
    age_group == "15 years and over"
  ) |>
  select(
    year,
    male,
    female
  ) 

head(cleaned_employee_data)
tail(cleaned_employee_data)

write_csv(
  x = cleaned_employee_data,
  file = "cleaned_employee_data.csv"
)

#### Explore ####
cleaned_employee_data <-
  read_csv(
    file = "cleaned_employee_data.csv",
    show_col_types = FALSE
  )

cleaned_employee_data |>
  ggplot(aes(x = year)) +
  geom_line(aes(y = male), color = "blue") +
  geom_line(aes(y = female), color = "red") +
  labs(
    x = "Year",
    y = "Employees",
    color = "Gender"
  ) 

names(raw_wage_data)

cleaned_education_data <-
  clean_names(raw_wage_data) |>
  filter (
    year == "2019",
    geography == "Canada",
    type_of_work == "Both full- and part-time",
    wages == "Average hourly wage rate",
    age_group == "15 years and over"
  ) |>
  select(
    education_level,
    male,
    female
  )

print(cleaned_education_data)

cleaned_education_data$education_level |>
  unique()

cleaned_education_data <-
  cleaned_education_data |>
  mutate(
    education_level =
      recode(
        education_level,
        "Total, all education levels" = "Total",
        "0 - 8  years" = "<HS",
        "Some high school" = "<HS",
        "High school graduate" = "HS",
        "Some post-secondary" = "<CC",                       
        "Post-secondary certificate or diploma" = "Cer/Dip",    
        "Trade certificate or diploma" = "Cer/Dip",             
        "Community college, CEGEP" = "CC",                 
        "University certificate below bachelors degree" = "<BD",
        "University degree" = "<BD",                      
        "Bachelor's degree" = "BD",                 
        "Above bachelor's degree" = ">BD",
        "PSE  (5,6,7,8,9))" = "PSE",                          
        "No PSE  (0,1,2,3,4)" = "No PSE"
      )
  )

print(cleaned_education_data)

write_csv(
  x = cleaned_education_data,
  file = "cleaned_education_data.csv"
)

#### Explore ####
cleaned_education_data <-
  read_csv(
    file = "cleaned_education_data.csv",
    show_col_types = FALSE
  )

cleaned_education_data |>
  ggplot(aes(x = education_level)) +
  geom_point(aes(y = male), color = "blue") +
  geom_point(aes(y = female), color = "red") +
  theme_minimal() +
  labs(
    x = "Education Level",
    y = "Average Hourly Wage",
    color = "Gender"
  ) 

names(raw_wage_data)

cleaned_age_data <-
  clean_names(raw_wage_data) |>
  filter (
    year == "2019",
    geography == "Canada",
    type_of_work == "Both full- and part-time",
    wages == "Average hourly wage rate",
    education_level == "Total, all education levels",
  ) |>
  select(
    age_group,
    male,
    female
  ) 

head(cleaned_age_data)

cleaned_age_data$age_group |>
  unique()

cleaned_age_data <-
  cleaned_age_data |>
  mutate(
    age_group =
      recode(
        age_group,
        "15 years and over" = ">15",
        "15-24 years" = "15-24",
        "20-34 years" = "20-34",
        "25 years and over" = ">25",
        "25-34 years" = "25-34",
        "25-54 years" = "25-54",
        "25-64 years" = "25-64",
        "55 years and over" = ">55"
      )
  )

print(cleaned_age_data)

write_csv(
  x = cleaned_age_data,
  file = "cleaned_age_data.csv"
)

#### Explore ####
cleaned_age_data <-
  read_csv(
    file = "cleaned_age_data.csv",
    show_col_types = FALSE
  )

cleaned_age_data |>
  #mutate(ag_group = c("Total", "<HS", "<HS", "HS", "<CC", "Cer/Dip", "Cer/Dip", "CC", "<BD", "Uni", "BD", ">BD", "No PSE", "PSE")) |>
  ggplot(aes(x = age_group)) +
  geom_point(aes(y = male), color = "blue") +
  geom_point(aes(y = female), color = "red") +
  theme_minimal() +
  labs(
    x = "Age Group",
    y = "Average Hourly Wage",
    color = "Gender"
  ) 

names(raw_wage_data)

cleaned_geography_data <-
  clean_names(raw_wage_data) |>
  filter (
    year == "2019",
    type_of_work == "Both full- and part-time",
    wages == "Average hourly wage rate",
    education_level == "Total, all education levels",
    age_group == "15 years and over"
  ) |>
  select(
    geography,
    male,
    female
  ) 

head(cleaned_geography_data)

cleaned_geography_data$geography |>
  unique()

cleaned_geography_data <-
  cleaned_geography_data |>
  mutate(
    geography =
      recode(
        geography,
        "Newfoundland and Labrador" = "NL",
        "Prince Edward Island" = "PE",
        "Nova Scotia" = "NS",
        "New Brunswick" = "NB",
        "Quebec" = "QC",
        "Ontario" = "ON",
        "Manitoba" = "MB",
        "Saskatchewan" = "SK",
        "Alberta" = "AB",
        "British Columbia" = "BC"
      )
  )

print(cleaned_geography_data)

write_csv(
  x = cleaned_geography_data,
  file = "cleaned_geography_data.csv"
)

#### Explore ####
cleaned_geography_data <-
  read_csv(
    file = "cleaned_geography_data.csv",
    show_col_types = FALSE
  )

cleaned_geography_data |>
  #mutate(ag_group = c("Total", "<HS", "<HS", "HS", "<CC", "Cer/Dip", "Cer/Dip", "CC", "<BD", "Uni", "BD", ">BD", "No PSE", "PSE")) |>
  ggplot(aes(x = geography)) +
  geom_point(aes(y = male), color = "blue") +
  geom_point(aes(y = female), color = "red") +
  theme_minimal() +
  labs(
    x = "Geography",
    y = "Average Hourly Wage",
    color = "Gender"
  ) 





