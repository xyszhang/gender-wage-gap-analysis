title: "Gender Wage Gap Persists in Canada"
author: "Sarah Zhang"
date: "1 February 2023"
abstract: "The analysis report is completed using the data collected by Open Data Toronto regarding different genders’ wages by education level. Detailed information on average earnings between men and women categorized by their age, education level, and geographical location was recorded. This dataset provides profound insights into the history of the gender wage gap and how the gap persists over the years regardless of age or education."

```{r}
#### Preamble ####
# Purpose: Read in data about wages by educational level in Canada and produce a # of graphs that compare wages between genders over the years.
# Author: Sarah Zhang
# Email: xys.zhang@mail.utoronto.ca
# Date: 1 February 2023
# Prerequisites: Need to know where to get wages by educational level data.
```

```{r}
#### Workspace set-up ####
# install.packages("tidyverse") 
# install.packages("janitor") 
#install.packages("opendatatoronto")
#install.packages("lubridate")
#install.packages("knitr")
#install.packages("AER")

library(AER)
library(knitr) #allow reproducible research in R
library(lubridate) #makes it easier to work with dates and times
library(opendatatoronto) #access the city of Toronto Open Data Portal
library(tidyverse) # A collection of data-related packages
library(janitor) # Helps clean datasets
```

```{r}
####Citations####
citation() # Get the citation information for R
citation("ggplot2")
citation("opendatatoronto")
citation("knitr")
citation("lubridate")
citation("tidyverse")
citation("janitor")
```

```{r}
####Simulate####
set.seed(853)

simulated_data <-
  tibble(
    year =
      rep(c(1997:2019), 1),
    female_wage =
      runif(
        n = 23,
        min = 0,
        max = 100
      ),
    male_wage =
      runif(
        n = 23,
        min = 0,
        max = 100
      )
  )

head(simulated_data)
```
```{r}
simulated_data$year |> min() == 1997
```
```{r}
simulated_data$year |> max() == 2019
```
```{r}
simulated_data$female_wage |> class() == "numeric"
```
```{r}
simulated_data$male_wage |> class() == "numeric"
```
```{r}
simulated_data$female_wage |> min() >= 0
```
```{r}
simulated_data$female_wage |> max() <= 100
```

```{r}
#### Acquire data ####
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
```
```{r}
names(raw_wage_data)
```
```{r}
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
```

```{r}
cleaned_wage_data <-
  cleaned_wage_data |>
  rename(
    avg_male_wage = male,
    avg_female_wage = female
  )

head(cleaned_wage_data)
```
```{r}
cleaned_wage_data$year |>
  unique() |>
  length() == 23
```

```{r}
cleaned_wage_data$avg_male_wage |> min() >= 0
```
```{r}
cleaned_wage_data$avg_male_wage |> max() <= 50
```
```{r}
cleaned_wage_data$avg_female_wage |> max() <= 50
```
```{r}
cleaned_wage_data$avg_female_wage |> min() >= 0
```

```{r}
write_csv(
  x = cleaned_wage_data,
  file = "cleaned_wage_data.csv"
)
```

```{r}
#### Explore ####
cleaned_wage_data <-
  read_csv(
    file = "cleaned_wage_data.csv",
    show_col_types = FALSE
  )
```

```{r}
cleaned_wage_data |>
  ggplot(aes(x = year)) +
  geom_line(aes(y = avg_male_wage), color = "blue") +
  geom_line(aes(y = avg_female_wage), color = "red") +
  labs(
    x = "Year",
    y = "Average Hourly Wage",
    color = "Gender"
  ) 
```

```{r}
names(raw_wage_data)
```
```{r}
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
```

```{r}
write_csv(
  x = cleaned_employee_data,
  file = "cleaned_employee_data.csv"
)
```

```{r}
#### Explore ####
cleaned_employee_data <-
  read_csv(
    file = "cleaned_employee_data.csv",
    show_col_types = FALSE
  )
```

```{r}
cleaned_employee_data |>
  ggplot(aes(x = year)) +
  geom_line(aes(y = male), color = "blue") +
  geom_line(aes(y = female), color = "red") +
  labs(
    x = "Year",
    y = "Employees",
    color = "Gender"
  ) 
```
```{r}
names(raw_wage_data)
```
```{r}
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
```
```{r}
cleaned_education_data$education_level |>
  unique()
```
```{r}
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
```


```{r}
write_csv(
  x = cleaned_education_data,
  file = "cleaned_education_data.csv"
)
```

```{r}
#### Explore ####
cleaned_education_data <-
  read_csv(
    file = "cleaned_education_data.csv",
    show_col_types = FALSE
  )
```

```{r}
cleaned_education_data |>
  #mutate(education_level = c("Total", "<HS", "<HS", "HS", "<CC", "Cer/Dip", "Cer/Dip", "CC", "<BD", "Uni", "BD", ">BD", "No PSE", "PSE")) |>
  ggplot(aes(x = education_level)) +
  geom_point(aes(y = male), color = "blue") +
  geom_point(aes(y = female), color = "red") +
  theme_minimal() +
  labs(
    x = "Education Level",
    y = "Average Hourly Wage",
    color = "Gender"
  ) 
```



```{r}
names(raw_wage_data)
```

```{r}
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
```
```{r}
cleaned_age_data$age_group |>
  unique()
```
```{r}
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
```

```{r}
write_csv(
  x = cleaned_age_data,
  file = "cleaned_age_data.csv"
)
```

```{r}
#### Explore ####
cleaned_age_data <-
  read_csv(
    file = "cleaned_age_data.csv",
    show_col_types = FALSE
  )
```

```{r}
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
```

```{r}
names(raw_wage_data)
```

```{r}
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
```

```{r}
cleaned_geography_data$geography |>
  unique()
```
```{r}
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
```

```{r}
write_csv(
  x = cleaned_geography_data,
  file = "cleaned_geography_data.csv"
)
```

```{r}
#### Explore ####
cleaned_geography_data <-
  read_csv(
    file = "cleaned_geography_data.csv",
    show_col_types = FALSE
  )
```

```{r}
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
```