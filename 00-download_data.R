
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

