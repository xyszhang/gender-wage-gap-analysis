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



simulated_data$year |> min() == 1997

simulated_data$year |> max() == 2019

simulated_data$female_wage |> class() == "numeric"

simulated_data$male_wage |> class() == "numeric"

simulated_data$female_wage |> min() >= 0

simulated_data$female_wage |> max() <= 100
