## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(infectiousR)
library(dplyr)
library(ggplot2)

## ----covid-usa-simple-plot, message=FALSE, warning=FALSE, fig.width=7, fig.height=5----

# Load the COVID-19 data (from your package)
covid_data <- get_us_states_covid_stats()

# Select the first 5 rows and remove columns with only NA values
covid_clean <- covid_data %>%
  slice_head(n = 5) %>%
  select(where(~ !all(is.na(.))))

# Plot: Bar plot with different colors and readable y-axis (no scientific notation)
ggplot(covid_clean, aes(x = reorder(state, -cases), y = cases, fill = state)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  labs(
    title = "COVID-19: Total Reported Cases by State (Top 5)",
    x = "State",
    y = "Total Cases"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


## ----covid-stats-simple-plot, message=FALSE, warning=FALSE, fig.width=7, fig.height=5----

get_covid_stats_by_country() %>%
  filter(country %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                       "Costa Rica", "Cuba", "Dominican Republic", "Ecuador",
                       "El Salvador", "Guatemala", "Honduras", "Mexico")) %>%
  select(-updated, -starts_with("today")) %>%
  mutate(case_rate = (cases/population)*100000) %>%
  ggplot(aes(x = reorder(country, -case_rate), 
         y = case_rate, 
         fill = country)) +
  geom_col() +
  scale_fill_manual(values = rainbow(n = 13)) +  # Built-in rainbow palette
  labs(title = "COVID-19 Case Rates in Latin America",
       subtitle = "Cases per 100,000 population",
       x = NULL,
       y = "Cases per 100k") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold"),
        legend.position = "none")


