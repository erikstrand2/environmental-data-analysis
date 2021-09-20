library(tidyverse)
library(scales)
library(janitor)

# import raw trade data 
raw_data <- 
  read_csv("~/Documents/EDA/InternationalMerchandiseTrade.csv", skip = 1) %>% 
  clean_names()

head(raw_data)

# clean trade data
clean_data <- 
  raw_data %>%
  rename(country = x2) %>% 
  filter(!is.na(system_of_trade)) %>% 
  pivot_wider(
    names_from = series, 
    values_from = value
  ) %>% 
  clean_names() %>% 
  select(
    country, 
    year, 
    balance = starts_with("balance")
  )

head(clean_data)

# import raw population data
population_raw <- 
  read_csv("~/Documents/EDA/population.csv", skip = 1) %>% 
  clean_names() %>% 
  rename(country = x2) %>% 
  filter(country %in% (clean_data %>% pull(country)))

head(population_raw)


# QUESTION 1
med_intl_trade <- 
  clean_data %>% 
  group_by(country) %>% 
  summarize(
    balance = median(balance)
  )

# answers in the table below: 
med_intl_trade

# QUESTION 2
med_intl_trade %>% 
  ggplot(aes(x = balance)) + 
  geom_histogram(binwidth = 25000) + 
  labs(
    title = "Median Import/Export Balance by Country, 1995-2019",
    x = "Median Import/Export Balance (By Million USD)", 
    y = "# of Countries"
  ) + 
  scale_x_continuous(
    labels = scales::label_dollar(),
    breaks = scales::breaks_width(100000)
  )

# QUESTION 3
sum_intl_trade <- 
  clean_data %>% 
  filter(!is.na(balance)) %>% 
  group_by(country) %>% 
  summarize(
    balance = last(cumsum(balance))
  )

#answers in the table below: 
sum_intl_trade


# QUESTION 4
population <- 
  population_raw %>% 
  pivot_wider(
    -c(footnotes, source),
    names_from = series, 
    values_from = value
  ) %>% 
  clean_names() %>% 
  select(
    country,
    year,
    pop = population_mid_year_estimates_millions
  ) %>% 
  group_by(country) %>% 
  summarize(
    last_pop = last(pop, order_by = year),
    first_pop = first(pop, order_by = year),
    pop_change = last_pop - first_pop, 
    perc_change = pop_change / first_pop
  )

# answers in the table below: 
population

# QUESTION 5
trade_pop <- 
  sum_intl_trade %>% 
  left_join(population, by = "country")

head(trade_pop)

trade_pop %>% 
  ggplot(aes(x = pop_change, y = balance)) + 
  geom_point() +
  scale_y_continuous(
    labels = scales::label_dollar(),
    breaks = scales::breaks_width(500000)
  ) + 
  labs(
    title = "Population Change by Cumulative Import/Export Balance, 2005-2019",
    x = "Population Change (Millions)", 
    y = "Cumulative Import/Export Balance (Million USD)"
  ) + 
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5))

trade_pop %>% 
  ggplot(aes(x = perc_change, y = balance)) + 
  geom_point() +
  scale_x_continuous(
    labels = scales::label_percent()
  ) + 
  scale_y_continuous(
    labels = scales::label_dollar(),
    breaks = scales::breaks_width(500000)
  ) + 
  labs(
    title = 
      "Percentage Pop. Change by Cumulative Import/Export Balance, 2005-2019",
    x = "Percentage Population Change", 
    y = "Cumulative Import/Export Balance (Million USD)"
  ) + 
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5))

# QUESTION 6
trade_pop <- 
  trade_pop %>% 
  mutate_at(
    vars(balance, pop_change, perc_change), 
    scale
  )


trade_pop %>%
  filter(abs(balance) >= 2) %>% 
  head()

trade_pop %>%
  filter(abs(pop_change) >= 2) %>% 
  head()

trade_pop %>%
  filter(abs(perc_change) >= 2) %>% 
  head()

