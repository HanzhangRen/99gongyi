# Setup
library(rstudioapi)
library(tidyverse)
library(janitor)
library(ggplot2)
library(showtext)
library(survey)
showtext_auto()
getSourceEditorContext()$path %>%
  dirname() %>%
  setwd()

# Data prep
# Get World Bank data on the density of doctors across the world. Filter for
# data that is the most recent data and was collected in the past 10 years.
physicians <- read.csv("physicians.csv") %>%
  clean_names() %>%
  pivot_longer(starts_with("x"),
    names_to = "year",
    names_prefix = "x",
    values_to = "doctors",
    values_drop_na = TRUE
  ) %>%
  filter(year > 2011) %>%
  group_by(country_name) %>%
  filter(year == max(year))

# Get data on disease burden from the Institute for Health Metrics and 
# Evaluation
daly <- read.csv("daly.csv")

# Match location names and merge data files
physicians$country_name[which(!physicians$country_name %in% daly$location)]
daly$location[which(!daly$location %in% physicians$country_name)]
from <- c("Democratic People's Republic of Korea",
  "Republic of Korea",
  "Iran (Islamic Republic of)",
  "Kyrgyzstan",
  "Slovakia",
  "Venezuela (Bolivarian Republic of)",
  "Czechia",
  "Egypt",
  "Viet Nam",
  "Republic of Moldova",
  "United States of America",
  "Turkey",
  "Bolivia (Plurinational State of)",
  "United Republic of Tanzania",
  "Democratic Republic of the Congo",
  "Lao People's Democratic Republic",
  "Gambia",
  "Bahamas",
  "Yemen",
  "Côte d'Ivoire",
  "Congo",
  "Saint Lucia",
  "Saint Vincent and the Grenadines"
)
to <- c("Korea, Dem. People's Rep.",
  "Korea, Rep.",
  "Iran, Islamic Rep.",
  "Kyrgyz Republic",
  "Slovak Republic",
  "Venezuela, RB",
  "Czech Republic",
  "Egypt, Arab Rep.",
  "Vietnam",
  "Moldova",
  "United States",
  "Turkiye",
  "Bolivia",
  "Tanzania",
  "Congo, Dem. Rep.",
  "Lao PDR",
  "Gambia, The",
  "Bahamas, The",
  "Yemen, Rep.",
  "Cote d'Ivoire",
  "Congo, Rep.",
  "St. Lucia",
  "St. Vincent and the Grenadines"
)
match <- function(from, to) {
  daly$location[daly$location == from] <<- to
}
walk2(from, to, match)
doctors <- inner_join(daly, physicians, 
                      by = c("location" = "country_name")) %>% 
  mutate(val = val / 100000)

# How many doctors are there in China
doctors[doctors$location == "China", ]
doctors$doctors[doctors$location == "China"] / 1000 * 1400000000

# Plot the concentration of doctors against disease burden
doctors %>%
  ggplot(aes(x = doctors, y = val, color = location)) +
  geom_point() +
  scale_color_manual(
    labels = c("China" = "中国"),
    breaks = "China",
    values = rep("Red", nrow(doctors))
  ) +
  labs(
    title = "图四：各地医生密度vs.伤病负担",
    x = "每千人医生数", y = "人均每年伤病负担（伤残调整寿命年）",
    color = "地区"
  )
ggsave("各地医生密度vs.伤病负担.png")

# This function takes a dataset of doctor density and health burden and
# estimate how much greater density of doctors is needed to reduce burden by 
# 5000.
reduce5000 <- function(data) {
  # Model the data
  glm <- svyglm(val ~ log(doctors), svydesign(~0, data = data, )) %>%
    summary()
  # Estimate the slope
  critical_value <- qt(.975, df = glm$df.residual)
  # To reduce burden by 5000, how much greater density of doctors do we need?
  exp(-.1 / c(
    glm$coefficients[[2, 1]] - critical_value * glm$coefficients[[2, 2]],
    glm$coefficients[[2, 1]],
    glm$coefficients[[2, 1]] + critical_value * glm$coefficients[[2, 2]]
  )) - 1
}
reduce5000(doctors)

# All countries with doctor density above 1
doctors1 <- filter(doctors, doctors > 1.5)
reduce5000(doctors1)