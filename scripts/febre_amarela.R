#load libraries

library(readr)
library(tidyverse)
library(ggplot2)

#load epizootic dataset
epi <- read_delim(
  file = "fa_epizpnh_1994-2025.csv",
  delim = ";",
  col_names = TRUE,
  locale = locale(encoding = "ISO-8859-1")
)

View(epi)

#load human dataset
human <- read_delim(
  file = "fa_casoshumanos_1994-2025.csv",
  delim = ";",
  col_names = TRUE,
  locale = locale(encoding = "ISO-8859-1")
)

View(human)

#how many cases per year?
epi_year <- epi %>%
  filter(!is.na(ANO_OCOR)) %>%
  count(ANO_OCOR)

epi_year


human_year <- human %>%
  filter(!is.na(ANO_IS)) %>%
  count(ANO_IS)

human_year

# 1) Epizootias: count per year + rename
epi_year <- epi %>%
  filter(!is.na(ANO_OCOR)) %>%
  count(ANO_OCOR) %>%
  rename(year = ANO_OCOR,
         epizootic_events = n)

# Plot epizootias
ggplot(epi_year, aes(x = year, y = epizootic_events)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Yellow Fever Epizootic Events in Brazil",
    subtitle = "Non-human primate surveillance, 1994–2025",
    x = "Year",
    y = "Number of epizootic events"
  ) +
  theme_minimal()

# 2) Humans: count per year + rename
human_year <- human %>%
  filter(!is.na(ANO_IS)) %>%
  count(ANO_IS) %>%
  rename(year = ANO_IS,
         human_cases = n)

# Plot human cases
ggplot(human_year, aes(x = year, y = human_cases)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Yellow Fever Human Cases in Brazil",
    subtitle = "Cases by year of symptom onset (ANO_IS), 1994–2025",
    x = "Year",
    y = "Number of human cases"
  ) +
  theme_minimal()

#mutate  to long format

combined_year <- bind_rows(
  epi_year %>%
    select(year, value = epizootic_events) %>%
    mutate(type = "Epizootic events (NHP)"),
  
  human_year %>%
    select(year, value = human_cases) %>%
    mutate(type = "Human cases")
)

#combined plot

ggplot(combined_year, aes(x = year, y = value, color = type)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c(
      "Epizootic events (NHP)" = "#1b9e77",  # green-teal
      "Human cases" = "#d95f02"              # warm orange
    )
  ) +
  labs(
    title = "Yellow Fever in Brazil: Epizootic Events and Human Cases",
    subtitle = "Annual counts, 1994–2025",
    x = "Year",
    y = "Number of events / cases",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )

# Optional: make region codes consistent + readable
region_labels <- c(
  "N"  = "North",
  "NE" = "Northeast",
  "CO" = "Center-West",
  "SE" = "Southeast",
  "S"  = "South"
)

# 1) Epizootias by year + region
epi_region_year <- epi %>%
  filter(!is.na(ANO_OCOR), !is.na(MACRORREG_OCOR)) %>%
  count(year = ANO_OCOR, region = MACRORREG_OCOR, name = "epizootic_events") %>%
  mutate(region = recode(region, !!!region_labels))

# 2) Human cases by year + region
human_region_year <- human %>%
  filter(!is.na(ANO_IS), !is.na(MACRORREG_LPI)) %>%
  count(year = ANO_IS, region = MACRORREG_LPI, name = "human_cases") %>%
  mutate(region = recode(region, !!!region_labels))

# 3) Combine to long format
combined_region_year <- bind_rows(
  epi_region_year %>%
    transmute(year, region, type = "Epizootic events (NHP)", value = epizootic_events),
  human_region_year %>%
    transmute(year, region, type = "Human cases", value = human_cases)
)

# 4) Faceted plot by region
ggplot(combined_region_year, aes(x = year, y = value, color = type)) +
  geom_line(linewidth = 1.05) +
  geom_point(size = 1.8) +
  facet_wrap(~ region, scales = "free_y") +
  scale_color_manual(
    values = c(
      "Epizootic events (NHP)" = "#1b9e77",
      "Human cases" = "#d95f02"
    )
  ) +
  labs(
    title = "Yellow Fever in Brazil: Epizootic Events and Human Cases by Region",
    subtitle = "Annual counts, 1994–2025 (faceted by macro-region)",
    x = "Year",
    y = "Number of events / cases",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )
