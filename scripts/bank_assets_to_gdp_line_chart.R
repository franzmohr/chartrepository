# bank, macro, gdp, assets

rm(list = ls())


# Choose language
lang <- "en"


library(dplyr)
library(ecb)
library(eurostat)
library(ggplot2)
library(lubridate)
library(tidyr)
library(zoo)


if (lang == "de") {
  temp_labels <- c("Österreich", "EU", "Andere")
  temp_title <- "Gesamte Aktiva zu BIP"
  temp_caption <- "Quelle: Eurostat, EZB."
}
if (lang == "en") {
  temp_labels <- c("Austria", "EU", "Other")
  temp_title <- "Totals assets to GDP"
  temp_caption <- "Source: ECB, Eurostat."
}


# Prepare nominal GDP data
gdp <- get_eurostat("NAMA_10_GDP", filters = list(na_item = "B1GQ",
                                                  unit = "CP_MEUR"),
                    cache = FALSE) %>%
  select(time, geo, values) %>%
  filter(!is.na(values)) %>%
  rename(date = time,
         ctry = geo,
         gdp = values) %>%
  # Filter for euro area counties
  #filter(ctry %in% c(ea_countries$code, "EA")) %>%
  # Filter for EU countries
  filter(ctry %in% c(eu_countries$code, "EU27_2020")) %>%
  mutate(ctry = case_when(ctry == "EL" ~ "GR",
                          ctry == "UK" ~ "GB",
                          ctry == "EU27_2020" ~ "EU",
                          TRUE ~ ctry))

# Prepare total asset data
assets <- bind_rows(get_data("CBD2.A..W0.67._Z._Z.A.A.A0000._X.ALL.CA._Z.LE._T.EUR"), # National data
                    get_data("CBD2.A.B0.W0.47._Z._Z.A.A.A0000._X.ALL.CA._Z.LE._T.EUR"), # EU data
                    get_data("CBD2.A.U2.W0.57._Z._Z.A.A.A0000._X.ALL.CA._Z.LE._T.EUR")) %>% # EA data
  select(obstime, ref_area, obsvalue) %>%
  filter(!is.na(obsvalue)) %>%
  rename(date = obstime,
         ctry = ref_area,
         assets = obsvalue) %>%
  mutate(assets = assets / 1000,
         date = as.Date(paste0(date, "-01-01")),
         ctry = case_when(ctry == "B0" ~ "EU",
                          ctry == "U2" ~ "EA",
                          TRUE ~ ctry))

# Combine results
result <- full_join(gdp, assets, by = c("date", "ctry")) %>%
  filter(!is.na(assets)) %>%
  mutate(value = assets / gdp) %>%
  filter(!is.na(value)) %>%
  mutate(name = ifelse(ctry %in% c("AT", "EU"), ctry, "other"),
         name = factor(name, levels = c("AT", "EU", "other"),
                       labels = temp_labels))

g <- ggplot(result, aes(x = date, y = value, colour = name, group = ctry, alpha = name)) +
  geom_line() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(.01, 0)) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_alpha_manual(values = c(1, 1, .3)) +
  labs(title = temp_title,
       caption = temp_caption) +
  theme(axis.title = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())

ggsave(g, filename = "figures/bank_assets_to_gdp_line_chart.png", height = 4, width = 11)

