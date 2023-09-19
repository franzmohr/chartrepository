# macro, forecast, gdp, growth, projection


rm(list = ls())

# Choose language
lang <- "en"

# Choose countries
ctry <- c("EA", "AT")


library(dplyr)
library(eurostat)
library(ggplot2)
library(lubridate)
library(tidyr)
library(zoo)

if (lang == "de") {
  temp_actual <- "Tatsächliche Werte"
  temp_y <- "Reales BIP-Wachstum (y-o-y)"
  temp_caption <- "Quelle: EK, Eurostat, EZB, IHS, IWF, OeNB, WIFO."
}
if (lang == "en") {
  temp_actual <- "Actual values"
  temp_y <- "Real GDP growth (y-o-y)"
  temp_caption <- "Source: EC, ECB, Eurostat, IHS, IMF, OeNB, WIFO."
}


# Actual GDP-data

# EU-Daten
actual <- get_eurostat(id = "nama_10_gdp",
                         filters = list(geo = ctry,
                                        unit = "CLV_PCH_PRE",
                                        na_item = "B1GQ"),
                       cache = FALSE) %>%
  rename(date = time,
         ctry = geo,
         value = values) %>%
  select(date, ctry, value) %>%
  filter(!is.na(value)) %>%
  arrange(ctry, date) %>%
  mutate(value = value / 100,
         date = floor_date(date, "year"))

recent <- actual %>%
  group_by(ctry) %>%
  filter(date == max(date)) %>%
  ungroup()

# Forecasts ----

# Helper table
eu_ctry <- bind_rows(eurostat::eu_countries,
                     data.frame(code = "EA", name = "Euro area", label = "Euro area"),
                     data.frame(code = "EU", name = "European Union", label = "European Union"))

# Download country list
temp <- tempfile()
download.file("https://github.com/franzmohr/macroprojections/raw/master/data/geo.csv", destfile = temp)
geo <- read.csv(temp) %>%
  left_join(eu_ctry, by = c("ctry_name" = "name")) %>%
  filter(!is.na(code)) %>%
  # Change Eurostat country codes to ISO2
  mutate(code = case_when(code == "UK" ~ "GB",
                          code == "EL" ~ "GR",
                          TRUE ~ code))
rm(eu_ctry)

# Download forecast
download.file("https://github.com/franzmohr/macroprojections/raw/master/data/forecasts.csv", destfile = temp)
fcst <- read.csv(temp) %>%
  filter(ctry %in% pull(geo, "ctry"),
         variable == "NGDP_RPCH") %>%
  left_join(geo, by = "ctry") %>%
  select(year, code, variable, value, institution, pubdate) %>%
  # Filter for most recent projection per institution
  group_by(code, institution, variable) %>%
  filter(pubdate == max(pubdate)) %>%
  ungroup() %>%
  filter(code %in% ctry) %>%
  rename(ctry = code) %>%
  mutate(date = as.Date(paste0(year, "-01-01")),
         value = value / 100) %>%
  select(date, ctry, value, institution)

# Institutions
download.file("https://github.com/franzmohr/macroprojections/raw/master/data/institutions.csv", destfile = temp)
inst <- read.csv(temp) %>%
  arrange(institution)

file.remove(temp)
rm(temp)



# Add most recent actual data to forecast data for visualisation
temp <- NULL
src <- unique(pull(fcst, "institution"))
for (i in src) {
  temp_i <- fcst %>%
    filter(institution == i) %>%
    bind_rows(recent) %>%
    mutate(institution = na.locf(institution, na.rm = FALSE)) %>%
    left_join(inst, by = "institution")
  temp <- bind_rows(temp, temp_i)
}
fcst <- temp

inst <- fcst %>%
  select(institution, institution_name) %>%
  distinct()

# Combine actual and forecast data
result <- bind_rows(actual, fcst) %>%
  mutate(name = ifelse(is.na(institution), "actual", institution),
         name = factor(name, levels = c("actual", pull(inst, "institution")),
                       labels = c(temp_actual, pull(inst, "institution_name")))) %>%
  filter(date >= "2015-01-01")

g <- ggplot(result, aes(x = date, y = value, colour = name)) +
  geom_hline(yintercept = 0) +
  geom_line() +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~ ctry) +
  guides(colour = guide_legend(ncol = 2)) +
  labs(y = temp_y,
       caption = temp_caption) +
  theme(axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())

ggsave(g, filename = "figures/gdp_growth_projections_for_eu_countries.png", height = 3.5, width = 8)
