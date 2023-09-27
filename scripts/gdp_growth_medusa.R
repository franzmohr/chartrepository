# macro, forecast, gdp, growth, projection, medusa

rm(list = ls())

# Choose language
lang <- "en"

# Choose countries
ctry <- c("EA","AT")

library(dplyr)
library(eurostat)
library(ggplot2)
library(lubridate)
library(tidyr)
library(zoo)


if (lang == "de") {
  temp_actual <- "Tatsächliche Werte"
  temp_y <- "Reales BIP-Wachstum (y-o-y)"
  temp_caption <- "Quelle: EK, EZB und nationale Quellen."
}
if (lang == "en") {
  temp_actual <- "Actual values"
  temp_y <- "Real GDP growth (y-o-y)"
  temp_caption <- "Source: EC, ECB and national sources."
}


# Actual GDP-data from Eurostat
actual <- get_eurostat(id = "nama_10_gdp",
                       filters = list(geo = ctry,
                                      unit = "CLV_PCH_PRE",
                                      na_item = "B1GQ")) %>%
  rename(date = time,
         ctry = geo,
         value = values) %>%
  select(date, ctry, value) %>%
  filter(!is.na(value)) %>%
  arrange(ctry, date) %>%
  mutate(date = as.Date(paste0(substring(date, 1, 5), "12-01"))) %>%
  mutate(value = value / 100)

# Generate monthly series by linear approximation of annual data
temp <- actual %>%
  group_by(ctry) %>%
  mutate(temp_from = date,
         temp_to = lead(date, 1) - 1,
         temp_to = case_when(is.na(temp_to) ~ temp_from,
                             TRUE ~ temp_to)) %>%
  ungroup()

actual <- temp %>%
  # Create nested monthly series
  mutate(date = purrr::map2(temp$temp_from, temp$temp_to, function(x, y) {seq.Date(from = as.Date(x), to = as.Date(y), by = "month")})) %>%
  select(-temp_from, -temp_to, -value) %>%
  unnest_longer(col = "date", values_to = "date") %>%
  left_join(actual, by = c("date", "ctry")) %>%
  # Linear approximation
  group_by(ctry) %>%
  mutate(value = na.approx(value, na.rm = FALSE)) %>%
  ungroup() %>%
  mutate(grp = "actual")


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

# Institution names
download.file("https://github.com/franzmohr/macroprojections/raw/master/data/institutions.csv", destfile = temp)
inst <- read.csv(temp) %>%
  arrange(institution)

# Download forecast
download.file("https://github.com/franzmohr/macroprojections/raw/master/data/forecasts.csv", destfile = temp)
fcst <- read.csv(temp) %>%
  filter(ctry %in% pull(geo, "ctry"),
         variable == "NGDP_RPCH") %>%
  #endsWith(pubdate, "12-01")) %>%
  left_join(geo, by = "ctry") %>%
  select(year, code, variable, value, institution, pubdate) %>%
  filter(code %in% ctry) %>%
  rename(ctry = code) %>%
  mutate(date = as.Date(paste0(year, "-12-01")),
         pubdate = as.Date(pubdate),
         pubdate = floor_date(pubdate, "month"),
         value = value / 100) %>%
  select(date, ctry, value, institution, pubdate)

# Interpolate for monthly data
fcst_start <- fcst %>%
  # Get publication date for each institution
  select(ctry, institution, pubdate) %>%
  distinct() %>%
  arrange(ctry, pubdate) %>%
  # Add actual value for the month of the publication date
  left_join(actual, by = c("pubdate" = "date", "ctry")) %>%
  filter(!is.na(value)) %>%
  mutate(date = pubdate)

fcst_delete <- fcst_start %>%
  select(date, ctry, institution, pubdate) %>%
  mutate(del = "true")

fcst <- fcst %>%
  left_join(fcst_delete, by = c("date", "ctry", "institution", "pubdate")) %>%
  filter(is.na(del)) %>%
  select(-del) %>%
  bind_rows(fcst_start) %>%
  arrange(date) %>%
  mutate(grp = paste0(institution, "_", pubdate))

# Combine actual and forecast data
result <- bind_rows(actual, fcst) %>%
  mutate(name = ifelse(is.na(institution), "actual", institution),
         name = factor(name, levels = c("actual", pull(inst, "institution")),
                       labels = c(temp_actual, pull(inst, "institution_name")))) %>%
  mutate(date = ceiling_date(date, "month") - 1)


g <- ggplot(result, aes(x = date, y = value, colour = name, group = grp)) +
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

g

ggsave(g, filename = "figures/gdp_growth_medusa.png", height = 5, width = 10)
