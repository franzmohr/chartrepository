# ecsurvey, recreation

rm(list = ls())

# Country codes
#ctry_code <- c("AT", "EA", "DE")
#ctry_names <- c("Österreich", "Euroraum", "Deutschland")
#ctry_names <- c("Austria", "Euro area", "Germany")

# Language of the plot
lang <- "en"
# Don't forget to change the country names accordingly!

# Minimum data in plot
min_date <- "2015-01-01"


# ******************************************************************************
# From here on everything should work automatically

library(dplyr)
library(ggplot2)
library(lubridate)
library(readxl)
library(tidyr)
library(zoo)


# Download data ----

# Source: https://ec.europa.eu/info/business-economy-euro/indicators-statistics/economic-databases/business-and-consumer-surveys/download-business-and-consumer-survey-data/time-series_en


tfile <- tempfile(tmpdir = tdir <- tempdir())

# Try download for current month
curr_date <- Sys.Date()
curr_month <- month(curr_date)
curr_month <- ifelse(nchar(curr_month) == 1, paste0("0", curr_month), curr_month)
curr_year <- substring(year(curr_date), 3, 4)
curr_month <- paste0(curr_year, curr_month)
try(download.file(paste0("https://ec.europa.eu/economy_finance/db_indicators/surveys/documents/series/nace2_ecfin_", curr_month, "/services_subsectors_sa_nace2.zip"),
                  destfile = tfile))
sdmx_files <- unzip(tfile, exdir = tdir)

# Download failed try it with download of one month earlier
if (length(sdmx_files) == 0) {
  curr_date <- floor_date(Sys.Date(), "month") - 1
  curr_month <- month(curr_date)
  curr_month <- ifelse(nchar(curr_month) == 1, paste0("0", curr_month), curr_month)
  curr_year <- substring(year(curr_date), 3, 4)
  curr_month <- paste0(curr_year, curr_month)
  try(download.file(paste0("https://ec.europa.eu/economy_finance/db_indicators/surveys/documents/series/nace2_ecfin_", curr_month, "/services_subsectors_sa_nace2.zip"),
                    destfile = tfile))
  sdmx_files <- unzip(tfile, exdir = tdir)
}

# Prepare data ----

temp_sdmx_files <- sdmx_files[which(grepl("sa_m_nace", sdmx_files))]

var_levels <- c("COF", as.character(1:6))

ecdata <- readxl::read_xlsx(temp_sdmx_files, sheet = "93", na = "NA", col_types = c("date", rep("numeric", 245))) %>%
  rename(date = `93`) %>%
  pivot_longer(cols = -c("date")) %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.na(value)) %>%
  mutate(name = gsub("SERV.", "", name),
         ctry = substring(name, 1, 2),
         name = substring(name, 7, nchar(name)),
         name = gsub(".BS.M", "", name, fixed = TRUE)) %>%
  select(date, ctry, name, value) %>%
  #filter(ctry %in% ctry_code) %>%
  filter(date >= min_date)

last_value <- format(as.yearmon(max(ecdata$date)), "%YM%m")

if (lang == "de") {
  var_labels <- c("Allgemeine Stimmung",
                  "Entwicklung der Geschäftslage in den letzten 3 Monaten",
                  "Entwicklung der Nachfrage in den letzten 3 Monaten",
                  "Erwartete Nachfrage in den nächsten 3 Monaten",
                  "Entwicklung der Beschäftigung in den letzten 3 Monaten",
                  "Erwartete Beschäftigung in den nächsten 3 Monaten",
                  "Erwartete Preise in den nächsten 3 Monaten")
  temp_title <- "Sport und Unterhaltung (EK Umfrage)"
  temp_caption <- "Quelle: Europäische Kommission. Subsektor 93 (Sports activities and amusement and recreation activities). Saisonal bereinigte Daten."
}

if (lang == "en") {
  var_labels <- c("Overall confidence",
                  "Business situation development\nover the past 3 months",
                  "Evolution of the demand over\nthe past 3 months",
                  "Expectation of the demand over\nthe next 3 months",
                  "Evolution of the employment over\nthe past 3 months",
                  "Expectations of the employment\nover the next 3 months",
                  "Expectations of the prices over\nthe next 3 months")
  temp_title <- "Sports activities and amusement and recreation activities (EC survey)"
  temp_caption <- "Source: European Commission. Subsector 93 (Sports activities and amusement and recreation activities). Seasonally adjusted series."
}

ecdata <- ecdata %>%
  #mutate(ctry = factor(ctry, levels = ctry_code, labels = ctry_names)) %>%
  mutate(name = factor(name, levels = var_levels, labels = var_labels))

g <- ggplot(ecdata, aes(x = date, y = value)) +
  geom_hline(yintercept = 0) +
  geom_line(aes(colour = ctry)) +
  scale_x_date(expand = c(.01, 0), date_labels = "%Y", date_breaks = "1 year") +
  facet_wrap(~ name, ncol = 3) +
  labs(title = temp_title,
       caption = temp_caption) +
  theme(strip.text = element_text(size = 6),
        axis.text = element_text(size = 6),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 6),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_blank())

g

ggsave(g, filename = "figures/ecsurvey_93_services_recreation_main.png", height = 7, width = 7)
