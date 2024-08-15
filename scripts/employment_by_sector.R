# macro, employment

rm(list = ls())

# Choose language
lang <- "de"

# Choose countries
ctry_eurostat <- "AT"
ctry <- "Österreich"

library(dplyr)
library(eurostat)
library(ggplot2)
library(lubridate)
library(tidyr)
library(zoo)


sectors <- c("A", "B-E", "F", "G-I", "J", "K", "L", "M_N", "O-Q", "R-U")

if (lang == "de") {
  temp_title <- paste0("Beschäftigung nach Sektor (", ctry, ")")
  temp_subtitle <- "Tsd. Personen"
  temp_caption <- "Quelle: Eurostat. Saison- und kalenderbereinigte Daten."
  sector_labels <- c("Land- und Forstwirtschaft,\nFischerei",
                     "Industrie\n(ohne Baugewerbe)",
                     "Baugewerbe/Bau",
                     "Handel, Instandhaltung,\nVerkehr, Gastgewerbe\nund Gastronomie",
                     "Information und\nKommunikation",
                     "Erbringung von Finanz-\nund Versicherungs-\ndienstleistungen",
                     "Grundstücks- und\nWohnungswesen",
                     "Erbringung freiberuflicher\nund sonstiger wirtschaft-\nlicher Dienstleistungen",
                     "Öffentliche Verwaltung,\nVerteidigung, Erziehung,\nGesundheits-/Sozialwesen",
                     "Kunst/Unterhaltung/Erholung,\nsonstige Dienstleistungen,\nprivate Haushalte,\nexterritoriale Organisationen")
}
if (lang == "en") {
  temp_title <- paste0("Employment by sector (", ctry, ")")
  temp_subtitle <- "Thousand persons"
  temp_caption <- "Source: Eurostat. Seasonally and calendar adjusted data."
  sector_labels <- c("Agriculture, forestry and fishing",
                     "Industry (except construction)",
                     "Construction",
                     "Wholesale and retail trade, transport, accommodation and food service activities",
                     "Information and communication",
                     "Financial and insurance activities",
                     "Real estate activities", 
                     "Professional, scientific and technical activities; administrative and support service activities",
                     "Public administration, defence, education, human health and social work activities",
                     "Arts, entertainment and recreation; other service activities; activities of household and extra-territorial organizations and bodies")
}

temp <- get_eurostat(id = "namq_10_a10_e",
                     filters = list(geo = ctry_eurostat,
                                    na_item = "EMP_DC",
                                    s_adj = "SCA",
                                    unit = "THS_PER"),
                     cache = FALSE) %>%
  filter(!is.na(values),
         !nace_r2 %in% c("TOTAL", "C")) %>%
  rename(date = time,
         ctry = geo,
         sctr = nace_r2,
         value = values) %>%
  select(date, ctry, sctr, value) %>%
  mutate(sctr = factor(sctr, levels = sectors, labels = sector_labels))

g <- ggplot(temp, aes(x = date, y = value, fill = ctry)) +
  geom_area(show.legend = FALSE) +
  facet_wrap(~ sctr) +
  labs(title = temp_title,
       subtitle = temp_subtitle,
       caption = temp_caption) +
  theme(axis.title = element_blank())


ggsave(g, filename = "figures/employment_by_sector.png", height = 8, width = 8)
