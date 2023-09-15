# inflation, macro

rm(list = ls())

# Choose language
lang <- "en"

# Choose country
ctry_code <- "AT"

library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)
library(zoo)


if (lang == "de") {
  temp_var_labels <- c("Dienstleistungen", "Industriegüter ohne Energie",
                       "Energie", "Nahrungsmittel")
  temp_line_labels <- c("HVPI-Inflation", "Kerninflation (ohne Energie, Nahrungsmittel)")
  temp_title <- paste0("Beitrag zur Inflation (", ctry_code , ")")
  temp_subtitle <- "Inflationsraten in %; Beiträge der Komponenten in Prozentpunkten"
  temp_caption <- "Quelle: Eurostat. Idee: OeNB (2020). Gesamtwirtschaftliche Prognose der OeNB für Österreich 2020 bis 2023."
}
if (lang == "en") {
  temp_var_labels <- c("Services", "Non-energy industrial goods",
                       "Energy", "Food including alcohol and tobacco")
  temp_line_labels <- c("HCPI-inflation", "Core inflation (w/o energy, food)")
  temp_title <- paste0("Contribution to inflation (", ctry_code, ")")
  temp_subtitle <- "Inflation in %; Contribution of component in percentage points"
  temp_caption <- "Source: Eurostat. Idea: OeNB (2020). Gesamtwirtschaftliche Prognose der OeNB für Österreich 2020 bis 2023."
}

# Weights
weights <- get_eurostat(id = "prc_hicp_inw",
                        filters = list(geo = ctry_code,
                                       coicop = c("FOOD", "NRG", "IGD_NNRG", "SERV")),
                        cache = FALSE) %>%
  mutate(year = substring(time, 1, 4)) %>%
  select(-time) %>%
  rename(weight = values)

# Growth prc_hicp_manr
index <- get_eurostat(id = "prc_hicp_manr",
                      filters = list(geo = ctry_code,
                                     coicop = c("CP00", "TOT_X_NRG_FOOD", "FOOD", "NRG", "IGD_NNRG", "SERV")),
                      cache = FALSE) %>%
  mutate(time = as.Date(paste0(as.character(time), "-01"))) %>%
  filter(time >= "2019-01-01",
         !is.na(values)) %>%
  mutate(values = values / 100)

comp <- index %>%
  filter(!coicop %in% c("CP00", "TOT_X_NRG_FOOD")) %>%
  mutate(year = substring(time, 1, 4)) %>%
  select(time, values, year, geo, coicop) %>%
  left_join(weights, by = c("year", "geo", "coicop")) %>%
  group_by(time, geo) %>%
  mutate(weight = weight / sum(weight),
         values = values * weight) %>%
  ungroup() %>%
  mutate(var = factor(coicop, levels = c("SERV", "IGD_NNRG", "NRG", "FOOD"),
                      labels = temp_var_labels))

line <- index %>%
  filter(coicop %in% c("CP00", "TOT_X_NRG_FOOD"),
         !is.na(values)) %>%
  mutate(line = factor(coicop, levels = c("CP00", "TOT_X_NRG_FOOD"),
                          labels = temp_line_labels))

g <- ggplot(comp, aes(x = time, y = values)) +
  geom_col(aes(fill = var), alpha = 1) +
  geom_line(data = line, aes(linetype = line), linewidth = 1.2) +
  scale_x_date(expand = c(.01, 0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  guides(fill = guide_legend(ncol = 2),
         linetype = guide_legend(ncol = 2, keywidth = 1.5)) +
  labs(title = temp_title, 
       subtitle = temp_subtitle,
       caption = temp_caption) +
  theme(legend.position="bottom",
        legend.box = "vertical",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

g

ggsave(g, filename = "figures/inflation_components_for_ea_countries.png", height = 5, width = 7)
