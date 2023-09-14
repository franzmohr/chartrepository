# inflation, macro

rm(list = ls())

library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)
library(zoo)

# Weights
weights <- get_eurostat(id = "prc_hicp_inw",
                        filters = list(geo = "AT",
                                       coicop = c("FOOD", "NRG", "IGD_NNRG", "SERV")))

weights <- weights %>%
  mutate(year = substring(time, 1, 4)) %>%
  select(-time) %>%
  rename(weight = values)

# Growth prc_hicp_manr
index <- get_eurostat(id = "prc_hicp_manr",
                      filters = list(geo = "AT",
                                     coicop = c("CP00", "TOT_X_NRG_FOOD", "FOOD", "NRG", "IGD_NNRG", "SERV"))) %>%
  mutate(time = as.Date(paste0(as.character(time), "-01"))) %>%
  filter(time >= "2019-01-01",
         !is.na(values)) %>%
  mutate(values = values / 100)

comp <- index %>%
  filter(!coicop %in% c("CP00", "TOT_X_NRG_FOOD")) %>%
  mutate(year = substring(time, 1, 4)) %>%
  left_join(weights, by = c("year", "geo", "coicop")) %>%
  group_by(time, geo) %>%
  mutate(weight = weight / sum(weight),
         values = values * weight) %>%
  ungroup() %>%
  mutate(var_de = factor(coicop, levels = c("SERV", "IGD_NNRG", "NRG", "FOOD"),
                         labels = c("Dienstleistungen", "Industriegüter ohne Energie",
                                    "Energie", "Nahrungsmittel")),
         var_en = factor(coicop, levels = c("SERV", "IGD_NNRG", "NRG", "FOOD"),
                         labels = c("Services", "Non-energy industrial goods",
                                    "Energy", "Food including alcohol and tobacco")))

line <- index %>%
  filter(coicop %in% c("CP00", "TOT_X_NRG_FOOD"),
         !is.na(values)) %>%
  mutate(line_de = factor(coicop, levels = c("CP00", "TOT_X_NRG_FOOD"),
                          labels = c("HVPI-Inflation", "Kerninflation (ohne Energie, Nahrungsmittel)")),
         line_en = factor(coicop, levels = c("CP00", "TOT_X_NRG_FOOD"),
                          labels = c("HCPI-inflation", "Core inflation (w/o energy, food)")))

g <- ggplot(comp, aes(x = time, y = values)) +
  geom_col(aes(fill = var_de), alpha = 1) +
  geom_line(data = line, aes(linetype = line_de), linewidth = 1.2) +
  scale_x_date(expand = c(.01, 0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  guides(fill = guide_legend(ncol = 2),
         linetype = guide_legend(ncol = 2, keywidth = 1.5)) +
  labs(title = "Beitrag zur Inflation (Österreich)", 
       subtitle = "Inflationsraten in %; Beiträge der Komponenten in Prozentpunkten",
       caption = "Quelle: Eurostat. Idee: OeNB (2020). Gesamtwirtschaftliche Prognose der OeNB für Österreich 2020 bis 2023.") +
  theme(legend.position="bottom",
        legend.box = "vertical",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

g

ggsave(g, filename = "figures/inflation_components_for_ea_countries.jpeg", height = 5, width = 7)
