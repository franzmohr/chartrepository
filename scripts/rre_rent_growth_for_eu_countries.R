# rre, rent, inflation

rm(list = ls())

# Choose countries
ctry_code <- c("AT", "DE", "ES", "FR", "EU27_2020")

# Choose language
lang <- "en"



library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)
library(zoo)



if (lang == "de") {
  ctry_labels <- c("Österreich", "Deutschland", "Spanien", "Frankreich", "EU (27)")
  temp_title <- "Wohnungsmieten"
  temp_subtitle <- "Jahreswachstum in Prozent"
  temp_caption <- "Quelle: Eurostat."
}
if (lang == "en") {
  ctry_labels <- c("Austria", "Germany", "Spain", "France", "EU (27)")
  temp_title <- "Actual rentals for housing"
  temp_subtitle <- "Annual growth in %"
  temp_caption <- "Source: Eurostat."
}

rent <- get_eurostat(id = "prc_hicp_manr",
                              filter = list(geo = ctry_code,
                                            coicop = "CP041"),
                              cache = FALSE) %>%
  mutate(date = as.Date(as.yearmon(as.character(time), "%Y-%m"))) %>%
  arrange(date) %>%
  filter(date >= "2015-01-01") %>%
  filter(!is.na(values)) %>%
  mutate(geo = factor(geo, levels = ctry_code, labels = ctry_labels),
         values = values / 100)

g <- ggplot(rent, aes(x = date, y = values, colour = geo)) +
  geom_hline(yintercept = 0, colour = "black") +
  geom_line(linewidth = 1.2) +
  scale_x_date(expand = c(.01, 0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  guides(colour = guide_legend(ncol = 2)) +
  labs(title = temp_title,
       subtitle = temp_subtitle,
       caption = temp_caption) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_blank())

g

ggsave(g, filename = "figures/rre_rent_growth_for_eu_countries.png", height = 5, width = 5)

