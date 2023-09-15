# macro, credit, rre

rm(list = ls())

# Choose language
lang <- "en" # "de" or "en"

# Choose country
ctry <- "AT"

# Load packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(zoo)


if (lang == "de") {
  temp_name_labels <- c("Unternehmenskredite", "Haushalte: Wohnbau", "Haushalte: Konsum", "Haushalte: Sonstige")
  temp_y <- "Neukredite in Mrd EUR"
  temp_caption <- "Quelle: EZB-MIR. Reines Neugeschäft."
}
if (lang == "en") {
  temp_name_labels <- c("Non-financial corporates", "Household: House purchase", "Household: Consumption", "Household: Other")
  temp_y <- "New loans in bn EUR"
  temp_caption <- "Source: ECB-MIR. Pure new loans."
}

temp <- bind_rows(ecb::get_data(paste0("MIR.M.", ctry, ".B.A2B+A2C+A2D.A.B.A.2250.EUR.P")),
                  ecb::get_data(paste0("MIR.M.", ctry, ".B.A2A.A.B.A.2240.EUR.P"))) %>%
  select(obstime, bs_item, obsvalue) %>%
  mutate(date = as.Date(as.yearmon(obstime, "%Y-%m")),
         value = obsvalue / 1000,
         name = factor(bs_item, levels = c("A2A", "A2C", "A2B", "A2D"),
                       labels = temp_name_labels)) %>%
  filter(date >= "2017-10-01")

g <- ggplot(temp, aes(x = date, y = value, fill = name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~name, nrow = 1) +
  labs(y = temp_y,
       caption = temp_caption) +
  theme(axis.title.x = element_blank())

g

ggsave(g, filename = "figures/credit_new_by_sector.png", width = 7)
