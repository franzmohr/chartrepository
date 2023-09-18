# rre, price, inflation

rm(list = ls())

# Choose language
lang <- "en"

# Countries, which should be highlighed
ctry <- c("AT", "DE")

library(dplyr)
library(ecb)
library(ggplot2)
library(tidyr)
library(zoo)


if (lang == "de") {
  temp_other <- "Andere"
  temp_title <- "Immobilienpreiswachstum"
  temp_subtitle <- "Jahreswachstum in Prozent"
  temp_caption <- "Quelle: EK, EZB-RESR."
}
if (lang == "en") {
  temp_other <- "Other"
  temp_title <- "RRE price growth"
  temp_subtitle <- "Annual growth in %"
  temp_caption <- "Source: EC, ECB-RESR."
}


# Load raw data via ECB from EC
temp <- get_data("RESR.Q.._T.N._TR.TVAL.4D0.TB.N.IX") %>%
  mutate(date = as.Date(as.yearqtr(obstime, "%Y-Q%q"))) %>%
  rename(value = obsvalue,
         name = ref_area) %>%
  select(date, name, value) %>%
  arrange(name, date) %>%
  group_by(name) %>%
  mutate(growth = value / lag(value, 4) - 1) %>%
  ungroup() %>%
  filter(!is.na(growth),
         date >= "2015-01-01",
         !name %in% c("B0", "B5", "U2")) %>%
  mutate(hlt = ifelse(name %in% ctry, name, temp_other),
         hlt = factor(hlt, levels = c(ctry, temp_other)))

g <- ggplot(temp, aes(x = date, y = growth, group = name)) +
  geom_hline(yintercept = 0, colour = "black") +
  geom_line(linewidth = 1.2, aes(colour = hlt, alpha = hlt)) +
  scale_x_date(expand = c(.01, 0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_alpha_manual(values = c(1, 1, .2)) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = temp_title,
       subtitle = temp_subtitle,
       caption = temp_caption) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_blank())

g

ggsave(g, filename = "figures/rre_price_growth_for_eu_countries.png", height = 5, width = 5)

