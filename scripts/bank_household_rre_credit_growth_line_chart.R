# bank, macro, credit, growth, household, rre

rm(list = ls())

# Choose language
lang <- "en"

# Countries that should be highlighted
hlt <- c("AT", "EA")

library(dplyr)
library(eurostat)
library(ecb)
library(ggplot2)
library(tidyr)
library(zoo)

# Get list of EA countries
ea_ctry <- eu_countries$code

if (lang == "de") {
  temp_title <- "Kredite an private Haushalte (Wohnbau)"
  temp_subtitle <- "Bereinigtes Jahreswachstum in Prozent"
  temp_caption <- "Quelle: EZB-BSI. Unkonsolidiert. Gegenparteien im Euroraum."
  temp_other <- "Andere"
}
if (lang == "en") {
  temp_title <- "Credit to private households (house purchase)"
  temp_subtitle <- "Adjusted annual growth rate"
  temp_caption <- "Source: ECB-BSI. Consolidated. Counterparties in euro area."
  temp_other <- "Other"
}

# Download data
result <- get_data("BSI.M..N.A.A22.A.I.U2.2250.Z01.A") %>%
  select(obstime, ref_area, obsvalue) %>%
  rename(date = obstime,
         geo = ref_area,
         value = obsvalue) %>%
  filter(geo %in% c(ea_ctry, "U2")) %>%
  mutate(date = as.Date(as.yearmon(date, "%Y-%m")),
         geo = case_when(geo == "U2" ~ "EA",
                         TRUE ~ geo),
         name = ifelse(geo %in% hlt, geo, "other"),
         name = factor(name, levels = c(hlt, "other"),
                       labels = c(hlt, temp_other)),
         alph = geo %in% hlt,
         value = value / 100) %>%
  filter(date >= "2010-01-01")

g <- ggplot(result, aes(x = date, y = value, group = geo, colour = name)) +
  geom_hline(yintercept = 0) +
  geom_line(aes(alpha = alph)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(.01, 0)) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_alpha_manual(values = c(.3, 1)) +
  guides(alpha = "none") +
  labs(title = temp_title,
       subtitle = temp_subtitle,
       caption = temp_caption) +
  theme(axis.title = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())

g

ggsave(g, filename = "figures/bank_household_rre_credit_growth_line_chart.png", height = 4, width = 11)

