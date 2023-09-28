# bank, macro, credit, growth, nfc, household, rre

rm(list = ls())

# Choose language
lang <- "en"

# Choose countries
ctry <- c("AT", "EA")
ctry_ecb <- c("AT", "U2") # Used for data download

library(dplyr)
library(eurostat)
library(ecb)
library(ggplot2)
library(tidyr)
library(zoo)

# Get list of EA countries
ea_ctry <- eu_countries$code

if (lang == "de") {
  temp_title <- "Kreditwachstum nach Gegenparteisektor"
  temp_subtitle <- "Bereinigtes Jahreswachstum in Prozent"
  temp_caption <- "Quelle: EZB-BSI. Unkonsolidiert. Gegenparteien im Euroraum."
  temp_labels <- c("Nicht-finanzielle Unternehmen", "Haushalt (gesamt)", "Haushalt (Konsum)", "Haushalt (Wohnbau)", "Haushalt (Sonstige)")
}
if (lang == "en") {
  temp_title <- "Credit growth by counterparty sector"
  temp_subtitle <- "Adjusted annual growth rate"
  temp_caption <- "Source: ECB-BSI. Consolidated. Counterparties in euro area."
  temp_labels <- c("Non-financial corporates", "Households (total)", "Haushalt (consumption)", "Haushalt (house purchase)", "Haushalt (other)")
}

# Download data
result <- get_data(paste0("BSI.M.", paste0(ctry_ecb, collapse = "+"),".N.A.A20+A21+A22+A23.A.I.U2.2240+2250.Z01.A")) %>%
  select(obstime, ref_area, bs_item, bs_count_sector, obsvalue) %>%
  mutate(var = paste0(bs_item, "_", bs_count_sector)) %>%
  rename(date = obstime,
         geo = ref_area,
         value = obsvalue) %>%
  mutate(date = as.Date(as.yearmon(date, "%Y-%m")),
         geo = case_when(geo == "U2" ~ "EA",
                         TRUE ~ geo),
         var = factor(var, levels = c("A20_2240", "A20_2250", "A21_2250", "A22_2250", "A23_2250"),
                      labels = temp_labels),
         value = value / 100) %>%
  filter(date >= "2009-01-01")

g <- ggplot(result, aes(x = date, y = value, colour = var)) +
  geom_hline(yintercept = 0) +
  geom_line() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(.01, 0)) +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~geo) +
  labs(title = temp_title,
       subtitle = temp_subtitle,
       caption = temp_caption) +
  theme(axis.title = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())

g

ggsave(g, filename = "figures/bank_credit_by_sector_line_chart.png", height = 4, width = 11)

