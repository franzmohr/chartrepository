# bank, macro, gdp, assets

rm(list = ls())


# Choose language
lang <- "en"

# Choose years
yrs <- c("2008", "2015","2022")


library(dplyr)
library(ecb)
library(eurostat)
library(ggplot2)
library(lubridate)
library(tidyr)
library(zoo)

if (lang == "de") {
  temp_title <- "Bilanzsumme der Banken zu BIP"
  temp_caption <- "Quelle: Eurostat, EZB. Cut-Off bei 500%."
  temp_eu <- "EU"
}
if (lang == "en") {
  temp_title <- "Total bank assets to GDP"
  temp_caption <- "Source: ECB, Eurostat. Cut-Off at 500%."
  temp_eu <- "EU"
}


# Prepare nominal GDP data
gdp <- get_eurostat("NAMA_10_GDP", filters = list(na_item = "B1GQ",
                                                  unit = "CP_MEUR"),
                    cache = FALSE) %>%
  select(time, geo, values) %>%
  filter(!is.na(values)) %>%
  rename(date = time,
         ctry = geo,
         gdp = values) %>%
  # Filter for euro area counties
  #filter(ctry %in% c(ea_countries$code, "EA")) %>%
  # Filter for EU countries
  filter(ctry %in% c(eu_countries$code, "EU27_2020")) %>%
  mutate(ctry = case_when(ctry == "EL" ~ "GR",
                          ctry == "UK" ~ "GB",
                          ctry == "EU27_2020" ~ "EU",
                          TRUE ~ ctry),
         date = as.Date(paste0(date, "-01-01")))

# Prepare total asset data
assets <- bind_rows(get_data("CBD2.A..W0.67._Z._Z.A.A.A0000._X.ALL.CA._Z.LE._T.EUR"), # National data
                    get_data("CBD2.A.B0.W0.47._Z._Z.A.A.A0000._X.ALL.CA._Z.LE._T.EUR"), # EU data
                    get_data("CBD2.A.U2.W0.57._Z._Z.A.A.A0000._X.ALL.CA._Z.LE._T.EUR")) %>% # EA data
  select(obstime, ref_area, obsvalue) %>%
  filter(!is.na(obsvalue)) %>%
  rename(date = obstime,
         ctry = ref_area,
         assets = obsvalue) %>%
  mutate(assets = assets / 1000,
         date = as.Date(paste0(date, "-01-01")),
         ctry = case_when(ctry == "B0" ~ "EU",
                          ctry == "U2" ~ "EA",
                          TRUE ~ ctry))

# Combine results
result <- full_join(gdp, assets, by = c("date", "ctry")) %>%
  filter(!is.na(assets)) %>%
  mutate(date = substring(date, 1, 4),
         value = assets / gdp) %>%
  filter(!is.na(value),
         date %in% yrs) %>%
  select(date, ctry, value) %>%
  pivot_wider(names_from = "ctry", values_from = "value", values_fill = 0) %>%
  pivot_longer(cols = -c("date")) %>%
  mutate(name = reorder(name, -value, mean))

temp_line <- result %>%
  filter(date == max(date),
         name == "EU") %>%
  select(date, value)

result <- result %>%
  filter(name != "EU") %>%
  mutate(text = ifelse(date == max(date), paste(round(value * 100,1), "%", sep = ""), ""))

g <- ggplot(result, aes(x = name, y = value, fill = date)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  geom_hline(aes(yintercept = temp_line$value,
                 linetype = paste0(temp_eu, " (", temp_line$date, ")")),
             linewidth = .9) +
  # Label for line
  geom_text(label = paste(round(temp_line$value * 100, 1), "%", sep = ""), size = 2.5,
            fontface = "bold", y = temp_line$value, x = length(unique(result$name)), vjust = -.5, hjust = .8) +
  # Labels for bars
  geom_text(aes(label = text, y = 0), angle = 90, hjust = -.05, size = 2.5, fontface = "bold",
            colour = "black", position = position_dodge(width = .9)) +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_cartesian(ylim = c(0, 5), expand = FALSE) +
  labs(title = temp_title,
       caption = temp_caption) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_blank())

ggsave(g, filename = "figures/bank_assets_to_gdp_bar_chart.png", height = 4, width = 11)
