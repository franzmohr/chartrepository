# inflation, components

rm(list = ls())

library(dplyr)
library(eurostat)
library(tidyr)
library(ggplot2)

# Countries of interest
ctry <- c("AT", "EA20")
ctry_labels <- c("Österreich", "Euroraum (20)")

# Language
lang <- "de"

# Starting date of the plot
min_date_plot <- "2019-01-01"

# Country used to obtain the most important drivers of inflation
ctry_for_selection <- "AT"
# Starting period used to obtain the most important derivers of inflation
min_date_for_selection <- "2023-07-01"
max_date_for_selection <- "2024-02-01"

# Labels for plot
if (lang == "de") {
  temp_title <- "Beitrag von Komponenten der Kerninflation zur Gesamtinflation"
  temp_subtitle <- "Gewichtete Preisveränderung in Prozentpunkten"
  temp_caption <- "Quelle: Eurostat. Eigene Berechnungen. COICOP Einheiten wurden auf Basis der Summe der quadrierten gewichteten Preis-\nänderungen der jeweiligen Komponente zwischen Juli 2023 und Februar 2024 ausgewählt.\nCode unter https://github.com/franzmohr/chartrepository." 
}
if (lang == "en") {
  temp_title <- "Contribution of core inflation components to overall inflation"
  temp_subtitle <- "Weighted change of growth in percentage points"
  temp_caption <- "Source: Eurostat. Own calculations. COICOP items were selected based on the sum of squared weighted price changes\nof the respective component between July 2023 and February 2024.\nCode at https://github.com/franzmohr//chartrepository." 
}

# ******************************************************************************
# ******************************************************************************
# No manual input should be necessary from here on
# ******************************************************************************
# ******************************************************************************



# Main contributors to core inflation
# All items excluding energy food alcohol and tobacco

file_date <- format(Sys.Date(), "%Y%m%d") # For name of exported file
name_jpeg_file <- paste0("pics/", file_date, "_core_inflation_main_drivers_", lang,".jpeg")

# Weights
weights <- get_eurostat(id = "prc_hicp_inw",
                        filters = list(geo = ctry),
                        cache = FALSE) %>%
  mutate(year = substring(time, 1, 4)) %>%
  select(-time, -freq) %>%
  rename(weight = values) %>%
  mutate(weight = weight / 1000)

# Growth prc_hicp_manr
index <- get_eurostat(id = "prc_hicp_manr",
                      filters = list(geo = ctry),
                      cache = FALSE) %>%
  mutate(time = as.Date(paste0(time, "-01"))) %>%
  filter(time >= min_date_plot,
         !is.na(values)) %>%
  select(-freq)

# Mapping from
# https://ec.europa.eu/eurostat/documents/3859598/18594110/KS-GQ-24-003-EN-N.pdf/8490b532-f9e2-5b63-16fa-d88cda14c7b4?version=3.0&t=1709119338351

comp <- index %>%
  # Only consider "raw data"
  filter(substring(coicop, 1, 2) == "CP",# coicop == "TOT_X_NRG_FOOD",
         # Entirely omitted
         !substring(coicop, 1, 4) %in% c("CP02"),
         !substring(coicop, 1, 5) %in% c("CP011", "CP012", "CP045"),
         # w/o NRG (ELC_GAS, FUEL)
         !coicop %in% c("CP0451", "CP0452", "CP0454", "CP0455"), # ELC_GAS
         !coicop %in% c("CP0453", "CP072", "CP0722", "CP07221", "CP07222", "CP07223"), # FUEL
         # w/o FOOD (FOOD_P, FOOD_NP)
         # FOOD_P
         !coicop %in% c("CP0111",
                        "CP01127", "CP01128",
                        "CP01132", "CP01134", "CP01135", "CP01136",
                        "CP01141", "CP01142", "CP01143", "CP01144", "CP01145", "CP01146",
                        "CP0115",
                        "CP01162", "CP01163", "CP01164",
                        "CP01172", "CP01173", "CP01174", "CP01175", "CP01176",
                        "CP0118",
                        "CP0119",
                        "CP012",
                        "CP02"),
         # FOOD_NP
         !coicop %in% c("01121", "CP01122", "CP01123", "CP01124", "CP01125", "CP01126",
                        "CP01131", "CP01133",
                        "CP01147",
                        "CP01161",
                        "CP01171"),
         nchar(coicop) > 4) %>% # coicop == "TOT_X_NRG_FOOD") %>%
  #nchar(coicop) <= 6) %>%
  mutate(year = substring(time, 1, 4)) %>%
  left_join(weights, by = c("year", "geo", "coicop")) %>%
  mutate(values = values * weight) %>%
  mutate(cond = case_when(coicop == "TOT_X_NRG_FOOD" ~ TRUE,
                          nchar(coicop) == 5 & !coicop %in% c("CP041", "CP111", "CP125") ~ TRUE,
                          nchar(coicop) == 6 & substring(coicop, 1, 5) %in% c("CP041", "CP111", "CP125") ~ TRUE,
                          TRUE ~ FALSE)) %>%
  filter(cond) %>%
  select(-weight)

# Get indicators with highest contribution to inflation in period
top_comp <- comp %>%
  filter(time >= min_date_for_selection,
         time <= max_date_for_selection,
         geo == ctry_for_selection) %>%
  group_by(coicop) %>%
  summarise(value = sum(values^2),
            .groups = "drop") %>%
  arrange(desc(value)) %>%
  slice(1:12) %>%
  pull("coicop") %>%
  as.character()

# Helper file with mapping of COICOP code and its title
coicop <- read.csv("scripts/coicop_mapping.csv") %>%
  filter(coicop %in% top_comp) %>%
  mutate(coicop = factor(coicop, levels = top_comp)) %>%
  arrange(coicop) %>%
  as.data.frame()

if (lang == "de") {
  coicop[, "var"] <- gsub("\\n", "\n", coicop[, "var_de"], fixed = TRUE)
}
if (lang == "en") {
  coicop[, "var"] <- gsub("\\n", "\n", coicop[, "var_en"], fixed = TRUE)
}


coicop_levels <- pull(coicop, "coicop")
coicop_labels <- pull(coicop, "var")

# comp %>%
#   select(time, values, geo, coicop) %>%
#   pivot_wider(names_from = "coicop", values_from = "values") %>%
#   pivot_longer(cols = -c("time", "geo", "TOT_X_NRG_FOOD")) %>%
#   group_by(time, geo) %>%
#   summarise(value = sum(value, na.rm = TRUE),
#             tot = mean(TOT_X_NRG_FOOD),
#             .groups = "drop") %>%
#   tail()

temp <- comp %>%
  filter(coicop %in% top_comp) %>%
  mutate(coicop = factor(coicop, levels = coicop_levels, labels = coicop_labels),
         geo = factor(geo, levels = ctry, labels = ctry_labels))

show_legend <- length(ctry) > 1

ggplot(temp, aes(x = time, y = values)) +
  geom_hline(yintercept = 0) +
  geom_line(aes(colour = geo), alpha = 1, show.legend = show_legend) +
  facet_wrap(~coicop, nrow = 3) +
  labs(title = temp_title,
       subtitle = temp_subtitle,
       caption = temp_caption) +
  scale_x_date(expand = c(.01, 0), date_breaks = "1 year", date_labels = "%Y") +
  theme(strip.text = element_text(size = 6),
        axis.text = element_text(size = 6),
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 6)) +
  theme(legend.box = "vertical")
