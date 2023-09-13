# macro, credit, rre

# Choose language
lang <- "de" # "de" or "en"

# Load packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(zoo)

if (lang == "de") {
  temp_var_labels <- c("MFIs", "Staat", "Sonstige Finanzunternehmen",
                       "Versicherungen u.\nPensionskassen", "Nichtfinanzielle\nUnternehmen", "Private\nHaushalte")
  temp_name_labels <- c("Bestand in Mrd EUR", "Bestandswachstum in %; Beitrag in Prozentpunkten")
  temp_title <- "Kredite nach Gegenparteisektor"
  temp_caption <- "Quelle: EZB-BSI. Gegenparteien im Euroraum."
}
if (lang == "en") {
  temp_var_labels <- c("MFIs", "Gen. Governmnet", "Other financial\ncorporations",
                       "Insurance and\npension funds", "Non-financial\ncorporations", "Householdes")
  temp_name_labels <- c("Outstanding amount in bn EUR", "Annual growth in percent; Contribution in percentage points")
  temp_title <- "Loans by counterparty sector"
  temp_caption <- "Source: ECB-BSI. Counterparties within euro area."
}

temp <- ecb::get_data("BSI.M.AT.N.A.A20.A.1.U2.1000+2100+2210+2220+2240+2250.Z01.E") %>%
  filter(obstime >= "2018-01") %>%
  select(obstime, count_area, bs_count_sector, obsvalue) %>%
  pivot_wider(names_from = "bs_count_sector", values_from = "obsvalue") %>%
  mutate(date = as.Date(as.yearmon(obstime, "%Y-%m"))) %>%
  select(-obstime, -count_area) %>%
  pivot_longer(cols = -c("date"), names_to = "var", values_to = "stock") %>%
  arrange(date) %>%
  mutate(stock = stock  / 1000) %>% # Mrd EUR
  group_by(date) %>%
  mutate(tot = sum(stock)) %>%
  group_by(var) %>%
  mutate(chg = stock - lag(stock, 12),
         growth = chg / lag(tot, 12) * 100) %>%
  ungroup() %>%
  filter(!is.na(growth)) %>%
  select(date, var, stock, growth) %>%
  pivot_longer(cols = c("stock", "growth")) %>%
  mutate(var = factor(var, levels = c("1000", "2100", "2210", "2220", "2240", "2250"),
                      labels = temp_var_labels),
         name = factor(name, levels = c("stock", "growth"),
                       labels = temp_name_labels))

g <- ggplot(temp, aes(x = date, y = value, fill = var)) +
  geom_col() +
  facet_wrap(~name, scales = "free_y") +
  labs(title = temp_title,
       caption = temp_caption) +
  theme(axis.title.x = element_blank())

g
