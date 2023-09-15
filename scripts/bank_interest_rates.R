# bank, interest, credit, deposit, rre

rm(list = ls())

library(ecb)
library(dplyr)
library(ggplot2)
library(tidyr)
library(zoo)

# Choose country
ctry <- "AT"

# Choose language
lang <- "en"

# Labels for different languages
if (lang == "de") {
  temp_credit_grp <- "Zinsen auf Neukredite"
  temp_credit_var_labels <- c("Zinsen auf neue Wohnbaukredite", "Zinsen auf neue Konsumkredite", "Zinsen auf sonstige Neukredite")
  temp_credit_mat_labels <- c("Bis 1J", "Zw. 1J u. 5J", "Über 5J", "Zw. 5J u. 10J", "Über 10J")
  
  temp_dep_grp <- "Zinsen auf neue Einlagen"
  temp_dep_var_labels <- c("Sichteinlagen", "Gebundene Einlagen (bis 1J)",#
                           "Gebundene Einlagen (1J-2J)", "Gebundene Einlagen (mehr als 2J)")
  
  temp_title <- paste0("Zinsen auf neue Kredite bzw. Einlagen von Haushalten (", ctry, ")")
  temp_caption <- "Quelle: EZB-MIR."
}

if (lang == "en") {
  temp_credit_grp <- "Interest rates on new loans"
  temp_credit_var_labels <- c("Housing purchase", "Consumer credit", "Other")
  temp_credit_mat_labels <- c("Up to 1Y", "1Y to 5Y", "Above 5Y", "5Y to 10Y", "Above 10Y")
  
  temp_dep_grp <- "Interest rates on new deposits"
  temp_dep_var_labels <- c("Overnight", "Up to 1Y",
                           "1Y to 2Y", "Above 2Y")
  
  temp_title <- paste0("Interest rates on new credit and deposits of households (", ctry, ")")
  temp_caption <- "Source: ECB-MIR."
}

start_date <- "2015-01-01"

# Households ----

hh_credit_b <- get_data(paste0("MIR.M.", ctry, ".B.A2B.F+I+J.R.A.2250.EUR.N"))
hh_credit_c <- get_data(paste0("MIR.M.", ctry, ".B.A2C.F+I+O+P.R.A.2250.EUR.N"))
hh_credit_d <- get_data(paste0("MIR.M.", ctry, ".B.A2D.F+I+J.R.A.2250.EUR.N"))

hh_deposit_overnight <- get_data(paste0("MIR.M.", ctry, ".B.L21..R.A.2250.EUR.N"))
hh_deposit_agreed <- get_data(paste0("MIR.M.", ctry, ".B.L22.F+G+H.R.A.2250.EUR.N"))

hh_credit <- bind_rows(hh_credit_b, hh_credit_c, hh_credit_d) %>%
  rename(date = obstime,
         var = bs_item,
         mat = maturity_not_irate,
         sec = bs_count_sector,
         value = obsvalue) %>%
  select(date, var, mat, sec, value) %>%
  mutate(value = value / 100,
         date = as.Date(as.yearmon(date, "%Y-%m")),
         grp = temp_credit_grp,
         var = factor(var,
                      levels = c("A2C", "A2B", "A2D"),
                      labels = temp_credit_var_labels),
         mat = factor(mat,
                      levels = c("F", "I", "J", "O", "P"),
                      labels = temp_credit_mat_labels)) %>%
  filter(date >= start_date)

hh_deposit <- bind_rows(hh_deposit_overnight, hh_deposit_agreed) %>%
  rename(date = obstime,
         var = bs_item,
         mat = maturity_not_irate,
         sec = bs_count_sector,
         value = obsvalue) %>%
  select(date, var, mat, sec, value) %>%
  mutate(value = value / 100,
         date = as.Date(as.yearmon(date, "%Y-%m")),
         grp = temp_dep_grp,
         var = paste0(var, "_", mat),
         var = factor(var, levels = c("L21_A", "L22_F", "L22_G", "L22_H"),
                      labels = temp_dep_var_labels)) %>%
  filter(date >= start_date)

# Needed to align graphs
max_rate <- bind_rows(hh_credit, hh_deposit) %>%
  pull("value") %>%
  max()

g_credit <- ggplot(hh_credit, aes(x = date, y = value)) +
  geom_line(aes(colour = mat)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  scale_colour_brewer(palette = "Set1") +
  facet_wrap(~var) +
  guides(colour = guide_legend(nrow = 1, title = "Laufzeit")) +
  coord_cartesian(ylim = c(0, max_rate * 1.06), expand = FALSE) +
  labs(title = temp_title) +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

g_deposit <- ggplot(hh_deposit, aes(x = date, y = value)) +
  geom_line(aes(colour = var)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  facet_wrap(~grp) +
  guides(colour = guide_legend(ncol = 1, title = "")) +
  coord_cartesian(ylim = c(0, max_rate * 1.06), expand = FALSE) +
  labs(caption = temp_caption) +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

g <- cowplot::plot_grid(g_credit, g_deposit,
                        ncol = 2, align = "h",
                        rel_widths = c(3, 1))

g

ggsave(g, filename = "figures/bank_interest_rate.png", height = 3.3, width = 11)
