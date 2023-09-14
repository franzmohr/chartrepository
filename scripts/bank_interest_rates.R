# bank, interest, credit, deposit

# Notes:
# - Requires translation into English
# - Add functionality to choose country

rm(list = ls())

# Präambel ----
library(ecb)
library(dplyr)
library(ggplot2)
library(tidyr)
library(zoo)

start_date <- "2015-01-01"

# Households ----

hh_credit_b <- get_data("MIR.M.AT.B.A2B.F+I+J.R.A.2250.EUR.N")
hh_credit_c <- get_data("MIR.M.AT.B.A2C.F+I+O+P.R.A.2250.EUR.N")
hh_credit_d <- get_data("MIR.M.AT.B.A2D.F+I+J.R.A.2250.EUR.N")

hh_deposit_overnight <- get_data("MIR.M.AT.B.L21..R.A.2250.EUR.N")
hh_deposit_agreed <- get_data("MIR.M.AT.B.L22.F+G+H.R.A.2250.EUR.N")


hh_credit <- bind_rows(hh_credit_b, hh_credit_c, hh_credit_d) %>%
  rename(date = obstime,
         var = bs_item,
         mat = maturity_not_irate,
         sec = bs_count_sector,
         value = obsvalue) %>%
  select(date, var, mat, sec, value) %>%
  mutate(value = value / 100,
         date = as.Date(as.yearmon(date, "%Y-%m")),
         grp = "Zinsen auf Neukredite",
         var = factor(var,
                      levels = c("A2C", "A2B", "A2D"),
                      labels = c("Zinsen auf neue Wohnbaukredite", "Zinsen auf neue Konsumkredite", "Zinsen auf sonstige Neukredite")),
         mat = factor(mat,
                      levels = c("F", "I", "J", "O", "P"),
                      labels = c("Bis 1J", "Zw. 1J u. 5J", "Über 5J", "Zw. 5J u. 10J", "Über 10J"))) %>%
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
         grp = "Zinsen auf neue Einlagen",
         var = paste0(var, "_", mat),
         var = factor(var, levels = c("L21_A", "L22_F", "L22_G", "L22_H"),
                      labels = c("Sichteinlagen", "Gebundene Einlagen (bis 1J)",
                                 "Gebundene Einlagen (1J-2J)", "Gebundene Einlagen (mehr als 2J)"))) %>%
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
  labs(title = "Zinsen auf neue Kredite bzw. Einlagen von Haushalten (Österreich)") +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

g_deposit <- ggplot(hh_deposit, aes(x = date, y = value)) +
  geom_line(aes(colour = var)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  facet_wrap(~grp) +
  guides(colour = guide_legend(ncol = 1, title = "")) +
  coord_cartesian(ylim = c(0, max_rate * 1.06), expand = FALSE) +
  labs(caption = "Quelle: EZB-MIR (https://sdw.ecb.europa.eu/browse.do?node=1513).") +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

g <- cowplot::plot_grid(g_credit, g_deposit,
                        ncol = 2, align = "h",
                        rel_widths = c(3, 1))

g

ggsave(g, filename = "figures/bank_interest_rate.png", height = 3.3, width = 11)
