# macro, interest

rm(list = ls())

library(alfred)
library(dplyr)
library(ecb)
library(ggplot2)
library(tidyr)
library(zoo)

lang <- "en"

temp_var_levels <- c("EA", "US", "GB")
if (lang == "de") {
  temp_var_labels <- c("EZB Hauptrefinanzierungssatz", "Fed Target Rate (Mittel)", "BoE Official Bank Rate")
  temp_title <- "Leitzinssätze"
  temp_subtitle <- "Prozent"
  temp_caption <- "Quelle: BoE, Fed, EZB."
}
if (lang == "en") {
  temp_var_labels <- c("ECB main refinancing rate", "Fed mean target rate", "BoE official bank rate")
  temp_title <- "Central bank policy rates"
  temp_subtitle <- "Percent"
  temp_caption <- "Source: BoE, Fed, EZB."
}

# Obtrain raw data ----

# Eurozone
interest_ea <- bind_rows(get_data("FM.B.U2.EUR.4F.KR.MRR_FR.LEV"),
                         get_data("FM.B.U2.EUR.4F.KR.MRR_MBR.LEV")) %>%
  mutate(obstime = as.Date(obstime)) %>%
  arrange(obstime) %>%
  rename(land_code = ref_area,
         value = obsvalue,
         date = obstime) %>%
  mutate(ctry = "EA") %>%
  filter(!is.na(value)) %>%
  select(date, ctry, value)

# US

us_lower <- try(get_fred_series("DFEDTARL", series_name = "value", observation_start = "1999-01-01"))

if (!inherits(us_lower, "try-error")) {
  
  us_upper <- try(get_fred_series("DFEDTARU", series_name = "value", observation_start = "1999-01-01"))
  
  if (!inherits(us_upper, "try-error")) {
    
    us_act <- try(get_fred_series("DFEDTAR", series_name = "value", observation_start = "1999-01-01"))
    
    if (!inherits(us_act, "try-error")) {
      interest_us <- bind_rows(us_lower, us_upper, us_act) %>%
        group_by(date) %>%
        summarise(value = mean(value),
                  .groups = "drop") %>%
        mutate(diff = c(NA, diff(value))) %>%
        filter(diff != 0 | is.na(diff)) %>%
        select(date, value) %>%
        mutate(ctry = "US") %>%
        select(date, ctry, value)
    }
  }
} else {
  interest_us <- NULL
}

# UK
temp <- tempfile()
download.file(url = "https://www.bankofengland.co.uk/boeapps/database/Bank-Rate.asp",
              destfile = temp)
interest_uk <- XML::readHTMLTable(temp)[["stats-table"]] %>%
  rename(DATE = `Date Changed`) %>%
  mutate(date = DATE,
         date = as.Date(date, format = "%d %b %y"),
         value = as.numeric(Rate)) %>%
  mutate(diff = c(NA, diff(value))) %>%
  filter(is.na(diff) | diff != 0) %>%
  select(date, value) %>%
  mutate(ctry = "GB") %>%
  arrange(date) %>%
  filter(date >= "1999-01-01") %>%
  select(date, ctry, value)

# Combine and prep data ----
result <- bind_rows(interest_ea, interest_us, interest_uk) %>%
  filter(!is.na(value)) %>%
  mutate(value = value / 100) %>%
  pivot_wider(names_from = "ctry", values_from = "value") %>%
  arrange(date)

result <- result %>%
  full_join(data.frame("date" = seq.Date(from = min(result$date), to = Sys.Date(), by = 1)), by = "date") %>%
  arrange(date) %>%
  pivot_longer(cols = -c("date")) %>%
  group_by(name) %>%
  mutate(value = na.locf(value, na.rm = FALSE)) %>%
  ungroup() %>%
  filter(!is.na(value)) %>%
  mutate(name = factor(name,
                       levels = temp_var_levels,
                       labels = temp_var_labels))

g <- ggplot(result, aes(x = date, y = value, colour = name)) +
  geom_hline(yintercept = 0) +
  geom_path(linewidth = .9) +
  scale_x_date(expand = c(.01, 0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = temp_title,
       subtitle = temp_subtitle,
       caption = temp_caption) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_blank())

g

ggsave(g, filename = "figures/central_bank_policy_rates.png", height = 4, width = 7)

