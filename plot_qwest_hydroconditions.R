library(readxl)
library(here)
library(dplyr)
library(janitor)
library(lubridate)
library(ggplot2)

hydro <- read_xlsx(here("data_raw/qwest_20250307.xlsx")) %>%
  clean_names() %>%
  mutate(date = ymd(date))%>%
  mutate(qwest7day = zoo::rollapply(qwest_cfs, 7, mean, align = 'right', partial = TRUE)) %>%
  filter(date > "2025-01-31")

png(here("figures/qwest.png"), height = 4, width = 6, units = "in", res = 300)
ggplot(hydro) + geom_line(aes(x = date, y = qwest7day), color = "navy") +
  labs(y = "7-day Average QWest (cfs)")+
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 12))
dev.off()
