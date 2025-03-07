library(readxl)
library(here)
library(dplyr)
library(janitor)
library(lubridate)
library(ggplot2)
library(readr)
library(tidyr)

anh <- read_csv(here("data_raw/ANH_temp_20250307.csv")) %>%
  clean_names() %>%
  separate(mm_dd, c("d", "m"), "-") %>%
  mutate(month = match(m, month.abb)) %>%
  mutate(year2 = case_when(year != "2025" ~ 1980,
                           year == "2025" ~ 2025),
         date = paste0("1980-", month, "-", d),
         date2 = ymd(date))



png(here("figures/anhtemp.png"), height = 4, width = 7, units = "in", res = 300)
ggplot(anh) + geom_line(aes(x = date2, y = value, color = year, linetype = year), linewidth = 1) +
  labs(y = "Water Temperature (°F)", color = "Year", linetype = "Year")+
  scale_color_manual(values = c("navy", "goldenrod3"))+
  scale_y_continuous(breaks = seq(48, 57, 1))+
  theme_bw() +
  theme(axis.text = element_text(size = 11),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 12))
dev.off()
