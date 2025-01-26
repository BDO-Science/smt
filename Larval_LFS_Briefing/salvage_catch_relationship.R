### Look at relationship between SLS/20mm and Salvage
# Catarina Pien

library(here)
library(dplyr)
library(readr)
library(ggplot2)
library(janitor)
library(lubridate)
library(viridis)
library(readxl)

# Read in data ----------------------
wytype <- read_csv(here("data_raw/WYType.csv")) %>%
  mutate(wy = factor(WY))%>%
  filter(Basin == "SanJoaquinValley")

lfs_salvage0 <- read_csv(here("data_raw/lfs_loss.csv"))
lfs_salvage <- lfs_salvage0 %>%
  clean_names() %>%
  filter(!is.na(facility),
         !is.na(nfish),
         !is.na(length)) %>%
  mutate(sample_time = mdy_hm(sample_time),
         year = year(sample_time),
         month = month(sample_time),
         date = date(sample_time),
         wy = if_else(month>=10, year+1, year)) %>%
  filter(wy > 2008) %>%
  mutate(date2 = case_when(month>=10 ~ ymd(paste0(1980, "-", month, "-", day(date))),
                           month<10 ~ ymd(paste0(1981,"-", month, "-", day(date) )))) %>%
  mutate(lifestage = if_else(length>=60, "Adult", if_else(length>=19, "Juvenile", "Larva"))) %>%
  mutate(expanded = nfish/sample_fraction)%>%
  filter(!is.na(expanded)) %>%
  group_by(wy, lifestage) %>%
  arrange(date) %>%
  mutate(cumulative_salvage = cumsum(expanded)) %>%
  ungroup() %>%
  select(facility, date, length, wy, month, date2, lifestage, expanded, cumulative_salvage) %>%
  mutate(week = week(date))%>%
  filter(week < 25)

sls <- read_excel(here("Larval_LFS_Briefing/SLS_Catch.xlsx"), sheet = 2) %>%
  mutate(week = week(Date),
         year = year(Date),
         month = month(Date),
         wy = if_else(month>=10, year+1, year)) %>%
  rename(date = Date)%>%
  filter(week< 25)

# twentymm <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.535.5&entityid=1e744fd85f10c4164d748831bc0454dd")
lfs_20 <- twentymm %>% filter(Taxa == "Spirinchus thaleichthys") %>%
  filter(Station %in% c(809, 812)) %>%
  mutate(Date = ymd(Date),
         week = week(Date),
         year = year(Date),
         month = month(Date),
         wy = if_else(month>=10, year+1, year)) %>%
  filter(year>=2009) %>%
  rename(date = Date)

# weekly plot ---------------------
sls_summary <- sls %>% group_by(wy, month, week) %>%
  summarize(sls = sum(Catch)) %>%
  ungroup()

lfs_20_summary <- lfs_20 %>% group_by(wy, month, week) %>%
  summarize(twenty = sum(Count)) %>%
  ungroup()

salvage_summary <- lfs_salvage %>%
  filter(lifestage == "Juvenile") %>%
  group_by(wy, month, week) %>%
  summarize(salvage = sum(expanded)) %>%
  ungroup()

sls_20mm_salvage <-  sls_summary %>%
  full_join(salvage_summary, by = c("wy", "month", "week")) %>%
  full_join(lfs_20_summary, by = c("wy", "month", "week")) %>%
  arrange(wy, month, week) %>%
  mutate(wy = factor(wy),
         month = factor(month))

lfs_long <- sls_20mm_salvage %>%
  pivot_longer(cols = sls:twenty, names_to = "sampling", values_to = "count", values_drop_na = T)%>%
  mutate(sampling2 = if_else(sampling %in% c("sls", "twenty"), "catch", sampling)) %>%
  left_join(wytype %>% select(wy, wytype = `Yr-type`)) %>%
  filter(month!=7)

# plot code
png(here("Larval_LFS_Briefing/weekly_catch_plot.png"), res = 300, width =6, height = 4, units = "in" )
ggplot(lfs_long %>% filter(wy!=2025)) +
  geom_point(aes(week, count, color = wytype, shape = sampling)) +
  facet_wrap(~sampling2, nrow = 3, scales = "free_y")+
  theme_bw()
dev.off()


# monthly plot -----------------------
sls_summary2 <- sls %>% group_by(wy, month) %>%
  summarize(sls = sum(Catch)) %>%
  ungroup()

lfs_20_summary2 <- lfs_20 %>% group_by(wy, month) %>%
  summarize(twenty = sum(Count)) %>%
  ungroup()

salvage_summary2 <- lfs_salvage %>%
  filter(lifestage == "Juvenile") %>%
  group_by(wy, month) %>%
  summarize(salvage = sum(expanded)) %>%
  ungroup()

sls_20mm_salvage2 <-  sls_summary2 %>%
  full_join(salvage_summary2, by = c("wy", "month")) %>%
  full_join(lfs_20_summary2, by = c("wy", "month")) %>%
  arrange(wy, month) %>%
  mutate(wy = factor(wy),
         month = factor(month))%>%
  left_join(wytype %>% select(wy, wytype = `Yr-type`))

lfs_long2 <- sls_20mm_salvage2 %>%
  pivot_longer(cols = sls:twenty, names_to = "sampling", values_to = "count", values_drop_na = T)%>%
  mutate(sampling2 = if_else(sampling %in% c("sls", "twenty"), "catch", sampling))%>%
  left_join(wytype %>% select(wy, wytype = `Yr-type`))

# plot code
ggplot(lfs_long2) +
  geom_point(aes(month, count, color = wytype, shape = sampling)) +
  facet_wrap(~sampling2, nrow = 3, scales = "free_y")+
  theme_bw()

ggplot(sls_20mm_salvage2) +
  geom_point(aes(twenty, salvage, color = month)) +
  facet_wrap(~wytype)

## annual totals ---------------------

sls_summary3 <- sls %>% group_by(wy) %>%
  summarize(sls = sum(Catch)) %>%
  ungroup()

lfs_20_summary3 <- lfs_20 %>% group_by(wy) %>%
  summarize(twenty = sum(Count)) %>%
  ungroup()

salvage_summary3 <- lfs_salvage %>%
  filter(lifestage == "Juvenile") %>%
  group_by(wy) %>%
  summarize(salvage = round(sum(expanded))) %>%
  ungroup()

lfs_comb <-  sls_summary3 %>%
  full_join(salvage_summary3, by = c("wy")) %>%
  full_join(lfs_20_summary3, by = c("wy")) %>%
  mutate(catch = sls + twenty) %>%
  arrange(wy) %>%
  mutate(wy = factor(wy, levels = c("W", "AN", "BN", "D", "C")))%>%
  left_join(wytype %>% select(wy, wytype = `Yr-type`)) %>%
  filter(wy!=2024)

# plot
threshold_salvage <- read_excel("Larval_LFS_Briefing/threshold_salvage.xlsx")%>%
  mutate(wytype = factor(wytype, levels = c("W", "AN", "BN", "D", "C")))

png(here("Larval_LFS_Briefing/salvage_catchthreshold_plot.png"), res = 300, width =6, height = 4, units = "in" )
ggplot(threshold_salvage) +
  geom_point(aes(days_catch,salvage, color = wytype), size = 3) +
  geom_smooth(aes(days_catch,salvage), method = "lm")+
  labs(x = "Days Catch Threshold Triggered", color = "WY Type", y = "Salvage")+
  viridis::scale_color_viridis(discrete = T) +
  theme_bw() +
  theme(legend.position = "top")
dev.off()

png(here("Larval_LFS_Briefing/salvage_actionthreshold_plot.png"), res = 300, width =6, height = 4, units = "in" )
ggplot(threshold_salvage) +
  geom_point(aes(days_action,salvage, color = wytype), size = 3)+
  geom_smooth(aes(days_catch,salvage), method = "lm")+
  labs(x = "Days Action Thresholds Triggered", color = "WY Type", y = "Salvage")+
  viridis::scale_color_viridis(discrete = T) +
  theme_bw() +
  theme(legend.position = "top")
dev.off()


