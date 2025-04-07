# Load packages
library(devtools)
library(CDECRetrieve)
library(tidyverse)
library(lubridate)
library(circular)
library(xml2)
library(rvest)
library(openair)

# Select today's date
theDATE <- today()

###############
# Wind Direction (Degrees) from CDEC
wd1 <- cdec_query("FRK", "10", "E", theDATE - 14, theDATE)
wd2 <- wd1 %>%
  rename(wind.dir.deg = parameter_value) %>%
  select(datetime, location_id, wind.dir.deg) %>%
  mutate(datetime = as_datetime(datetime))

###############
# Wind Speed (MPH) from CDEC
ws1 <- cdec_query("FRK", "9", "E", theDATE - 14, theDATE)
ws2 <- ws1 %>%
  rename(wind.spd.mph = parameter_value) %>%
  select(datetime, location_id, wind.spd.mph) %>%
  mutate(datetime = as_datetime(datetime))

###############
# Turbidity at OBI
ot1 <- cdec_query("OBI", "221", "E", theDATE - 14, theDATE)
ot2 <- ot1 %>%
  rename(turb.NTU = parameter_value) %>%
  select(datetime, location_id, turb.NTU) %>%
  mutate(datetime = as_datetime(datetime))

# Turbidity at FRK
ft1 <- cdec_query("FRK", "27", "E", theDATE - 14, theDATE)
ft2 <- ft1 %>%
  rename(turb.NTU = parameter_value) %>%
  select(datetime, location_id, turb.NTU) %>%
  mutate(datetime = as_datetime(datetime))

###############
# Combine wind and turbidity data
tu1 <- bind_rows(ft2, ot2)
a1 <- left_join(wd2, ws2, by = c("datetime", "location_id"))
a2 <- left_join(a1, ft2, by = c("datetime", "location_id")) %>%
  select(datetime, location_id, wind.spd.mph, wind.dir.deg)
a3 <- inner_join(a2, tu1, by = "datetime", suffix = c(".wind", ".turb"))

# Daily summaries
wd.a4 <- a3 %>% group_by(date = date(datetime)) %>%
  summarise(dir.deg.daily = mean(wind.dir.deg, na.rm = TRUE))
ws.a4 <- a3 %>% group_by(date = date(datetime)) %>%
  summarise(spd.mph.daily = mean(wind.spd.mph, na.rm = TRUE))
tu.a4 <- a3 %>% group_by(location_id.turb, date = date(datetime)) %>%
  summarise(turb.NTU.daily = mean(turb.NTU, na.rm = TRUE))

tu.a5 <- left_join(tu.a4, wd.a4, by = "date")
tu.a6 <- left_join(tu.a5, ws.a4, by = "date")

###############
# Combine wind speed + direction
wsd1 <- left_join(wd2, ws2, by = c("datetime", "location_id")) %>%
  select(datetime, wind.spd.mph, wind.dir.deg)

# Add date column as Date
wsd2 <- wsd1 %>%
  mutate(date = as.Date(datetime))

###############
# Assign cardinal directions manually
wsd2 <- wsd2 %>%
  mutate(wd_cardinal = case_when(
    wind.dir.deg >= 348.75 | wind.dir.deg < 11.25  ~ "N",
    wind.dir.deg >= 11.25  & wind.dir.deg < 33.75  ~ "NNE",
    wind.dir.deg >= 33.75  & wind.dir.deg < 56.25  ~ "NE",
    wind.dir.deg >= 56.25  & wind.dir.deg < 78.75  ~ "ENE",
    wind.dir.deg >= 78.75  & wind.dir.deg < 101.25 ~ "E",
    wind.dir.deg >= 101.25 & wind.dir.deg < 123.75 ~ "ESE",
    wind.dir.deg >= 123.75 & wind.dir.deg < 146.25 ~ "SE",
    wind.dir.deg >= 146.25 & wind.dir.deg < 168.75 ~ "SSE",
    wind.dir.deg >= 168.75 & wind.dir.deg < 191.25 ~ "S",
    wind.dir.deg >= 191.25 & wind.dir.deg < 213.75 ~ "SSW",
    wind.dir.deg >= 213.75 & wind.dir.deg < 236.25 ~ "SW",
    wind.dir.deg >= 236.25 & wind.dir.deg < 258.75 ~ "WSW",
    wind.dir.deg >= 258.75 & wind.dir.deg < 281.25 ~ "W",
    wind.dir.deg >= 281.25 & wind.dir.deg < 303.75 ~ "WNW",
    wind.dir.deg >= 303.75 & wind.dir.deg < 326.25 ~ "NW",
    wind.dir.deg >= 326.25 & wind.dir.deg < 348.75 ~ "NNW",
    TRUE ~ NA_character_
  ))

###############
# ✅ Composite Daily Wind Rose (panel per day, may have some label overlap)
windRose(wsd2, 
         ws = "wind.spd.mph", 
         wd = "wind.dir.deg", 
         type = "date",
         angle = 10,
         cols = c("tan", "light blue", "dark blue", "dark green", "orange"), 
         grid.line = 5,
         breaks = c(0, 10, 20, 30, 40, 50),
         paddle = FALSE,
         key.header = "This is in MPH not Meters/second")

###############
# ✅ Overall Wind Rose (all 14 days combined into one rose)
windRose(wsd2, 
         ws = "wind.spd.mph", 
         wd = "wind.dir.deg", 
         angle = 10,
         cols = c("tan", "light blue", "dark blue", "dark green", "orange"), 
         grid.line = 5,
         breaks = c(0, 10, 20, 30, 40, 50),
         paddle = FALSE,
         key.header = "This is in MPH not Meters/second")
