# calculate three station average temperature

library(sharpshootR)
library(lubridate)
library(dplyr)

startDate = today()-7
endDate = today()

stas <- c("ANH", "MSD", "RVB")

# read in data
temp <- lapply(stas,
               function(x){
                 CDECquery(id = x, sensor = 25, interval = "D", start = startDate,
                           end = endDate)
                   # mutate(datetime = format(as.POSIXct(datetime), "%Y-%m-%d %H:%M:%S"),
                          # h = hour(datetime)) %>%
                   # mutate(date = date(datetime),
                          # date2 = if_else(h == 0, date-1, date))
                 })

# bind df
temp_df <- bind_rows(temp)%>%
  mutate(temperature = value,
         # datetime = ymd_hms(datetime),
         date = date(datetime),
         month = as.numeric(month)) %>%
  filter(temperature>=0) %>%
  select(date,month, wy = water_year, station = station_id, temperature) %>%
  filter(!is.na(station),
         !is.na(date)) %>%
  mutate(temp_c = (temperature -32) *5/9)

# three station average
temp_average = temp_df %>%
  group_by(date) %>%
  summarize(threesta = mean(temp_c))
