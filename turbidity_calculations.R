### Quick code to check turbidity for TBA

# If daily average is not available, use 0100 to 2400 for hourly averages
# A reminder that, if you are using the historical data retrieval,
# the data is displayed in PST, if using the real-time/current data retrieval,
# the data is listed in local time (currently PDT).
# During the day light saving period, PST and PDT will have one hour lag.
# That is why people are confusing whether to average from 0000 to 2300 or from 0100 to 0000.
# It all depends what time frame and what kind of data you are using.

library(sharpshootR)
library(lubridate)
library(dplyr)

startDate = today()-5
endDate = today()

# Pull data and reassign dates
turb<- CDECquery("HOL", "221", "H", start = startDate,
                     end = endDate) %>%
  mutate(datetime = format(as.POSIXct(datetime), "%Y-%m-%d %H:%M:%S"),
         h = hour(datetime)) %>%
  filter(!is.na(datetime)) %>%
  mutate(date = date(datetime),
         date2 = if_else(h == 0, date-1, date))

# Calculate daily average
turb_avg_0100 <- turb %>%
  group_by(date2) %>%
  summarize(mean = mean(value))

turb_avg_0000 <- turb %>%
  group_by(date) %>%
  summarize(mean = mean(value))

# Pull data and reassign dates
OBI<- CDECquery("OBI", "221", "E", start = startDate,
                 end = endDate) %>%
  mutate(datetime = format(as.POSIXct(datetime), "%Y-%m-%d %H:%M:%S"),
         h = hour(datetime)) %>%
  filter(!is.na(datetime)) %>%
  mutate(date = date(datetime),
         date2 = if_else(h == 0, date-1, date))

# Calculate daily average
obi_avg <- OBI %>%
  group_by(date2) %>%
  summarize(mean = mean(value))
