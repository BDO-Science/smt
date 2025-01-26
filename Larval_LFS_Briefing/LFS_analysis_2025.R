# LFS Larval and Juvenile Protection Action analysis for LFS Briefing 2025


setwd("C:/Users/lmccormick/OneDrive - DOI/Documents/RTO/Briefings/LFS_Analysis")


library("ggplot2")
library("dplyr")
library("lubridate")
library("tidyverse")
library("ggalluvial")
library("tidyr")
library("stringr")


sls.all <- read.delim("SLS_record.txt", header = TRUE, sep = "\t", quote = "\"", 
                 dec = ".", fill = TRUE, comment.char = "",
                 stringsAsFactors=FALSE)

sls.lfs <- sls.all[which(sls.all$FishCode == 2),]

lfs <- sls.lfs[which(sls.lfs$Station == 809 | sls.lfs$Station == 812),]

ymd(lfs$Date)

lfs$Year <- year(ymd(lfs$Date))

yr <- lfs %>% 
  group_by(Year, Station) %>% 
  summarize(total=sum(Catch) )

#qwest<- read_csv("./Dayflow_1996-23_winter.csv")
# QWest file now includes data from the SLS catch generated above
qwest <- read.delim("Dayflow_1996-23_winter_update.txt", header = TRUE, sep = "\t", quote = "\"", 
                      dec = ".", fill = TRUE, comment.char = "",
                      stringsAsFactors=FALSE)

qwest$Date <- ymd(qwest$Date)
qwest$Year <- as.factor(qwest$Year)
qwest$ogdate <- format(qwest$Date, "1900-%m-%d")
#qwest$md <- as_date(format(qwest$md, "%m-%d"))
qwest$ogdate <- ymd(qwest$ogdate)


# Line plot for QWest data
ggplot(qwest, aes(x=qwest$ogdate, y= qwest$QWEST.7, color= qwest$Year, group= qwest$Year))+
  geom_line()+
  scale_x_date(date_labels = "%b %d")+
  geom_hline(yintercept = 1500, color= "black", size= 1)#+
 # ylim(-5000, 15000)



#Exceedance plot

#Qwest exceedance
qwest$qwexceed <- NA
qwest$qwexceed[which(qwest$QWEST.7 <1500)] <- "Yes"
qwest$qwexceed[which(qwest$QWEST.7 >=1500)] <- "No"

#Plot with just qwest exceedance
ggplot(qwest, aes(x = qwest$ogdate, y = qwest$Year, fill = qwest$qwexceed)) +
  geom_tile(color = "white") +  # Add white borders between tiles
  scale_x_date(date_labels = "%b %d", date_breaks = "1 month") +  # Format x-axis as month/day
  scale_fill_manual(values = c("Yes" = "red", "No" = "blue")) +  # Customize colors for Yes/No
  scale_y_continuous(breaks=c(2009, 2011, 2013, 2015, 2017, 2019, 2021, 2023), limits=c(2008, 2024))+
  labs(
    title = "QWest Exceedance (<+1500 cfs)",
    x = "Date (Month-Day)",
    y = "Year",
    fill = "Exceedance"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 0.2),  # Rotate x-axis labels for better readability
    panel.grid = element_blank()  # Remove gridlines for cleaner heatmap
  )

#Catch thresholds
qwest$index <- NA

qwest$index[which(qwest$Year == 2009)] <- 920
qwest$index[which(qwest$Year == 2010)] <- 104
qwest$index[which(qwest$Year == 2011)] <- 164
qwest$index[which(qwest$Year == 2012)] <- 2807
qwest$index[which(qwest$Year == 2013)] <- 1125
qwest$index[which(qwest$Year == 2014)] <- 592
qwest$index[which(qwest$Year == 2015)] <- 164
qwest$index[which(qwest$Year == 2016)] <- 0
qwest$index[which(qwest$Year == 2017)] <- 0
qwest$index[which(qwest$Year == 2018)] <- 384
qwest$index[which(qwest$Year == 2019)] <- 0
qwest$index[which(qwest$Year == 2020)] <- 85
qwest$index[which(qwest$Year == 2021)] <- 244
qwest$index[which(qwest$Year == 2022)] <- 869
qwest$index[which(qwest$Year == 2023)] <- 541

qwest$cthresh <- NA
qwest$cthresh[which(qwest$index >= 0 & qwest$index <= 149)] <- 10
qwest$cthresh[which(qwest$index >= 150 & qwest$index <= 299)] <- 20
qwest$cthresh[which(qwest$index >= 300 & qwest$index <= 499)] <- 30
qwest$cthresh[which(qwest$index >= 500 & qwest$index <= 999)] <- 40
qwest$cthresh[which(qwest$index >= 1000)] <- 50


#Catch exceedance Y/N

qwest$cexceed <- NA

qwest$cexceed[which(qwest$Total.catch >= qwest$cthresh)] <- "Yes"
qwest$cexceed[which(qwest$Total.catch < qwest$cthresh)] <- "No"

# Triggered?
qwest$trigtype <- NA
#qwest$trigtype[which(is.na(qwest$cexceed))] <- "NA"
qwest$trigtype[which(qwest$qwexceed == "Yes" & (qwest$cexceed == "No"| is.na(qwest$cexceed)))] <- "QWest exceed"

qwest$trigtype[which(qwest$qwexceed == "Yes" & qwest$cexceed == "Yes")] <- "Action triggered"

qwest$trigtype[which(qwest$qwexceed == "No"  & (qwest$cexceed == "No" | is.na(qwest$cexceed)))] <- "No exceed"

qwest$trigtype[which(qwest$qwexceed == "No" & qwest$cexceed == "Yes")] <- "Catch exceed"

qwest$trigtype[1:6] <- NA

qwest$trigtype <- as.factor(qwest$trigtype)
qwest$trigtype <- ordered(qwest$trigtype, levels= c("Action triggered", "QWest exceed", "Catch exceed", "No exceed", "NA"))

# Export expanded data:
save(qwest, file = "LFS_Action_Analysis_Data.RData")

#Plot with all exceedance options
ggplot(qwest, aes(x = qwest$ogdate, y = qwest$Year, fill = qwest$trigtype)) +
  geom_tile(color = "white") +  # Add white borders between tiles
  scale_x_date(date_labels = "%b %d", date_breaks = "1 month") +  # Format x-axis as month/day
  scale_fill_manual(values = c("Action triggered" = "red", "QWest exceed" = "orange", "Catch exceed"= "turquoise", "No exceed" = "blue", "NA" = "gray")) +  # Customize colors for trigger type
  #scale_y_continuous(breaks=c(2009, 2011, 2013, 2015, 2017, 2019, 2021, 2023), limits=c(2008, 2024))+
  labs(
    title = "Historic LFS Larval Action Triggers",
    x = "Date (Month-Day)",
    y = "Year",
    fill = "Exceedance Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 0.6),  # Rotate x-axis labels for better readability
    panel.grid = element_blank()  # Remove gridlines for cleaner heatmap
  )


#Summarize data:
table2 <- qwest %>%
  group_by(Year, trigtype) %>%
  summarize(N = n())
