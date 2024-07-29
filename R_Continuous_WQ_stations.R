## Script to create figures for water quality data for the 2022 Summer-Fall Action Report (Brian Mahardja)
## Modified for Fall X2 Briefing, 2024 (Cat Pien)
# Including maps

library(tidyverse)
library(lubridate)
library(grid)
library(sharpshootR)
library(geofacet)
require(wql)
library(magrittr)
library(viridis)
library(scales)
library(sf)
library(ggspatial)
library(plotrix)
library(ggrepel)
library(here)

#Path to local drive
root <- here::here("../../GitHub/Summer_Fall_Action_2022")
setwd(root)

data_root<-file.path(root,"data-raw")
code_root <- file.path(root,"R")
output_root <- file.path(here(),"figures")

# Map figure denoting locations of continuous water quality stations ------

#Read in bay shape files
CountyWater <- st_read(file.path(data_root,"Map/DJFMP Water bodies","CountyWaterforDJFMPMap.shp"))

CountyWater
sort(unique(CountyWater$FULLNAME))

#Read in bay shape files without county lines
bay <- st_read(file.path(data_root,"Map/BayStudy_Shapefile","sfbs_pg_5bays.shp"))

#Read in Delta files
DeltaSubregionsWater <- st_read(file.path(data_root,"Map/DJFMP Water bodies","DeltaSubregionsWater.shp"))

#------------------------------CDEC stations added here
HUN_station<-data.frame(station_id="HUN",
                        Latitude=paste(CDEC_StationInfo('HUN') %>% extract2(1) %>% select(Latitude)),
                        Longitude=paste(CDEC_StationInfo('HUN') %>% extract2(1) %>% select(Longitude)))
BDL_station<-data.frame(station_id="BDL",
                        Latitude=paste(CDEC_StationInfo('BDL') %>% extract2(1) %>% select(Latitude)),
                        Longitude=paste(CDEC_StationInfo('BDL') %>% extract2(1) %>% select(Longitude)))
NSL_station<-data.frame(station_id="NSL",
                        Latitude=paste(CDEC_StationInfo('NSL') %>% extract2(1) %>% select(Latitude)),
                        Longitude=paste(CDEC_StationInfo('NSL') %>% extract2(1) %>% select(Longitude)))
GZL_station<-data.frame(station_id="GZL",
                        Latitude=paste(CDEC_StationInfo('GZL') %>% extract2(1) %>% select(Latitude)),
                        Longitude=paste(CDEC_StationInfo('GZL') %>% extract2(1) %>% select(Longitude)))
MAL_station<-data.frame(station_id="MAL",
                        Latitude=paste(CDEC_StationInfo('MAL') %>% extract2(1) %>% select(Latitude)),
                        Longitude=paste(CDEC_StationInfo('MAL') %>% extract2(1) %>% select(Longitude)))

#Exclude SDI station for 2022 since it seems to be missing data during summer-fall 2022
#SDI_station<-data.frame(station_id="SDI",
#                        Latitude=paste(CDEC_StationInfo('SDI') %>% extract2(1) %>% select(Latitude)),
#                        Longitude=paste(CDEC_StationInfo('SDI') %>% extract2(1) %>% select(Longitude)))

RVB_station<-data.frame(station_id="RVB",
                        Latitude=paste(CDEC_StationInfo('RVB') %>% extract2(1) %>% select(Latitude)),
                        Longitude=paste(CDEC_StationInfo('RVB') %>% extract2(1) %>% select(Longitude)))
#New stations since 2021
GZB_station<-data.frame(station_id="GZB",
                        Latitude=paste(CDEC_StationInfo('GZB') %>% extract2(1) %>% select(Latitude)),
                        Longitude=paste(CDEC_StationInfo('GZB') %>% extract2(1) %>% select(Longitude)))
GZM_station<-data.frame(station_id="GZM",
                        Latitude=paste(CDEC_StationInfo('GZM') %>% extract2(1) %>% select(Latitude)),
                        Longitude=paste(CDEC_StationInfo('GZM') %>% extract2(1) %>% select(Longitude)))

CDEC_stations<-bind_rows(HUN_station,BDL_station,NSL_station,GZL_station,MAL_station,RVB_station,GZB_station,GZM_station)

#Fix the longitude information
CDEC_stations$Longitude<-as.numeric(CDEC_stations$Longitude)
CDEC_stations$Latitude<-as.numeric(CDEC_stations$Latitude)

CDEC_stations$Longitude<-CDEC_stations$Longitude*(-1)
str(CDEC_stations)

## Add Tule Red location since it's not up online
#Using add_row() function to add observation to data frame
CDEC_stations <- CDEC_stations %>% add_row(station_id="TRB",
                                           Longitude=-121.9960145,
                                           Latitude=38.133841)

# make the coordinate cols spatial (X/Easting/lon, Y/Northing/lat)
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"
df.SP <- st_as_sf(CDEC_stations, coords = c("Longitude", "Latitude"), crs = crsLONGLAT)
df.SP

df.SP$Latitude<-CDEC_stations$Latitude
df.SP$Longitude<-CDEC_stations$Longitude


#Create the map figure
water_quality_map<-ggplot() + theme_bw()+
  geom_sf(data = DeltaSubregionsWater, fill = 'grey', lwd = 0.5, color='black') +
  geom_sf(data = CountyWater, fill = 'grey', lwd = 0.5, color='black') +
  geom_sf(data = bay, fill = 'grey', lwd = 0.5, color='black') +
  geom_sf(data=df.SP,color="red",size=3)+
  geom_label_repel(data=df.SP, aes(x=Longitude,y=Latitude,label=station_id),min.segment.length = unit(0, 'lines'),segment.alpha=0.7,color="blue")+
  coord_sf(xlim = c(-122.2, -121.6), ylim = c(37.9, 38.35),crs=crsLONGLAT)  +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_y = unit(1.0, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "tr", width_hint = 0.5)+
  theme(plot.tag= element_text(size=20, color="black"), axis.text.x = element_text(size=16, color="black"),axis.text.y = element_text(size=16, color="black"),axis.title.x=element_blank(),axis.title.y=element_blank())
water_quality_map


# Print map
tiff(filename=file.path(output_root,"Figure_WQ_Stations_Map.tiff"),
     type="cairo",
     units="in",
     width=11*1,
     height=11*1,
     pointsize=18,
     res=450, compression="lzw")
water_quality_map
dev.off()


# Abiotic factor graphs -----------------------------
#Using data from multiple monitoring stations to depict conditions within the LSZ
#(e.g., a composite metric using data from National Steel, Hunters Cut, and Belden's Landing stations).
#Data from MAL station will be used for Suisun Bay and from SDI and RIV station will be used for the Lower Sac.

#Set date here
start_date<-"2024-05-01"
end_date<- today()-1


## Salinity -----------------------
GZL_cond <- CDECquery(id='GZL', sensor=100, interval='E', start=start_date, end=end_date)
MAL_cond <- CDECquery(id='MAL', sensor=100, interval='E', start=start_date, end=end_date)
#SDI_cond <- CDECquery(id='SDI', sensor=100, interval='E', start=start_date, end=end_date)
RVB_cond <- CDECquery(id='RVB', sensor=100, interval='E', start=start_date, end=end_date)
#National Steel, Hunters Cut, and Belden's Landing
NSL_cond <- CDECquery(id='NSL', sensor=100, interval='E', start=start_date, end=end_date)
BDL_cond <- CDECquery(id='BDL', sensor=100, interval='E', start=start_date, end=end_date)
HUN_cond <- CDECquery(id='HUN', sensor=100, interval='E', start=start_date, end=end_date)

GZB_cond <- CDECquery(id='GZB', sensor=100, interval='E', start=start_date, end=end_date)
GZM_cond <- CDECquery(id='GZM', sensor=100, interval='E', start=start_date, end=end_date)

# CSE_cond <- CDECquery(id='CSE', sensor=100, interval='E', start=start_date, end=end_date)
# GOD_cond <- CDECquery(id='GOD', sensor=100, interval='E', start=start_date, end=end_date)
# MSL_cond <- CDECquery(id='MSL', sensor=100, interval='E', start=start_date, end=end_date)

# #Tule Red data is not up online, sent by Jamel
# TRB_Jun <- read.csv(file.path(data_root,"WaterQuality","TRB_data_June.csv"))
# TRB_AugSep <- read.csv(file.path(data_root,"WaterQuality","2022-08-09_TuleRedBreach_2022-09-21 (formatted).csv"))
#
# TRB<-bind_rows(TRB_Jun, TRB_AugSep)
# str(TRB)
#
# #Convert to consistent format with the rest of CDEC data
# TRB <- TRB  %>% mutate(station_id="TRB", datetime = as.POSIXct(strptime(paste(time),"%m/%d/%Y %H:%M"))) %>%
#   mutate(Date=as.Date(time,"%m/%d/%Y"))
#
# #Create conductivity data for TRB
# TRB_cond<- TRB %>% filter(analyte_name=="Specific Conductance") %>% select(station_id,datetime,value)

#Combine Data
SalinityData<-bind_rows(GZL_cond,MAL_cond,RVB_cond,NSL_cond,BDL_cond,HUN_cond,GZB_cond,GZM_cond)

remove(GZL_cond,MAL_cond,RVB_cond,NSL_cond,BDL_cond,HUN_cond,GZB_cond,GZM_cond)

#Convert conductivity to ppt (assuming conductivity adjusted to temp of 25 C)
SalinityData$ppt<-ec2pss(SalinityData$value/1000, t=25)
str(SalinityData)
SalinityData$station_id<-as.factor(SalinityData$station_id)

#Order the factor
SalinityData$station_id <- ordered(SalinityData$station_id, levels = c("GZM","HUN", "BDL", "NSL","GZL","GZB","TRB","MAL","RVB"))
unique(SalinityData$station_id)

#Create figure
plot_salinity <- ggplot2::ggplot(data=SalinityData, ggplot2::aes(x=datetime, y=ppt))+
  facet_wrap(~ station_id, ncol=4)+
  ggplot2::theme_bw()+
  ggplot2::geom_point(alpha=0.2)+
  ggplot2::geom_smooth(method = 'loess',se=FALSE,span = 0.3)+
  ggplot2::theme(plot.title=element_text(size=9),
                 axis.text.x=element_text(size=9, color="black"),
                 axis.text.y = element_text(size=8, color="black"),
                 axis.title.x = element_blank(),
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 7),
                 strip.background = element_rect(size=0.3))+
  ggplot2::geom_hline(yintercept=6, linetype="dashed", color = "red")+
  ggplot2::ylab("Salinity (ppt)")
plot_salinity

#Print figure
png(filename=file.path(output_root,"Figure_Salinity_stations.png"),
    type="cairo",
    units="in",
    width=10, #10*1,
    height=5, #22*1,
    pointsize=5, #12,
    res=300)
print(plot_salinity)
dev.off()

## Temperature------------------------------------
GZL_temp <- CDECquery(id='GZL', sensor=25, interval='E', start=start_date, end=end_date)
MAL_temp <- CDECquery(id='MAL', sensor=25, interval='E', start=start_date, end=end_date)
#SDI_temp <- CDECquery(id='SDI', sensor=25, interval='E', start=start_date, end=end_date)
RVB_temp <- CDECquery(id='RVB', sensor=25, interval='H', start=start_date, end=end_date)
#National Steel, Hunters Cut, and Belden's Landing
NSL_temp <- CDECquery(id='NSL', sensor=25, interval='E', start=start_date, end=end_date)
BDL_temp <- CDECquery(id='BDL', sensor=25, interval='E', start=start_date, end=end_date)
HUN_temp <- CDECquery(id='HUN', sensor=25, interval='E', start=start_date, end=end_date)
#New data for 2021
GZB_temp <- CDECquery(id='GZB', sensor=25, interval='E', start=start_date, end=end_date)
GZM_temp <- CDECquery(id='GZM', sensor=25, interval='E', start=start_date, end=end_date)

#Create conductivity data for TRB
TRB_temp<- TRB %>% filter(analyte_name=="Water Temperature") %>% select(station_id,datetime,value)

#Combine Data
TemperatureData<-bind_rows(GZL_temp,MAL_temp,RVB_temp,NSL_temp,BDL_temp,HUN_temp,GZB_temp,GZM_temp)

remove(GZL_temp,MAL_temp,RVB_temp,NSL_temp,BDL_temp,HUN_temp,GZB_temp,GZM_temp)

#Convert Fanrenheit to Celsius
TemperatureData$temperature<-ifelse(TemperatureData$station_id=="TRB",TemperatureData$value,(TemperatureData$value-32)*(5/9))

TemperatureData$station_id<-as.factor(TemperatureData$station_id)
#Order the factor
TemperatureData$station_id <- ordered(TemperatureData$station_id, levels = c("GZM","HUN", "BDL", "NSL","GZL","GZB","TRB","MAL","RVB"))

#Temperature limit in BiOp is 75 F
(75-32)*(5/9)

#Create figure
plot_temperature <- ggplot2::ggplot(data=TemperatureData, ggplot2::aes(x=datetime, y=temperature))+ facet_wrap(~ station_id,ncol=4)+
  ggplot2::theme_bw()+
  ggplot2::geom_point(alpha=0.2)+
  ggplot2::geom_smooth(method = 'loess',se=FALSE,span = 0.3)+
  ggplot2::theme(plot.title=element_text(size=9),
                 axis.text.x=element_text(size=9, color="black"),
                 axis.text.y = element_text(size=8, color="black"),
                 axis.title.x = element_blank(),
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 7),
                 strip.background = element_rect(size=0.3))+
  ggplot2::geom_hline(yintercept=23.88889, linetype="dashed", color = "red")+
  ggplot2::ylim(10,30)+
  ggplot2::ylab("Temperature (C)")
plot_temperature

#Print figure
png(filename=file.path(output_root,"Figure_Temperature_stations.png"),
    type="cairo",
    units="in",
    width=10, #10*1,
    height=5, #22*1,
    pointsize=5, #12,
    res=300)
print(plot_temperature)
dev.off()


## Turbidity --------------------------

#SDI is a USGS station, so labeled as FNU
GZL_turb <- CDECquery(id='GZL', sensor=27, interval='E', start=start_date, end=end_date)
MAL_turb <- CDECquery(id='MAL', sensor=27, interval='E', start=start_date, end=end_date)
#SDI_turb <- CDECquery(id='SDI', sensor=221, interval='E', start=start_date, end=end_date)
RVB_turb <- CDECquery(id='RVB', sensor=27, interval='E', start=start_date, end=end_date)
#National Steel, Hunters Cut, and Belden's Landing
NSL_turb <- CDECquery(id='NSL', sensor=27, interval='E', start=start_date, end=end_date)
BDL_turb <- CDECquery(id='BDL', sensor=27, interval='E', start=start_date, end=end_date)
HUN_turb <- CDECquery(id='HUN', sensor=27, interval='E', start=start_date, end=end_date)
#New data for 2021
GZB_turb <- CDECquery(id='GZB', sensor=27, interval='E', start=start_date, end=end_date)
GZM_turb <- CDECquery(id='GZM', sensor=221, interval='E', start=start_date, end=end_date)


#Combine Data
TurbidityData<-bind_rows(GZL_turb,MAL_turb,NSL_turb,BDL_turb,HUN_turb,RVB_turb,GZB_turb,GZM_turb)
remove(GZL_turb,MAL_turb,NSL_turb,BDL_turb,HUN_turb,RVB_turb,GZB_turb,GZM_turb)

TurbidityData$station_id<-as.factor(TurbidityData$station_id)
#Order the factor
TurbidityData$station_id <- ordered(TurbidityData$station_id, levels = c("GZM","HUN", "BDL", "NSL","GZL","GZB","TRB","MAL","RVB"))

#Calculate number of observations of NTU >100
NTUover100<- TurbidityData %>% filter(value>100)
nrow(NTUover100)/nrow(TurbidityData)*100

#Create figure
plot_turbidity <- ggplot2::ggplot(data=TurbidityData, ggplot2::aes(x=datetime, y=value))+ facet_wrap(~ station_id, ncol=4)+
  ggplot2::theme_bw()+
  ggplot2::geom_point(alpha=0.2)+
  ggplot2::geom_smooth(method = 'loess',se=FALSE,span=0.3)+
  ggplot2::theme(plot.title=element_text(size=9),
                 axis.text.x=element_text(size=9, color="black"),
                 axis.text.y = element_text(size=8, color="black"),
                 axis.title.x = element_blank(),
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 7),
                 strip.background = element_rect(size=0.3))+
  ggplot2::geom_hline(yintercept=12, linetype="dashed", color = "red")+
  ggplot2::ylim(0,150)+
  ggplot2::ylab("Turbidity (NTU)")
plot_turbidity

#Print figure
png(filename=file.path(output_root,"Figure_Turbidity_stations.png"),
    type="cairo",
    units="in",
    width=10, #10*1,
    height=5, #22*1,
    pointsize=5, #12,
    res=300)
print(plot_turbidity)
dev.off()

#Percent of time suitable for Delta Smelt -----------------------

#Only keep the stations with good data

#Calculate percent of time water quality was suitable for Delta Smelt
Salinity_habitat_data<- SalinityData %>%
  filter(station_id %in% c("GZL","GZM","HUN","BDL","NSL","MAL","RVB")) %>%
  mutate(Date=as.Date(datetime),Parameter="Salinity", PercentSuitable=ifelse(ppt<=6,1,0)) %>% group_by(station_id,Date,Parameter) %>% summarise(PercentSuitable=mean(PercentSuitable))
Temperature_habitat_data<- TemperatureData %>%
  filter(station_id %in% c("GZL","GZM","HUN","BDL","NSL","MAL","RVB")) %>%
  mutate(Date=as.Date(datetime),Parameter="Temperature", PercentSuitable=ifelse(temperature<=23.88889,1,0)) %>% group_by(station_id,Date,Parameter) %>% summarise(PercentSuitable=mean(PercentSuitable))
Turbidity_habitat_data<- TurbidityData %>%
  filter(station_id %in% c("GZL","GZM","HUN","BDL","NSL","MAL","RVB")) %>%
  mutate(Date=as.Date(datetime),Parameter="Turbidity", PercentSuitable=ifelse(value>=12,1,0)) %>% group_by(station_id,Date,Parameter) %>% summarise(PercentSuitable=mean(PercentSuitable))

Salinity_habitat_data$station_id<-as.character(Salinity_habitat_data$station_id)
Temperature_habitat_data$station_id<-as.character(Temperature_habitat_data$station_id)
Turbidity_habitat_data$station_id<-as.character(Turbidity_habitat_data$station_id)

#Combine the datasets
Suitable_habitat <- dplyr::bind_rows(Salinity_habitat_data,Temperature_habitat_data,Turbidity_habitat_data)
remove(Salinity_habitat_data,Temperature_habitat_data,Turbidity_habitat_data)


Suitable_habitat$station_id<-as.factor(Suitable_habitat$station_id)
#Order the factor
Suitable_habitat$station_id <- ordered(Suitable_habitat$station_id, levels = c("GZL","GZM","HUN","BDL","NSL","MAL","RVB"))

Suitable_habitat <- Suitable_habitat %>%
  mutate(region = case_when(station_id %in% c("BDL","NSL", "HUN")~ "Suisun Marsh",
                            station_id %in% c("GZB", "GZL", "GZM")~ "Suisun Bay",
                            station_id %in% c("MAL", "RVB")~"Sacramento River"))
#Create figure
plot_habitat <-ggplot2::ggplot(Suitable_habitat,aes(y=Parameter,x=Date,fill=PercentSuitable))+
  ggplot2:: geom_tile() +scale_fill_viridis(name="Percent Suitable",option ="magma")+
  ggplot2::facet_grid(station_id~.)+
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  ggplot2::guides(fill = guide_legend(title = "Proportion of time\nparameter was\n'suitable' for \nDelta Smelt"))+
  theme_bw() +
ggplot2::theme(plot.title=element_text(size=12),
               axis.text.x=element_text(size=12, color="black"),
               axis.text.y = element_text(size=12, color="black"),
               axis.title.x = element_blank(),
               axis.title.y = element_blank(),
               strip.text = element_text(size = 12),
               legend.text=element_text(size = 12),
               strip.background = element_rect(size=0.3))
plot_habitat


#Print figure
png(filename=file.path(output_root,"Figure_Habitat.png"),
    type="cairo",
    units="in",
    width=10, #10*1,
    height=6, #22*1,
    pointsize=5, #12,
    res=500)
print(plot_habitat)
dev.off()



############################### Lisbon Flow data
#Sensor 20 = Flow, Discharge (cfs)
#Sensor 25 = TEMPERATURE (F)
#Sensor 27 = Turbidity (NTU)
#Sensor 28 = CHLOROPHYLL (ug/L)
#Sensor 61 = DISSOLVED OXYGEN (MG/L)
#Sensor 62 = PH (PH)
#Sensor 100 = Conductivity (us/cm)

#Get Lisbon data

LIS_flow <- CDECquery(id='LIS', sensor=20, interval='E', start='2022-06-01', end='2022-10-31')
str(LIS_flow)

plot_LIS_flow <- ggplot2::ggplot(data=LIS_flow, ggplot2::aes(x=datetime, y=value))+
  ggplot2::theme_bw()+
  ggplot2::geom_point(alpha=0.2)+
  ggplot2::geom_smooth(method = 'loess',se=FALSE,span = 0.3)+
  ggplot2::theme(plot.title=element_text(size=9),
                 axis.text.x=element_text(size=9, color="black"),
                 axis.text.y = element_text(size=8, color="black"),
                 axis.title.x = element_blank(),
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 7),
                 strip.background = element_rect(size=0.3))+
  ggplot2::ylab(bquote(~Flow~"("*ft^3*"/s)"))+
  ggplot2::labs(title="Lisbon Weir (LIS)")
plot_LIS_flow


#Print figure
tiff(filename=file.path(output_root,"Figure_Lisbon_Flow.tiff"),
     type="cairo",
     units="in",
     width=8, #10*1,
     height=6, #22*1,
     pointsize=5, #12,
     res=600,
     compression="lzw")
print(plot_LIS_flow)
dev.off()

remove(plot_LIS_flow,LIS_flow)


######################### Solo Belden's Landing salinity figure per Armin Halston's request
SalinityData

## Get the start and end points for highlighted regions
mindates<-c("2022-09-08 01:00:00", "2022-10-01 01:00:00")
maxdates<-c("2022-09-24 01:00:00", "2022-10-31 01:00:00")

SMSG_highlight<-data.frame(MinDate=mindates,MaxDate=maxdates,mindata=c(0,0),maxdata=c(12,12))
SMSG_highlight$MaxDate<-as.POSIXct(SMSG_highlight$MaxDate)
SMSG_highlight$MinDate<-as.POSIXct(SMSG_highlight$MinDate)


plot_BDL <- ggplot2::ggplot()+
  ggplot2::theme_bw()+
  ggplot2::geom_point(data= (SalinityData %>% filter(station_id == "BDL")), ggplot2::aes(x=datetime, y=ppt),alpha=0.2)+
  ggplot2::geom_smooth(data= (SalinityData %>% filter(station_id == "BDL")), ggplot2::aes(x=datetime, y=ppt),method = 'loess',se=FALSE,span = 0.3)+
  #ggplot2::geom_rect(data=SMSG_highlight, ggplot2::aes(xmin=MinDate, xmax=MaxDate, ymin=mindata, ymax=maxdata), alpha=0.3, fill="darkorange1")+
  ggplot2::theme(plot.title=element_text(size=9),
                 axis.text.x=element_text(size=9, color="black"),
                 axis.text.y = element_text(size=8, color="black"),
                 axis.title.x = element_blank(),
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 7),
                 strip.background = element_rect(size=0.3))+
  ggplot2::geom_hline(yintercept=6, linetype="dashed", color = "red")+
  ggplot2::ylab("Salinity (ppt)")+ ylim(0,11)

plot_BDL

#Print figure
png(filename=file.path(output_root,"Figure_BDL_Salinity.png"),
    type="cairo",
    units="in",
    width=6, #10*1,
    height=4, #22*1,
    pointsize=5, #12,
    res=400)
print(plot_BDL)
dev.off()


# WQ Combined Plot based on 2023 Summer-Fall Plot --------------------------------------
thresholds <- data.frame(parameter = c("Salinity (ppt)", "Temperature (C)", "Turbidity (FNU)"),
                         value = c(6, 23.88889, 12))

wq1 <- left_join(TemperatureData %>% select(station_id, datetime, temp = temperature, month), SalinityData %>% select(station_id, datetime, sal = ppt))
wq2 <- left_join(wq1, TurbidityData %>% select(station_id, datetime, turb=value))
wq_long <- wq2 %>% mutate(date = date(datetime)) %>%
  filter(temp > 10, turb < 150) %>%
  group_by(date, station_id) %>%
  summarize(Temperature = mean(temp),
            Salinity = mean(sal),
            Turbidity = mean(turb)) %>%
  ungroup() %>%
  pivot_longer(cols = c(Temperature, Salinity, Turbidity), names_to = "parameter", values_to = "value") %>%
  mutate(region = case_when(station_id %in% c("BDL","NSL", "HUN")~ "Suisun Marsh",
                            station_id %in% c("GZB", "GZL" , "GZM")~ "Suisun Bay",
                            station_id %in% c("MAL", "RVB")~"Sacramento River")) %>%
  mutate(parameter = case_when(parameter == "Temperature" ~ "Temperature (C)",
                               parameter == "Salinity" ~ "Salinity (ppt)",
                               parameter == "Turbidity" ~ "Turbidity (FNU)"))
plot_wq <- ggplot() +
  geom_line(data = wq_long, aes(date, y = value, color = station_id)) +
  geom_hline(data = thresholds, aes(yintercept = value), linetype = "dashed") +
  facet_grid(parameter~region, scales = "free_y") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        axis.title = element_blank())
plot_wq

png(filename=file.path(output_root,"Figure_cont_wq_2024_summer.png"),
    type="cairo",
    units="in",
    width=6.5, #10*1,
    height=4, #22*1,
    pointsize=5, #12,
    res=400)
print(plot_wq)
dev.off()

Suitable <- Suitable_habitat %>%
  mutate(suit = if_else(PercentSuitable <1, 0L, 1L)) %>%
  mutate(days = length(unique(Suitable_habitat$Date))) %>%
  group_by(region, Date, days) %>%
  summarize(propTotal = mean(suit)) %>%
  ungroup() %>%
  mutate(suitableday = if_else(propTotal <1, 0L, 1L)) %>%
  group_by(region, days) %>%
  summarize(Suitable = sum(suitableday)) %>%
  ungroup() %>%
  mutate(percent = Suitable/days*100)

# Spring Temperature Average -------------------------
start_date = "2024-03-01"
end_date = "2024-05-31"
#Temperature
GZL_temp <- CDECquery(id='GZL', sensor=25, interval='E', start=start_date, end=end_date)
MAL_temp <- CDECquery(id='MAL', sensor=25, interval='E', start=start_date, end=end_date)
#SDI_temp <- CDECquery(id='SDI', sensor=25, interval='E', start=start_date, end=end_date)
RVB_temp <- CDECquery(id='RVB', sensor=25, interval='H', start=start_date, end=end_date)
#National Steel, Hunters Cut, and Belden's Landing
NSL_temp <- CDECquery(id='NSL', sensor=25, interval='E', start=start_date, end=end_date)
BDL_temp <- CDECquery(id='BDL', sensor=25, interval='E', start=start_date, end=end_date)
HUN_temp <- CDECquery(id='HUN', sensor=25, interval='E', start=start_date, end=end_date)
#New data for 2021
GZB_temp <- CDECquery(id='GZB', sensor=25, interval='E', start=start_date, end=end_date)
GZM_temp <- CDECquery(id='GZM', sensor=25, interval='E', start=start_date, end=end_date)

#Combine Data
TemperatureData2<-bind_rows(GZL_temp,MAL_temp,RVB_temp,NSL_temp,BDL_temp,HUN_temp,GZB_temp,GZM_temp)
TempSum2 <- TemperatureData2%>%
  filter(value>40) %>%
  mutate(date = date(datetime)) %>%
  group_by(station_id,date) %>%
  summarize(Temperature = mean(value)) %>%
  ungroup()

remove(GZL_temp,MAL_temp,RVB_temp,NSL_temp,BDL_temp,HUN_temp,GZB_temp,GZM_temp)

ggplot() +
  geom_line(data = TempSum2, aes(x = date, y = Temperature, color = station_id)) +
  geom_hline(yintercept = (15.8*9/5)+32) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b-%d") +
  theme_bw()

table <- TempSum2 %>%
  mutate(region = case_when(station_id %in% c("BDL","NSL", "HUN")~ "Suisun Marsh",
                            station_id %in% c("GZB", "GZL" , "GZM")~ "Suisun Bay",
                            station_id %in% c("MAL", "RVB")~"Sacramento River")) %>%
  group_by(region) %>%
  summarize(mean = mean(Temperature)) %>%
  ungroup() %>%
  mutate(C = (mean - 32)*5/9)
