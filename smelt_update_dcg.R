library(here)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(deltamapr)
library(ggspatial)
library(sf)
library(janitor)
library(viridis)

# TFCF: 37.815176 -121.560709 (WGS84)


# Read date --------------------
ds <- read_excel(here("data_raw", "ds_catch_20250123.xlsx"), sheet = 2) %>%
  clean_names() %>%
  mutate(sample_date = ymd(sample_date),
         year = year(sample_date),
         month = month(sample_date),
         wy = if_else(month>9, year + 1, year))

releases <- read_excel(here("data_raw", "Releases_2025.xlsx")) %>%
  clean_names() %>%
  mutate(release_date = mdy(release_date),
         Year = year(release_date),
         Month = month(release_date),
         WY = if_else(Month>9, Year + 1, Year),
         final_number_released = as.numeric(final_number_released),
         final_number_released = if_else(is.na(final_number_released), as.numeric(approx_fish), final_number_released)) %>%
  mutate(release_name = paste(release_date, release_site))%>%
  mutate(release_name = if_else(is.na(release_name), "Unknown/Not released", release_name),
         release_site = if_else(release_site == "NA", "Unknown/Not released", release_site))

# Releases ----------------
(release_summary_plot <- ggplot(releases) +
  geom_col(aes(x = release_date, y = final_number_released, fill = status)) +
  scale_fill_manual(values = c("navy", "steelblue4"))+
  labs(y = "Number of fish", x = "Release Date") +
  theme_bw() +
  theme(legend.position = "top",
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        legend.title = element_text(size = 13)))

png(here("figures/releases_2025_plot.png"), width = 7, height = 5, units = "in", res = 300)
release_summary_plot
dev.off()

sum(releases$final_number_released) - 45000

# Detections ------------------
ds_current <- ds %>%
  filter(wy == 2025) %>%
  mutate(longitude_start = if_else(sub_region == "TFCF", -121.560709, longitude_start),
         latitude_start = if_else(sub_region == "TFCF", 37.815176, latitude_start))

ds_sf <-  ds_current %>%
  st_as_sf(coords = c("longitude_start", "latitude_start"), crs = 4326, remove = FALSE) %>%
  st_transform(crs = st_crs(WW_Delta)) %>%
  group_by(latitude_start, longitude_start, mark_code, survey, release_site, sub_region) %>%
  summarize(total_catch = n()) %>%
  ungroup() %>%
  mutate(total_catch_f = as.factor(total_catch))%>%
  mutate(release_site = if_else(release_site == "NA", "Unknown/Not released", release_site))

releases_sf <- releases %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(crs = st_crs(WW_Delta))

(map_detections_a <- ggplot() +
    geom_sf(data = WW_Delta, color = "darkslategray3") +
    # geom_sf(data = R_EDSM_Strata_1718P1, aes(fill = Stratum), color = NA, alpha = 0.1,inherit.aes = FALSE)+
    geom_sf(data = ds_sf, aes(shape = mark_code, color = release_site, size = total_catch_f),  inherit.aes = FALSE) +
    geom_sf(data = releases_sf,  shape = 18, size =3,  color = "magenta3", fill = "magenta3", inherit.aes = FALSE) +
    geom_sf_label(data = releases_sf, mapping = aes(label = release_site), size = 3, nudge_x = -0.03, nudge_y = 0.04) +
    annotation_north_arrow(location = "tl", which_north = "true",
                           pad_x = unit(.1, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering) +
    annotation_scale(location = "bl", bar_cols = c("black", "white", "black", "white")) +
    guides(fill = guide_legend(nrow = 4, byrow = TRUE)) +
    scale_x_continuous(limits = c(-122.35, -121.3)) +
    scale_y_continuous(limits = c(37.8, 38.5)) +
    scale_color_manual(values = c("black", "chocolate4", "dodgerblue2"))+
    scale_size_manual(values = c(1.5, 2.5, 4, 4, 4, 6.5)) +
    # scale_shape_manual(values = c(17, 16, 14))+
    viridis::scale_fill_viridis(option = "turbo", discrete = TRUE) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, vjust = 0.5),
          legend.position = "top", legend.title = element_blank(),
          legend.box = "vertical",
          legend.text = element_text(size = 8)) +
  guides(size = "none"))

png(here("figures/detections_2025_plot.png"), width = 6, height = 6, units = "in", res = 300)
map_detections_a
dev.off()

# Breakdown by release
ds_release_sf <- left_join(ds_sf, releases %>% select(c(mark_code, release_name)), by = "mark_code") %>%
  mutate(release_name = if_else(is.na(release_name), "Unknown/Not released", release_name),
         release_site = if_else(release_site == "NA", "Unknown/Not released", release_site))

(release_barplot <- ggplot(ds_release_sf)+
  geom_col(aes(release_site, total_catch, fill = release_name)) +
  labs(y = "Total Fish", x = "Release Site") +
  scale_fill_viridis(discrete = TRUE, option = "viridis")+
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = "top", legend.title = element_blank(),
        legend.box = "vertical",
        legend.text = element_text(size = 10)) +
  guides(fill = guide_legend(nrow = 3)))

png(here("figures/release_detections_2025_plot.png"), width = 6, height = 6, units = "in", res = 300)
release_barplot
dev.off()

# Longfin Smelt -------------------

## Chipps

lfs_chipps <-  read_excel(here("data_raw", "lfs_chipps_2025.xlsx")) %>%
  clean_names() %>%
  mutate(date = ymd(date),
         year = year(date),
         month = month(date),
         wy = if_else(month>9, year + 1, year)) %>%
  group_by(date) %>%
  summarize(total = sum(catch)) %>%
  ungroup()

(lfs_chipps_plot <- ggplot(lfs_chipps) +
    geom_col(aes(x = date, y = total), fill = "steelblue4", color = "navy") +
    labs(y = "Number of fish", x = "Date") +
    theme_bw() +
    theme(legend.position = "top",
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.text = element_text(size = 12),
          axis.title = element_text(size = 13),
          legend.title = element_text(size = 13)))

png(here("figures/lfs_chipps_plot.png"), width = 7, height = 5, units = "in", res = 300)
lfs_chipps_plot
dev.off()

## EDSM
lfs_edsm <-  read_excel(here("data_raw", "lfs_edsm_2025.xlsx")) %>%
  clean_names() %>%
  mutate(date = ymd(sample_date),
         year = year(sample_date),
         month = month(sample_date),
         wy = if_else(month>9, year + 1, year))

lfs_edsm_summary <- lfs_edsm %>%
  group_by(sample_date) %>%
  summarize(total = sum(sum_of_catch_count)) %>%
  ungroup()

lfs_edsm_sf <- lfs_edsm %>%
  st_as_sf(coords = c("longitude_start", "latitude_start"), crs = 4326, remove = FALSE) %>%
  st_transform(crs = st_crs(WW_Delta))%>%
  group_by(sample_date) %>%
  summarize(total = sum(sum_of_catch_count)) %>%
  ungroup() %>%
  mutate(Survey = "EDSM")

(map_detections_lfs_edsm <- ggplot() +
    geom_sf(data = WW_Delta, color = "steelblue4") +
    # geom_sf(data = R_EDSM_Strata_1718P1, aes(fill = Stratum), color = NA, alpha = 0.1,inherit.aes = FALSE)+
    geom_sf(data = lfs_edsm_sf, aes(size = total), inherit.aes = FALSE) +
    annotation_north_arrow(location = "tl", which_north = "true",
                           pad_x = unit(.1, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering) +
    annotation_scale(location = "bl", bar_cols = c("black", "white", "black", "white")) +
    guides(fill = guide_legend(nrow = 4, byrow = TRUE)) +
    scale_x_continuous(limits = c(-122.35, -121.6)) +
    scale_y_continuous(limits = c(38, 38.4)) +
    # scale_shape_manual(values = c(17, 16, 14))+
    viridis::scale_fill_viridis(option = "turbo", discrete = TRUE) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, vjust = 0.5),
          legend.position = "top", legend.title = element_blank(),
          legend.box = "vertical",
          legend.text = element_text(size = 8)) +
    guides(size = "none"))

png(here("figures/lfs_edsm_map.png"), width = 7, height = 5, units = "in", res = 300)
map_detections_lfs_edsm
dev.off()

## Larval/Juvenile
lfs_larval <-  read_excel(here("data_raw", "lfs_sls_2025.xlsx")) %>%
  clean_names() %>%
  rename(Station = station) %>%
  mutate(date = ymd(date),
         year = year(date),
         month = month(date),
         wy = if_else(month>9, year + 1, year))
  group_by(date) %>%
  summarize(total = sum(catch)) %>%
  ungroup()

stations <- readr::read_csv(here("data_clean", "station_stratum_crosswalk.csv"))
lfs_larval_sta <- left_join(lfs_larval, stations) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(crs = st_crs(WW_Delta))

lfs_larval_summary <- lfs_larval_sta %>%
  group_by(Station,itp_station) %>%
  summarize(n = sum(catch))

lfs_larval_dates <- lfs_larval_sta %>%
  group_by(date, Stratum) %>%
  summarize(total = sum(catch))


(map_detections_lfs_l <- ggplot() +
    geom_sf(data = WW_Delta, color = "steelblue4") +
    # geom_sf(data = R_EDSM_Strata_1718P1, aes(fill = Stratum), color = NA, alpha = 0.1,inherit.aes = FALSE)+
    geom_sf(data = lfs_larval_summary, aes(size = n, shape = itp_station, color = itp_station), inherit.aes = FALSE) +
    annotation_north_arrow(location = "tl", which_north = "true",
                           pad_x = unit(.1, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering) +
    annotation_scale(location = "bl", bar_cols = c("black", "white", "black", "white")) +
    guides(fill = guide_legend(nrow = 4, byrow = TRUE)) +
    scale_x_continuous(limits = c(-122.55, -121.4)) +
    scale_y_continuous(limits = c(37.8, 38.4)) +
    scale_color_manual(values = c("black", "magenta4"))+
    # scale_shape_manual(values = c(17, 16, 14))+
    viridis::scale_fill_viridis(option = "turbo", discrete = TRUE) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, vjust = 0.5),
          legend.position = "top", legend.title = element_blank(),
          legend.box = "vertical",
          legend.text = element_text(size = 8)) +
    guides(size = "none",
           shape = "none",
           color = "none"))

png(here("figures/lfs_sls_map.png"), width = 7, height = 5, units = "in", res = 300)
map_detections_lfs_l
dev.off()

(lfs_sls_barplot <- ggplot(lfs_larval_dates) +
    geom_col(aes(x = date, y = total, fill= Stratum),color = "black") +
    labs(y = "Number of fish", x = "Date") +
    scale_fill_viridis(option = "turbo", discrete =TRUE) +
    theme_bw() +
    theme(legend.position = "top",
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.text = element_text(size = 10),
          axis.title = element_text(size = 13),
          legend.title = element_blank())+
  guides(fill=guide_legend(nrow=5, byrow = TRUE)))

png(here("figures/lfs_sls_barplot.png"), width = 7, height = 5, units = "in", res = 300)
lfs_sls_barplot
dev.off()

sum(lfs_larval$catch, na.rm = TRUE)
