library(rio)
library(rgdal)
library(raster)
library(ncdf4)
library(sp)
library(ggplot2)
library(tmap)
library(RColorBrewer)
library(shapefiles)
library(tidyverse)
library(chron)
library(tibble)
library(lattice)
library(lubridate)
library(dplyr)

# Start off by visualising the yield over Morocco
# First wheat
Shp_Mor <- shapefile("~/Desktop/Diss-data/Morocco/Shapefiles/Morocco_shp.shp")
shp_Mor <- as.data.frame(Shp_Mor@polygons[[1]]@Polygons[[1]]@coords)
mwheat <- "~/Desktop/Diss-data/Morocco/Total_Yield_kgha/Wheat/Wheat_yield.tif"
mor_wheat <- raster(mwheat)
plot(mor_wheat)
plot(Shp_Mor, add=T)

mor_wheat_df <- as.data.frame(mor_wheat, xy=TRUE)
mor_wheat_df$Wheat_yield <- mor_wheat_df$Wheat_yield/1000
ggplot(data = mor_wheat_df) +
  geom_raster(aes(x = x, y = y, fill = Wheat_yield)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x= V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Wheat Yield \n(tonne ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))


# Now barley

mbar <- "~/Desktop/Diss-data/Morocco/Total_Yield_kgha/Barley/barley_yield.tif"
mor_barley <- raster(mbar)
plot(mor_barley)
plot(Shp_Mor, add=T)

mor_barley_df <- as.data.frame(mor_barley, xy=TRUE)
mor_barley_df$Barley_yield <- mor_barley_df$Barley_yield/1000
ggplot(data = mor_barley_df) +
  geom_raster(aes(x = x, y = y, fill = Barley_yield)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x= V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Barley Yield \n(tonne ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))


# Now maize

mmaiz <- "~/Desktop/Diss-data/Morocco/Total_Yield_kgha/Maize/Maize_yield.tif"
mor_maiz <- raster(mmaiz)
plot(mor_maiz)
plot(Shp_Mor, add=T)

mor_maiz_df <- as.data.frame(mor_maiz, xy=TRUE)
mor_maiz_df$Maize_yield <- mor_maiz_df$Maize_yield/1000
ggplot(data = mor_maiz_df) +
  geom_raster(aes(x = x, y = y, fill = Maize_yield)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x= V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Maize Yield \n(tonne ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

# Now sorghum

msorg <- "~/Desktop/Diss-data/Morocco/Total_Yield_kgha/Sorghum/Sorghum_yield.tif"
mor_sorg <- raster(msorg)
plot(mor_sorg)
plot(Shp_Mor, add=T)

mor_sorg_df <- as.data.frame(mor_sorg, xy=TRUE)
mor_sorg_df$Sorghum_yield <- mor_sorg_df$Sorghum_yield/1000
ggplot(data = mor_sorg_df) +
  geom_raster(aes(x = x, y = y, fill = Sorghum_yield)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x= V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Sorghum Yield \n(tonne ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))


# Now rice


mrice <- "~/Desktop/Diss-data/Morocco/Total_Yield_kgha/Rice/Rice_yield.tif"
mor_rice <- raster(mrice)
plot(mor_rice)
plot(Shp_Mor, add=T)

mor_rice_df <- as.data.frame(mor_rice, xy=TRUE)
mor_rice_df$Rice_yield <- mor_rice_df$Rice_yield/1000
ggplot(data = mor_rice_df) +
  geom_raster(aes(x = x, y = y, fill = Rice_yield)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x= V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Rice Yield \n(tonne ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))


# Now repeat with area where they are harvested 
# First wheat

mwheat_area <- "~/Desktop/Diss-data/Morocco/Area/Wheat/Wheat_area.tif"
mor_wheat_area <- raster(mwheat_area)
plot(mor_wheat_area)
plot(Shp_Mor, add=T)

mor_wheat_area_df <- as.data.frame(mor_wheat_area, xy=TRUE)
mor_wheat_area_df$Wheat_area <- mor_wheat_area_df$Wheat_area/1000
ggplot(data = mor_wheat_area_df) +
  geom_raster(aes(x = x, y = y, fill = Wheat_area)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x= V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Wheat Harvested \nArea (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))


# Now barley

mbarl_area <- "~/Desktop/Diss-data/Morocco/Area/Barley/Barley_area.tif"
mor_barl_area <- raster(mbarl_area)
plot(mor_barl_area)
plot(Shp_Mor, add=T)

mor_barl_area_df <- as.data.frame(mor_barl_area, xy=TRUE)
mor_barl_area_df$Barley_area <- mor_barl_area_df$Barley_area/1000
ggplot(data = mor_barl_area_df) +
  geom_raster(aes(x = x, y = y, fill = Barley_area)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x= V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Barley Harvested \nArea (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))


# Now maize

mmaiz_area <- "~/Desktop/Diss-data/Morocco/Area/Maize/Maize_area.tif"
mor_maiz_area <- raster(mmaiz_area)
plot(mor_maiz_area)
plot(Shp_Mor, add=T)

mor_maiz_area_df <- as.data.frame(mor_maiz_area, xy=TRUE)
mor_maiz_area_df$Maize_area <- mor_maiz_area_df$Maize_area/1000
ggplot(data = mor_maiz_area_df) +
  geom_raster(aes(x = x, y = y, fill = Maize_area)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x= V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Maize Harvested \nArea (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))


# Now sorghum

msorg_area <- "~/Desktop/Diss-data/Morocco/Area/Sorghum/Sorghum_area.tif"
mor_sorg_area <- raster(msorg_area)
plot(mor_sorg_area)
plot(Shp_Mor, add=T)

mor_sorg_area_df <- as.data.frame(mor_sorg_area, xy=TRUE)
mor_sorg_area_df$Sorghum_area <- mor_sorg_area_df$Sorghum_area/1000
ggplot(data = mor_sorg_area_df) +
  geom_raster(aes(x = x, y = y, fill = Sorghum_area)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x= V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Sorghum Harvested \nArea (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))


# Now rice

mrice_area <- "~/Desktop/Diss-data/Morocco/Area/Rice/Rice_area.tif"
mor_rice_area <- raster(mrice_area)
plot(mor_rice_area)
plot(Shp_Mor, add=T)

mor_rice_area_df <- as.data.frame(mor_rice_area, xy=TRUE)
mor_rice_area_df$Rice_area <- mor_rice_area_df$Rice_area/1000
ggplot(data = mor_rice_area_df) +
  geom_raster(aes(x = x, y = y, fill = Rice_area)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x= V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Rice Harvested \nArea (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))


# Now let's visualise the sand percentage over the country
Mor_sand <- "~/Desktop/Diss-data/Morocco/Sand/Mor_sand.tif"
Mor_sand <- raster(Mor_sand)
plot(Mor_sand)
plot(Shp_Mor, add=T)


# Now FAO stat data for yields in Morocco
import("~/Desktop/Diss-data/Morocco/Morocco_crop_1961_2018.csv")
convert("~/Desktop/Diss-data/Morocco/Morocco_crop_1961_2018.csv", "~/Desktop/Diss-data/Morocco/Morocco_crop_1961_2018.rds")
import("~/Desktop/Diss-data/Morocco/Morocco_crop_1961_2018.rds")
Dat_faostat_mor <- read_rds("~/Desktop/Diss-data/Morocco/Morocco_crop_1961_2018.rds")




# And now for precipitation data

Stk_precip_Mor <- brick("~/Desktop/Global Weather/Model 1/precipitation_rcp85_land-gcm_global_60km_01_mon_189912-209911.nc")
Shp_Mor <- shapefile("~/Desktop/Diss-data/Morocco/Shapefiles/Morocco_shp.shp")
Stk_precip_Mor <- Stk_precip_Mor %>% crop(Shp_Mor)
plot(Stk_precip_Mor[[1:12]])
print(Stk_precip_Mor[[2]])
summary(Stk_precip_Mor)

dates <- as.array(Stk_precip_Mor@z)
dates <- dates$time[dates$time != "00"]

dates <- as.Date(dates, format="%Y-%m-%d")
Stk_precip_Mor <- setZ(Stk_precip_Mor, as.Date(dates))

raspt_Mor <- rasterToPoints(Stk_precip_Mor)
dt_Mor <- tibble(Layer = names(Stk_precip_Mor), dttm = getZ(Stk_precip_Mor))
raspt_Mor2 <- raspt_Mor %>%
  as_data_frame() %>%
  rename(lon = x, lat = y) %>%
  gather(Layer, value, -lon, -lat) %>%
  left_join(dt_Mor, by = "Layer") %>%
  dplyr::select(lon, lat, dttm, value)
colnames(raspt_Mor2)
raspt_Mor2 <- raspt_Mor2 %>% rename("x" = "lon") 
raspt_Mor2 <- raspt_Mor2 %>% rename("y" = "lat")

dates <- as.Date(raspt_Mor2$dttm)
str(dates)
raspt_Mor2$month <- month(dates)
raspt_Mor2$year <- year(dates)
raspt_Mor2$dttm <- NULL
raspt_Mor2 <- raspt_Mor2 %>% rename("precip_mm" = "value")

# convert from cm to mm
raspt_Mor2$precip_mm <- raspt_Mor2$precip_mm*10
raspt_Mor2$temp_centigrade <- NULL

# Let do that again with temperature

Stk_temp_Mor <- brick("~/Desktop/Global Weather/Model 1/tempmean_rcp85_land-gcm_global_60km_01_mon_189912-209911.nc")
Stk_temp_Mor <- Stk_temp_Mor %>% crop(Shp_Mor)
plot(Stk_temp_Mor[[1:12]])
print(Stk_temp_Mor[[2]])
summary(Stk_temp_Mor)

dates <- as.array(Stk_temp_Mor@z)
dates <- dates$time[dates$time != "00"]

dates <- as.Date(dates, format="%Y-%m-%d")
Stk_temp_Mor <- setZ(Stk_temp_Mor, as.Date(dates))

temp_Mor <- rasterToPoints(Stk_temp_Mor)
dtt_Mor <- data_frame(Layer = names(Stk_temp_Mor), dttm = as.Date(getZ(Stk_temp_Mor)))
temp_Mor2 <- temp_Mor %>%
  as_data_frame() %>%
  rename(lon = x, lat = y) %>%
  gather(Layer, value, -lon, -lat) %>%
  left_join(dtt_Mor, by = "Layer") %>%
  dplyr::select(lon, lat, dttm, value)
colnames(temp_Mor2)
temp_Mor2 <- temp_Mor2 %>% rename("x" = "lon") 
temp_Mor2 <- temp_Mor2 %>% rename("y" = "lat")

dates <- temp_Mor2$dttm
str(dates)
temp_Mor2$month <- month(dates)
temp_Mor2$year <- year(dates)
temp_Mor2$dttm <- NULL
temp_Mor2$precip_mm <- NULL
temp_Mor2 <- temp_Mor2 %>% rename("temp_centigrade" = "value")

Mor_weather <- merge(temp_Mor2,raspt_Mor2,by=c('x', 'y', 'month', 'year'))

# Now with evapotranspiration

Stk_evp_Mor <- brick("~/Desktop/Global Weather/Model 1/evspsbl_rcp85_land-gcm_global_60km_01_mon_189912-209911.nc")
Stk_evp_Mor <- Stk_evp_Mor %>% crop(Shp_Mor)

plot(Stk_evp_Mor[[1:12]])
print(Stk_evp_Mor[[2]])
summary(Stk_evp_Mor)

dates <- as.array(Stk_evp_Mor@z)
dates <- dates$time[dates$time != "00"]

dates <- as.Date(dates, format="%Y-%m-%d")

Stk_evp_Mor <- setZ(Stk_evp_Mor, as.Date(dates))

evp_Mor <- rasterToPoints(Stk_evp_Mor)
Stk_evp_Mor <- dropLayer(Stk_evp_Mor, "band")
dttt_Mor <- data_frame(Layer = names(Stk_evp_Mor), dttm = as.Date(getZ(Stk_evp_Mor)))
evp_Mor2 <- evp_Mor %>%
  as_data_frame() %>%
  rename(lon = x, lat = y) %>%
  gather(Layer, value, -lon, -lat) %>%
  left_join(dttt_Mor, by = "Layer") %>%
  dplyr::select(lon, lat, dttm, value)
colnames(evp_Mor2)
evp_Mor2 <- evp_Mor2 %>% rename("x" = "lon") 
evp_Mor2 <- evp_Mor2 %>% rename("y" = "lat")

dates <- evp_Mor2$dttm
str(dates)
evp_Mor2$month <- month(dates)
evp_Mor2$year <- year(dates)
evp_Mor2$dttm <- NULL
evp_Mor2$precip_mm <- NULL
evp_Mor2 <- evp_Mor2 %>% rename("pet_mm" = "value")

# I think these values are in meters so let's convert them to mm
evp_Mor2$pet_mm <- evp_Mor2$pet_mm*1000

Mor_weather <- merge(Mor_weather,evp_Mor2,by=c('x', 'y', 'month', 'year'))
Mor_weather$pet_mm.x <- NULL
Mor_weather$pet_mm.y <- NULL


# Put them all together
Mor_weather <- Mor_weather %>%
  group_nest(x, y)

Mor_weather$cell_no <- c(1:560)
Mor_weather <- Mor_weather[, c(1, 2, 4, 3)]
print(Mor_weather[[4]][[1]])
Mor_weather <- Mor_weather %>% mutate(filtered = map(data, ~ filter(., year >= 1961)))
Mor_weather$data <- NULL

saveRDS(Mor_weather, "~/Desktop/Diss-data/Morocco/Morocco_weather.rds")


