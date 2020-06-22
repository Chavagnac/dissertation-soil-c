install.packages("sf")
install.packages("lattice")
install.packages(("shapefiles"))
install.packages("maptools")
library(rgdal)
library("sp")
library("maptools")
library(tidyverse)
library(rio)
library(rgdal)
library(raster)
library(ncdf4)
library(sp)
library(ggplot2)
library(tmap)
library(RColorBrewer)
library(shapefiles)
library(chron)
library(tibble)
library(lattice)
library(maptools)
library(lubridate)


trice <- "Thailand/Yield/Rice/Thailand_rice.tif"
thai_rice <- raster(trice)

thai_shp <- readOGR("Thailand/Shapefiles/shapefile.shp", 'shapefile')
plot(thai_rice)
plot(thai_shp, add=T)
asg <- thai_shp@polygons
shape <- as.data.frame(asg[[1]]@Polygons[[1]]@coords)

thai_rice_df <- as.data.frame(thai_rice, xy=TRUE)
thai_rice_df$Thailand_rice <- thai_rice_df$Thailand_rice/1000
ggplot(data = thai_rice_df) +
  geom_raster(aes(x = x, y = y, fill = Thailand_rice)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shape, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Rice Yield (tonne ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

# Now for the sand
Thai_sand <- "~/Desktop/Diss-data/Thailand/Sand/Thai_sand.tif"
Thai_sand <- raster(Thai_sand)
plot(Thai_sand)
plot(Shp_thai, add=T)

# NOW FOR THE PRECIPITATION DATA!! need to update the shp file - it's not doing what i want it to do

Shp_thai <- shapefile("~/Desktop/Diss-data/Thailand/Shapefiles/Thai.shp")

Stk_precip_thai <- brick("~/Desktop/Global Weather/Model 1/precipitation_rcp85_land-gcm_global_60km_01_mon_189912-209911.nc")
Stk_precip_thai <- Stk_precip_thai %>% crop(Shp_thai)
plot(Stk_precip_thai[[1:12]])
print(Stk_precip_thai[[2]])
summary(Stk_precip_thai)
print(Stk_precip_thai@z[[1]])

dates <- as.array(Stk_precip_thai@z)
dates <- dates$time[dates$time != "00"]

dates <- as.Date(dates, format="%Y-%m-%d")

Stk_precip_thai <- setZ(Stk_precip_thai, as.Date(dates))

raspt_thai <- rasterToPoints(Stk_precip_thai)
dt_thai <- data_frame(Layer = names(Stk_precip_thai), dttm = as.Date(getZ(Stk_precip_thai)))
raspt_thai2 <- raspt_thai %>%
  as_data_frame() %>%
  rename(lon = x, lat = y) %>%
  gather(Layer, value, -lon, -lat) %>%
  left_join(dt_thai, by = "Layer") %>%
  dplyr::select(lon, lat, dttm, value)
colnames(raspt_thai2)
raspt_thai2 <- raspt_thai2 %>% rename("x" = "lon") 
raspt_thai2 <- raspt_thai2 %>% rename("y" = "lat")

dates <- raspt_thai2$dttm
str(dates)
raspt_thai2$month <- month(dates)
raspt_thai2$year <- year(dates)
raspt_thai2$dttm <- NULL
raspt_thai2 <- raspt_thai2 %>% rename("precip_mm" = "value")
# convert from cm to mm
raspt_thai2$precip_mm <- raspt_thai2$precip_mm*10

## for now we're stuck on the first step but we might as well get the code down

Stk_temp_thai <- brick("~/Desktop/Global Weather/Model 1/tempmean_rcp85_land-gcm_global_60km_01_mon_189912-209911.nc")
Stk_temp_thai <- Stk_temp_thai %>% crop(Shp_thai)
plot(Stk_temp_thai[[1:12]])
print(Stk_temp_thai[[1]])
summary(Stk_temp_thai)

dates <- as.array(Stk_temp_thai@z)
dates <- dates$time[dates$time != "00"]

dates <- as.Date(dates, format="%Y-%m-%d")

Stk_temp_thai <- setZ(Stk_temp_thai, as.Date(dates))

thai_temp <- rasterToPoints(Stk_temp_thai)
dtt <- data_frame(Layer = names(Stk_temp_thai), dttm = as.Date(getZ(Stk_temp_thai)))
thai_temp2 <- thai_temp %>%
  as_data_frame() %>%
  rename(lon = x, lat = y) %>%
  gather(Layer, value, -lon, -lat) %>%
  left_join(dtt, by = "Layer") %>%
  dplyr::select(lon, lat, dttm, value)
colnames(thai_temp2)
thai_temp2 <- thai_temp2 %>% rename("x" = "lon") 
thai_temp2 <- thai_temp2 %>% rename("y" = "lat")

dates <- thai_temp2$dttm
str(dates)
thai_temp2$month <- month(dates)
thai_temp2$year <- year(dates)
thai_temp2$dttm <- NULL
thai_temp2$precip_mm <- NULL
thai_temp2 <- thai_temp2 %>% rename("temp_centigrade" = "value")

Thai_weather <- merge(thai_temp2,raspt_thai2,by=c('x', 'y', 'month', 'year'))

# and now for evapotranspiration

Stk_evp_thai <- brick("~/Desktop/Global Weather/Model 1/evspsbl_rcp85_land-gcm_global_60km_01_mon_189912-209911.nc")
Stk_evp_thai <- Stk_evp_thai %>% crop(Shp_thai)

plot(Stk_evp_thai[[1:12]])
print(Stk_evp_thai[[1]])
summary(Stk_evp_thai)

dates <- as.array(Stk_evp_thai@z)
dates <- dates$time[dates$time != "00"]

dates <- as.Date(dates, format="%Y-%m-%d")

Stk_evp_thai <- setZ(Stk_evp_thai, as.Date(dates))

thai_evp <- rasterToPoints(Stk_evp_thai)
dttt <- data_frame(Layer = names(Stk_evp_thai), dttm = as.Date(getZ(Stk_evp_thai)))
thai_evp2 <- thai_evp %>%
  as_data_frame() %>%
  rename(lon = x, lat = y) %>%
  gather(Layer, value, -lon, -lat) %>%
  left_join(dttt, by = "Layer") %>%
  dplyr::select(lon, lat, dttm, value)
colnames(thai_evp2)
thai_evp2 <- thai_evp2 %>% rename("x" = "lon") 
thai_evp2 <- thai_evp2 %>% rename("y" = "lat")

dates <- thai_evp2$dttm
str(dates)
thai_evp2$month <- month(dates)
thai_evp2$year <- year(dates)
thai_evp2$dttm <- NULL
thai_evp2$precip_mm <- NULL
thai_evp2 <- thai_evp2 %>% rename("pet_mm" = "value")

# I think these values are in meters so let's convert them to mm
thai_evp2$pet_mm <- thai_evp2$pet_mm*1000

Thai_weather <- merge(Thai_weather,thai_evp2,by=c('x', 'y', 'month', 'year'))
Thai_weather$pet_mm.x <- NULL
Thai_weather$pet_mm.y <- NULL

raspt_thai2$temp_centigrade <- NULL

Thai_weather <- Thai_weather %>%
  group_nest(x, y)

Thai_weather$cell_no <- c(1:270)
Thai_weather <- Thai_weather[, c(1, 2, 4, 3)]
Thai_weather <- Thai_weather %>% mutate(filtered = map(data, ~ filter(., year >= 1961)))
Thai_weather$data <- NULL
saveRDS(Thai_weather, "~/Desktop/Diss-data/Thailand/Thai_weather.rds")
