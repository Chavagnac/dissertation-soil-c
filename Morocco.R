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

# Now barley

# Now maize

# Now sorghum

# Now rice

# Now repeat with area where they are harvested 
# First wheat

# Now barley

# Now maize

# Now sorghum

# Now rice

# Now let's visualise the sand percentage over the country


# Now FAO stat data for yields in Morocco

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
