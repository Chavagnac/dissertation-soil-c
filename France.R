

install.packages("rio")
install.packages("rgdal")
install.packages("sp")
install.packages("ncdf")
install.packages("chron")
install.packages("ncdf4")
install.packages("rasterVis")
install.packages("gpclib")
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


# First we start off with visualising the yield over France

list.files('~/Desktop/Diss-data/France/Shapefiles', pattern='\\.shp$')
file.exists('~/Desktop/Diss-data/France/Shapefiles/Export_Output_3.shp')
fr_shp <- readOGR(dsn=path.expand("~/Desktop/Diss-data/France/Shapefiles/Export_Output_3.shp"))
afg <- fr_shp@polygons
shape_fr <- as.data.frame(afg[[1]]@Polygons[[1]]@coords)
plot(fr_shp, col=NA)


fbar <- "France/Total_Yield_kgha/Barley/barley-france.tif"
france_barley <- raster(fbar)
plot(france_barley)
plot(fr_shp, add=T)


fra_barley_df <- as.data.frame(france_barley, xy=TRUE)
fra_barley_df$barley.france <- fra_barley_df$barley.france/1000
ggplot(data = fra_barley_df) +
  geom_raster(aes(x = x, y = y, fill = barley.france)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shape_fr, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Barley Yield (tonne ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))


fmaiz <- "France/Total_Yield_kgha/Maize/France_maize.tif"
france_maize <- raster(fmaiz)
plot(france_maize)
plot(fr_shp, add=T)
fra_maize_df <- as.data.frame(france_maize, xy=TRUE)
fra_maize_df$France_maize <- fra_maize_df$France_maize/1000
ggplot(data = fra_maize_df) +
  geom_raster(aes(x = x, y = y, fill = France_maize)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shape_fr, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Maize Yield (tonne ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))


fpot <- "France/Total_Yield_kgha/Potatoes/France_potato.tif"
france_potato <- raster(fpot)
plot(france_potato)
plot(fr_shp, add=T)
fra_potat_df <- as.data.frame(france_potato, xy=TRUE)
fra_potat_df$France_potato <- fra_potat_df$France_potato/1000
ggplot(data = fra_potat_df) +
  geom_raster(aes(x = x, y = y, fill = France_potato)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shape_fr, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Potato Yield (tonne ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))


frap <- "France/Total_Yield_kgha/Rapeseed/France_rape.tif"
france_rapeseed <- raster(frap)
plot(france_rapeseed)
plot(fr_shp, add=T)
fra_rape_df <- as.data.frame(france_rapeseed, xy=TRUE)
fra_rape_df$France_rape <- fra_rape_df$France_rape/1000
ggplot(data = fra_rape_df) +
  geom_raster(aes(x = x, y = y, fill = France_rape)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shape_fr, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Rapeseed Yield (tonne ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))


fsug <- "France/Total_Yield_kgha/Sugarbeet/sugarbeet-france.tif"
france_beet <- raster(fsug)
plot(france_beet)
plot(fr_shp, add=T)
fra_beet_df <- as.data.frame(france_beet, xy=TRUE)
fra_beet_df$sugarbeet.france <- fra_beet_df$sugarbeet.france/1000
ggplot(data = fra_beet_df) +
  geom_raster(aes(x = x, y = y, fill = sugarbeet.france)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shape_fr, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Sugarbeet Yield \n(tonne ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))


fwheat <- "France/Total_Yield_kgha/Wheat/France_wheat.tif"
france_wheat <- raster(fwheat)
plot(france_wheat)
plot(fr_shp, add=T)
fra_wheat_df <- as.data.frame(france_wheat, xy=TRUE)
fra_wheat_df$France_wheat <- fra_wheat_df$France_wheat/1000
ggplot(data = fra_wheat_df) +
  geom_raster(aes(x = x, y = y, fill = France_wheat)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shape_fr, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Wheat Yield (tonne ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

# Now the same for area of harvest

fbar_A <- "France/Area/Barley/fr_barley_area.tif"
france_barley_area <- raster(fbar_A)
plot(france_barley_area)
plot(fr_shp, add=T)

fra_barley_df_a <- as.data.frame(france_barley_area, xy=TRUE)
ggplot(data = fra_barley_df_a) +
  geom_raster(aes(x = x, y = y, fill = fr_barley_area)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shape_fr, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Barley Harvested \nArea (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))


fmaizA <- "France/Area/Maize/fr_maize_area.tif"
france_maize_area <- raster(fmaizA)
plot(france_maize_area)
plot(fr_shp, add=T)
fra_maize_df_a <- as.data.frame(france_maize_area, xy=TRUE)

ggplot(data = fra_maize_df_a) +
  geom_raster(aes(x = x, y = y, fill = fr_maize_area)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shape_fr, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Maize Harvested \nArea (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

fpotA <- "France/Area/Potatoes/fr_potato_area.tif"
france_potato_area <- raster(fpotA)
plot(france_potato_area)
plot(fr_shp, add=T)
fra_potat_df_a <- as.data.frame(france_potato_area, xy=TRUE)

ggplot(data = fra_potat_df_a) +
  geom_raster(aes(x = x, y = y, fill = fr_potato_area)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shape_fr, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Potato Harvested \nArea (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

frapA <- "France/Area/Rapeseed/fr_rape_area.tif"
france_rapeseed_area <- raster(frapA)
plot(france_rapeseed_area)
plot(fr_shp, add=T)
fra_rape_df_a <- as.data.frame(france_rapeseed_area, xy=TRUE)

ggplot(data = fra_rape_df_a) +
  geom_raster(aes(x = x, y = y, fill = fr_rape_area)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shape_fr, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Rapeseed Harvested \nArea (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))


fsugA <- "France/Area/Sugarbeet/fr_sugbeet_area.tif"
france_beet_area <- raster(fsugA)
plot(france_beet_area)
plot(fr_shp, add=T)
fra_beet_df_a <- as.data.frame(france_beet_area, xy=TRUE)

ggplot(data = fra_beet_df_a) +
  geom_raster(aes(x = x, y = y, fill = fr_sugbeet_area)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shape_fr, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Sugarbeet \nHarvested \nArea (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

fwheatA <- "France/Area/Wheat/fr_wheat_area.tif"
france_wheat_a <- raster(fwheatA)
plot(france_wheat_a)
plot(fr_shp, add=T)
fra_wheat_df_a <- as.data.frame(france_wheat_a, xy=TRUE)

ggplot(data = fra_wheat_df_a) +
  geom_raster(aes(x = x, y = y, fill = fr_wheat_area)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shape_fr, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Wheat Harvested \nArea (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

# Lets add the sand percentage in the mix now
Fr_sand <- "~/Desktop/Diss-data/France/Sand/Fr_sand.tif"
Fr_sand <- raster(Fr_sand)
plot(Fr_sand)
plot(Shp_Frr, add=T)

### OK now for the big guns. We are downloading the FAOstat time series data for 
# area and yields in france since idk how long but a long long time!! This is the 
# moment where we freak - there will be another freak when we get to temp and precipitation.
# Sand however will be easy so that will be the congrats you made it moment.

import("~/Desktop/Diss-data/France/France_crop_1961_2018.csv")
convert("~/Desktop/Diss-data/France/France_crop_1961_2018.csv", "~/Desktop/Diss-data/France/France_crop_1961_2018.rds")
import("~/Desktop/Diss-data/France/France_crop_1961_2018.rds")
Dat_faostat_fr <- read_rds("~/Desktop/Diss-data/France/France_crop_1961_2018.rds")


## Well that was much easier than expected... Now onto the next bit - temp and precipitation

## Now precipitation as a dataframe tibble.

Stk_precip <- brick("~/Desktop/Global Weather/Model 1/precipitation_rcp85_land-gcm_global_60km_01_mon_189912-209911.nc")
Shp_Frr <- shapefile("~/Desktop/Diss-data/France/Shapefiles/Export_Output_3.shp")
Stk_precip <- Stk_precip %>% crop(Shp_Frr)
plot(Stk_precip[[1:12]])
print(Stk_precip[[2]])
summary(Stk_precip)

dates <- as.array(Stk_precip@z)
dates <- dates$time[dates$time != "00"]

dates <- as.Date(dates, format="%Y-%m-%d")
Stk_precip <- setZ(Stk_precip, as.Date(dates))

raspt <- rasterToPoints(Stk_precip)
dt <- tibble(Layer = names(Stk_precip), dttm = getZ(Stk_precip))
raspt2 <- raspt %>%
  as_data_frame() %>%
  rename(lon = x, lat = y) %>%
  gather(Layer, value, -lon, -lat) %>%
  left_join(dt, by = "Layer") %>%
  dplyr::select(lon, lat, dttm, value)
colnames(raspt2)
raspt2 <- raspt2 %>% rename("x" = "lon") 
raspt2 <- raspt2 %>% rename("y" = "lat")

dates <- as.Date(raspt2$dttm)
str(dates)
raspt2$month <- month(dates)
raspt2$year <- year(dates)
raspt2$dttm <- NULL
raspt2 <- raspt2 %>% rename("precip_mm" = "value")
# Convert from cm to mm
raspt2$precip_mm <- raspt2$precip_mm*10
raspt2$temp_centigrade <- NULL

# Now for temperature

Stk_temp <- brick("~/Desktop/Global Weather/Model 1/tempmean_rcp85_land-gcm_global_60km_01_mon_189912-209911.nc")
Stk_temp <- Stk_temp %>% crop(Shp_Frr)
plot(Stk_temp[[1:12]])
print(Stk_temp[[2]])
summary(Stk_temp)

dates <- as.array(Stk_temp@z)
dates <- dates$time[dates$time != "00"]

dates <- as.Date(dates, format="%Y-%m-%d")
Stk_temp <- setZ(Stk_temp, as.Date(dates))

fr_temp <- rasterToPoints(Stk_temp)
dtt <- data_frame(Layer = names(Stk_temp), dttm = as.Date(getZ(Stk_temp)))
fr_temp2 <- fr_temp %>%
  as_data_frame() %>%
  rename(lon = x, lat = y) %>%
  gather(Layer, value, -lon, -lat) %>%
  left_join(dtt, by = "Layer") %>%
  dplyr::select(lon, lat, dttm, value)
colnames(fr_temp2)
fr_temp2 <- fr_temp2 %>% rename("x" = "lon") 
fr_temp2 <- fr_temp2 %>% rename("y" = "lat")

dates <- fr_temp2$dttm
str(dates)
fr_temp2$month <- month(dates)
fr_temp2$year <- year(dates)
fr_temp2$dttm <- NULL
fr_temp2$precip_mm <- NULL
fr_temp2 <- fr_temp2 %>% rename("temp_centigrade" = "value")

France_weather <- merge(fr_temp2,raspt2,by=c('x', 'y', 'month', 'year'))

# Now for evapotranspiration

Stk_evp <- brick("~/Desktop/Global Weather/Model 1/evspsbl_rcp85_land-gcm_global_60km_01_mon_189912-209911.nc")
Stk_evp <- Stk_evp %>% crop(Shp_Frr)

plot(Stk_evp[[1:12]])
print(Stk_evp[[2]])
summary(Stk_evp)

dates <- as.array(Stk_evp@z)
dates <- dates$time[dates$time != "00"]

dates <- as.Date(dates, format="%Y-%m-%d")

Stk_evp <- setZ(Stk_evp, as.Date(dates))

fr_evp <- rasterToPoints(Stk_evp)
Stk_evp <- dropLayer(Stk_evp, "band")
dttt <- data_frame(Layer = names(Stk_evp), dttm = as.Date(getZ(Stk_evp)))
fr_evp2 <- fr_evp %>%
  as_data_frame() %>%
  rename(lon = x, lat = y) %>%
  gather(Layer, value, -lon, -lat) %>%
  left_join(dttt, by = "Layer") %>%
  dplyr::select(lon, lat, dttm, value)
colnames(fr_evp2)
fr_evp2 <- fr_evp2 %>% rename("x" = "lon") 
fr_evp2 <- fr_evp2 %>% rename("y" = "lat")

dates <- fr_evp2$dttm
str(dates)
fr_evp2$month <- month(dates)
fr_evp2$year <- year(dates)
fr_evp2$dttm <- NULL
fr_evp2$precip_mm <- NULL
fr_evp2 <- fr_evp2 %>% rename("pet_mm" = "value")

# I think these values are in meters so let's convert them to mm
fr_evp2$pet_mm <- fr_evp2$pet_mm*1000

France_weather <- merge(France_weather,fr_evp2,by=c('x', 'y', 'month', 'year'))
France_weather$pet_mm.x <- NULL
France_weather$pet_mm.y <- NULL
France_weather <- France_weather %>% mutate(adjusted_pr = case_when(
  month == 1~ as.numeric(precip_mm)*31,
  month == 3~ as.numeric(precip_mm)*31,
  month ==5~ as.numeric(precip_mm)*31,
  month ==7~ as.numeric(precip_mm)*31,
  month ==8~ as.numeric(precip_mm)*31,
  month ==10~ as.numeric(precip_mm)*31,
  month ==12 ~ as.numeric(precip_mm)*31,
  month == 4~ as.numeric(precip_mm)*30,
  month ==6~ as.numeric(precip_mm)*30,
  month ==9~ as.numeric(precip_mm)*30,
  month ==11 ~ as.numeric(precip_mm)*30, 
  month==2~ as.numeric(precip_mm)*28 ))
# OK we have merged everything together!!! Now let's make it a nested tibble
# THIS HAS NOT BEEN PROVED TO WORK SO WE'RE NOT TOUCHING IT UNTIL WE CAN FIGURE OUT AND UPLOAD PET_MM!!!
France_weather <- France_weather %>%
  group_nest(x, y)

France_weather$cell_no <- c(1:306)
France_weather <- France_weather[, c(1, 2, 4, 3)]
print(France_weather[[4]][[1]])
France_weather <- France_weather %>% mutate(filtered = map(data, ~ filter(., year >= 1961)))
France_weather$data <- NULL

saveRDS(France_weather, "~/Desktop/Diss-data/France/France_weather.rds")

