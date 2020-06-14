install.packages("rio")
install.packages("rgdal")
install.packages("sp")
library("rio")
library("rgdal")
library("raster")
library("sp")
library("ggplot2")
library(tmap)
library(RColorBrewer)
library(shapefiles)
library(tidyverse)

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


### OK now for the big guns. We are downloading the FAOstat time series data for 
# area and yields in france since idk how long but a long long time!! This is the 
# moment where we freak - there will be another freak when we get to temp and precipitation.
# Sand however will be easy so that will be the congrats you made it moment.

import("France/France_crop_1961_2018.csv")
convert("France/France_crop_1961_2018.csv", "France/France_crop_1961_2018.rds")
import("France/France_crop_1961_2018.rds")
Dat_faostat_fr <- read_rds("France/France_crop_1961_2018.rds")


## Well that was much easier than expected... Now onto the next bit - temp and precipitation
