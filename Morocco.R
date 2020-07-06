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
  geom_hline(yintercept=30, linetype="dashed")+
  geom_vline(xintercept=-9, linetype="dashed")+
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
  geom_hline(yintercept=30, linetype="dashed")+
  geom_vline(xintercept=-9, linetype="dashed")+
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
  geom_hline(yintercept=30, linetype="dashed")+
  geom_vline(xintercept=-9, linetype="dashed")+
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
  geom_hline(yintercept=30, linetype="dashed")+
  geom_vline(xintercept=-9.4, linetype="dashed")+
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
  geom_hline(yintercept=30, linetype="dashed")+
  geom_vline(xintercept=-9, linetype="dashed")+
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

# now sugarbeet
Shp_Mor <- shapefile("~/Desktop/Diss-data/Morocco/Shapefiles/Morocco_shp.shp")
shp_Mor <- as.data.frame(Shp_Mor@polygons[[1]]@Polygons[[1]]@coords)
mbeet <- "~/Desktop/Diss-data/Morocco/Total_Yield_kgha/Sugarbeet/Mor_sugarbeet_yield.tif"
mor_beet <- raster(mbeet)
plot(mor_beet)
plot(Shp_Mor, add=T)

mor_beet_df <- as.data.frame(mor_beet, xy=TRUE)
mor_beet_df$Mor_sugarbeet_yield <- mor_beet_df$Mor_sugarbeet_yield/1000
ggplot(data = mor_beet_df) +
  geom_raster(aes(x = x, y = y, fill = Mor_sugarbeet_yield)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x= V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Sugarbeet Yield \n(tonne ha-1)") +
  geom_hline(yintercept=30, linetype="dashed")+
  geom_vline(xintercept=-9, linetype="dashed")+
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

# Now sugarcane
Shp_Mor <- shapefile("~/Desktop/Diss-data/Morocco/Shapefiles/Morocco_shp.shp")
shp_Mor <- as.data.frame(Shp_Mor@polygons[[1]]@Polygons[[1]]@coords)
mcane <- "~/Desktop/Diss-data/Morocco/Total_Yield_kgha/Sugarcane/Mor_sugarcane_yield.tif"
mor_cane <- raster(mcane)
plot(mor_cane)
plot(Shp_Mor, add=T)

mor_cane_df <- as.data.frame(mor_cane, xy=TRUE)
mor_cane_df$Mor_sugarcane_yield <- mor_cane_df$Mor_sugarcane_yield/1000
ggplot(data = mor_cane_df) +
  geom_raster(aes(x = x, y = y, fill = Mor_sugarcane_yield)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x= V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Sugarcane Yield \n(tonne ha-1)") +
  geom_hline(yintercept=30, linetype="dashed")+
  geom_vline(xintercept=-9, linetype="dashed")+
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))


# Now potatoes
Shp_Mor <- shapefile("~/Desktop/Diss-data/Morocco/Shapefiles/Morocco_shp.shp")
shp_Mor <- as.data.frame(Shp_Mor@polygons[[1]]@Polygons[[1]]@coords)
mpot <- "~/Desktop/Diss-data/Morocco/Total_Yield_kgha/Potatoes/Mor_pota_yield.tif"
mor_pot <- raster(mpot)
plot(mor_pot)
plot(Shp_Mor, add=T)

mor_pot_df <- as.data.frame(mor_pot, xy=TRUE)
mor_pot_df$Mor_pota_yield <- mor_pot_df$Mor_pota_yield/1000
ggplot(data = mor_pot_df) +
  geom_raster(aes(x = x, y = y, fill = Mor_pota_yield)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x= V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Potato Yield \n(tonne ha-1)") +
  geom_hline(yintercept=30, linetype="dashed")+
  geom_vline(xintercept=-9, linetype="dashed")+
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

# Now soybean

Shp_Mor <- shapefile("~/Desktop/Diss-data/Morocco/Shapefiles/Morocco_shp.shp")
shp_Mor <- as.data.frame(Shp_Mor@polygons[[1]]@Polygons[[1]]@coords)
msoy <- "~/Desktop/Diss-data/Morocco/Total_Yield_kgha/Soybean/Mor_soybean_yield.tif"
mor_soy <- raster(msoy)
plot(mor_soy)
plot(Shp_Mor, add=T)

mor_soy_df <- as.data.frame(mor_soy, xy=TRUE)
mor_soy_df$Mor_soybean_yield <- mor_soy_df$Mor_soybean_yield/1000
ggplot(data = mor_soy_df) +
  geom_raster(aes(x = x, y = y, fill = Mor_soybean_yield)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x= V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Soybean Yield \n(tonne ha-1)") +
  geom_hline(yintercept=30, linetype="dashed")+
  geom_vline(xintercept=-9, linetype="dashed")+
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))


# Now pearl millet

mmil <- "~/Desktop/Diss-data/Morocco/Total_Yield_kgha/Millet/Mor_millet_yield.tif"
mor_mil <- raster(mmil)
plot(mor_mil)
plot(Shp_Mor, add=T)

mor_mil_df <- as.data.frame(mor_mil, xy=TRUE)
mor_mil_df$Mor_millet_yield <- mor_mil_df$Mor_millet_yield/1000
ggplot(data = mor_mil_df) +
  geom_raster(aes(x = x, y = y, fill = Mor_millet_yield)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x= V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Pearl Millet \nYield \n(tonne ha-1)") +
  geom_hline(yintercept=30, linetype="dashed")+
  geom_vline(xintercept=-9, linetype="dashed")+
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

# Now Sugarbeet
mbeet_area <- "~/Desktop/Diss-data/Morocco/Area/Sugarbeet/Mor_sugarbeet.tif"
mor_beet_area <- raster(mbeet_area)
plot(mor_beet_area)
plot(Shp_Mor, add=T)

mor_beet_area_df <- as.data.frame(mor_beet_area, xy=TRUE)
mor_beet_area_df$Mor_sugarbeet <- mor_beet_area_df$Mor_sugarbeet/1000
ggplot(data = mor_beet_area_df) +
  geom_raster(aes(x = x, y = y, fill = Mor_sugarbeet)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x= V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Sugarbeet Harvested \nArea (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))


# Now sugarcane
mcane_area <- "~/Desktop/Diss-data/Morocco/Area/Sugarcane/Mor_sugarcane.tif"
mor_cane_area <- raster(mcane_area)
plot(mor_cane_area)
plot(Shp_Mor, add=T)

mor_cane_area_df <- as.data.frame(mor_cane_area, xy=TRUE)
ggplot(data = mor_cane_area_df) +
  geom_raster(aes(x = x, y = y, fill = Mor_sugarcane)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x= V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Sugarcane Harvested \nArea (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))


# Now Potatoes
mpot_area <- "~/Desktop/Diss-data/Morocco/Area/Potatoes/Mor_potato.tif"
mor_pot_area <- raster(mpot_area)
plot(mor_cane_area)
plot(Shp_Mor, add=T)

mor_pot_area_df <- as.data.frame(mor_pot_area, xy=TRUE)
ggplot(data = mor_pot_area_df) +
  geom_raster(aes(x = x, y = y, fill = Mor_potato)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x= V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Potato Harvested \nArea (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))



# Now let's visualise the sand percentage over the country
Mor_sand <- "~/Desktop/Diss-data/Morocco/Sand/Mor_sand.tif"
Mor_sand <- raster(Mor_sand)
plot(Mor_sand)
plot(Shp_Mor, add=T)
sand_Mor <- rasterToPoints(Mor_sand)
sand_Mor2 <- as.data.frame(sand_Mor)

ggplot(data = sand_Mor2) +
  geom_raster(aes(x = x, y = y, fill = Mor_sand)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x= V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Soil Sand \nComposition (%)") +
  geom_hline(yintercept=30.27779, linetype="dashed")+
  geom_vline(xintercept=-9.58333, linetype="dashed")
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))


view(sand_Mor2$x==-9.583333, sand_Mor2$y==29.72223)
	

# Now FAO stat data for yields in Morocco
#import("~/Desktop/Diss-data/Morocco/Morocco_crop_1961_2018.csv")
#convert("~/Desktop/Diss-data/Morocco/Morocco_crop_1961_2018.csv", "~/Desktop/Diss-data/Morocco/Morocco_crop_1961_2018.rds")
#import("~/Desktop/Diss-data/Morocco/Morocco_crop_1961_2018.rds")
#Dat_faostat_mor <- read_rds("~/Desktop/Diss-data/Morocco/Morocco_crop_1961_2018.rds")




# And now for precipitation data

#Stk_precip_Mor <- brick("~/Desktop/Global Weather/Model 1/precipitation_rcp85_land-gcm_global_60km_01_mon_189912-209911.nc")
#Shp_Mor <- shapefile("~/Desktop/Diss-data/Morocco/Shapefiles/Morocco_shp.shp")
#Stk_precip_Mor <- Stk_precip_Mor %>% crop(Shp_Mor)
#plot(Stk_precip_Mor[[1:12]])
#print(Stk_precip_Mor[[2]])
#summary(Stk_precip_Mor)

#dates <- as.array(Stk_precip_Mor@z)
#dates <- dates$time[dates$time != "00"]

#dates <- as.Date(dates, format="%Y-%m-%d")
#Stk_precip_Mor <- setZ(Stk_precip_Mor, as.Date(dates))

#raspt_Mor <- rasterToPoints(Stk_precip_Mor)
#dt_Mor <- tibble(Layer = names(Stk_precip_Mor), dttm = getZ(Stk_precip_Mor))
#raspt_Mor2 <- raspt_Mor %>%
#  as_data_frame() %>%
#  rename(lon = x, lat = y) %>%
#  gather(Layer, value, -lon, -lat) %>%
#  left_join(dt_Mor, by = "Layer") %>%
#  dplyr::select(lon, lat, dttm, value)
#colnames(raspt_Mor2)
#raspt_Mor2 <- raspt_Mor2 %>% rename("x" = "lon") 
#raspt_Mor2 <- raspt_Mor2 %>% rename("y" = "lat")

#dates <- as.Date(raspt_Mor2$dttm)
#str(dates)
#raspt_Mor2$month <- month(dates)
#raspt_Mor2$year <- year(dates)
#raspt_Mor2$dttm <- NULL
#raspt_Mor2 <- raspt_Mor2 %>% rename("precip_mm" = "value")

#raspt_Mor2$temp_centigrade <- NULL

# Let do that again with temperature

#Stk_temp_Mor <- brick("~/Desktop/Global Weather/Model 1/tempmean_rcp85_land-gcm_global_60km_01_mon_189912-209911.nc")
#Stk_temp_Mor <- Stk_temp_Mor %>% crop(Shp_Mor)
#plot(Stk_temp_Mor[[1:12]])
#print(Stk_temp_Mor[[2]])
#summary(Stk_temp_Mor)

#dates <- as.array(Stk_temp_Mor@z)
#dates <- dates$time[dates$time != "00"]

#dates <- as.Date(dates, format="%Y-%m-%d")
#Stk_temp_Mor <- setZ(Stk_temp_Mor, as.Date(dates))

#temp_Mor <- rasterToPoints(Stk_temp_Mor)
#dtt_Mor <- data_frame(Layer = names(Stk_temp_Mor), dttm = as.Date(getZ(Stk_temp_Mor)))
#temp_Mor2 <- temp_Mor %>%
#  as_data_frame() %>%
#  rename(lon = x, lat = y) %>%
#  gather(Layer, value, -lon, -lat) %>%
#  left_join(dtt_Mor, by = "Layer") %>%
#  dplyr::select(lon, lat, dttm, value)
#colnames(temp_Mor2)
#temp_Mor2 <- temp_Mor2 %>% rename("x" = "lon") 
#temp_Mor2 <- temp_Mor2 %>% rename("y" = "lat")

#dates <- temp_Mor2$dttm
#str(dates)
#temp_Mor2$month <- month(dates)
#temp_Mor2$year <- year(dates)
#temp_Mor2$dttm <- NULL
#temp_Mor2$precip_mm <- NULL
#temp_Mor2 <- temp_Mor2 %>% rename("temp_centigrade" = "value")

#Mor_weather <- merge(temp_Mor2,raspt_Mor2,by=c('x', 'y', 'month', 'year'))

# Let's try alasdair's method for evapotranspiration
#install.packages("insol")
#library(insol)
#library(raster)
#library(tidyverse)
#library(ncdf4)
#library(lubridate)

#lats <- Mor_weather %>%
#  pull(y) %>%
#  unique()
#
##  mutate(month = month(date),
#jdays <- tibble(date = seq(from = ymd("2019-01-01"), to = ymd("2019-12-31"), by = as.difftime(days(1)))) %>%
#         jday = yday(date),
#         mday = days_in_month(date)) %>%
#  group_by(month) %>%
#  summarise(mday = mean(mday),
#            jday = mean(jday))
#

#daylength <- tibble(y = rep(lats, 12),
#                    month = rep(1:12, each = length(lats))) %>%
#  left_join(jdays, by = "month") %>%
#  mutate(lon = 0,
#         time_zone = 0,
#         daylength = pmap_dbl(list(y, lon, jday, time_zone), function(a, b, c, d){
#           return(daylength(a, b, c, d)[3])
#rtp <- function(x, power){
#         }))
#  sign(x) * abs(x) ^ (power)
#}#

#Mor_weather <- Mor_weather %>%
#  mutate(data_reg = map2(y, data_reg, function(lat, df){
#    df %>%
#      mutate(y = lat) %>%
#      group_by(year) %>%
#      mutate(I = sum(rtp(temp_centigrade / 5, 1.514)),
#             alpha = 675*10^-9 * rtp(I, 3) - 771*10^-7 * rtp(I, 2) + 1792*10^-5 * I + 0.49239,
#             pet_mm = rtp(16 * ((10 * temp_centigrade) / I), alpha)) %>%
#      ungroup() %>%
     # left_join(daylength %>% select(y, month, daylength, mday), by = c("y", "month")) %>%
    #  mutate(pet_mm = pet_mm * daylength / 12 * mday / 30,
   #          pet_mm = ifelse(pet_mm < 1, 1, pet_mm)) %>% # prevents errors with negative PET/div by zero
  #    select(-y, -I, -alpha, -mday, -daylength) %>%
 #     mutate(precip_mm = ifelse(precip_mm < 0, 0, precip_mm)) # # another quick and dirty fix for potential errors - occasional negative precipitation values
#  }))





# Now with evapotranspiration

#Stk_evp_Mor <- brick("~/Desktop/Global Weather/Model 1/evspsbl_rcp85_land-gcm_global_60km_01_mon_189912-209911.nc")
#Stk_evp_Mor <- Stk_evp_Mor %>% crop(Shp_Mor)

#plot(Stk_evp_Mor[[1:12]])
#print(Stk_evp_Mor[[2]])
#summary(Stk_evp_Mor)

#dates <- as.array(Stk_evp_Mor@z)
#dates <- dates$time[dates$time != "00"]
#
#dates <- as.Date(dates, format="%Y-%m-%d")
#
#Stk_evp_Mor <- setZ(Stk_evp_Mor, as.Date(dates))

#evp_Mor <- rasterToPoints(Stk_evp_Mor)
#dttt_Mor <- data_frame(Layer = names(Stk_evp_Mor), dttm = as.Date(getZ(Stk_evp_Mor)))
#evp_Mor2 <- evp_Mor %>%
#  as_data_frame() %>%
#  rename(lon = x, lat = y) %>%
#  gather(Layer, value, -lon, -lat) %>%
#  left_join(dttt_Mor, by = "Layer") %>%
#  dplyr::select(lon, lat, dttm, value)
#colnames(evp_Mor2)
#evp_Mor2 <- evp_Mor2 %>% rename("x" = "lon") 
#evp_Mor2 <- evp_Mor2 %>% rename("y" = "lat")

#dates <- evp_Mor2$dttm
#str(dates)
#evp_Mor2$month <- month(dates)
#evp_Mor2$year <- year(dates)
#evp_Mor2$dttm <- NULL
#evp_Mor2$precip_mm <- NULL
#evp_Mor2 <- evp_Mor2 %>% rename("pet_mm" = "value")

# I think these values are in meters so let's convert them to mm
# evp_Mor2$pet_mm <- evp_Mor2$pet_mm*1000

#Mor_weather <- merge(Mor_weather,evp_Mor2,by=c('x', 'y', 'month', 'year'))
#Mor_weather$pet_mm.x <- NULL
#Mor_weather$pet_mm.y <- NULL
#Mor_weather <- Mor_weather %>% mutate(adjusted_pr = case_when(
#  month == 1~ as.numeric(precip_mm)*31,
#  month == 3~ as.numeric(precip_mm)*31,
#  month ==5~ as.numeric(precip_mm)*31,
#  month ==7~ as.numeric(precip_mm)*31,
#  month ==8~ as.numeric(precip_mm)*31,
#  month ==10~ as.numeric(precip_mm)*31,
#  month ==12 ~ as.numeric(precip_mm)*31,
#  month == 4~ as.numeric(precip_mm)*30,
#  month ==9~ as.numeric(precip_mm)*30,
##  month ==6~ as.numeric(precip_mm)*30,
#  month ==11 ~ as.numeric(precip_mm)*30, 
#  month==2~ as.numeric(precip_mm)*28 ))
#
#Mor_weather <- Mor_weather %>% mutate(adjusted_evp = case_when(
#  month == 1~ as.numeric(pet_mm)*31,
#  month == 3~ as.numeric(pet_mm)*31,
#  month ==5~ as.numeric(pet_mm)*31,
#  month ==7~ as.numeric(pet_mm)*31,
#  month ==8~ as.numeric(pet_mm)*31,
#  month ==10~ as.numeric(pet_mm)*31,
#  month ==12 ~ as.numeric(pet_mm)*31,
#  month == 4~ as.numeric(pet_mm)*30,
#  month ==6~ as.numeric(pet_mm)*30,
#  month ==9~ as.numeric(pet_mm)*30,
#  month ==11 ~ as.numeric(pet_mm)*30, 
#  month==2~ as.numeric(pet_mm)*28 ))
## Put them all together
#Mor_weather <- Mor_weather %>%
#  group_nest(x, y)

#Mor_weather$cell_no <- c(1:560)
#Mor_weather <- Mor_weather[, c(1, 2, 4, 3)]
#print(Mor_weather[[4]][[1]])
#Mor_weather <- Mor_weather %>% mutate(filtered = map(data, ~ filter(., year >= 1961)))
#Mor_weather$data <- NULL


#for (i in Mor_weather$filtered) {
#  if (Mor_weather %>% month == 1 ||Mor_weather %>% month == 3 ||Mor_weather %>% month ==5||Mor_weather %>% month ==7||Mor_weather %>% month ==8||Mor_weather %>% month ==10||Mor_weather %>% month ==12){
#    Mor_weather %>% precip_mm <- Mor_weather %>% precip_mm*31
#  }}

#saveRDS(Mor_weather, "~/Desktop/Diss-data/Morocco/Morocco_weather.rds")

# Now let's make an estimate of crop yields for the next 10 years according to Alasdair's equation
# First wheat
Mor_FAO <- read.csv("~/Desktop/Mor_FAO.csv")
saveRDS(Mor_FAO, "~/Desktop/Diss-data/Morocco/Morocco_FAO.rds")
Dat_faostat_mor <- read_rds("~/Desktop/Diss-data/Morocco/Morocco_FAO.rds")
sample_n(Dat_faostat_mor, 10, replace = F)

Brk_croparea <- read_rds("~/Desktop/Diss-data/Morocco/morocco-crop-area-ha-2010.rds")
Brk_cropyield <- read_rds("~/Desktop/Diss-data/Morocco/morocco-crop-yield-tonnes-per-ha-2010.rds")

crop_type <- "wheat" # name of crop of interest
frac_renew <- 1 / 1 # fraction of crop renewed every year (for e.g. crop renewed every three years, frac_renew = 1 / 3)
frac_remove <- 0.7 # fraction of crop residues removed

manure_type <- "sheep" # type of animal used to produce manure
manure_nrate <- 0 # application rate of manure in kg N per hectare
till_type <- "full" # type of tillage, either full, reduced or zero

sim_start_year <- 1961 # year simulation to start (min = 1961)
sim_end_year <- 2097 ## year simulation to end (max = 2097)

lat_lon <- tibble(x = -9.583333, y = 30.27779) # default chosen here is a high-yielding arable land near Marrackech
temp_cropname <- crop_type %>% str_replace_all("-", "\\.") # necessary adjustment as raster doesn't like hyphens

# extract relevant crop raster from crop bricks
Ras_cropyield <- Brk_cropyield[[which(names(Brk_cropyield) == temp_cropname)]]
Ras_croparea <- Brk_croparea[[which(names(Brk_croparea) == temp_cropname)]]
rm(temp_cropname)

# extract from rasters based on points
yield_tha <- raster::extract(Ras_cropyield, lat_lon)
area_ha <- raster::extract(Ras_croparea, lat_lon)
sand_pc <- raster::extract(Ras_sand, lat_lon)
clim_coord_no <- raster::extract(Ras_clim, lat_lon)

Dat_crop_ts <- Dat_faostat_mor %>% 
  filter(crop == crop_type,
         year >= sim_start_year) %>%
  mutate(yield_rel = (yield_tha / yield_tha[year == 2010]),
         area_rel = (area_harvested / area_harvested[year == 2010])) %>%
  dplyr::select(crop, year, yield_rel, area_rel)

# convert to yields for extracted area
Dat_crop_ts <- Dat_crop_ts %>%
  mutate(yield_tha = yield_rel * yield_tha,
         area_ha = area_rel * area_ha)

# plot
Dat_crop_ts %>%
  ggplot(aes(x = year, y = yield_tha)) +
  geom_line()

# 10 year mean and sd for crop yield
yield_mean <- Dat_crop_ts %>% tail(10) %>% pull(yield_tha) %>% mean()
yield_sd <- Dat_crop_ts %>% tail(10) %>% pull(yield_tha) %>% sd()
area_mean <- Dat_crop_ts %>% tail(10) %>% pull(area_ha) %>% mean()
area_sd <- Dat_crop_ts %>% tail(10) %>% pull(area_ha) %>% sd()

# randomly generated barley yield to 2070 based on 10-year performance
set.seed(260592)
Dat_preds <- tibble(year = 2019:sim_end_year,
                    yield_tha = rnorm(n = length(2019:sim_end_year), mean = yield_mean, sd = yield_sd),
                    area_ha = rnorm(n = length(2019:sim_end_year), mean = area_mean, sd = area_sd))

# bind simulation with historical data
Dat_crop_ts <- bind_rows("historical" = Dat_crop_ts,
                         "simulated" = Dat_preds,
                         .id = "origin")

# plot to check
Mor_wheat_yield <- Dat_crop_ts %>%
  ggplot(aes(x = year, y = yield_tha, colour = origin)) +
  geom_line()

Mor_wheat_area <- data.frame(Dat_crop_ts$origin,Dat_crop_ts$year, Dat_crop_ts$area_ha)
######################################################################
# write out crop and manure data files (MODIFY FILE NAMES AS NEEDED) #
######################################################################

# write out crop data
Dat_crop_ts %>%
  mutate(crop_type = crop_type,
         frac_renew = frac_renew,
         frac_remove = frac_remove,
         till_type = till_type,
         sand_frac = sand_pc / 100) %>%
  dplyr::select(origin, year, crop_type, yield_tha, frac_renew, frac_remove, sand_frac, till_type) %>%
  write_csv("~/Desktop/Diss-data/Morocco/morocco-wheat-crop-data.csv")

# write out manure data
tibble(year = sim_start_year:sim_end_year,
       man_type = manure_type,
       man_nrate = manure_nrate) %>%
  write_csv("~/Desktop/Diss-data/Morocco/morocco-example-manure-data.csv")

## Now barley

Dat_faostat_mor <- read_rds("~/Desktop/Diss-data/Morocco/Morocco_FAO.rds")
sample_n(Dat_faostat_mor, 10, replace = F)

Brk_croparea <- read_rds("~/Desktop/Diss-data/Morocco/morocco-crop-area-ha-2010.rds")
Brk_cropyield <- read_rds("~/Desktop/Diss-data/Morocco/morocco-crop-yield-tonnes-per-ha-2010.rds")

crop_type <- "barley" # name of crop of interest
frac_renew <- 1 / 1 # fraction of crop renewed every year (for e.g. crop renewed every three years, frac_renew = 1 / 3)
frac_remove <- 0.7 # fraction of crop residues removed

manure_type <- "sheep" # type of animal used to produce manure
manure_nrate <- 0 # application rate of manure in kg N per hectare
till_type <- "full" # type of tillage, either full, reduced or zero

sim_start_year <- 1961 # year simulation to start (min = 1961)
sim_end_year <- 2097 ## year simulation to end (max = 2097)

lat_lon <- tibble(x = -9.583333, y = 30.27779) # default chosen here is a high-yielding arable land near Marrackech
temp_cropname <- crop_type %>% str_replace_all("-", "\\.") # necessary adjustment as raster doesn't like hyphens

# extract relevant crop raster from crop bricks
Ras_cropyield <- Brk_cropyield[[which(names(Brk_cropyield) == temp_cropname)]]
Ras_croparea <- Brk_croparea[[which(names(Brk_croparea) == temp_cropname)]]
rm(temp_cropname)

# extract from rasters based on points
yield_tha <- raster::extract(Ras_cropyield, lat_lon)
area_ha <- raster::extract(Ras_croparea, lat_lon)
sand_pc <- raster::extract(Ras_sand, lat_lon)
clim_coord_no <- raster::extract(Ras_clim, lat_lon)

Dat_crop_ts <- Dat_faostat_mor %>% 
  filter(crop == crop_type,
         year >= sim_start_year) %>%
  mutate(yield_rel = (yield_tha / yield_tha[year == 2010]),
         area_rel = (area_harvested / area_harvested[year == 2010])) %>%
  dplyr::select(crop, year, yield_rel, area_rel)

# convert to yields for extracted area
Dat_crop_ts <- Dat_crop_ts %>%
  mutate(yield_tha = yield_rel * yield_tha,
         area_ha = area_rel * area_ha)

# plot
Dat_crop_ts %>%
  ggplot(aes(x = year, y = yield_tha)) +
  geom_line()

# 10 year mean and sd for crop yield
yield_mean <- Dat_crop_ts %>% tail(10) %>% pull(yield_tha) %>% mean()
yield_sd <- Dat_crop_ts %>% tail(10) %>% pull(yield_tha) %>% sd()
area_mean <- Dat_crop_ts %>% tail(10) %>% pull(area_ha) %>% mean()
area_sd <- Dat_crop_ts %>% tail(10) %>% pull(area_ha) %>% sd()

# randomly generated barley yield to 2070 based on 10-year performance
set.seed(260592)
Dat_preds <- tibble(year = 2019:sim_end_year,
                    yield_tha = rnorm(n = length(2019:sim_end_year), mean = yield_mean, sd = yield_sd),
                    area_ha = rnorm(n = length(2019:sim_end_year), mean = area_mean, sd = area_sd))

# bind simulation with historical data
Dat_crop_ts <- bind_rows("historical" = Dat_crop_ts,
                         "simulated" = Dat_pred_clim5preds,
                         .id = "origin")

# plot to check
Mor_barley_yield <- Dat_crop_ts %>%
  ggplot(aes(x = year, y = yield_tha, colour = origin)) +
  geom_line()

Mor_barley_area <- data.frame(Dat_crop_ts$origin,Dat_crop_ts$year, Dat_crop_ts$area_ha)

######################################################################
# write out crop and manure data files (MODIFY FILE NAMES AS NEEDED) #
######################################################################

# write out crop data
Dat_crop_ts %>%
  mutate(crop_type = crop_type,
         frac_renew = frac_renew,
         frac_remove = frac_remove,
         till_type = till_type,
         sand_frac = sand_pc / 100) %>%
  dplyr::select(origin, year, crop_type, yield_tha, frac_renew, frac_remove, sand_frac, till_type) %>%
  write_csv("~/Desktop/Diss-data/Morocco/morocco-barley-crop-data.csv")

## Now maize

Dat_faostat_mor <- read_rds("~/Desktop/Diss-data/Morocco/Morocco_FAO.rds")
sample_n(Dat_faostat_mor, 10, replace = F)

Brk_croparea <- read_rds("~/Desktop/Diss-data/Morocco/morocco-crop-area-ha-2010.rds")
Brk_cropyield <- read_rds("~/Desktop/Diss-data/Morocco/morocco-crop-yield-tonnes-per-ha-2010.rds")

crop_type <- "maize" # name of crop of interest
frac_renew <- 1 / 1 # fraction of crop renewed every year (for e.g. crop renewed every three years, frac_renew = 1 / 3)
frac_remove <- 0.7 # fraction of crop residues removed

manure_type <- "sheep" # type of animal used to produce manure
manure_nrate <- 0 # application rate of manure in kg N per hectare
till_type <- "full" # type of tillage, either full, reduced or zero

sim_start_year <- 1961 # year simulation to start (min = 1961)
sim_end_year <- 2097 ## year simulation to end (max = 2097)

lat_lon <- tibble(x = -9.583333, y = 30.27779) # default chosen here is a high-yielding arable land near Marrackech
temp_cropname <- crop_type %>% str_replace_all("-", "\\.") # necessary adjustment as raster doesn't like hyphens

# extract relevant crop raster from crop bricks
Ras_cropyield <- Brk_cropyield[[which(names(Brk_cropyield) == temp_cropname)]]
Ras_croparea <- Brk_croparea[[which(names(Brk_croparea) == temp_cropname)]]
rm(temp_cropname)

# extract from rasters based on points
yield_tha <- raster::extract(Ras_cropyield, lat_lon)
area_ha <- raster::extract(Ras_croparea, lat_lon)
sand_pc <- raster::extract(Ras_sand, lat_lon)
clim_coord_no <- raster::extract(Ras_clim, lat_lon)

Dat_crop_ts <- Dat_faostat_mor %>% 
  filter(crop == crop_type,
         year >= sim_start_year) %>%
  mutate(yield_rel = (yield_tha / yield_tha[year == 2010]),
         area_rel = (area_harvested / area_harvested[year == 2010])) %>%
  dplyr::select(crop, year, yield_rel, area_rel)

# convert to yields for extracted area
Dat_crop_ts <- Dat_crop_ts %>%
  mutate(yield_tha = yield_rel * yield_tha,
         area_ha = area_rel * area_ha)

# plot
Dat_crop_ts %>%
  ggplot(aes(x = year, y = yield_tha)) +
  geom_line()

# 10 year mean and sd for crop yield
yield_mean <- Dat_crop_ts %>% tail(10) %>% pull(yield_tha) %>% mean()
yield_sd <- Dat_crop_ts %>% tail(10) %>% pull(yield_tha) %>% sd()
area_mean <- Dat_crop_ts %>% tail(10) %>% pull(area_ha) %>% mean()
area_sd <- Dat_crop_ts %>% tail(10) %>% pull(area_ha) %>% sd()

# randomly generated barley yield to 2070 based on 10-year performance
set.seed(260592)
Dat_preds <- tibble(year = 2019:sim_end_year,
                    yield_tha = rnorm(n = length(2019:sim_end_year), mean = yield_mean, sd = yield_sd),
                    area_ha = rnorm(n = length(2019:sim_end_year), mean = area_mean, sd = area_sd))

# bind simulation with historical data
Dat_crop_ts <- bind_rows("historical" = Dat_crop_ts,
                         "simulated" = Dat_preds,
                         .id = "origin")

# plot to check
Mor_maize_yield <- Dat_crop_ts %>%
  ggplot(aes(x = year, y = yield_tha, colour = origin)) +
  geom_line()

Mor_maize_area <- data.frame(Dat_crop_ts$origin,Dat_crop_ts$year, Dat_crop_ts$area_ha)
######################################################################
# write out crop and manure data files (MODIFY FILE NAMES AS NEEDED) #
######################################################################

# write out crop data
Dat_crop_ts %>%
  mutate(crop_type = crop_type,
         frac_renew = frac_renew,
         frac_remove = frac_remove,
         till_type = till_type,
         sand_frac = sand_pc / 100) %>%
  dplyr::select(origin, year, crop_type, yield_tha, frac_renew, frac_remove, sand_frac, till_type) %>%
  write_csv("~/Desktop/Diss-data/Morocco/morocco-maize-crop-data.csv")

## now potato


Dat_faostat_mor <- read_rds("~/Desktop/Diss-data/Morocco/Morocco_FAO.rds")
sample_n(Dat_faostat_mor, 10, replace = F)

Brk_croparea <- read_rds("~/Desktop/Diss-data/Morocco/morocco-crop-area-ha-2010.rds")
Brk_cropyield <- read_rds("~/Desktop/Diss-data/Morocco/morocco-crop-yield-tonnes-per-ha-2010.rds")

crop_type <- "potato" # name of crop of interest
frac_renew <- 1 / 1 # fraction of crop renewed every year (for e.g. crop renewed every three years, frac_renew = 1 / 3)
frac_remove <- 0.7 # fraction of crop residues removed

manure_type <- "sheep" # type of animal used to produce manure
manure_nrate <- 0 # application rate of manure in kg N per hectare
till_type <- "full" # type of tillage, either full, reduced or zero

sim_start_year <- 1961 # year simulation to start (min = 1961)
sim_end_year <- 2097 ## year simulation to end (max = 2097)

lat_lon <- tibble(x = -9.583333, y = 30.27779) # default chosen here is a high-yielding arable land near Marrackech
temp_cropname <- crop_type %>% str_replace_all("-", "\\.") # necessary adjustment as raster doesn't like hyphens

# extract relevant crop raster from crop bricks
Ras_cropyield <- Brk_cropyield[[which(names(Brk_cropyield) == temp_cropname)]]
Ras_croparea <- Brk_croparea[[which(names(Brk_croparea) == temp_cropname)]]
rm(temp_cropname)

# extract from rasters based on points
yield_tha <- raster::extract(Ras_cropyield, lat_lon)
area_ha <- raster::extract(Ras_croparea, lat_lon)
sand_pc <- raster::extract(Ras_sand, lat_lon)
clim_coord_no <- raster::extract(Ras_clim, lat_lon)

Dat_crop_ts <- Dat_faostat_mor %>% 
  filter(crop == crop_type,
         year >= sim_start_year) %>%
  mutate(yield_rel = (yield_tha / yield_tha[year == 2010]),
         area_rel = (area_harvested / area_harvested[year == 2010])) %>%
  dplyr::select(crop, year, yield_rel, area_rel)

# convert to yields for extracted area
Dat_crop_ts <- Dat_crop_ts %>%
  mutate(yield_tha = yield_rel * yield_tha,
         area_ha = area_rel * area_ha)

# plot
Dat_crop_ts %>%
  ggplot(aes(x = year, y = yield_tha)) +
  geom_line()

# 10 year mean and sd for crop yield
yield_mean <- Dat_crop_ts %>% tail(10) %>% pull(yield_tha) %>% mean()
yield_sd <- Dat_crop_ts %>% tail(10) %>% pull(yield_tha) %>% sd()
area_mean <- Dat_crop_ts %>% tail(10) %>% pull(area_ha) %>% mean()
area_sd <- Dat_crop_ts %>% tail(10) %>% pull(area_ha) %>% sd()

# randomly generated barley yield to 2070 based on 10-year performance
set.seed(260592)
Dat_preds <- tibble(year = 2019:sim_end_year,
                    yield_tha = rnorm(n = length(2019:sim_end_year), mean = yield_mean, sd = yield_sd),
                    area_ha = rnorm(n = length(2019:sim_end_year), mean = area_mean, sd = area_sd))

# bind simulation with historical data
Dat_crop_ts <- bind_rows("historical" = Dat_crop_ts,
                         "simulated" = Dat_preds,
                         .id = "origin")

# plot to check
Mor_pot_yield <- Dat_crop_ts %>%
  ggplot(aes(x = year, y = yield_tha, colour = origin)) +
  geom_line()

Mor_pot_area <-data.frame(Dat_crop_ts$origin,Dat_crop_ts$year, Dat_crop_ts$area_ha)

######################################################################
# write out crop and manure data files (MODIFY FILE NAMES AS NEEDED) #
######################################################################

# write out crop data
Dat_crop_ts %>%
  mutate(crop_type = crop_type,
         frac_renew = frac_renew,
         frac_remove = frac_remove,
         till_type = till_type,
         sand_frac = sand_pc / 100) %>%
  dplyr::select(origin, year, crop_type, yield_tha, frac_renew, frac_remove, sand_frac, till_type) %>%
  write_csv("~/Desktop/Diss-data/Morocco/morocco-potato-crop-data.csv")

## Now other pulses


Dat_faostat_mor <- read_rds("~/Desktop/Diss-data/Morocco/Morocco_FAO.rds")
sample_n(Dat_faostat_mor, 10, replace = F)

Brk_croparea <- read_rds("~/Desktop/Diss-data/Morocco/morocco-crop-area-ha-2010.rds")
Brk_cropyield <- read_rds("~/Desktop/Diss-data/Morocco/morocco-crop-yield-tonnes-per-ha-2010.rds")

crop_type <- "other-pulses" # name of crop of interest
frac_renew <- 1 / 1 # fraction of crop renewed every year (for e.g. crop renewed every three years, frac_renew = 1 / 3)
frac_remove <- 0.7 # fraction of crop residues removed

manure_type <- "sheep" # type of animal used to produce manure
manure_nrate <- 0 # application rate of manure in kg N per hectare
till_type <- "full" # type of tillage, either full, reduced or zero

sim_start_year <- 1961 # year simulation to start (min = 1961)
sim_end_year <- 2097 ## year simulation to end (max = 2097)

lat_lon <- tibble(x = -9.583333, y =30.27779) # default chosen here is a high-yielding arable land near Marrackech
temp_cropname <- crop_type %>% str_replace_all("-", "\\.") # necessary adjustment as raster doesn't like hyphens

# extract relevant crop raster from crop bricks
Ras_cropyield <- Brk_cropyield[[which(names(Brk_cropyield) == temp_cropname)]]
Ras_croparea <- Brk_croparea[[which(names(Brk_croparea) == temp_cropname)]]
rm(temp_cropname)

# extract from rasters based on points
yield_tha <- raster::extract(Ras_cropyield, lat_lon)
area_ha <- raster::extract(Ras_croparea, lat_lon)
sand_pc <- raster::extract(Ras_sand, lat_lon)
clim_coord_no <- raster::extract(Ras_clim, lat_lon)

Dat_crop_ts <- Dat_faostat_mor %>% 
  filter(crop == crop_type,
         year >= sim_start_year) %>%
  mutate(yield_rel = (yield_tha / yield_tha[year == 2010]),
         area_rel = (area_harvested / area_harvested[year == 2010])) %>%
  dplyr::select(crop, year, yield_rel, area_rel)

# convert to yields for extracted area
Dat_crop_ts <- Dat_crop_ts %>%
  mutate(yield_tha = yield_rel * yield_tha,
         area_ha = area_rel * area_ha)

# plot
Dat_crop_ts %>%
  ggplot(aes(x = year, y = yield_tha)) +
  geom_line()

# 10 year mean and sd for crop yield
yield_mean <- Dat_crop_ts %>% tail(10) %>% pull(yield_tha) %>% mean()
yield_sd <- Dat_crop_ts %>% tail(10) %>% pull(yield_tha) %>% sd()
area_mean <- Dat_crop_ts %>% tail(10) %>% pull(area_ha) %>% mean()
area_sd <- Dat_crop_ts %>% tail(10) %>% pull(area_ha) %>% sd()

# randomly generated barley yield to 2070 based on 10-year performance
set.seed(260592)
Dat_preds <- tibble(year = 2019:sim_end_year,
                    yield_tha = rnorm(n = length(2019:sim_end_year), mean = yield_mean, sd = yield_sd),
                    area_ha = rnorm(n = length(2019:sim_end_year), mean = area_mean, sd = area_sd))

# bind simulation with historical data
Dat_crop_ts <- bind_rows("historical" = Dat_crop_ts,
                         "simulated" = Dat_preds,
                         .id = "origin")

# plot to check
Mor_pulse_yield <- Dat_crop_ts %>%
  ggplot(aes(x = year, y = yield_tha, colour = origin)) +
  geom_line()

Mor_pulse_area <- data.frame(Dat_crop_ts$origin,Dat_crop_ts$year, Dat_crop_ts$area_ha)

######################################################################
# write out crop and manure data files (MODIFY FILE NAMES AS NEEDED) #
######################################################################

# write out crop data
Dat_crop_ts %>%
  mutate(crop_type = crop_type,
         frac_renew = frac_renew,
         frac_remove = frac_remove,
         till_type = till_type,
         sand_frac = sand_pc / 100) %>%
  dplyr::select(origin, year, crop_type, yield_tha, frac_renew, frac_remove, sand_frac, till_type) %>%
  write_csv("~/Desktop/Diss-data/Morocco/morocco-pulse-crop-data.csv")

tibble(year = sim_start_year:sim_end_year,
       man_type = manure_type,
       man_nrate = manure_nrate) %>%
  write_csv("~/Desktop/Diss-data/Morocco/Manure/morocco-sheep-manure-data.csv")


## Now millet

Dat_faostat_mor <- read_rds("~/Desktop/Diss-data/Morocco/Morocco_FAO.rds")
sample_n(Dat_faostat_mor, 10, replace = F)

Brk_croparea <- read_rds("~/Desktop/Diss-data/Morocco/morocco-crop-area-ha-2010.rds")
Brk_cropyield <- read_rds("~/Desktop/Diss-data/Morocco/morocco-crop-yield-tonnes-per-ha-2010.rds")

crop_type <- "millet" # name of crop of interest
frac_renew <- 1 / 1 # fraction of crop renewed every year (for e.g. crop renewed every three years, frac_renew = 1 / 3)
frac_remove <- 0.7 # fraction of crop residues removed

manure_type <- "sheep" # type of animal used to produce manure
manure_nrate <- 0 # application rate of manure in kg N per hectare
till_type <- "full" # type of tillage, either full, reduced or zero

sim_start_year <- 1961 # year simulation to start (min = 1961)
sim_end_year <- 2097 ## year simulation to end (max = 2097)

lat_lon <- tibble(x = -9.583333, y = 30.27779) # default chosen here is a high-yielding arable land near Marrackech
temp_cropname <- crop_type %>% str_replace_all("-", "\\.") # necessary adjustment as raster doesn't like hyphens

# extract relevant crop raster from crop bricks
Ras_cropyield <- Brk_cropyield[[which(names(Brk_cropyield) == temp_cropname)]]
Ras_croparea <- Brk_croparea[[which(names(Brk_croparea) == temp_cropname)]]
rm(temp_cropname)

# extract from rasters based on points
yield_tha <- raster::extract(Ras_cropyield, lat_lon)
area_ha <- raster::extract(Ras_croparea, lat_lon)
sand_pc <- raster::extract(Ras_sand, lat_lon)
clim_coord_no <- raster::extract(Ras_clim, lat_lon)

Dat_crop_ts <- Dat_faostat_mor %>% 
  filter(crop == crop_type,
         year >= sim_start_year) %>%
  mutate(yield_rel = (yield_tha / yield_tha[year == 2010]),
         area_rel = (area_harvested / area_harvested[year == 2010])) %>%
  dplyr::select(crop, year, yield_rel, area_rel)

# convert to yields for extracted area
Dat_crop_ts <- Dat_crop_ts %>%
  mutate(yield_tha = yield_rel * yield_tha,
         area_ha = area_rel * area_ha)

# plot
Dat_crop_ts %>%
  ggplot(aes(x = year, y = yield_tha)) +
  geom_line()

# 10 year mean and sd for crop yield
yield_mean <- Dat_crop_ts %>% tail(10) %>% pull(yield_tha) %>% mean()
yield_sd <- Dat_crop_ts %>% tail(10) %>% pull(yield_tha) %>% sd()
area_mean <- Dat_crop_ts %>% tail(10) %>% pull(area_ha) %>% mean()
area_sd <- Dat_crop_ts %>% tail(10) %>% pull(area_ha) %>% sd()

# randomly generated barley yield to 2070 based on 10-year performance
set.seed(260592)
Dat_preds <- tibble(year = 2019:sim_end_year,
                    yield_tha = rnorm(n = length(2019:sim_end_year), mean = yield_mean, sd = yield_sd),
                    area_ha = rnorm(n = length(2019:sim_end_year), mean = area_mean, sd = area_sd))

# bind simulation with historical data
Dat_crop_ts <- bind_rows("historical" = Dat_crop_ts,
                         "simulated" = Dat_preds,
                         .id = "origin")

# plot to check
Mor_millet_yield <-  %>% Dat_crop_ts
  ggplot(aes(x = year, y = yield_tha, colour = origin)) +
  geom_line()

Mor_millet_area <- data.frame(Dat_crop_ts$origin,Dat_crop_ts$year, Dat_crop_ts$area_ha)
  
#  Dat_crop_ts %>%
#  ggplot(aes(x = year, y = area_ha, colour = origin)) +
#  geom_line()

######################################################################
# write out crop and manure data files (MODIFY FILE NAMES AS NEEDED) #
######################################################################

# write out crop data
Dat_crop_ts %>%
  mutate(crop_type = crop_type,
         frac_renew = frac_renew,
         frac_remove = frac_remove,
         till_type = till_type,
         sand_frac = sand_pc / 100) %>%
  dplyr::select(origin, year, crop_type, yield_tha, frac_renew, frac_remove, sand_frac, till_type) %>%
  write_csv("~/Desktop/Diss-data/Morocco/morocco-millet-crop-data.csv")

plot(Mor_maize_area)
plot(Mor_millet_area)
plot(Mor_pot_area)
plot(Mor_wheat_area)
plot(Mor_barley_area)

Mor_maize_area_df <- as.data.frame(Mor_maize_area)

area_estimate <- ggplot()+
  geom_line(data=Mor_maize_area, mapping= aes(x=Dat_crop_ts.year, y=Dat_crop_ts.area_ha, color='Maize'))+
  geom_line(data=Mor_millet_area, mapping= aes(x=Dat_crop_ts.year, y=Dat_crop_ts.area_ha, color='Millet'))+
  geom_line(data=Mor_pot_area, mapping= aes(x=Dat_crop_ts.year, y=Dat_crop_ts.area_ha, color='Potato'))+
  geom_line(data=Mor_wheat_area, mapping= aes(x=Dat_crop_ts.year, y=Dat_crop_ts.area_ha, color='Wheat'))+
  geom_line(data=Mor_barley_area, mapping= aes(x=Dat_crop_ts.year, y=Dat_crop_ts.area_ha, color='Barley'))+
  labs(x="Year", y="Area (ha)", color = "Crops") +
  theme(panel.background = element_rect(fill = "white"))+
  theme(axis.line = element_line(color = "black"))+
  scale_color_manual(values = color)
  
area_estimate  
  
  

plot(Mor_maize_yield)
plot(Mor_millet_yield)
plot(Mor_pot_yield)
plot(Mor_barley_yield)
plot(Mor_wheat_yield)

plot_maize_y <- read_csv("~/Desktop/Diss-data/Morocco/morocco-maize-crop-data.csv")
plot_maize_y$yield_tha <- plot_maize_y$yield_tha/1000
plot_barley_y <- read_csv("~/Desktop/Diss-data/Morocco/morocco-barley-crop-data.csv")
plot_barley_y$yield_tha<- plot_barley_y$yield_tha/1000
plot_wheat_y <- read_csv("~/Desktop/Diss-data/Morocco/morocco-wheat-crop-data.csv")
plot_wheat_y$yield_tha<-plot_wheat_y$yield_tha/1000
plot_pot_y <- read_csv("~/Desktop/Diss-data/Morocco/morocco-potato-crop-data.csv")
plot_pot_y$yield_tha<-plot_pot_y$yield_tha/1000
plot_mill_y <- read_csv("~/Desktop/Diss-data/Morocco/morocco-millet-crop-data.csv")
plot_mill_y$yield_tha<-plot_mill_y$yield_tha/1000

color <- c("Maize"="black", "Barley"="blue","Wheat"="red", "Potato"="green", "Millet"='orange')

yield_estimate <- ggplot()+
  geom_line(data=plot_mill_y, mapping= aes(x=year, y=yield_tha, color='Millet'))+
  geom_line(data=plot_maize_y, mapping= aes(x=year, y=yield_tha, color='Maize'))+
  geom_line(data=plot_barley_y, mapping= aes(x=year, y=yield_tha, color='Barley'))+
  geom_line(data=plot_wheat_y, mapping= aes(x=year, y=yield_tha, color='Wheat'))+
  #geom_line(data=plot_pot_y, mapping= aes(x=year, y=yield_tha, color='Potato'))+
  labs(x="Year", y="Yield (t ha-1)", color = "Crops") +
  scale_color_manual(values = color)+
  theme(panel.background = element_rect(fill = "white", colour = "white"))+
  theme(axis.line = element_line(color = "black"))

yield_estimate

#geom_line(data=plot_pot_y, mapping= aes(x=year, y=yield_tha, color='Potato'))

#### NOW WE DO MANURE COMPARISONS
  
manure <- read_csv("~/Desktop/Diss-data/Morocco/Manure/compare-manure.csv")

ggplot()+
  geom_line(data=manure, aes(x=Year, y=Total.Carbon, colour=Manure))+
  labs(x="Year", y="Total Carbon Stock (t ha-1)", fill="Manure Type") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10))+
  theme(panel.background = element_rect(fill = "white", colour = "white"))+
  theme(axis.line = element_line(color = "black"))

# now crops

Crops <- read_csv("~/Desktop/Diss-data/Morocco/Crops/compare-crop.csv")

ggplot()+
  geom_line(data=Crops, aes(x=Year, y=Total.Carbon, colour=Crop))+
  labs(x="Year", y="Total Carbon Stock (t ha-1)", fill="Crop") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10))+
  theme(panel.background = element_rect(fill = "white", colour = "white"))+
  theme(axis.line = element_line(color = "black"))

Till <- read_csv("~/Desktop/Diss-data/Morocco/Crops/compare-till.csv")

ggplot()+
  geom_line(data=Till, aes(x=Year, y=Total.Carbon, colour=PercentageResidueRemoved))+
  labs(x="Year", y="Total Carbon Stock (t ha-1)", colour="Percentage \nResidue \nRemoved \nFrom Field") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10))+
  theme(panel.background = element_rect(fill = "white", colour = "white"))+
  theme(axis.line = element_line(color = "black"))

View(Dat_clim[[4]][[1]])


