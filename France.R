

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

# Let's do the same as with morocco but now with Sweden
readdir <- ("~/Desktop/Diss-data/SpamYield")
file.names <- dir(readdir, pattern =".tif")

Crop_yield_stack <- raster::stack()
for(i in 1:length(file.names)){
  readpath <- paste(readdir, file.names[i], sep="/") # aggregate strings to create filepath
  x <- raster(readpath) # read raster
  Crop_yield_stack <- addLayer(Crop_yield_stack, x)
  rm(x)
  print(file.names[i])
}
rm(readdir, file.names, readpath, i)

readdir <- ("~/Desktop/Diss-data/SpamArea")
file.names <- dir(readdir, pattern =".tif")

Crop_area_stack <- raster::stack()
for(i in 1:length(file.names)){
  readpath <- paste(readdir, file.names[i], sep="/") # aggregate strings to create filepath
  x <- raster(readpath) # read raster
  Crop_area_stack <- addLayer(Crop_area_stack, x)
  rm(x)
  print(file.names[i])
}

Shp_sw <- shapefile("~/Desktop/Diss-data/Sweden/Sweden.shp")
shp_sw <- as.data.frame(Shp_sw@polygons[[1]]@Polygons[[1]]@coords)

Crop_area_stack <- Crop_area_stack %>%
  crop(Shp_sw) %>%
  mask(Shp_sw)

Crop_yield_stack <- Crop_yield_stack %>%
  crop(Shp_sw) %>%
  mask(Shp_sw)

plot(Crop_area_stack[[1]])
plot(Shp_sw, add = T)
hist(Crop_area_stack[[1]])

isnt_zero <- function(x){
  y <- x == 0 | is.na(x)
  z <- F %in% y == T
  return(z)
}

Dat_area <- Crop_area_stack %>%
  as.data.frame()
Dat_yield <- Crop_yield_stack%>%
  as.data.frame()

nonzero <- Dat_area %>%
  select_if(isnt_zero) %>%
  colnames()
non_zero <- Dat_yield %>%
  select_if(isnt_zero) %>%
  colnames()

Crop_area_stack <- Crop_area_stack[[which(Crop_area_stack %>% names() %in% nonzero)]]
Crop_yield_stack <- Crop_yield_stack[[which(Crop_yield_stack %>% names() %in% non_zero)]]

#,str_replace(non_zero "phys_area", "yield"))]]

plot(Crop_area_stack[[1:11]])
plot(Crop_yield_stack[[1:11]])


Sweden_fao <- read.csv("~/Desktop/Sweden_fao.csv")
saveRDS(Sweden_fao, "~/Desktop/Diss-data/Sweden/sweden_FAO.rds")
Dat_fs <- read_rds("~/Desktop/Diss-data/Sweden/sweden_FAO.rds")

crop_names <- tibble(orig = names(Crop_area_stack))
crop_names1 <- tibble(orig = names(Crop_yield_stack))

crop_names <- crop_names %>%
  mutate(trans = c("barley", "beans", "cereals", "NA","pulses", "potato",
                   "rapeseed","sugarbeet", "NA","NA", "wheat"))
crop_names1 <- crop_names1 %>%
  mutate(trans1 = c("barley", "beans", "cereals", "NA","pulses", "potato",
                    "rapeseed","sugarbeet", "NA","NA", "wheat"))

crop_names <- crop_names %>%
  drop_na()
keep <- crop_names$orig

crop_names1 <- crop_names1 %>%
  drop_na()
keep1 <- crop_names1$orig
# filter stacks
Crop_area_stack <- Crop_area_stack[[which(Crop_area_stack %>% names() %in% keep)]]
Crop_yield_stack <- Crop_yield_stack[[which(Crop_yield_stack %>% names() %in% keep1)]]
#str_replace(keep, "phys_area", "yield"))]]

# rename
names(Crop_area_stack) <- crop_names$trans
names(Crop_yield_stack) <- crop_names1$trans1
Crop_area_stack <- readAll(Crop_area_stack)
Crop_yield_stack <- readAll(Crop_yield_stack)

# write out data
write_rds(Crop_area_stack, "~/Desktop/Diss-data/Sweden/sweden-crop-area-ha-2010.rds")
write_rds(Crop_yield_stack, "~/Desktop/Diss-data/Sweden/sweden-crop-yield-tonnes-per-ha-2010.rds")


# Now we visualise. First barley


Brk_croparea <- read_rds("~/Desktop/Diss-data/Sweden/sweden-crop-area-ha-2010.rds")
Brk_cropyield <- read_rds("~/Desktop/Diss-data/Sweden/sweden-crop-yield-tonnes-per-ha-2010.rds")

Ras_cropyield <- Brk_cropyield[[which(names(Brk_cropyield) == "barley")]]
Ras_croparea <- Brk_croparea[[which(names(Brk_croparea) == "barley")]]
rm(temp_cropname)
plot(Ras_croparea)
barley_area <- Brk_croparea[[which(names(Brk_croparea) == "barley")]]
wheat_area <- Brk_croparea[[which(names(Brk_croparea) == "wheat")]]
plot(barley_area)
plot(Shp_sw, add=T)
plot(wheat_area, padj=-1.5, cex.axis=0.7, xlab="Longitude (degree)", ylab="Latitude (degree)", cex.lab=0.75)
axis(1,padj=-1.5, cex.axis=0.7, tck=-0.01, )
plot(Shp_sw, add=T)

barley_area_df <- as.data.frame(Brk_croparea[[which(names(Brk_croparea) == "barley")]], xy = TRUE) 
str(barley_area_df)

bean_area_df <- as.data.frame(Brk_croparea[[which(names(Brk_croparea) == "beans")]], xy = TRUE) 
str(barley_area_df)

cereal_area_df <- as.data.frame(Brk_croparea[[which(names(Brk_croparea) == "cereals")]], xy = TRUE) 
str(barley_area_df)

pulses_area_df <- as.data.frame(Brk_croparea[[which(names(Brk_croparea) == "pulses")]], xy = TRUE) 
str(barley_area_df)

pot_area_df <- as.data.frame(Brk_croparea[[which(names(Brk_croparea) == "potato")]], xy = TRUE) 
str(barley_area_df)

rape_area_df <- as.data.frame(Brk_croparea[[which(names(Brk_croparea) == "rapeseed")]], xy = TRUE) 
str(barley_area_df)

sugb_area_df <- as.data.frame(Brk_croparea[[which(names(Brk_croparea) == "sugarbeet")]], xy = TRUE) 
str(barley_area_df)

wheat_area_df <- as.data.frame(Brk_croparea[[which(names(Brk_croparea) == "wheat")]], xy = TRUE) 
str(wheat_area_df)


ggplot(data = barley_area_df) +
  geom_raster(aes(x = x, y = y, fill = barley)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_sw, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=60, linetype="dashed")+
  geom_vline(xintercept=17.5, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Barley Harvested \nArea (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))


ggplot(data = bean_area_df) +
  geom_raster(aes(x = x, y = y, fill = beans)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_sw, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=60, linetype="dashed")+
  geom_vline(xintercept=17.5, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Beans Harvested \nArea (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))


ggplot(data = cereal_area_df) +
  geom_raster(aes(x = x, y = y, fill = cereals)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_sw, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=60, linetype="dashed")+
  geom_vline(xintercept=17.5, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Cereals Harvested \nArea (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

ggplot(data = pulses_area_df) +
  geom_raster(aes(x = x, y = y, fill = pulses)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_sw, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=60, linetype="dashed")+
  geom_vline(xintercept=17.5, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Pulses Harvested \nArea (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

ggplot(data = rape_area_df) +
  geom_raster(aes(x = x, y = y, fill = rapeseed)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_sw, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=60, linetype="dashed")+
  geom_vline(xintercept=17.5, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Rapeseed Harvested \nArea (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

ggplot(data = pot_area_df) +
  geom_raster(aes(x = x, y = y, fill = potato)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_sw, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=60, linetype="dashed")+
  geom_vline(xintercept=17.5, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Potatoes \nHarvested Area (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

ggplot(data = sugb_area_df) +
  geom_raster(aes(x = x, y = y, fill = sugarbeet)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_sw, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=60, linetype="dashed")+
  geom_vline(xintercept=17.5, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Sugarbeet \nHarvested Area (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

ggplot(data = wheat_area_df) +
  geom_raster(aes(x = x, y = y, fill = wheat)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_sw, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=60, linetype="dashed")+
  geom_vline(xintercept=17.5, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Wheat \nHarvested Area (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

# Now for yield


barley_y_df <- as.data.frame(Brk_cropyield[[which(names(Brk_cropyield) == "barley")]], xy = TRUE)
barley_y_df$barley <-barley_y_df$barley/1000
wheat_y_df <- as.data.frame(Brk_cropyield[[which(names(Brk_cropyield) == "wheat")]], xy = TRUE) 
wheat_y_df$wheat <- wheat_y_df$wheat/1000
bean_y_df <- as.data.frame(Brk_cropyield[[which(names(Brk_cropyield) == "beans")]], xy = TRUE) 
bean_y_df$beans <- bean_y_df$beans/1000
cereal_y_df <- as.data.frame(Brk_cropyield[[which(names(Brk_cropyield) == "cereals")]], xy = TRUE) 
cereal_y_df$cereals <- cereal_y_df$cereals/1000
pulse_y_df <- as.data.frame(Brk_cropyield[[which(names(Brk_cropyield) == "pulses")]], xy = TRUE) 
pulse_y_df$pulses <- pulse_y_df$pulses/1000
rape_y_df <- as.data.frame(Brk_cropyield[[which(names(Brk_cropyield) == "rapeseed")]], xy = TRUE) 
rape_y_df$rapeseed<-rape_y_df$rapeseed/1000
pot_y_df <- as.data.frame(Brk_cropyield[[which(names(Brk_cropyield) == "potato")]], xy = TRUE) 
pot_y_df$potato<-pot_y_df$potato/1000
sugb_y_df <- as.data.frame(Brk_cropyield[[which(names(Brk_cropyield) == "sugarbeet")]], xy = TRUE) 
sugb_y_df$sugarbeet<-sugb_y_df$sugarbeet/1000

ggplot(data = barley_y_df) +
  geom_raster(aes(x = x, y = y, fill = barley)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_sw, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=60, linetype="dashed")+
  geom_vline(xintercept=17.5, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Barley Yield \n(t ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))


ggplot(data = wheat_y_df) +
  geom_raster(aes(x = x, y = y, fill = wheat)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_sw, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=60, linetype="dashed")+
  geom_vline(xintercept=17.5, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Wheat Yield \n(t ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))


ggplot(data = bean_y_df) +
  geom_raster(aes(x = x, y = y, fill = beans)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_sw, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=60, linetype="dashed")+
  geom_vline(xintercept=17.5, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Bean Yield \n(t ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

ggplot(data = cereal_y_df) +
  geom_raster(aes(x = x, y = y, fill = cereals)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_sw, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=60, linetype="dashed")+
  geom_vline(xintercept=17.5, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Cereal Yield \n(t ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

ggplot(data = pulse_y_df) +
  geom_raster(aes(x = x, y = y, fill = pulses)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_sw, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=60, linetype="dashed")+
  geom_vline(xintercept=17.5, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Pulse Yield \n(t ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

ggplot(data = rape_y_df) +
  geom_raster(aes(x = x, y = y, fill = rapeseed)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_sw, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=60, linetype="dashed")+
  geom_vline(xintercept=17.5, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Rapeseed \nYield (t ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

ggplot(data = sugb_y_df) +
  geom_raster(aes(x = x, y = y, fill = sugarbeet)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_sw, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=60, linetype="dashed")+
  geom_vline(xintercept=17.5, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Sugar Beet \nYield (t ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

ggplot(data = pot_y_df) +
  geom_raster(aes(x = x, y = y, fill = potato)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_sw, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=60, linetype="dashed")+
  geom_vline(xintercept=17.5, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Potato Yield \n(t ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))





# Now let's make an estimate of crop yields for the next 10 years according to Alasdair's equation
# First wheat
sw_fao <- read.csv("~/Desktop/Sweden_fao.csv")
saveRDS(sw_fao, "~/Desktop/Diss-data/Sweden/sweden_FAO.rds")
Dat_faostat_sw <- read_rds("~/Desktop/Diss-data/Sweden/sweden_FAO.rds")
sample_n(Dat_faostat_sw, 10, replace = F)
Ras_clim <- read_rds("~/Desktop/Global Weather/Model 1/Sweden-climate-data-helper-raster.rds")

Brk_croparea <- read_rds("~/Desktop/Diss-data/Sweden/sweden-crop-area-ha-2010.rds")
Brk_cropyield <- read_rds("~/Desktop/Diss-data/Sweden/sweden-crop-yield-tonnes-per-ha-2010.rds")

crop_type <- "wheat" # name of crop of interest
frac_renew <- 1 / 1 # fraction of crop renewed every year (for e.g. crop renewed every three years, frac_renew = 1 / 3)
frac_remove <- 0.265 # fraction of crop residues removed

manure_type <- "dairy-cattle" # type of animal used to produce manure
manure_nrate <- 32 # application rate of manure in kg N per hectare
till_type <- "reduced" # type of tillage, either full, reduced or zero

sim_start_year <- 1961 # year simulation to start (min = 1961)
sim_end_year <- 2097 ## year simulation to end (max = 2097)

lat_lon <- tibble(x = 17.5, y = 60) # default chosen here is a high-yielding arable land near Marrackech
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

Dat_crop_ts <- Dat_faostat_sw %>% 
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
sw_wheat_yield <- Dat_crop_ts %>%
  ggplot(aes(x = year, y = yield_tha, colour = origin)) +
  geom_line()

sw_wheat_area <- data.frame(Dat_crop_ts$origin,Dat_crop_ts$year, Dat_crop_ts$area_ha)
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
  write_csv("~/Desktop/Diss-data/Sweden/sweden-wheat-crop-data.csv")

# write out manure data
tibble(year = sim_start_year:sim_end_year,
       man_type = manure_type,
       man_nrate = manure_nrate) %>%
  write_csv("~/Desktop/Diss-data/Sweden/sweden-example-manure-data.csv")

## Now barley

Dat_faostat_sw <- read_rds("~/Desktop/Diss-data/Sweden/sweden_FAO.rds")
sample_n(Dat_faostat_sw, 10, replace = F)

Brk_croparea <- read_rds("~/Desktop/Diss-data/Sweden/sweden-crop-area-ha-2010.rds")
Brk_cropyield <- read_rds("~/Desktop/Diss-data/Sweden/sweden-crop-yield-tonnes-per-ha-2010.rds")

crop_type <- "barley" # name of crop of interest
frac_renew <- 1 / 1 # fraction of crop renewed every year (for e.g. crop renewed every three years, frac_renew = 1 / 3)
frac_remove <- 0.505 # fraction of crop residues removed

manure_type <- "dairy-cattle" # type of animal used to produce manure
manure_nrate <- 32 # application rate of manure in kg N per hectare
till_type <- "reduced" # type of tillage, either full, reduced or zero

sim_start_year <- 1961 # year simulation to start (min = 1961)
sim_end_year <- 2097 ## year simulation to end (max = 2097)

lat_lon <- tibble(x = 17.5, y = 60) # default chosen here is a high-yielding arable land near Marrackech
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

Dat_crop_ts <- Dat_faostat_sw %>% 
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
sw_barley_yield <- Dat_crop_ts %>%
  ggplot(aes(x = year, y = yield_tha, colour = origin)) +
  geom_line()

sw_barley_area <- data.frame(Dat_crop_ts$origin,Dat_crop_ts$year, Dat_crop_ts$area_ha)

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
  write_csv("~/Desktop/Diss-data/Sweden/sweden-barley-crop-data.csv")

## Now beans
# no data after year 2000
## now potato

Dat_faostat_sw <- read_rds("~/Desktop/Diss-data/Sweden/sweden_FAO.rds")
sample_n(Dat_faostat_sw, 10, replace = F)

Brk_croparea <- read_rds("~/Desktop/Diss-data/Sweden/sweden-crop-area-ha-2010.rds")
Brk_cropyield <- read_rds("~/Desktop/Diss-data/Sweden/sweden-crop-yield-tonnes-per-ha-2010.rds")

crop_type <- "potato" # name of crop of interest
frac_renew <- 1 / 1 # fraction of crop renewed every year (for e.g. crop renewed every three years, frac_renew = 1 / 3)
frac_remove <- 0 # fraction of crop residues removed

manure_type <- "dairy-cattle" # type of animal used to produce manure
manure_nrate <- 32 # application rate of manure in kg N per hectare
till_type <- "reduced" # type of tillage, either full, reduced or zero

sim_start_year <- 1961 # year simulation to start (min = 1961)
sim_end_year <- 2097 ## year simulation to end (max = 2097)

lat_lon <- tibble(x = 17.5, y = 60) # default chosen here is a high-yielding arable land near Marrackech
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

Dat_crop_ts <- Dat_faostat_sw %>% 
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
sw_pot_yield <- Dat_crop_ts %>%
  ggplot(aes(x = year, y = yield_tha, colour = origin)) +
  geom_line()

sw_pot_area <-data.frame(Dat_crop_ts$origin,Dat_crop_ts$year, Dat_crop_ts$area_ha)

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
  write_csv("~/Desktop/Diss-data/Sweden/sweden-potato-crop-data.csv")

## Now rapeseed


Dat_faostat_sw <- read_rds("~/Desktop/Diss-data/Sweden/sweden_FAO.rds")
sample_n(Dat_faostat_sw, 10, replace = F)

Brk_croparea <- read_rds("~/Desktop/Diss-data/Sweden/sweden-crop-area-ha-2010.rds")
Brk_cropyield <- read_rds("~/Desktop/Diss-data/Sweden/sweden-crop-yield-tonnes-per-ha-2010.rds")

crop_type <- "rapeseed" # name of crop of interest
frac_renew <- 1 / 1 # fraction of crop renewed every year (for e.g. crop renewed every three years, frac_renew = 1 / 3)
frac_remove <- 0.3 # fraction of crop residues removed

manure_type <- "dairy-cattle" # type of animal used to produce manure
manure_nrate <- 32 # application rate of manure in kg N per hectare
till_type <- "reduced" # type of tillage, either full, reduced or zero

sim_start_year <- 1961 # year simulation to start (min = 1961)
sim_end_year <- 2097 ## year simulation to end (max = 2097)

lat_lon <- tibble(x = 17.5, y =60) # default chosen here is a high-yielding arable land near Marrackech
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

Dat_crop_ts <- Dat_faostat_sw %>% 
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
sw_rape_yield <- Dat_crop_ts %>%
  ggplot(aes(x = year, y = yield_tha, colour = origin)) +
  geom_line()

sw_rape_area <- data.frame(Dat_crop_ts$origin,Dat_crop_ts$year, Dat_crop_ts$area_ha)

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
  write_csv("~/Desktop/Diss-data/Sweden/sweden-rape-crop-data.csv")


## Now sugarbeet

# removed because none are produced at this area
sw_wheat_yield
sw_barley_yield
sw_pot_yield
sw_rape_yield

plot(sw_wheat_area)
plot(sw_barley_area)
plot(sw_pot_area)
plot(sw_rape_area)

ggplot()+
  geom_line(data=sw_rape_area, mapping=aes(x=Dat_crop_ts.year, y=Dat_crop_ts.area_ha, color="Rapeseed"))

color <- c("Maize"="black", "Barley"="blue","Wheat"="red", "Potato"="green", "Millet"='orange', "Rapeseed"='orange')

area_estimate <- ggplot()+
  geom_line(data=sw_wheat_area, mapping= aes(x=Dat_crop_ts.year, y=Dat_crop_ts.area_ha, color='Wheat'))+
  geom_line(data=sw_barley_area, mapping= aes(x=Dat_crop_ts.year, y=Dat_crop_ts.area_ha, color='Barley'))+
  geom_line(data=sw_pot_area, mapping= aes(x=Dat_crop_ts.year, y=Dat_crop_ts.area_ha, color='Potato'))+
  geom_line(data=sw_rape_area, mapping=aes(x=Dat_crop_ts.year, y=Dat_crop_ts.area_ha, color="Rapeseed"))+
  labs(x="Year", y="Area (ha)", color = "Crops") +
  theme(panel.background = element_rect(fill = "white"))+
  theme(axis.line = element_line(color = "black"))+
  scale_color_manual(values = color)

area_estimate  


plot_rape_y <- read_csv("~/Desktop/Diss-data/Sweden/sweden-rape-crop-data.csv")
plot_rape_y$yield_tha <- plot_rape_y$yield_tha/1000
plot_barley_y <- read_csv("~/Desktop/Diss-data/Sweden/sweden-barley-crop-data.csv")
plot_barley_y$yield_tha<- plot_barley_y$yield_tha/1000
plot_wheat_y <- read_csv("~/Desktop/Diss-data/Sweden/sweden-wheat-crop-data.csv")
plot_wheat_y$yield_tha<-plot_wheat_y$yield_tha/1000
plot_pot_y <- read_csv("~/Desktop/Diss-data/Sweden/sweden-potato-crop-data.csv")
plot_pot_y$yield_tha<-plot_pot_y$yield_tha/1000


yield_estimate <- ggplot()+
  geom_line(data=plot_rape_y, mapping= aes(x=year, y=yield_tha, color='Rapeseed'))+
  geom_line(data=plot_barley_y, mapping= aes(x=year, y=yield_tha, color='Barley'))+
  geom_line(data=plot_wheat_y, mapping= aes(x=year, y=yield_tha, color='Wheat'))+
  geom_line(data=plot_pot_y, mapping= aes(x=year, y=yield_tha, color='Potato'))+
  labs(x="Year", y="Yield (t ha-1)", color = "Crops") +
  scale_color_manual(values = color)+
  theme(panel.background = element_rect(fill = "white", colour = "white"))+
  theme(axis.line = element_line(color = "black"))

yield_estimate


# Now for sand NEED TO DO
library(tidyverse)
library(raster)

Ras_sand <- raster("~/Desktop/Diss-data/Sweden/Sweden_soil.tif")

Ras_sand <- Ras_sand %>%
  crop(Shp_sw) %>%
  mask(Shp_sw)

plot(Ras_sand)
names(Ras_sand) <- "soil_sand_pc"

Ras_sand <- readAll(Ras_sand) # throws an error cos it's already read it in — keep anyway jic
write_rds(Ras_sand, "~/Desktop/Diss-data/Sweden/sweden-soil-sand-percentage.rds")

see_sand <- rasterToPoints(Ras_sand)

sw_sand <- as.data.frame(see_sand)

ggplot(data = sw_sand) +
  geom_raster(aes(x = x, y = y, fill= soil_sand_pc)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_sw, aes(x= V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Soil Sand \nComposition (%)") +
  geom_hline(yintercept=60, linetype="dashed")+
  geom_vline(xintercept=17.5, linetype="dashed")+
  theme(panel.background = element_rect(fill = "white", colour = "white"))+
  theme(axis.line = element_line(color = "black"))+
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))






# Lets to temp, precip and PEt here

library(raster)
library(tidyverse)
library(ncdf4)
library(lubridate)

nc_open("~/Desktop/Global Weather/Model 1/tempmean_rcp85_land-gcm_global_60km_01_mon_189912-209911.nc")
nc_open("~/Desktop/Global Weather/Model 1/precipitation_rcp85_land-gcm_global_60km_01_mon_189912-209911.nc")

Stk_temp <- brick("~/Desktop/Global Weather/Model 1/tempmean_rcp85_land-gcm_global_60km_01_mon_189912-209911.nc")
Stk_precip <- brick("~/Desktop/Global Weather/Model 1/precipitation_rcp85_land-gcm_global_60km_01_mon_189912-209911.nc")

# read in shapefile and mask brick
Shp_Fr <- shapefile("~/Desktop/Diss-data/Sweden/Sweden.shp")
Stk_temp <- Stk_temp %>% crop(Shp_Fr)
Stk_precip <- Stk_precip %>% crop(Shp_Fr)

plot(Stk_temp[[1:12]])
plot(Stk_precip[[1:12]])

# make names unique
sum(names(Stk_temp) != names(Stk_precip)) # names/dates match exactly

names(Stk_temp) <- names(Stk_temp) %>% str_replace("^X", "temp")
names(Stk_precip) <- names(Stk_precip) %>% str_replace("^X", "precip")

# convert all to dataframe
Dat_clim <- stack(Stk_temp, Stk_precip) %>% as.data.frame(xy = T) %>% as_tibble()

# gather and spread by variable type
Dat_clim <- Dat_clim %>%
  gather(-x, -y, key = "key", value = "value") %>%
  mutate(date = key %>%
           str_replace_all("[:lower:]", "") %>%
           str_replace_all("\\.", "-") %>%
           ymd_hms(),
         metric = key %>%
           str_extract("^[:lower:]+(?=\\d)")) %>%
  select(-key) %>%
  spread(key = metric, value = value) %>%
  rename(temp_centigrade = temp,
         precip_mm = precip)

nrow(Dat_clim %>% drop_na()) # no NAs to speak of

# convert precipitation to monthly total
Dat_clim <- Dat_clim %>%
  mutate(precip_mm = precip_mm * days_in_month(date))

# calculate month and year and nest
Dat_clim <- Dat_clim %>%
  mutate(date = as_date(date),
         month = month(date),
         year = year(date)) %>%
  group_by(x, y) %>%
  nest() %>%
  ungroup()

# compress and interpolate where necessary
# this UKCP data is bloody awkward to get in monthly format
temp <- tibble(date = seq(from = ymd("1902-01-16"), to = ymd("2096-12-17"), by = as.difftime(months(1))) %>% ymd(),
               month = month(date),
               year = year(date)) %>%
  select(-date)

Dat_clim <- Dat_clim %>%
  mutate(data_reg = data %>%
           map(function(df){
             df %>%
               group_by(month, year) %>%
               summarise(temp_centigrade = mean(temp_centigrade),
                         precip_mm = mean(precip_mm))
           }),
         data_reg = data_reg %>%
           map(function(df){
             df %>%
               right_join(temp, by = c("month", "year"))
           }),
         data_reg = data_reg %>%
           map(function(df){
             df %>%
               mutate(temp_centigrade = temp_centigrade %>% forecast::na.interp() %>% as.numeric(),
                      precip_mm = precip_mm %>% forecast::na.interp() %>% as.numeric())
           })
  )

# calculate pet using thornthwaite method
# https://upcommons.upc.edu/bitstream/handle/2117/89152/Appendix_10.pdf?sequence=3&isAllowed=y

# daylength calculations using insol
#install.packages("insol")
#install.packages("forecast")
library(insol)
library(forecast)


lats <- Dat_clim %>%
  pull(y) %>%
  unique()

jdays <- tibble(date = seq(from = ymd("2019-01-01"), to = ymd("2019-12-31"), by = as.difftime(days(1)))) %>%
  mutate(month = month(date),
         jday = yday(date),
         mday = days_in_month(date)) %>%
  group_by(month) %>%
  summarise(mday = mean(mday),
            jday = mean(jday))


daylength <- tibble(y = rep(lats, 12),
                    month = rep(1:12, each = length(lats))) %>%
  left_join(jdays, by = "month") %>%
  mutate(lon = 0,
         time_zone = 0,
         daylength = pmap_dbl(list(y, lon, jday, time_zone), function(a, b, c, d){
           return(daylength(a, b, c, d)[3])
         }))

# function required to combat annoying R 'feature' (returns NaN for negative numbers raised to non-integer powers...)
rtp <- function(x, power){
  sign(x) * abs(x) ^ (power)
}

Dat_clim <- Dat_clim %>%
  mutate(data_reg = map2(y, data_reg, function(lat, df){
    df %>%
      mutate(y = lat) %>%
      group_by(year) %>%
      mutate(I = sum(rtp(temp_centigrade / 5, 1.514)),
             alpha = 675*10^-9 * rtp(I, 3) - 771*10^-7 * rtp(I, 2) + 1792*10^-5 * I + 0.49239,
             pet_mm = rtp(16 * ((10 * temp_centigrade) / I), alpha)) %>%
      ungroup() %>%
      left_join(daylength %>% select(y, month, daylength, mday), by = c("y", "month")) %>%
      mutate(pet_mm = pet_mm * daylength / 12 * mday / 30,
             pet_mm = ifelse(pet_mm < 1, 1, pet_mm)) %>% # prevents errors with negative PET/div by zero
      select(-y, -I, -alpha, -mday, -daylength) %>%
      mutate(precip_mm = ifelse(precip_mm < 0, 0, precip_mm)) # # another quick and dirty fix for potential errors - occasional negative precipitation values
  }))

# number cells (x-y pairs) to allow raster-based extraction of data
Dat_clim <- Dat_clim %>%
  mutate(cell_no = 1:nrow(Dat_clim)) %>%
  select(x, y, cell_no, data_full = data_reg)

# filter data to > 1961 (no data for crops from before this)
Dat_clim <- Dat_clim %>%
  mutate(data_full = data_full %>%
           map(function(df){
             df %>%
               filter(year >= 1961)
           }))

# write out full climate data
write_rds(Dat_clim, "~/Desktop/Global Weather/Model 1/France-full-climate-data-1902-2097.rds")

# how about a helper raster to allow spatial point querying of data?
# number lat/lon values and transform to raster, keep numbers in climate df
Ras_help <- Dat_clim %>%
  select(x, y, z = cell_no) %>%
  rasterFromXYZ()

# write out
write_rds(Ras_help, "~/Desktop/Global Weather/Model 1/Sweden-climate-data-helper-raster.rds")

Dat_clim <- read_rds("~/Desktop/Global Weather/Model 1/FINAL-sweden-full-climate-data-1902-2097.rds")
Ras_clim <- read_rds("~/Desktop/Global Weather/Model 1/Sweden-climate-data-helper-raster.rds")
sample_n(Dat_clim, 10, replace = F)
Dat_clim$data_full[[1]] %>% head(10)

plot(Ras_clim)
plot(Shp_Fr, add = T)




########### THIS IS WHERE YOU FIGURE OUT LOCATION!##############


lat_lon <- tibble(x = 17.5, y = 60)
clim_coord_no <- raster::extract(Ras_clim, lat_lon)
clim_coord <- as.data.frame(clim_coord_no)

sim_start_year <- 1961 # year simulation to start (min = 1961)
sim_end_year <- 2097 ## year simulation to end (max = 2097)
mean_sim_end <- 0

# climate uncertainty (fractional std. dev. default i.e. no uncertainty = 0)
sd_sim_end <- 0.3

# number of Monte Carlo repetitions
# (more than 100 at your own risk — depending on your processor it may be too much for it to handle)
MC_n <- 100

Dat_clim <- Dat_clim %>%
  filter(cell_no == clim_coord_no) %>%
  dplyr::select(-cell_no) %>%
  slice(rep(1, MC_n)) %>%
  add_column(sample = 1:MC_n, .before = "data_full") %>%
  mutate(data_full = pmap(list(mean_sim_end, sd_sim_end, sim_start_year, sim_end_year, data_full), function(mean, sd, start, end, df){
    
    df <- df %>% filter(year >= start,
                        year <= end)
    
    det <- df %>% filter(year < 2020) %>% nrow()
    stoch <- df %>% filter(year >= 2020) %>% nrow()
    
    mean_seq <- seq(from = 0, to = mean, length.out = stoch)
    sd_seq <- seq(from = 0, to = sd, length.out = stoch)
    
    # stationary autoregressive process
    x <- w <- rnorm(n = stoch, mean = mean_seq, sd = sd_seq)
    for(t in 2:stoch) x[t] <- (x[t - 1] / 2) + w[t]
    x1 <- c(rep(0, det), x)
    
    x <- w <- rnorm(n = stoch, mean = mean_seq, sd = sd_seq)
    for(t in 2:stoch) x[t] <- (x[t - 1] / 2) + w[t]
    x2 <- c(rep(0, det), x)
    
    x <- w <- rnorm(n = stoch, mean = mean_seq, sd = sd_seq)
    for(t in 2:stoch) x[t] <- (x[t - 1] / 2) + w[t]
    x3 <- c(rep(0, det), x)
    
    df %>%
      mutate(temp_centigrade = temp_centigrade * (1 + x1),
             precip_mm = precip_mm * (1 + x2),
             pet_mm = pet_mm * (1 + x3),
             temp_centigrade = ifelse(temp_centigrade < 0, 0, temp_centigrade),
             precip_mm = ifelse(precip_mm < 0, 0, precip_mm),
             pet_mm = ifelse(pet_mm < 0, 0, pet_mm)) %>%
      return()
  }))

# write out climate data  
write_rds(Dat_clim, "~/Desktop/Global Weather/Model 1/Sweden-example-climate-data.rds")

# Now lets take a look at temperature
Dat_clim$data_full[[1]] %>% head(100)
predicted_clim_raw <- as.data.frame(Dat_clim$data_full)
pred_precip <- data.frame(predicted_clim_raw$year, predicted_clim_raw$month, predicted_clim_raw$precip_mm)
pred_precip <- aggregate(pred_precip$predicted_clim_raw.precip_mm, by=list(predicted_clim_raw.year =pred_precip$predicted_clim_raw.year), FUN=mean)
names(pred_precip)[names(pred_precip)=='predicted_clim_raw.year'] <- 'Year'
names(pred_precip)[names(pred_precip)=='x'] <- 'Precipitation'
# names(predicted_precip)[names(predicted_precip)=='predicted_clim_raw.month'] <- 'month'
predicted_temp <- data.frame(predicted_clim_raw$month, predicted_clim_raw$year, predicted_clim_raw$temp_centigrade)
predicted_temp <- aggregate(predicted_temp$predicted_clim_raw.temp_centigrade, by=list(predicted_clim_raw.year =predicted_temp$predicted_clim_raw.year), FUN=mean)
predicted_temp

# names(predicted_temp)[names(predicted_temp)=='predicted_clim_raw.month'] <- 'month'
names(predicted_temp)[names(predicted_temp)=='predicted_clim_raw.year'] <- 'year'
names(predicted_temp)[names(predicted_temp)=='x'] <- 'Temperature'
# names(predicted_temp)[names(predicted_temp)=='predicted_clim_raw.precip_mm'] <- 'Precipitation'

predicted_temp[which.max(predicted_temp$Temperature),]

av_pred_clim <- data.frame(predicted_temp$year, predicted_temp$Temperature, pred_precip$Precipitation)
names(av_pred_clim)[names(av_pred_clim)=='predicted_temp.year'] <- 'Year'
names(av_pred_clim)[names(av_pred_clim)=='predicted_temp.Temperature'] <- 'Temperature'
names(av_pred_clim)[names(av_pred_clim)=='pred_precip.Precipitation'] <- 'Precipitation'

ggplot(av_pred_clim, aes(x=Year)) +
  
  geom_line( aes(y=Temperature), color='red') + 
  geom_line( aes(y=Precipitation/3), color='blue') + 
  scale_y_continuous(name = "Temperature",
                     sec.axis = sec_axis(~.*3, name="Precipitation"))+
  labs(colour = c("Temperature", "Precipitation"))


# ggplot()+
#  geom_line(data=predicted_temp, 
#            aes(x = year, y= Temperature, colour='red'))+
#  geom_line(data=pred_precip, 
#            aes(x = Year, y= Precipitation), color='blue')



Dat_clim$data_full[[1]] %>% head(10)
predicted_clim_raw <- as.data.frame(Dat_clim$data_full)

predicted_clim5 <- data.frame(predicted_clim_raw$month.5, predicted_clim_raw$year.5, predicted_clim_raw$temp_centigrade.5)
predicted_clim5
pred_precip5 <- data.frame(predicted_clim_raw$year.5, predicted_clim_raw$precip_mm.5)
pred_precip5 <- aggregate(pred_precip5$predicted_clim_raw.precip_mm.5, by=list(predicted_clim_raw.year.5 =pred_precip5$predicted_clim_raw.year.5), FUN=sum)
names(pred_precip5)[names(pred_precip5)=='predicted_clim_raw.year.5'] <- 'Year'
names(pred_precip5)[names(pred_precip5)=='x'] <- 'Precipitation'

predicted_clim5 <- data.frame(predicted_clim_raw$month.5, predicted_clim_raw$year.5, predicted_clim_raw$temp_centigrade.5)
predicted_clim5

names(predicted_clim5)[names(predicted_clim5)=='predicted_clim_raw.month.5'] <- 'month'
names(predicted_clim5)[names(predicted_clim5)=='predicted_clim_raw.year.5'] <- 'year'
names(predicted_clim5)[names(predicted_clim5)=='predicted_clim_raw.temp_centigrade.5'] <- 'Temperature'

predicted_clim5[which.max(predicted_clim5$Temperature),]


av_pred_clim5 <- aggregate(predicted_clim5$Temperature, list(year=predicted_clim5$year), FUN=mean)
names(av_pred_clim5)[names(av_pred_clim5)=='x'] <- 'Temperature'
av_pred_clim5 <- data.frame(av_pred_clim5$year, av_pred_clim5$Temperature, pred_precip5$Precipitation)
names(av_pred_clim5)[names(av_pred_clim5)=='av_pred_clim5.year'] <- 'Year'
names(av_pred_clim5)[names(av_pred_clim5)=='av_pred_clim5.Temperature'] <- 'Temperature'
names(av_pred_clim5)[names(av_pred_clim5)=='pred_precip5.Precipitation'] <- 'Precipitation'


pred_precip1 <- data.frame(predicted_clim_raw$year.1, predicted_clim_raw$precip_mm.1)
pred_precip1 <- aggregate(pred_precip1$predicted_clim_raw.precip_mm.1, by=list(predicted_clim_raw.year.1 =pred_precip1$predicted_clim_raw.year.1), FUN=sum)
names(pred_precip1)[names(pred_precip1)=='predicted_clim_raw.year.1'] <- 'Year'
names(pred_precip1)[names(pred_precip1)=='x'] <- 'Precipitation'

predicted_clim1 <- data.frame(predicted_clim_raw$month.1, predicted_clim_raw$year.1, predicted_clim_raw$temp_centigrade.1)
predicted_clim1

names(predicted_clim1)[names(predicted_clim1)=='predicted_clim_raw.month.1'] <- 'month'
names(predicted_clim1)[names(predicted_clim1)=='predicted_clim_raw.year.1'] <- 'year'
names(predicted_clim1)[names(predicted_clim1)=='predicted_clim_raw.temp_centigrade.1'] <- 'Temperature'


av_pred_clim1 <- aggregate(predicted_clim1$Temperature, list(year=predicted_clim1$year), FUN=mean)
names(av_pred_clim1)[names(av_pred_clim1)=='x'] <- 'Temperature'
av_pred_clim1 <- data.frame(av_pred_clim1$year, av_pred_clim1$Temperature, pred_precip1$Precipitation)
names(av_pred_clim1)[names(av_pred_clim1)=='av_pred_clim1.year'] <- 'Year'
names(av_pred_clim1)[names(av_pred_clim1)=='av_pred_clim1.Temperature'] <- 'Temperature'
names(av_pred_clim1)[names(av_pred_clim1)=='pred_precip1.Precipitation'] <- 'Precipitation'

names(predicted_clim1)[names(predicted_clim1)=='predicted_clim_raw.month.1'] <- 'month'
names(predicted_clim1)[names(predicted_clim1)=='predicted_clim_raw.year.1'] <- 'year'
names(predicted_clim1)[names(predicted_clim1)=='predicted_clim_raw.temp_centigrade.1'] <- 'Temperature'

predicted_clim1[which.max(predicted_clim1$Temperature),]

predicted_clim2 <- data.frame(predicted_clim_raw$month.2, predicted_clim_raw$year.2, predicted_clim_raw$temp_centigrade.2)
predicted_clim2
pred_precip2 <- data.frame(predicted_clim_raw$year.2, predicted_clim_raw$precip_mm.2)
pred_precip2 <- aggregate(pred_precip2$predicted_clim_raw.precip_mm.2, by=list(predicted_clim_raw.year.2 =pred_precip2$predicted_clim_raw.year.2), FUN=sum)
names(pred_precip2)[names(pred_precip2)=='predicted_clim_raw.year.2'] <- 'Year'
names(pred_precip2)[names(pred_precip2)=='x'] <- 'Precipitation'



names(predicted_clim2)[names(predicted_clim2)=='predicted_clim_raw.month.2'] <- 'month'
names(predicted_clim2)[names(predicted_clim2)=='predicted_clim_raw.year.2'] <- 'year'
names(predicted_clim2)[names(predicted_clim2)=='predicted_clim_raw.temp_centigrade.2'] <- 'Temperature'

predicted_clim2[which.max(predicted_clim2$Temperature),]

predicted_clim3 <- data.frame(predicted_clim_raw$month.3, predicted_clim_raw$year.3, predicted_clim_raw$temp_centigrade.3)
predicted_clim3
pred_precip3 <- data.frame(predicted_clim_raw$year.3, predicted_clim_raw$precip_mm.3)
pred_precip3 <- aggregate(pred_precip3$predicted_clim_raw.precip_mm.3, by=list(predicted_clim_raw.year.3 =pred_precip3$predicted_clim_raw.year.3), FUN=sum)
names(pred_precip3)[names(pred_precip3)=='predicted_clim_raw.year.3'] <- 'Year'
names(pred_precip3)[names(pred_precip3)=='x'] <- 'Precipitation'


names(predicted_clim3)[names(predicted_clim3)=='predicted_clim_raw.month.3'] <- 'month'
names(predicted_clim3)[names(predicted_clim3)=='predicted_clim_raw.year.3'] <- 'year'
names(predicted_clim3)[names(predicted_clim3)=='predicted_clim_raw.temp_centigrade.3'] <- 'Temperature'

predicted_clim3[which.max(predicted_clim3$Temperature),]

predicted_clim4 <- data.frame(predicted_clim_raw$month.4, predicted_clim_raw$year.4, predicted_clim_raw$temp_centigrade.4)
predicted_clim4
pred_precip4 <- data.frame(predicted_clim_raw$year.4, predicted_clim_raw$precip_mm.4)
pred_precip4 <- aggregate(pred_precip4$predicted_clim_raw.precip_mm.4, by=list(predicted_clim_raw.year.4 =pred_precip4$predicted_clim_raw.year.4), FUN=sum)
names(pred_precip4)[names(pred_precip4)=='predicted_clim_raw.year.4'] <- 'Year'
names(pred_precip4)[names(pred_precip4)=='x'] <- 'Precipitation'


names(predicted_clim4)[names(predicted_clim4)=='predicted_clim_raw.month.4'] <- 'month'
names(predicted_clim4)[names(predicted_clim4)=='predicted_clim_raw.year.4'] <- 'year'
names(predicted_clim4)[names(predicted_clim4)=='predicted_clim_raw.temp_centigrade.4'] <- 'Temperature'

predicted_clim4[which.max(predicted_clim4$Temperature),]

av_pred_clim1 <- aggregate(predicted_clim1$Temperature, list(year=predicted_clim1$year), FUN=mean)
names(av_pred_clim1)[names(av_pred_clim1)=='x'] <- 'Temperature'
av_pred_clim1 <- data.frame(av_pred_clim1$year, av_pred_clim1$Temperature, pred_precip1$Precipitation)
names(av_pred_clim1)[names(av_pred_clim1)=='av_pred_clim1.year'] <- 'Year'
names(av_pred_clim1)[names(av_pred_clim1)=='av_pred_clim1.Temperature'] <- 'Temperature'
names(av_pred_clim1)[names(av_pred_clim1)=='pred_precip1.Precipitation'] <- 'Precipitation'


av_pred_clim2 <- aggregate(predicted_clim2$Temperature, list(year=predicted_clim2$year), FUN=mean)
names(av_pred_clim2)[names(av_pred_clim2)=='x'] <- 'Temperature'
av_pred_clim2 <- data.frame(av_pred_clim2$year, av_pred_clim2$Temperature, pred_precip2$Precipitation)
names(av_pred_clim2)[names(av_pred_clim2)=='av_pred_clim2.year'] <- 'Year'
names(av_pred_clim2)[names(av_pred_clim2)=='av_pred_clim2.Temperature'] <- 'Temperature'
names(av_pred_clim2)[names(av_pred_clim2)=='pred_precip2.Precipitation'] <- 'Precipitation'


av_pred_clim3 <- aggregate(predicted_clim3$Temperature, list(year=predicted_clim3$year), FUN=mean)
names(av_pred_clim3)[names(av_pred_clim3)=='x'] <- 'Temperature'
av_pred_clim3 <- data.frame(av_pred_clim3$year, av_pred_clim3$Temperature, pred_precip3$Precipitation)
names(av_pred_clim3)[names(av_pred_clim3)=='av_pred_clim3.year'] <- 'Year'
names(av_pred_clim3)[names(av_pred_clim3)=='av_pred_clim3.Temperature'] <- 'Temperature'
names(av_pred_clim3)[names(av_pred_clim3)=='pred_precip3.Precipitation'] <- 'Precipitation'

av_pred_clim4 <- aggregate(predicted_clim4$Temperature, list(year=predicted_clim4$year), FUN=mean)
names(av_pred_clim4)[names(av_pred_clim4)=='x'] <- 'Temperature'
av_pred_clim4 <- data.frame(av_pred_clim4$year, av_pred_clim4$Temperature, pred_precip4$Precipitation)
names(av_pred_clim4)[names(av_pred_clim4)=='av_pred_clim4.year'] <- 'Year'
names(av_pred_clim4)[names(av_pred_clim4)=='av_pred_clim4.Temperature'] <- 'Temperature'
names(av_pred_clim4)[names(av_pred_clim4)=='pred_precip4.Precipitation'] <- 'Precipitation'

p <- ggplot() +
  geom_line(data=av_pred_clim5, mapping= aes(x=Year, y=Temperature), color='red') + 
  geom_line(data=av_pred_clim5, mapping=aes(x=Year, y=Precipitation/40), color='blue') +
  geom_line(data=av_pred_clim1, mapping=aes(x=Year, y=Temperature), color='red') + 
  geom_line(data=av_pred_clim1, mapping=aes(x=Year,y=Precipitation/40), color='blue')+
  geom_line(data=av_pred_clim2, mapping=aes(x=Year, y=Temperature), color='red') + 
  geom_line(data=av_pred_clim2, mapping=aes(x=Year,y=Precipitation/40), color='blue')+ 
  geom_line(data=av_pred_clim3, mapping=aes(x=Year, y=Temperature), color='red') + 
  geom_line(data=av_pred_clim3, mapping=aes(x=Year, y=Precipitation/40), color='blue')+  
  geom_line(data=av_pred_clim4, mapping=aes(x=Year, y=Temperature), color='red') + 
  geom_line(data=av_pred_clim4, mapping=aes(x=Year, y=Precipitation/40), color='blue') + 
  scale_y_continuous(name = "Annual Average Temperature (Celcius)",
                     sec.axis = sec_axis(~.*40, name="Annual Total Precipitation (mm)"))+
  labs(colour = c("Annual Average Temperature (Celcius)", "Annual Total Precipitation (mm)"))+
  theme(panel.background = element_rect(fill = "white", colour = "white"))+
  theme(axis.line = element_line(color = "black"))
p


mean(av_pred_clim1$Precipitation)
mean(av_pred_clim2$Precipitation)
mean(av_pred_clim3$Precipitation)
mean(av_pred_clim4$Precipitation)
mean(av_pred_clim5$Precipitation)

x.sub1 <- subset(av_pred_clim1, Year <2019)
mean(x.sub1$Precipitation)
min(x.sub1$Temperature)
max(x.sub1$Temperature)

x.sub1[which.max(x.sub1$Temperature),]
x.sub1[which.min(x.sub1$Temperature),]
av_pred_clim1[which.max(av_pred_clim1$Temperature),]
av_pred_clim2[which.max(av_pred_clim2$Temperature),]
av_pred_clim3[which.max(av_pred_clim3$Temperature),]
av_pred_clim4[which.max(av_pred_clim4$Temperature),]
av_pred_clim5[which.max(av_pred_clim5$Temperature),]


















