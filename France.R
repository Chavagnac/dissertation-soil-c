

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


color <- c("Maize"="black", "Barley"="blue","Wheat"="red", "Potato"="green", "Millet"='orange')

yield_estimate <- ggplot()+
  geom_line(data=plot_rape_y, mapping= aes(x=year, y=yield_tha, color='Millet'))+
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

# check out ncdf data and read in as raster bricks
nc_open("~/Desktop/Global Weather/Model 1/precipitation_rcp85_land-gcm_global_60km_01_mon_189912-209911.nc")
nc_open("~/Desktop/Global Weather/Model 1/tempmean_rcp85_land-gcm_global_60km_01_mon_189912-209911.nc")

Stk_temp <- brick("~/Desktop/Global Weather/Model 1/tempmean_rcp85_land-gcm_global_60km_01_mon_189912-209911.nc")
Stk_precip <- brick("~/Desktop/Global Weather/Model 1/precipitation_rcp85_land-gcm_global_60km_01_mon_189912-209911.nc")

# read in shapefile and mask brick
Stk_temp <- Stk_temp %>% crop(Shp_sw)
Stk_precip <- Stk_precip %>% crop(Shp_sw)

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
library(insol)

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
write_rds(Dat_clim, "~/Desktop/Global Weather/Model 1/FINAL-sweden-full-climate-data-1902-2097.rds")





































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

