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

Crop_area_stack <- Crop_area_stack %>%
  crop(Shp_Mor) %>%
  mask(Shp_Mor)

Crop_yield_stack <- Crop_yield_stack %>%
  crop(Shp_Mor) %>%
  mask(Shp_Mor)

plot(Crop_area_stack[[1]])
plot(Shp_Mor, add = T)
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
Crop_yield_stack <- Crop_yield_stack[[which(Crop_yield_stack %>% names() %in% str_replace(nonzero, "phys_area", "yield"))]]

plot(Crop_area_stack[[1:16]])
plot(Crop_area_stack[[17:29]])

Mor_FAO <- read.csv("~/Desktop/Morocco_crop_1961_2018.csv")
saveRDS(Mor_FAO, "~/Desktop/Diss-data/Morocco/Morocco_FAO.rds")
Dat_fs <- read_rds("~/Desktop/Diss-data/Morocco/Morocco_FAO.rds")

crop_names <- tibble(orig = names(Crop_area_stack))


crop_names <- crop_names %>%
 mutate(trans = c("NA", "barley", "NA", "NA","NA", "NA",
 "maize","NA", "NA","NA", "NA","NA"," millet", "potato","NA","NA",
 "rice","NA", "sorghum", "soybean", "sugarbeet", "sugarcane","NA", "NA","NA", "NA","NA","NA", 
 "wheat"))

crop_names <- crop_names %>%
  drop_na()
keep <- crop_names$orig

# filter stacks
Crop_area_stack <- Crop_area_stack[[which(Crop_area_stack %>% names() %in% keep)]]
Crop_yield_stack <- Crop_yield_stack[[which(Crop_yield_stack %>% names() %in% str_replace(keep, "phys_area", "yield"))]]

# rename
names(Crop_area_stack) <- crop_names$trans
names(Crop_yield_stack) <- crop_names$trans
Crop_area_stack <- readAll(Crop_area_stack)
Crop_yield_stack <- readAll(Crop_yield_stack)

# write out data
write_rds(Crop_area_stack, "~/Desktop/Diss-data/Morocco/morocco-crop-area-ha-2010.rds")
write_rds(Crop_yield_stack, "~/Desktop/Diss-data/Morocco/morocco-crop-yield-tonnes-per-ha-2010.rds")

# Now for sand
library(tidyverse)
library(raster)

Ras_sand <- raster("~/Desktop/Diss-data/Morocco/Sand/Mor_sand.tif")

Ras_sand <- Ras_sand %>%
  crop(Shp_Mor) %>%
  mask(Shp_Mor)

plot(Ras_sand)
names(Ras_sand) <- "soil_sand_pc"

Ras_sand <- readAll(Ras_sand) # throws an error cos it's already read it in — keep anyway jic
write_rds(Ras_sand, "~/Desktop/Diss-data/Morocco/Sand/morocco-soil-sand-percentage.rds")


