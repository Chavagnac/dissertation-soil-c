install.packages("tmap")
library(tmap)
library(RColorBrewer)

Ras_cropyield <- Brk_cropyield[[which(names(Brk_cropyield) == "barley")]]
Ras_croparea <- Brk_croparea[[which(names(Brk_croparea) == "barley")]]
rm(temp_cropname)
plot(Ras_croparea)
barley_area <- Brk_croparea[[which(names(Brk_croparea) == "barley")]]
wheat_area <- Brk_croparea[[which(names(Brk_croparea) == "wheat")]]
plot(barley_area)
plot(Shp_Mor, add=T)
plot(wheat_area, padj=-1.5, cex.axis=0.7, xlab="Longitude (degree)", ylab="Latitude (degree)", cex.lab=0.75)
axis(1,padj=-1.5, cex.axis=0.7, tck=-0.01, )
plot(Shp_Mor, add=T)

ggplot(data = barley_area_df) +
  geom_raster(aes(x = x, y = y, fill = barley)) +
                scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                                       na.value="white")+
  geom_polygon(data=shape, aes(x=V1, y=V2), 
              fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Barley area (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))


ggplot(data = wheat_area_df) +
  geom_raster(aes(x = x, y = y, fill = wheat)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shape, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Wheat area (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

# first we need to convert the raster data to dataframes
barley_area_df <- as.data.frame(Brk_croparea[[which(names(Brk_croparea) == "barley")]], xy = TRUE) 
str(barley_area_df)

wheat_area_df <- as.data.frame(Brk_croparea[[which(names(Brk_croparea) == "wheat")]], xy = TRUE) 
str(wheat_area_df)


ggplot() +
  geom_raster(data = barley_area_df , 
              aes(x = x, y = y, 
                  fill = barley)) + 
  geom_raster(data = wheat_area_df, 
              aes(x = x, y = y, 
                  fill=wheat)) +  
  scale_fill_viridis_c() +  
  scale_alpha(range = c(0.15, 0.65), guide = "none") +  
  ggtitle("Elevation with hillshade") +
  coord_quickmap()

# trying to overlay colours. Alpha defines how the overlap affects transparency
ggplot() +
  geom_raster(data = barley_area_df , 
              aes(x = x, y = y, 
                  fill = barley,
                  alpha=0.5)) +
  geom_raster(data = wheat_area_df, 
              aes(x = x, y = y, 
                  fill=wheat,
                  alpha = 0.5)) +  
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral"))) +  
  scale_alpha(range = c(0.1, 0.65), guide = "none") +  
  ggtitle("Elevation with hillshade") +
  coord_quickmap()

# THIS IS THE ONE! THIS IS A HEAT MAP THAT SHOWS OVERLAP THROUGH SUM
ggplot() +
  geom_path(data = barley_area_df, aes(x = x, y = y))

overlap <- overlay(barley_area, wheat_area, fun=function(barley,wheat){return(barley+wheat)})
plot(overlap)
overlap 

overlap_df <- as.data.frame(overlap[[which(names(overlap) == "values")]], xy = TRUE) 
str(overlap_df)
ggplot()+
  geom_raster(data = overlap_df, 
              aes(x = x, y = y, 
                  fill=layer)) +  
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white") + 
  geom_polygon(data=shape, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none") + 
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Overlapping wheat \nand barley \nproduction (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

  ggtitle("Overlap of area where Barley and Wheat crops are grown") +
  coord_quickmap()

# LOOK ABOVE!

file.exists("~/.ssh/id_rsa.pub")


fill()# well i'm not sure what i'm looking at so let's try something else
tm_shape(barley_area_df$barley) + tm_raster(barley_area) +
  tm_fill() +
  tm_borders() 

# Trying to add outline of morocco to maps of yields
plot(Shp_Mor)
shape <- as.data.frame(asg[[1]]@Polygons[[1]]@coords) 
str(shape)
plot(shape)
Shp_Mor@polygons$coords

asg<-Shp_Mor@polygons
t(shape)
ggplot()+
  geom_polygon(data=shape, aes(x=V1, y=V2), 
               fill=NA,color="grey50", size=1)
# now that we have the overlap, let's take a look at how the 
# crop yields work within that overlap to see which should be favoured

barley_yield <- Brk_cropyield[[which(names(Brk_cropyield) == "barley")]]
wheat_yield <- Brk_cropyield[[which(names(Brk_cropyield) == "wheat")]]
plot(barley_yield)
plot(wheat_yield)
plot(Shp_Mor, add=T)

barley_yield_df <- as.data.frame(barley_yield, xy = TRUE) 
str(barley_yield_df)
wheat_yield_df <- as.data.frame(wheat_yield, xy = TRUE) 
str(wheat_yield_df)

overlap_yield <- overlay(barley_yield, wheat_yield, fun=function(barley,wheat){return(barley+wheat)})
plot(overlap_yield)

overlap_yield_df <- as.data.frame(overlap_yield[[which(names(overlap_yield) == "values")]], xy = TRUE) 
str(overlap_yield_df)

ggplot()+
  geom_raster(data = overlap_yield_df, 
              aes(x = x, y = y, 
                  fill=layer)) +  
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white") + 
  geom_polygon(data=shape, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none") +
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Overlapping \nwheat and \nbarley yield (t ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

  ggtitle("Overlap of yields where Barley and Wheat crops are both grown") +
  coord_quickmap()

# The highest yields are in areas where they are not the most grown. 
# Is this being mishandled? What is normally grown there?
plot(Brk_cropyield[[which(names(Brk_cropyield) == "beans.and.pulses")]])
plot(Shp_Mor, add=T)
plot(Brk_croparea[[which(names(Brk_croparea) == "beans.and.pulses")]])
plot(Shp_Mor, add=T)


plot(Brk_cropyield[[which(names(Brk_cropyield) == "rye")]])
plot(Shp_Mor, add=T)
plot(Brk_croparea[[which(names(Brk_croparea) == "rye")]])
plot(Shp_Mor, add=T)

plot(Brk_cropyield[[which(names(Brk_cropyield) == "peanut")]])
plot(Shp_Mor, add=T)
plot(Brk_croparea[[which(names(Brk_croparea) == "peanut")]])
plot(Shp_Mor, add=T)

plot(Brk_cropyield[[which(names(Brk_cropyield) == "maize")]])
plot(Shp_Mor, add=T)
plot(Brk_croparea[[which(names(Brk_croparea) == "maize")]])
plot(Shp_Mor, add=T)

plot(Brk_cropyield[[which(names(Brk_cropyield) == "millet")]])
plot(Shp_Mor, add=T)
plot(Brk_croparea[[which(names(Brk_croparea) == "millet")]])
plot(Shp_Mor, add=T)

plot(Brk_cropyield[[which(names(Brk_cropyield) == "potato")]])
plot(Shp_Mor, add=T)
plot(Brk_croparea[[which(names(Brk_croparea) == "potato")]])
plot(Shp_Mor, add=T)

plot(Brk_cropyield[[which(names(Brk_cropyield) == "rice")]])
plot(Shp_Mor, add=T)
plot(Brk_croparea[[which(names(Brk_croparea) == "rice")]])
plot(Shp_Mor, add=T)

plot(Brk_cropyield[[which(names(Brk_cropyield) == "tubers")]])
plot(Shp_Mor, add=T)
plot(Brk_croparea[[which(names(Brk_croparea) == "tubers")]])
plot(Shp_Mor, add=T)

plot(Brk_cropyield[[which(names(Brk_cropyield) == "sorghum")]])
plot(Shp_Mor, add=T)
plot(Brk_croparea[[which(names(Brk_croparea) == "sorghum")]])
plot(Shp_Mor, add=T)

plot(Brk_cropyield[[which(names(Brk_cropyield) == "soybean")]])
plot(Shp_Mor, add=T)
plot(Brk_croparea[[which(names(Brk_croparea) == "soybean")]])
plot(Shp_Mor, add=T)

# there's no overlap. Could it have something to do with sand cover?
plot(Ras_sand)
plot(Shp_Mor, add = T)

lat_lon <- tibble(x = -4.9, y = 31) # default chosen here is a high-yielding arable land near Marrackech
yield_tha <- raster::extract(Ras_cropyield, lat_lon)
area_ha <- raster::extract(Ras_croparea, lat_lon)
sand_pc <- raster::extract(Ras_sand, lat_lon)
clim_coord_no <- raster::extract(Ras_clim, lat_lon)

sand_pc <- raster::extract(Ras_sand, lat_lon)
sand_df <- as.data.frame(Ras_sand[[which(names(Ras_sand) == "values")]], xy = TRUE) 
str(sand_df)

ggplot()+
  geom_raster(data = sand_df, 
              aes(x = x, y = y, 
                  fill=soil_sand_pc)) +  
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white") + 
  geom_polygon(data=shape, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none") + 
  ggtitle("Sand cover in Morocco") +
  coord_quickmap()

# Now let's take a look at how climate change is expected to change temperatures
# in morocco
Dat_clim$data_full[[1]] %>% head(10)
predicted_clim_raw <- as.data.frame(Dat_clim$data_full)
pred_precip <- data.frame(predicted_clim_raw$year, predicted_clim_raw$precip_mm)
pred_precip <- aggregate(pred_precip$predicted_clim_raw.precip_mm, by=list(predicted_clim_raw.year =pred_precip$predicted_clim_raw.year), FUN=sum)
names(pred_precip)[names(pred_precip)=='predicted_clim_raw.year'] <- 'Year'
names(pred_precip)[names(pred_precip)=='x'] <- 'Precipitation'
predicted_clim <- data.frame(predicted_clim_raw$month, predicted_clim_raw$year, predicted_clim_raw$temp_centigrade)
predicted_clim

names(predicted_clim)[names(predicted_clim)=='predicted_clim_raw.month'] <- 'month'
names(predicted_clim)[names(predicted_clim)=='predicted_clim_raw.year'] <- 'year'
names(predicted_clim)[names(predicted_clim)=='predicted_clim_raw.temp_centigrade'] <- 'Temperature'
names(predicted_clim)[names(predicted_clim)=='predicted_clim_raw.precip_mm'] <- 'Precipitation'

predicted_clim[which.max(predicted_clim$Temperature),]

ggplot()+
  geom_line(data=predicted_clim, 
            aes(x = interaction(month, year), y= Temperature, group=year, colour='red'))

ggplot()+
  geom_line(data=predicted_clim, 
            aes(x = interaction(month, year), y= Precipitation, group=year), color='blue')

# Now let's get the averages and overlay them to get a better idea of how temp and 
# rainfall is expected to change
av_pred_clim <- aggregate(predicted_clim$Temperature, list(year=predicted_clim$year), FUN=mean)
names(av_pred_clim)[names(av_pred_clim)=='x'] <- 'Temperature'
av_pred_clim <- data.frame(av_pred_clim$year, av_pred_clim$Temperature, pred_precip$Precipitation)
names(av_pred_clim)[names(av_pred_clim)=='av_pred_clim.year'] <- 'Year'
names(av_pred_clim)[names(av_pred_clim)=='av_pred_clim.Temperature'] <- 'Temperature'
names(av_pred_clim)[names(av_pred_clim)=='pred_precip.Precipitation'] <- 'Precipitation'


ggplot(av_pred_clim, aes(x=Year)) +
  
  geom_line( aes(y=Temperature), color='red') + 
  geom_line( aes(y=Precipitation/3), color='blue') + 
  scale_y_continuous(name = "Temperature",
                     sec.axis = sec_axis(~.*3, name="Precipitation"))+
  labs(colour = c("Temperature", "Precipitation"))

# Now that we have this down, let's see how yield is affected by temperature changes
# for wheat. Wheat fails at 34 degrees by a factor of 7.6% per day above that 
# temp. Let's make a function that generates wheat production based on months with 
# averages above 34 degrees. 

Dat_crop_wheat <- Dat_faostat %>% 
  filter(crop == 'wheat',
         year >= sim_start_year) %>%
  mutate(yield_rel = yield_tha / yield_tha[year == 2010],
         area_rel = area_harvested / area_harvested[year == 2010]) %>%
  dplyr::select(crop, year, yield_rel, area_rel)
Dat_crop_wheat <- Dat_crop_ts %>%
  mutate(yield_tha = yield_rel * yield_tha,
         area_ha = area_rel * area_ha)
Dat_crop_wheat %>%
  ggplot(aes(x = year, y = yield_tha)) +
  geom_line()
yield_mean <- Dat_crop_wheat %>% tail(10) %>% pull(yield_tha) %>% mean()
yield_sd <- Dat_crop_wheat %>% tail(10) %>% pull(yield_tha) %>% sd()
area_mean <- Dat_crop_wheat %>% tail(10) %>% pull(area_ha) %>% mean()
area_sd <- Dat_crop_wheat %>% tail(10) %>% pull(area_ha) %>% sd()

# randomly generated barley yield to 2070 based on 10-year performance
set.seed(260592)
Dat_preds <- tibble(year = 2019:sim_end_year,
                    yield_tha = rnorm(n = length(2019:sim_end_year), mean = yield_mean, sd = yield_sd),
                    area_ha = rnorm(n = length(2019:sim_end_year), mean = area_mean, sd = area_sd))

# bind simulation with historical data
Dat_crop_wheat <- bind_rows("historical" = Dat_crop_ts,
                         "simulated" = Dat_preds,
                         .id = "origin")

# plot to check
Dat_crop_wheat %>%
  ggplot(aes(x = year, y = yield_tha, colour = origin)) +
  geom_line()

Dat_crop_wheat %>%
  ggplot(aes(x = year, y = area_ha, colour = origin)) +
  geom_line()

# now for the heavy duty of combining the two dataframes under a new one according 
# to temperature impact on yield. 

temp_wheat_fail <- predicted_clim %>% filter(Temperature >= 34 )
temp_wheat_fail <- temp_wheat_fail %>% filter(month == 3| month ==4| month ==5)

Dat_preds_wheat <- Dat_preds

for (i in predicted_clim$year)
  if ((predicted_clim$month == 3 |predicted_clim$month == 4| predicted_clim$month == 5) & predicted_clim$Temperature >=34 &
    i == Dat_preds_wheat$year){Dat_preds_wheat$yield_tha <- 0}
  
for (i in Dat_preds_wheat$year)
  if (predicted_clim$month == 4 && predicted_clim$Temperature >=34 &&
    predicted_clim$year == Dat_preds_wheat$year){Dat_preds_wheat$yield_tha <- 0}
for (i in Dat_preds_wheat$year)
  if (predicted_clim$month == 5 && predicted_clim$Temperature >=34 &&
    predicted_clim$year == Dat_preds_wheat$year){Dat_preds_wheat$yield_tha <- 0}

    
    
Dat_crop_wheat_test <- bind_rows("historical" = Dat_crop_ts,
                            "simulated" = Dat_preds_wheat,
                            .id = "origin")

# plot to check
Dat_crop_wheat_test %>%
  ggplot(aes(x = year, y = yield_tha, colour = origin)) +
  geom_line()






# We now have the years where yield fails. Now let's make the yields reflect that. 
# Honestly for a dataset that small, it's easier to just change it manually













Dat_preds_wheat <- Dat_preds
Dat_preds_wheat[31, "yield_tha"] <- 0
Dat_preds_wheat[34, "yield_tha"] <- 0
Dat_preds_wheat[40, "yield_tha"] <- 0
Dat_preds_wheat[42, "yield_tha"] <- 0
Dat_preds_wheat[43, "yield_tha"] <- 0
Dat_preds_wheat[44, "yield_tha"] <- 0
Dat_preds_wheat[46, "yield_tha"] <- 0
Dat_preds_wheat[48, "yield_tha"] <- 0
Dat_preds_wheat[50, "yield_tha"] <- 0
Dat_preds_wheat[60, "yield_tha"] <- 0
    
Dat_crop_wheat <- bind_rows("historical" = Dat_crop_ts,
                            "simulated" = Dat_preds_wheat,
                            .id = "origin")
Dat_crop_wheat %>%
  ggplot(aes(x = year, y = yield_tha, colour = origin)) +
  geom_line()

Dat_crop_wheat %>%
  ggplot(aes(x = year, y = area_ha, colour = origin)) +
  geom_line()

# so basically that land is barren to wheat after 2058. Let's code that in to relect it. 

Dat_preds_wheat_real <- Dat_preds_wheat
Dat_preds_wheat_real[30:79, "yield_tha"] <- 0
Dat_crop_wheat <- bind_rows("historical" = Dat_crop_ts,
                            "simulated" = Dat_preds_wheat_real,
                            .id = "origin")

Dat_crop_wheat %>%
  ggplot(aes(x = year, y = yield_tha, colour = origin))+
  geom_line()+
  labs(x="Year", y="Yield (t ha-1)", colour="Simulation") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

  geom_line()

# Now for barley

temp_barley_fail <- predicted_clim %>% filter(Temperature >= 27 )
temp_barley_fail <- temp_wheat_fail %>% filter(month == 11| month ==12)

# There is no prediction for temperatures above 27 degrees in november 
# december for anytime in the future.







# Let's just do 5 data simulations since i can't figure out how to do a for loop

Dat_clim$data_full[[1]] %>% head(10)
predicted_clim_raw <- as.data.frame(Dat_clim$data_full)
pred_precip1 <- data.frame(predicted_clim_raw$year.1, predicted_clim_raw$precip_mm.1)
pred_precip1 <- aggregate(pred_precip1$predicted_clim_raw.precip_mm.1, by=list(predicted_clim_raw.year.1 =pred_precip1$predicted_clim_raw.year.1), FUN=sum)
names(pred_precip1)[names(pred_precip1)=='predicted_clim_raw.year.1'] <- 'Year'
names(pred_precip1)[names(pred_precip1)=='x'] <- 'Precipitation'

predicted_clim1 <- data.frame(predicted_clim_raw$month.1, predicted_clim_raw$year.1, predicted_clim_raw$temp_centigrade.1)
predicted_clim1

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
  geom_line(data=av_pred_clim, mapping= aes(x=Year, y=Temperature), color='red') + 
  geom_line(data=av_pred_clim, mapping=aes(x=Year, y=Precipitation/8), color='blue') +
  geom_line(data=av_pred_clim1, mapping=aes(x=Year, y=Temperature), color='red') + 
  geom_line(data=av_pred_clim1, mapping=aes(x=Year,y=Precipitation/8), color='blue')+
  geom_line(data=av_pred_clim2, mapping=aes(x=Year, y=Temperature), color='red') + 
  geom_line(data=av_pred_clim2, mapping=aes(x=Year,y=Precipitation/8), color='blue')+ 
  geom_line(data=av_pred_clim3, mapping=aes(x=Year, y=Temperature), color='red') + 
  geom_line(data=av_pred_clim3, mapping=aes(x=Year, y=Precipitation/8), color='blue')+  
  geom_line(data=av_pred_clim4, mapping=aes(x=Year, y=Temperature), color='red') + 
  geom_line(data=av_pred_clim4, mapping=aes(x=Year, y=Precipitation/8), color='blue') + 
  scale_y_continuous(name = "Annual Average Temperature (Celcius)",
                              sec.axis = sec_axis(~.*8, name="Annual Average Precipitation (mm)"))+
  labs(colour = c("Annual Average Temperature (Celcius)", "Annual Average Precipitation (mm)"))    
p

# Let's see how yield is affected now 

temp_wheat_fail1 <- predicted_clim1 %>% filter(Temperature >= 34 )
temp_wheat_fail1 <- temp_wheat_fail1 %>% filter(month == 3| month ==4| month ==5)
temp_wheat_fail2 <- predicted_clim2 %>% filter(Temperature >= 34 )
temp_wheat_fail2 <- temp_wheat_fail2 %>% filter(month == 3| month ==4| month ==5)
temp_wheat_fail3 <- predicted_clim3 %>% filter(Temperature >= 34 )
temp_wheat_fail3 <- temp_wheat_fail3 %>% filter(month == 3| month ==4| month ==5)
temp_wheat_fail4 <- predicted_clim4 %>% filter(Temperature >= 34 )
temp_wheat_fail4 <- temp_wheat_fail4 %>% filter(month == 3| month ==4| month ==5)
temp_wheat_fail_sum <- rbind(temp_wheat_fail,temp_wheat_fail1,temp_wheat_fail2, temp_wheat_fail3, temp_wheat_fail4)
temp_wheat_fail_sum

# wheat fails consistently every  year from 2048 onwards. 

# sand profiles
ggplot(data = sand_df) +
  geom_raster(aes(x = x, y = y, fill = soil_sand_pc)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shape, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Soil Sand \nComposition (%)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=8))

# barley yield estimates
Dat_crop_barley <- Dat_faostat %>% 
  filter(crop == 'barley',
         year >= '1961') %>%
  mutate(yield_rel = yield_tha / yield_tha[year == 2010],
         area_rel = area_harvested / area_harvested[year == 2010]) %>%
  dplyr::select(crop, year, yield_rel, area_rel, yield_tha, area_harvested)

Dat_crop_barley <- Dat_crop_barley %>%
  mutate(yield_tha = yield_rel * yield_tha,
         area_ha = area_rel * area_harvested)
Dat_crop_barley %>%
  ggplot(aes(x = year, y = yield_tha)) +
  geom_line()
yield_mean <- Dat_crop_barley %>% tail(10) %>% pull(yield_tha) %>% mean()
yield_sd <- Dat_crop_barley %>% tail(10) %>% pull(yield_tha) %>% sd()
area_mean <- Dat_crop_barley %>% tail(10) %>% pull(area_ha) %>% mean()
area_sd <- Dat_crop_barley %>% tail(10) %>% pull(area_ha) %>% sd()
set.seed(260592)
Dat_preds_b <- tibble(year = 2019:2097,
                    yield_tha = rnorm(n = length(2019:2097), mean = yield_mean, sd = yield_sd),
                    area_ha = rnorm(n = length(2019:2097), mean = area_mean, sd = area_sd))

# bind simulation with historical data
Dat_crop_barley <- bind_rows("historical" = Dat_crop_barley,
                            "simulated" = Dat_preds_b,
                            .id = "origin")    

barley_historical <- data.frame(Dat_faostat$year, Dat_faostat$crop, Dat_faostat$yield_tha, Dat_faostat$area_harvested)
barley_historical <- subset(barley_historical, Dat_faostat.crop=='barley')
write.csv(Dat_crop_barley,"C:\\Users\\Capucine\\Desktop\\Dat_crop_barley_future.csv", row.names = FALSE)
write.csv(barley_historical,"C:\\Users\\Capucine\\Desktop\\Dat_crop_barley_past.csv", row.names = FALSE)
# Now for rye

Dat_crop_rye <- Dat_faostat %>% 
  filter(crop == 'rye',
         year >= '1961') %>%
  mutate(yield_rel = yield_tha / yield_tha[year == 2010],
         area_rel = area_harvested / area_harvested[year == 2010]) %>%
  dplyr::select(crop, year, yield_rel, area_rel, yield_tha, area_harvested)

Dat_crop_rye <- Dat_crop_rye %>%
  mutate(yield_tha = yield_rel * yield_tha,
         area_ha = area_rel * area_ha)
Dat_crop_rye %>%
  ggplot(aes(x = year, y = yield_tha)) +
  geom_line()
yield_mean <- Dat_crop_rye %>% tail(10) %>% pull(yield_tha) %>% mean()
yield_sd <- Dat_crop_rye %>% tail(10) %>% pull(yield_tha) %>% sd()
area_mean <- Dat_crop_rye %>% tail(10) %>% pull(area_ha) %>% mean()
area_sd <- Dat_crop_rye %>% tail(10) %>% pull(area_ha) %>% sd()
set.seed(260592)
Dat_preds_r <- tibble(year = 2019:2097,
                      yield_tha = rnorm(n = length(2019:2097), mean = yield_mean, sd = yield_sd),
                      area_ha = rnorm(n = length(2019:2097), mean = area_mean, sd = area_sd))

# bind simulation with historical data
Dat_crop_rye <- bind_rows("historical" = Dat_crop_rye,
                             "simulated" = Dat_preds_r,
                             .id = "origin")    
write.csv(Dat_crop_rye,"C:\\Users\\Capucine\\Desktop\\Dat_crop_rye.csv", row.names = FALSE)

# and finally sorghum

Dat_crop_sorghum <- Dat_faostat %>% 
  filter(crop == 'sorghum',
         year >= "1961") %>%
  mutate(yield_rel = yield_tha / yield_tha[year == 2010],
         area_rel = area_harvested / area_harvested[year == 2010]) %>%
  dplyr::select(crop, year, yield_rel, area_rel, yield_tha, area_harvested)
Dat_crop_sorghum <- Dat_crop_sorghum %>%
  mutate(yield_tha = yield_rel * yield_tha,
         area_ha = area_rel * area_ha)
Dat_crop_sorghum %>%
  ggplot(aes(x = year, y = yield_tha)) +
  geom_line()
yield_mean <- Dat_crop_sorghum %>% tail(10) %>% pull(yield_tha) %>% mean()
yield_sd <- Dat_crop_sorghum %>% tail(10) %>% pull(yield_tha) %>% sd()
area_mean <- Dat_crop_sorghum %>% tail(10) %>% pull(area_ha) %>% mean()
area_sd <- Dat_crop_sorghum %>% tail(10) %>% pull(area_ha) %>% sd()
set.seed(260592)
Dat_preds_s <- tibble(year = 2019:2097,
                      yield_tha = rnorm(n = length(2019:2097), mean = yield_mean, sd = yield_sd),
                      area_ha = rnorm(n = length(2019:2097), mean = area_mean, sd = area_sd))

# bind simulation with historical data
Dat_crop_sorghum <- bind_rows("historical" = Dat_crop_sorghum,
                             "simulated" = Dat_preds_s,
                             .id = "origin")    
write.csv(Dat_crop_sorghum,"C:\\Users\\Capucine\\Desktop\\Dat_crop_sorghum.csv", row.names = FALSE)

# Let's compare crops now shall we

ggplot()+
  geom_line(data=Crops, aes(x=Year, y=Total.Carbon, colour=Crop))+
  labs(x="Year", y="Total Carbon Stock (t ha-1)", fill="Crop") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10))

# Now onto manure
ggplot()+
  geom_line(data=Manure, aes(x=Year, y=Total.Carbon, colour=Manure))+
  labs(x="Year", y="Total Carbon Stock (t ha-1)", fill="Manure Type") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10))

# and finally residue
ggplot()+
  geom_line(data=Residue, aes(x=Year, y=Total.Carbon, colour=Residue))+
  labs(x="Year", y="Total Carbon Stock (t ha-1)", colour="Residue \nremoved \nfrom field") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=9))
Residue$Residue <- factor(Residue$Residue, levels = c('100%', '50%', '30%'))


