

Brk_croparea <- read_rds("~/Desktop/Diss-data/Morocco/morocco-crop-area-ha-2010.rds")
Brk_cropyield <- read_rds("~/Desktop/Diss-data/Morocco/morocco-crop-yield-tonnes-per-ha-2010.rds")

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

barley_area_df <- as.data.frame(Brk_croparea[[which(names(Brk_croparea) == "barley")]], xy = TRUE) 
str(barley_area_df)

wheat_area_df <- as.data.frame(Brk_croparea[[which(names(Brk_croparea) == "wheat")]], xy = TRUE) 
str(wheat_area_df)

chick_area_df <- as.data.frame(Brk_croparea[[which(names(Brk_croparea) == "chickpea")]], xy = TRUE) 
str(barley_area_df)

lentil_area_df <- as.data.frame(Brk_croparea[[which(names(Brk_croparea) == "lentil")]], xy = TRUE) 
str(barley_area_df)

maize_area_df <- as.data.frame(Brk_croparea[[which(names(Brk_croparea) == "maize")]], xy = TRUE) 
str(barley_area_df)

otherc_area_df <- as.data.frame(Brk_croparea[[which(names(Brk_croparea) == "other.cereal")]], xy = TRUE) 
str(barley_area_df)

otherp_area_df <- as.data.frame(Brk_croparea[[which(names(Brk_croparea) == "other.pulse")]], xy = TRUE) 
str(barley_area_df)

otherr_area_df <- as.data.frame(Brk_croparea[[which(names(Brk_croparea) == "other.root")]], xy = TRUE) 
str(barley_area_df)

mill_area_df <- as.data.frame(Brk_croparea[[which(names(Brk_croparea) == "millet")]], xy = TRUE) 
str(barley_area_df)

pot_area_df <- as.data.frame(Brk_croparea[[which(names(Brk_croparea) == "potato")]], xy = TRUE) 
str(barley_area_df)

rice_area_df <- as.data.frame(Brk_croparea[[which(names(Brk_croparea) == "rice")]], xy = TRUE) 
str(barley_area_df)

sorghum_area_df <- as.data.frame(Brk_croparea[[which(names(Brk_croparea) == "sorghum")]], xy = TRUE) 
str(barley_area_df)

soyb_area_df <- as.data.frame(Brk_croparea[[which(names(Brk_croparea) == "soybean")]], xy = TRUE) 
str(barley_area_df)

spot_area_df <- as.data.frame(Brk_croparea[[which(names(Brk_croparea) == "sweet.potato")]], xy = TRUE) 
str(barley_area_df)


ggplot(data = barley_area_df) +
  geom_raster(aes(x = x, y = y, fill = barley)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=30, linetype="dashed")+
  geom_vline(xintercept=-9.6, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Barley Harvested \nArea (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))


ggplot(data = wheat_area_df) +
  geom_raster(aes(x = x, y = y, fill = wheat)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=30, linetype="dashed")+
  geom_vline(xintercept=-9.6, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Wheat Harvested \nArea (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))


ggplot(data = chick_area_df) +
  geom_raster(aes(x = x, y = y, fill = chickpea)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=30, linetype="dashed")+
  geom_vline(xintercept=-9.6, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Chickpea Harvested \nArea (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

ggplot(data = lentil_area_df) +
  geom_raster(aes(x = x, y = y, fill = lentil)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=30, linetype="dashed")+
  geom_vline(xintercept=-9.6, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Lentil Harvested \nArea (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

ggplot(data = maize_area_df) +
  geom_raster(aes(x = x, y = y, fill = maize)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=30, linetype="dashed")+
  geom_vline(xintercept=-9.6, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Maize Harvested \nArea (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

ggplot(data = otherc_area_df) +
  geom_raster(aes(x = x, y = y, fill = other.cereal)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=30, linetype="dashed")+
  geom_vline(xintercept=-9.6, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Other Cereal \nHarvested Area (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

ggplot(data = otherp_area_df) +
  geom_raster(aes(x = x, y = y, fill = other.pulse)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=30, linetype="dashed")+
  geom_vline(xintercept=-9.6, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Other Pulses \nHarvested Area (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

ggplot(data = otherr_area_df) +
  geom_raster(aes(x = x, y = y, fill = other.root)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=30, linetype="dashed")+
  geom_vline(xintercept=-9.6, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Other Roots \nHarvested Area (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

ggplot(data = mill_area_df) +
  geom_raster(aes(x = x, y = y, fill = millet)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=30, linetype="dashed")+
  geom_vline(xintercept=-9.6, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Millet Harvested \nArea (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

ggplot(data = pot_area_df) +
  geom_raster(aes(x = x, y = y, fill = potato)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=30, linetype="dashed")+
  geom_vline(xintercept=-9.6, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Potato Harvested \nArea (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

ggplot(data = rice_area_df) +
  geom_raster(aes(x = x, y = y, fill = rice)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_vline(xintercept=-9.6, linetype="dashed")+
  geom_hline(yintercept=30, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Rice Harvested \nArea (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

ggplot(data = sorghum_area_df) +
  geom_raster(aes(x = x, y = y, fill = sorghum)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_vline(xintercept=-9.6, linetype="dashed")+
  geom_hline(yintercept=30, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Sorghum Harvested \nArea (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

ggplot(data = soyb_area_df) +
  geom_raster(aes(x = x, y = y, fill = soybean)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_vline(xintercept=-9.6, linetype="dashed")+
  geom_hline(yintercept=30, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Soybean Harvested \nArea (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

ggplot(data = spot_area_df) +
  geom_raster(aes(x = x, y = y, fill = sweet.potato)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_vline(xintercept=-9.6, linetype="dashed")+
  geom_hline(yintercept=30, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Sweet Potato \nHarvested Area (ha)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

# Now for yield

barley_y_df <- as.data.frame(Brk_cropyield[[which(names(Brk_cropyield) == "barley")]], xy = TRUE)
barley_y_df$barley <-barley_y_df$barley/1000
wheat_y_df <- as.data.frame(Brk_cropyield[[which(names(Brk_cropyield) == "wheat")]], xy = TRUE) 
wheat_y_df$wheat <- wheat_y_df$wheat/1000
chick_y_df <- as.data.frame(Brk_cropyield[[which(names(Brk_cropyield) == "chickpea")]], xy = TRUE) 
chick_y_df$chickpea <- chick_y_df$chickpea/1000
lentil_y_df <- as.data.frame(Brk_cropyield[[which(names(Brk_cropyield) == "lentil")]], xy = TRUE) 
lentil_y_df$lentil <- lentil_y_df$lentil/1000
maize_y_df <- as.data.frame(Brk_cropyield[[which(names(Brk_cropyield) == "maize")]], xy = TRUE) 
maize_y_df$maize <- maize_y_df$maize/1000
otherc_y_df <- as.data.frame(Brk_cropyield[[which(names(Brk_cropyield) == "other.cereal")]], xy = TRUE) 
otherc_y_df$other.cereal<-otherc_y_df$other.cereal/1000
otherp_y_df <- as.data.frame(Brk_cropyield[[which(names(Brk_cropyield) == "other.pulse")]], xy = TRUE) 
otherp_y_df$other.pulse<-otherp_y_df$other.pulse/1000
otherr_y_df <- as.data.frame(Brk_cropyield[[which(names(Brk_cropyield) == "other.root")]], xy = TRUE) 
otherr_y_df$other.root<-otherr_y_df$other.root/1000
mill_y_df <- as.data.frame(Brk_cropyield[[which(names(Brk_cropyield) == "millet")]], xy = TRUE) 
mill_y_df$millet<-mill_y_df$millet/1000
pot_y_df <- as.data.frame(Brk_cropyield[[which(names(Brk_cropyield) == "potato")]], xy = TRUE) 
pot_y_df$potato<-pot_y_df$potato/1000
rice_y_df <- as.data.frame(Brk_cropyield[[which(names(Brk_cropyield) == "rice")]], xy = TRUE) 
rice_y_df$rice<-rice_y_df$rice/1000
sorghum_y_df <- as.data.frame(Brk_cropyield[[which(names(Brk_cropyield) == "sorghum")]], xy = TRUE) 
sorghum_y_df$sorghum<-sorghum_y_df$sorghum/1000
soyb_y_df <- as.data.frame(Brk_cropyield[[which(names(Brk_cropyield) == "soybean")]], xy = TRUE) 
soyb_y_df$soybean<-soyb_y_df$soybean/1000
spot_y_df <- as.data.frame(Brk_cropyield[[which(names(Brk_cropyield) == "sweet.potato")]], xy = TRUE) 
spot_y_df$sweet.potato<-spot_y_df$sweet.potato/1000

ggplot(data = barley_y_df) +
  geom_raster(aes(x = x, y = y, fill = barley)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=30, linetype="dashed")+
  geom_vline(xintercept=-9.6, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Barley Yield \n(t ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))


ggplot(data = wheat_y_df) +
  geom_raster(aes(x = x, y = y, fill = wheat)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=30, linetype="dashed")+
  geom_vline(xintercept=-9.6, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Wheat Yield \n(t ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))


ggplot(data = chick_y_df) +
  geom_raster(aes(x = x, y = y, fill = chickpea)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=30, linetype="dashed")+
  geom_vline(xintercept=-9.6, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Chickpea Yield \n(t ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

ggplot(data = lentil_y_df) +
  geom_raster(aes(x = x, y = y, fill = lentil)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=30, linetype="dashed")+
  geom_vline(xintercept=-9.6, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Lentil Yield \n(t ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

ggplot(data = maize_y_df) +
  geom_raster(aes(x = x, y = y, fill = maize)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=30, linetype="dashed")+
  geom_vline(xintercept=-9.6, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Maize Yield \n(t ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

ggplot(data = otherc_y_df) +
  geom_raster(aes(x = x, y = y, fill = other.cereal)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=30, linetype="dashed")+
  geom_vline(xintercept=-9.6, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Other Cereal \nYield (t ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

ggplot(data = otherp_y_df) +
  geom_raster(aes(x = x, y = y, fill = other.pulse)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=30, linetype="dashed")+
  geom_vline(xintercept=-9.6, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Other Pulses \nYield (t ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

ggplot(data = otherr_y_df) +
  geom_raster(aes(x = x, y = y, fill = other.root)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=30, linetype="dashed")+
  geom_vline(xintercept=-9.6, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Other Roots \nYield (t ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

ggplot(data = mill_y_df) +
  geom_raster(aes(x = x, y = y, fill = millet)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=30, linetype="dashed")+
  geom_vline(xintercept=-9.6, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Millet Yield \n(t ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

ggplot(data = pot_y_df) +
  geom_raster(aes(x = x, y = y, fill = potato)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_hline(yintercept=30, linetype="dashed")+
  geom_vline(xintercept=-9.6, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Potato Yield \n(t ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

ggplot(data = rice_y_df) +
  geom_raster(aes(x = x, y = y, fill = rice)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_vline(xintercept=-9.6, linetype="dashed")+
  geom_hline(yintercept=30, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Rice Yield \n(t ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

ggplot(data = sorghum_y_df) +
  geom_raster(aes(x = x, y = y, fill = sorghum)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_vline(xintercept=-9.6, linetype="dashed")+
  geom_hline(yintercept=30, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Sorghum Yield \n(t ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

ggplot(data = soyb_y_df) +
  geom_raster(aes(x = x, y = y, fill = soybean)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_vline(xintercept=-9.6, linetype="dashed")+
  geom_hline(yintercept=30, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Soybean Yield \n(t ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))

ggplot(data = spot_y_df) +
  geom_raster(aes(x = x, y = y, fill = sweet.potato)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  geom_polygon(data=shp_Mor, aes(x=V1, y=V2), 
               fill=NA,color="black", size=1)+
  geom_vline(xintercept=-9.6, linetype="dashed")+
  geom_hline(yintercept=30, linetype="dashed")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Sweet Potato \nYield (t ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))








