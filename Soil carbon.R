# Plotting Soil organic carbon worldwide

Soil_C <- "~/Desktop/GSOCmapV1.2.0.tif"
map_c  <- raster(Soil_C)
plot(map_c)

mor_c_df <- as.data.frame(map_c, xy=TRUE)
ggplot(data = mor_c_df) +
  geom_raster(aes(x = x, y = y, fill = GSOCmapV1.2.0)) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")),
                       na.value="white")+
  scale_alpha(range = c(0.1, 0.65), guide = "none")+
  labs(x="Longitude (degree)", y="Latitude (degree)", fill="Soil Organic Carbon \n(tonne ha-1)") +
  theme(plot.title = element_text(hjust=0.5)) + theme(axis.text.x=element_text(size=9), legend.title = element_text(size=10), legend.key.height=unit(1, "cm"))
