install.packages("rio")
install.packages("rgdal")
library("rio")
library("rgdal")
library("raster")

fbar <- "barley-france.tif"
france_barley <- raster(fbar)
plot(france_barley)

fmaiz <- "France_maize.tif"
france_maize <- raster(fmaiz)
plot(france_maize)

fpot <- "France_potato.tif"
france_potato <- raster(fpot)
plot(france_potato)

frap <- "France_rape.tif"
france_rapeseed <- raster(frap)
plot(france_rapeseed)

fsug <- "sugarbeet-france.tif"
france_beet <- raster(fsug)
plot(france_beet)

fwheat <- "France_wheat.tif"
france_wheat <- raster(fwheat)
plot(france_wheat)
