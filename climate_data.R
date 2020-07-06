library(raster)
library(tidyverse)
library(ncdf4)
library(lubridate)

#gisdata_repo <- "GIS data repository"

# check out ncdf data and read in as raster bricks
nc_open("~/Desktop/Global Weather/Model 1/tempmean_rcp85_land-gcm_global_60km_01_mon_189912-209911.nc")
nc_open("~/Desktop/Global Weather/Model 1/precipitation_rcp85_land-gcm_global_60km_01_mon_189912-209911.nc")

Stk_temp <- brick("~/Desktop/Global Weather/Model 1/tempmean_rcp85_land-gcm_global_60km_01_mon_189912-209911.nc")
Stk_precip <- brick("~/Desktop/Global Weather/Model 1/precipitation_rcp85_land-gcm_global_60km_01_mon_189912-209911.nc")

# read in shapefile and mask brick
Shp_Mor <- shapefile("~/Desktop/Diss-data/Morocco/Shapefiles/Morocco_shp.shp")
Stk_temp <- Stk_temp %>% crop(Shp_Mor)
Stk_precip <- Stk_precip %>% crop(Shp_Mor)

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
write_rds(Dat_clim, "~/Desktop/Global Weather/Model 1/morocco-full-climate-data-1902-2097.rds")

# how about a helper raster to allow spatial point querying of data?
# number lat/lon values and transform to raster, keep numbers in climate df
Ras_help <- Dat_clim %>%
  select(x, y, z = cell_no) %>%
  rasterFromXYZ()

# write out
write_rds(Ras_help, "~/Desktop/Global Weather/Model 1/morocco-climate-data-helper-raster.rds")

Dat_clim <- read_rds("~/Desktop/Global Weather/Model 1/FINAL-Mor-full-climate-data-1902-2097.rds")
#Ras_clim <- read_rds("~/Desktop/Global Weather/Model 1/morocco-climate-data-helper-raster.rds")
sample_n(Dat_clim, 10, replace = F)
Dat_clim$data_full[[1]] %>% head(10)

plot(Ras_clim)
plot(Shp_Mor, add = T)

lat_lon <- tibble(x = -9.583333, y = 30.27779)
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
write_rds(Dat_clim, "~/Desktop/Global Weather/Model 1/morocco-example-climate-data.rds")

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
  geom_line(data=av_pred_clim5, mapping=aes(x=Year, y=Precipitation/20), color='blue') +
  geom_line(data=av_pred_clim1, mapping=aes(x=Year, y=Temperature), color='red') + 
  geom_line(data=av_pred_clim1, mapping=aes(x=Year,y=Precipitation/20), color='blue')+
  geom_line(data=av_pred_clim2, mapping=aes(x=Year, y=Temperature), color='red') + 
  geom_line(data=av_pred_clim2, mapping=aes(x=Year,y=Precipitation/20), color='blue')+ 
  geom_line(data=av_pred_clim3, mapping=aes(x=Year, y=Temperature), color='red') + 
  geom_line(data=av_pred_clim3, mapping=aes(x=Year, y=Precipitation/20), color='blue')+  
  geom_line(data=av_pred_clim4, mapping=aes(x=Year, y=Temperature), color='red') + 
  geom_line(data=av_pred_clim4, mapping=aes(x=Year, y=Precipitation/20), color='blue') + 
  scale_y_continuous(name = "Annual Average Temperature (Celcius)",
                     sec.axis = sec_axis(~.*20, name="Annual Total Precipitation (mm)"))+
  labs(colour = c("Annual Average Temperature (Celcius)", "Annual Total Precipitation (mm)"))    
p




