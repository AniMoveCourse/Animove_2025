
#________________________________________________________
## Script showing examples of how to download
## different products from the CDS via ecmwfr package
#________________________________________________________
## Martina Scacco, December 2025

#________________
## Info about the use of the package can be found here:
# https://github.com/bluegreen-labs/ecmwfr

## Info about other products
# https://cds.climate.copernicus.eu/datasets

## Parameters (variables) and details about products (collections)
# https://confluence.ecmwf.int/display/CKB/ERA5%3A+data+documentation
# E.g. for ERA5: https://confluence.ecmwf.int/display/CKB/ERA5%3A+data+documentation#ERA5:datadocumentation-Parameterlistings
# E.g. for CORDEX: https://confluence.ecmwf.int/display/CKB/CORDEX%3A+Regional+climate+projections#CORDEX:Regionalclimateprojections-Listofpublishedparameters
# General: https://codes.ecmwf.int/grib/param-db/

#________________
# Sometimes choosing between products can seem hard. You can consult the ECMWF e-learning platform
# https://learning.ecmwf.int/course/index.php?categoryid=8

library(ecmwfr)
library(terra)
library(rnaturalearth)

#___________________
## Download borders:

borders <- ne_countries(country = c("Nicaragua","Costa Rica","Panama"), 
                        scale = "medium", returnclass = "sf")
# Define an area of interest. In CDS this is expressed in lat long 
# in the order North, West, South, East (ymax, xmin, ymin, xmax)
mybbox <- ext(borders) * 1.2 # expand a little in all direction
mybbox <- as.numeric(st_bbox(mybbox)[c("ymax", "xmin", "ymin", "xmax")]) # reorder for cds

#______________
## Set key:

# You can find your API key in your profile after logging in at https://cds.climate.copernicus.eu/
mykey <- "yourAPI"
wf_set_key(key = mykey)

#_____
setwd("/home/martina/ownCloud/Martina/Teaching/Animove/Animove2025_CostaRica/Animove2025_CostaRica_MaterialPreparation/EarthObservationModelling/")
dir.create("ecmwfR_testDownload")
outputPath <- "./ecmwfR_testDownload"

#________________
## Browse datasets:

catalogue <- wf_datasets()
View(catalogue)

# fuzzy search example
catalogue$name[grepl("era5*.single", catalogue$name)]
catalogue$name[grepl("cmip*", catalogue$name)]

## Interesting products for which you can find an example below:
# "reanalysis-era5-single-levels"
# "reanalysis-era5-pressure-levels"
# "derived-era5-single-levels-daily-statistics"
# "projections-cmip5-daily-pressure-levels"
# "projections-cmip6"
# "projections-cordex-domains-single-levels"

# !!! Before downloading any product, you have to log in on the CDS, go to the data catalogue to the product you need
# then to download, and click on "accept terms" at the bottom of the download page

#________________
## Download hourly data at surface:

## Info and parameters: https://confluence.ecmwf.int/display/CKB/ERA5%3A+data+documentation#ERA5:datadocumentation-Table1
## Product name: "reanalysis-era5-single-levels" 
## Product specific arguments: "time" e.g. c(06:00, 12:00)

years <- as.character(2025)
months <- sprintf("%02d", 1:2)
mydays <- sprintf("%02d", 1:10) # days is a function, so we call this object mydays
hours <- paste0(sprintf("%02d", 0:12), ":00")

request <- list(
  dataset_short_name = "reanalysis-era5-single-levels",
  product_type   = "reanalysis",
  variable       = c("10m_u_component_of_wind", "10m_v_component_of_wind"),
  year           = years,
  month          = months,
  day            = mydays,
  time           = hours,
  grid           = c(".9",".9"), # default is 0.25. You can specify a different resolution if spatial aggregation is desired
  area           = mybbox,  # for Costa Rica, bbox created at the top of the script
  format         = "netcdf",
  target         = "surface_uv_wind_janFeb_test.nc" #file name
  )
# send the request
wf_request(user = myuser,
           request = request,
           transfer = TRUE,
           path = outputPath, # path to store the file
           verbose = TRUE)

# import and plot file
(surfVar <- rast(paste0(outputPath,"/surface_uv_wind_janFeb_test.nc")))
plot(surfVar[[1]])
plot(borders$geometry, add=T, col=NA, border="white", lwd=2)
# Please note that if we download a spatial subset the longitudes are automatically converted to -180 to 180
# Instead if we download the entire globe (see below for cmip6), longitudes are 0-360, which means we need to rotate them to match other spatial datasets
# temp_rotated <- rotate(temp)


#________________
## Download hourly data at pressure levels:

## Info and parameters: https://confluence.ecmwf.int/display/CKB/ERA5%3A+data+documentation#ERA5:datadocumentation-Table9
## Product name: ""reanalysis-era5-pressure-levels"" 
## Product specific arguments: "time" e.g. c(06:00, 12:00), "pressure_level" e.g. c("650", "900")

years <- as.character(2023:2024)
months <- sprintf("%02d", 1:2)
mydays <- sprintf("%02d", 1:10)
hours <- paste0(sprintf("%02d", 10:23), ":00")
levels <- c("800", "900", "1000")
vars_pressure <- c("Geopotential", "u_component_of_wind" , "v_component_of_wind")

## Note: when you work with pressure levels, you need to always download the variable geopotential
## in order to convert the pressure level into a corresponding geometric height in metres
## E.g. height_850hpa = geopotential_850hpa / 9.80665

## These data are big, so we chop them in smaller files and save them all in one folder:
dir.create(paste0(outputPath, "/era5test_pressLev"))

lapply(years[1], function(yr){
  lapply(months[1], function(mn){
    lapply(vars_pressure, function(var){
      request <- list(
        dataset_short_name = "reanalysis-era5-pressure-levels",
        product_type = "reanalysis",
        variable = var,
        pressure_level = levels,
        year = yr,
        month = mn,
        day = mydays,
        time = hours,
        grid = c(".9",".9"),
        area = mybbox,  # for Costa Rica, bbox created at the top of the script
        format = "netcdf",
        target = paste0(yr, "_", mn, "_pressLev_", var, ".nc"))
      
      wf_request(user = myuser,
                 request = request,
                 transfer = TRUE,
                 path = paste0(outputPath, "/era5test_pressLev"),
                 verbose = TRUE)
      
    })
  })
})

# Import and plot. Note that here we have multiple columns per day because each day has multiple vertical (pressure) levels
(fls <- list.files(paste0(outputPath, "/era5test_pressLev"), full.names = T))
(geop <- rast(fls[1]))
# file contains one variable, for one month, for all days, all hours and all pressure levels (6 steps)
# So each file is a day-hour-pressureLevel combination
head(names(geop))
nlyr(geop) == length(mydays) * length(hours) * length(levels)
plot(geop[[1]]) #plot 1st hour of 1st day of 1st pressure level
plot(borders$geometry, add=T, col=NA, border="white", lwd=2)

# Using ncdf4 we can keep the multidimensionality of the object
library(ncdf4)
(nc <- nc_open(fls[1]))
str(nc)
names(nc)
# check dimensions
names(nc$dim)
# time
str(nc$dim$valid_time)
timesteps <- as.POSIXct(nc$dim$valid_time$vals, origin = "1970-01-01", tz = "UTC")
range(timesteps)
length(unique(timesteps))
# pressure levels
nc$dim$pressure_level$vals
# variable
geop2 <- ncvar_get(nc, "z")
dim(geop2)



#________________
## Download daily aggregates:

## Info and parameters: https://confluence.ecmwf.int/display/CKB/ERA5+family+post-processed+daily+statistics+documentation
## Product name: "derived-era5-single-levels-daily-statistics" 
## Product-specific arguments: 
# "daily_statistic" for aggregation e.g. 'daily_mean'
# "time_zone" for daily aggregation e.g. 'utc+00:00'

years <- "2025"
months <- sprintf("%02d", 1:2)
mydays <- sprintf("%02d", 1:31)
vars <- c("2m_temperature", "10m_u_component_of_wind", "10m_v_component_of_wind")

# Prepare request
request <- list(
  dataset_short_name = "derived-era5-single-levels-daily-statistics",
  product_type = "reanalysis",
  variable = vars,
  year = years,
  month = months,
  day = mydays,
  daily_statistic = "daily_mean", # type of aggregation
  #frequency = "6_hourly",
  time_zone = "utc+00:00", # !!important to compute daily aggregation
  area = mybbox,  # for Costa Rica, bbox created at the top of the script
  format = "netcdf", # format
  target = "2025_dailyAggregates_temp_wind" # file name
)
# Download
wf_request(user = myuser,
           request = request,
           transfer = TRUE,
           path = outputPath, # path to store the file
           verbose = TRUE)

#________________
## Download historical or future climatologies (CMIP and CORDEX):

## An overview of the different available CMIP is at: https://pcmdi.llnl.gov/mips/cmip5/
## Note that the available variables and resolution changes a lot between different CMIP and models

#__
## Example for CMIP5 (former IPCC programme), which has also wind components available but different request format:
## https://doi.org/10.1175/BAMS-D-11-00094.1 reference paper with experiment details
## https://pcmdi.llnl.gov/mips/cmip5/RCP_journal_special.pdf?id=26 RCP details (Representative Concentration Pathways) of the different scenarios

## Product-specific arguments: 
# product name: "projections-cmip5-daily-pressure-levels"
# period argument can be specified as one character (in CMIP6 and CORDEX not possible)
# multiple variables are allowed (in CMIP6 and CORDEX not possible)
# but pay attention to variable name, each product has different names

myperiod <- "20500101-20501231" #Only entire period can be specified, not specific months or days
varsCmip5 <- c("temperature","u_component_of_wind","v_component_of_wind")

request <- list(
  dataset_short_name = "projections-cmip5-daily-pressure-levels",
  experiment = "rcp_2_6",
  variable = varsCmip5,
  model = "mpi_esm_mr",
  ensemble_member = "r1i1p1",
  period = myperiod,
  area = mybbox,  # for Costa Rica, bbox created at the top of the script
  target = "CMIP5_TestDaily_2050"
)
wf_request(user = myuser,
           request = request,
           transfer = TRUE,
           path = outputPath,
           verbose = TRUE)

#__
## Example for CMIP6 (current IPCC programme), which has also wind components available but different request format:

# The current IPCC CMIP6 is still in progress, and the last update on CDS was done in 2021 (another expected soon)
# So you might consider downloading these data from ISIMIP or ESGF platforms
## Info and parameters for CMIP6: 
# https://confluence.ecmwf.int/display/CKB/CMIP%3A+Global+climate+projections
# https://cds.climate.copernicus.eu/datasets/projections-cmip6?tab=overview

## Product-specific arguments: 
# Product name: "projections-cmip6" 
# "year" (up to 2100) "temporal_resolution" e.g. "daily", "experiment" e.g. "ssp1_2_6" or "ssp2_4_5" and "model" e.g. "mpi_esm1_2_lr" need to be specified
# one variable at a time

futYears <- as.character(2091:2100)
mydays <- sprintf("%02d", 1:31)
months <- sprintf("%02d", 1:12)

## Example for CMIP6 (this does not have wind components U-V at daily scale, but at monthly yes)
request <- list(
  dataset_short_name = "projections-cmip6",
  temporal_resolution = "daily",
  experiment = "ssp2_4_5",  # scenario
  variable = "near_surface_air_temperature",  # one variable at a time
  model = "mpi_esm1_2_lr",  # one model at a time
  day = days,
  month = months,
  year = futYears,
  target = "CMIP6_TestDaily_2100"
)
wf_request(user = myuser,
           request = request,
           transfer = TRUE,
           path = outputPath,
           verbose = TRUE)

# Import and plot
unzip(paste0(outputPath,"/CMIP6_TestDaily_2100.zip"), exdir=paste0(outputPath,"/CMIP6_TestDaily_2100"))
(fls <- list.files(paste0(outputPath,"/CMIP6_TestDaily_2100"), full.names = T))
(temp <- rast(fls[3]))
range(dates <- time(temp))

nlyr(temp) == length(dates)

plot(temp[[1]])
ext(temp)
temp_rotate <- terra::rotate(temp[[1]])
plot(temp_rotate[[1]])
plot(borders$geometry, add=T, col=NA, border="white", lwd=2)

#____________
## Example for CORDEX regional model (0.11 resolution)
## https://cds.climate.copernicus.eu/datasets/projections-cordex-domains-single-levels?tab=download
# time period is specified by start_year and end_year
# both gcm (global model) and rcm (regional model) have to be specified
# only one variable at a time

varsCordex <- c("850hpa_u_component_of_the_wind","850hpa_v_component_of_the_wind")

lapply(varsCordex, function(v){
request <- list(
  dataset_short_name = "projections-cordex-domains-single-levels",
  domain = "europe",
  experiment = "rcp_2_6",
  horizontal_resolution = "0_11_degree_x_0_11_degree",
  temporal_resolution = "daily_mean",
  variable = v,
  gcm_model = "mpi_m_mpi_esm_lr",
  rcm_model = "mpi_csc_remo2009",
  ensemble_member = "r1i1p1",
  start_year = "2056",
  end_year = "2060",
  target = paste0("CORDX_TestDaily_2056-2060_",v)
)
wf_request(user = myuser,
           request = request,
           transfer = TRUE,
           path = outputPath,
           verbose = TRUE)
})