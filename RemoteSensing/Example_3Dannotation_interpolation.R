
#########################################################
###                  AniMove 2025                     ###    
###    Script by Martina Scacco and Kamran Safi       ###
#########################################################
###       ERA5 annotation in 2D space + time          ###
#########################################################

## For a more complete integration of era5 annotations and move2 objects see:
##https://github.com/kamransafi/ERA5_move2

library(ecmwfr)
library(terra)
library(dplyr)

# wd to your earth observation data folder
setwd("/home/martina/ownCloud/Martina/Teaching/Animove/Animove2025_CostaRica/Animove2025_CostaRica_MaterialPreparation/EarthObservationModelling/data")

# Open data. The data has already been thinned to roughly an hourly resolution
albi <- readRDS("albatross_track.rds")
str(albi)

# Convert to sf and plot
albi_sf <- st_as_sf(albi, coords = c("location_long", "location_lat"), crs = "EPSG:4326")
mapview(albi_sf)

# Get spatial coverage (bbox)
albibbox <- ext(albi_sf) * 1.5 # expand a little in all direction

# Check temporal coverage
range(albi_sf$timestamp)

#_______________________________________________
# Download era5 weather reanalysis data from CDS
# we need to download data at "single levels"

dir.create("albi_windData")

# set up your cds account (find it in your profile after logging in at https://cds.climate.copernicus.eu/)
mykey <- "yourAPI"
wf_set_key(key = mykey)

# Remember to manually click on accept terms at the bottom of the download page on the CDS catalogue 

# time steps to download
dates <- as.Date(unique(date(albi$timestamp)))
hours <- paste0(sprintf("%02d", 0:23), ":00")

# variables to download
vars <- c("10m_u_component_of_wind", "10m_v_component_of_wind")

# format bbox for cds (North, West, South, East (ymax, xmin, ymin, xmax))
albibbox_cds <- as.numeric(st_bbox(albibbox)[c("ymax", "xmin", "ymin", "xmax")])

# these data are lighter, in this cse we can download them all in one file
request <- list(
  "dataset_short_name" = "reanalysis-era5-single-levels",
  "product_type"   = "reanalysis",
  "variable"       = vars,
  "year"           = unique(format(dates, "%Y")),
  "month"          = unique(format(dates, "%m")),
  "day"            = unique(format(dates, "%d")),
  "time"           = hours,
  "area"           = albibbox_cds,
  "format"         = "netcdf",
  "target"         = paste0("albi_era5_singleLev.nc"))

wf_request(user = myuser,
           request = request,
           transfer = TRUE,
           path = "albi_windData", #store in wd data
           verbose = TRUE)

#___________________________________________________________
# Annotate albatross data with era5 data in 2D space + time

(f <- list.files("albi_windData", pattern="single", full.names=T))
r <- rast(f)

# variables in raster
rastvars <- paste0(unique(varnames(r)),"_")  # variables contained in r
# Timesteps of raster, extracted from column names, these are in seconds since 1-1-1970
timeSteps <- as.numeric(unique(gsub(".*=","", names(r))))
timeSteps <- as.POSIXct(timeSteps, origin = "1970-01-01", tz = "UTC") 

# interpolate rast values in space at the locations of albi, so same nrows
interpolated_inSpace <- terra::extract(r, albi_sf, method = "bilinear", ID=F)

# interpolate in time separately for each variable and for each pressure level
interpolated_inTime <- bind_cols(lapply(rastvars, function(var){
  varSlice <- interpolated_inSpace[, grep(var, names(interpolated_inSpace))] # all time steps for each variable
  # interpolate the variable value in time for that combination
  timeInt_oneVar <- vapply(seq_len(nrow(varSlice)),
                           function(i) {
                             approx(timeSteps, # unique time steps
                                    varSlice[i, ], # corresponding rast values at those time steps
                                    xout = albi_sf$timestamp[i])$y # interpolate rast values at the times of albi
                           }, numeric(1)) # this is the variable interpolated at all locations for one variable
  timeInt_oneVar <- data.frame(timeInt_oneVar)
  names(timeInt_oneVar) <- var
  return(timeInt_oneVar)
}))
annotated_albi <- bind_cols(albi_sf, interpolated_inTime)

# Calculate wind speed 
annotated_albi <- annotated_albi %>%
  mutate(wind_speed_10_ms = as.numeric(sqrt(u10_^2 + v10_^2)))

# Plot wind speed on track
mapview(annotated_albi, zcol = "wind_speed_10_ms")

