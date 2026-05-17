
#########################################################
###                  AniMove 2025                     ###    
###    Script by Martina Scacco and Kamran Safi       ###
#########################################################
###       ERA5 annotation in 3D space + time          ###
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

# Check heights
albi_sf$height_ellips <- abs(rnorm(nrow(albi_sf), mean=1000, sd=200)) # let's make up some random heights
plot(height_ellips~timestamp, albi_sf, type="l")

# Download geoid
url <- "https://download.agisoft.com/gtg/us_nga_egm2008_1.tif"
(geoid <- rast(paste0("/vsicurl/", url)))
geoid <- crop(geoid, albibbox)

# Calculate height asl from ellipsoid and explore heights
N <- extract(geoid, albi_sf)[,1]  # geoid undulation
albi_sf$height_asl <- albi_sf$height_ellips - N
plot(height_ellips~timestamp, albi_sf, type="l")
lines(height_asl~timestamp, albi_sf, col="red")

#_______________________________________________
# Download era5 weather reanalysis data from CDS
# we need to download data at "pressure levels"

dir.create("albi_windData")

# set up your cds account (find it in your profile after logging in at https://cds.climate.copernicus.eu/)
mykey <- "yourAPI"
wf_set_key(key = mykey)
# Remember to manually click on accept terms at the bottom of the download page on the CDS catalogue 

# time steps to download
dates <- as.Date(unique(date(albi$timestamp)))
hours <- paste0(sprintf("%02d", 0:23), ":00")

# variables to download
# Note that to work with pressure levels you always need to download geopotential to be able to convert each pressure level to heights above sea level
vars <- c("Geopotential","u_component_of_wind", "v_component_of_wind")

# pressure levels to download
summary(albi_sf$height_asl)
levs <- c("975","900","850") # more or less covering tha range of the above heights

# format bbox for cds (North, West, South, East (ymax, xmin, ymin, xmax))
albibbox_cds <- as.numeric(st_bbox(albibbox)[c("ymax", "xmin", "ymin", "xmax")])

# In this case we decide to download the env data in daily files
lapply(dates, function(onedate){
  request <- list(
    "dataset_short_name" = "reanalysis-era5-pressure-levels",
    "product_type"   = "reanalysis",
    "variable"       = vars,
    "pressure_level" = levs,
    "year"           = format(onedate, "%Y"),
    "month"          = format(onedate, "%m"),
    "day"            = format(onedate, "%d"),
    "time"           = hours,
    "area"           = albibbox_cds,
    "format"         = "netcdf",
    "target"         = paste0("albi_era5_pressLev_",onedate,".nc"))
  
  wf_request(user = myuser,
             request = request,
             transfer = TRUE,
             path = "albi_windData", #store in wd data
             verbose = TRUE)
})


#_______________________________________________
# Annotate albatross data with era5 data in 4D

## Read in env data (we should have three files, one per variable)
(fls <- list.files("albi_windData", full.names = T))

## Split the tracking data by day
albi_daylist <- split(albi_sf, date(albi_sf$timestamp))

annotated_albi <- bind_rows(lapply(albi_daylist, function(albiday){
  # when available take always day before and day after to make sure first and last point of the day can be annotated
  date <- unique(date(albiday$timestamp))
  f <- grep(paste(date-1,date,date+1, sep = "|"), fls, value=T)
  r <- rast(f)
  if(!"z"%in%varnames(r) & !"Geopotential"%in%longnames(r)){
    stop("I cannot find geopotential among the variables. Without geopotential vertical annotation of the track to the pressure levels is not possible.")
  }
  rastvars <- paste0(unique(varnames(r)),"_")  # variables contained in r
  rastlevs <- paste0("level=",unique(depth(r))) # levels contained in r
  # extract unique timesteps from column names, these are in seconds since 1-1-1970
  timeSteps <- as.numeric(unique(gsub(".*=","", names(r))))
  timeSteps <- as.POSIXct(timeSteps, origin = "1970-01-01", tz = "UTC") # should give either 48 or 72 (depending if 3 or 2 daily files were available)
  # interpolate rast values at the locations of albi, so same nrows
  interpolated_inSpace <- terra::extract(r, albiday, method = "bilinear", ID=F)
  # interpolate in time separately for each variable and for each pressure level
  interpolated_inTime <- bind_cols(lapply(rastvars, function(var){
    return(bind_cols(lapply(rastlevs, function(pl){
      varLev <- paste0(var,".*",pl)
      # take the var-pressLev combination. Each row is one location, each column is one time step, per var and per plev
      varPressSlice <- interpolated_inSpace[, grep(varLev, names(interpolated_inSpace))] # all time steps in one pressure level
      # interpolate the variable value in time for that combination
      timeInt_onePl <- vapply(seq_len(nrow(varPressSlice)),
                              function(i) {
                                approx(timeSteps, # unique time steps
                                       varPressSlice[i, ], # corresponding rast values at those time steps
                                       xout = albiday$timestamp[i])$y # interpolate rast values at the times of albi
                              }, numeric(1)) # this is the variable interpolated at all locations for one press lev
      timeInt_onePl <- data.frame(timeInt_onePl)
      names(timeInt_onePl) <- paste0(var,pl)
      return(timeInt_onePl)
    })))
  }))
  # Convert geopotential height to geometric asl height of each pressure level at each location 
  levHeights <- interpolated_inTime[, grep("z_", names(interpolated_inTime))] / 9.80665
  # Now for each variable separately we interpolate vertically between pressure levels
  interpolated_inHeight <- bind_cols(lapply(rastvars[rastvars!="z_"], function(var){
    varSlice <- interpolated_inTime[, grep(var, names(interpolated_inTime))]
    # interpolate var per height based on levHeights
    heightInt_oneVar <- vapply(seq_len(nrow(varSlice)),
                            function(i) {
                              approx(levHeights[i,], # unique time steps
                                     varSlice[i, ], # corresponding rast values at those time steps
                                     xout = albiday$height_ellips[i])$y # interpolate rast values at the times of albi
                            }, numeric(1)) # this is the variable interpolated at all locations for one press lev
    heightInt_oneVar <- data.frame(heightInt_oneVar)
    names(heightInt_oneVar) <- paste0(var)
    return(heightInt_oneVar)
  }))
  annotated_albyday <- bind_cols(albiday, interpolated_inHeight)
  return(annotated_albyday)
}))
 
# Calculate wind speed 
annotated_albi <- annotated_albi %>%
  mutate(wind_speed_ms = as.numeric(sqrt(u_^2 + v_^2)))

# Plot wind speed on track
mapview(annotated_albi, zcol = "wind_speed_ms")
