##############################################################
###                     AniMove 2025                       ###    
### Script by Chloe Bracis, Thomas Mueller, Martina Scacco ###
##############################################################
###                  Recursion Exercise                    ###
##############################################################
# chloe.bracis@gmail.com
# https://cran.r-project.org/web/packages/recurse/vignettes/recurse.html

library(recurse)
library(move2)
library(mapview)
library(sf)
library(ggplot2)
library(ggforce)
library(ggspatial)
library(ggmap)
library(RgoogleMaps)
library(fields)
library(lubridate)
library(terra)
library(tidyterra)
library(scales)

# set wd to the MovementAnalysis/data folder in your computer
setwd("/home/martina/ownCloud/Martina/Teaching/Animove/Animove2025_CostaRica/Animove2025_CostaRica_MaterialPreparation/MovementAnalysis/data/")

#___________________________
### Initial exploration ####

### Read movebank elephant "Habiba" from Wall et al. 2014
elephants <- mt_read("Elliptical Time-Density Model (Wall et al. 2014) African Elephant Dataset (Source-Save the Elephants).csv")
habiba <- filter_track_data(elephants, .track_id = "Habiba")
mapview::mapView(mt_track_lines(habiba)$geometry) #as points

### About 2 weeks of data
range(mt_time(habiba))
difftime(max(mt_time(habiba)), min(mt_time(habiba)))

### Color code data day by day
uniqueDays <- unique(day(mt_time(habiba)))

ggplot() + theme_void() +
  geom_sf(data = mt_segments(habiba), aes(color = as.character(day(mt_time(habiba))))) +
  scale_color_manual(values = alpha(rainbow(length(uniqueDays)), 0.5)) +
  guides(color = "none")
# -> it appears that the looping behavior happens every day


#______________________________________
### Examine the number of revisits ####

### project to azimuthal equidistant projection (important to project your data in metres before applying these methods)
# and we center the projection to the median of the data coordinates
aeqd_crs <- mt_aeqd_crs(habiba, "center", "m") 
habibaAEQD <- sf::st_transform(habiba, aeqd_crs)

### Calculate recursions for a 500m Radius (you only need coords, time and radius)
r <- 500 # 500m 
habibavisits <- getRecursions(habibaAEQD, radius = r, timeunits = "hours") #radius in metres

# examine output object
class(habibavisits)
names(habibavisits)
str(habibavisits)
# length(revisits) = n.locations; 
# nrow(revisitStats) = n.locations * n.revisits at location
head(habibavisits$revisits) # first location is visited 8 times
head(habibavisits$revisitStats, 9) # here each visit is listed here with additional info about entrance, exit time and duration of the visit

### Plot revisits in graphics
# plot their distribution
hist(habibavisits$revisits, breaks = 10, col = "darkgrey", main = "", xlab = "Revisits (radius 500m)")
# plot revisits on trajectory
ggplot() +
  geom_sf(data=habibaAEQD, aes(color=habibavisits$revisits)) +
  scale_color_viridis_c(option = "plasma", name="N. visits")

### Plot revisits on a background map
# note: these maps expects latlong coordinates, so we use habiba (not habibaAEQD)
bb <- sf::st_bbox(habiba)
names(bb) <- c("left","bottom","right","top")

register_stadiamaps("10db4c8f-8cf3-447e-a26a-a554bde5c7d8", write = FALSE)
m <- ggmap::get_map(location = bb, zoom = 13, 
                    source = "stadia", maptype = 'stamen_terrain')
ggmap(m) +
  geom_sf(data = mt_track_lines(habiba), inherit.aes = FALSE, color="black") +
  geom_sf(data = habiba, aes(color=habibavisits$revisits), alpha=0.5, inherit.aes = FALSE) +
  scale_color_viridis_c(option = "plasma", name="N. visits")



#______________________________________
### Examine the first passage time ####

# the first visits passes through the center of the circle, therefore
# first visit at each location = first passage time
# (this only works in the recurse package if you use single individuals, not available for multiple individuals)
habibavisits$firstPassageTime <- habibavisits$revisitStats$timeInside[habibavisits$revisitStats$visitIdx == 1]

hist(as.numeric(habibavisits$firstPassageTime), breaks = 20, col = "darkgrey", main = "FPT in 500 m", 
     xlab = "First passage time (hrs)")
# the histogram indicates a bimodal distribution potentially indicating two different behaviors
# natural split between locations that are crossed in < 6 h and locations that take > 6 h to cross

# Highlight on map locations with first passage > 6 h
cutOff = 6
longFPTs <- habibavisits$firstPassageTime > cutOff
ggmap(m) +
  geom_sf(data = mt_track_lines(habiba), inherit.aes = FALSE, color="black") +
  geom_sf(data = habiba, aes(color=longFPTs), inherit.aes = FALSE) +
  scale_color_manual(values=alpha(c("grey","blue"),0.3), name="FPT > 6 hours")

# FPT > 6 h seem to happen at night
boxplot(as.numeric(habibavisits$firstPassageTime) ~ hour(habiba$timestamp), 
		outline = FALSE, col = "grey", xlab = "Daytime (hrs)", ylab = "First passage (hrs)")


#__________________________________________
### Examine residence/utilization time ####

# Examine residence/utilization time, the sum of all visits in each radius
# This indicates cumulative use of a certain location, summing subsequent visits
head(habibavisits$residenceTime)
hist(as.numeric(habibavisits$residenceTime), breaks = 20, col = "darkgrey", main = "", xlab= "Residence time (hrs)")

# there seems to be a bimodal distribution, separated at about 20 hrs total visit time
ggmap(m) +
  geom_sf(data = mt_track_lines(habiba), inherit.aes = FALSE, color="black") +
  geom_sf(data = habiba, aes(color=habibavisits$residenceTime), alpha=0.5, inherit.aes = FALSE) +
  scale_color_viridis_c(option = "inferno", name="Cumulative residence time (h)")

# this looks different than the previous map
# in fact, even though the previous plot highlighted the long FPT of some locations at night
# the sum of the time spent in other locations is higher if these get rivisited many many times
# this map looks similar to the first map we plotted, which was showing the number of revisits

boxplot(as.numeric(habibavisits$residenceTime) ~ hour(habiba$timestamp), 
        outline = FALSE, col = "grey", xlab = "Daytime (hrs)", ylab = "Residence time (hrs)")
# in fact the relationship to time is opposite to that of FPT
# here we have longer cumulative residence time during the day

# So it seems that we have few but long visits during the night of certain locations
# and short but frequent revisits of other locations


#_____________________________
### Examine revisitations ####

### Time since last visit in days
hist(as.numeric(habibavisits$revisitStats$timeSinceLastVisit / 24), freq = TRUE, 
     xlab = "Time since last visit (days)", col = "darkgrey", main = "")

### Revisitation time after 1 week
returnsAfterOneWeek <- as.vector(na.omit(habibavisits$revisitStats$coordIdx[as.numeric(habibavisits$revisitStats$timeSinceLastVisit / 24) > 7]))

ggmap(m) +
  geom_sf(data = mt_track_lines(habiba), inherit.aes = FALSE, color="black") +
  geom_sf(data = habiba[returnsAfterOneWeek, ], color = alpha("blue", 0.2), inherit.aes = FALSE)

# Some places are revisited after one week

### Shorter revisitation times
hist(as.numeric(habibavisits$revisitStats$timeSinceLastVisit / 24), freq = TRUE, 
   xlab = "Time since last visit (days)", col = "darkgrey", main = "", 
   xlim = c(0, 4), ylim = c(0, 400), breaks = 70)

# there seems to be a hint for periodicity - possible suggests periodogram analyses


#_________________________________
### Play with different radii ####

## Yes, it will inform us about revisits at different scales
## We can use the distance matrix in the recurse object to orient on what makes sense

stepLengths <- drop_units(mt_distance(habibaAEQD, units="m"))
summary(stepLengths)
hist(stepLengths)
# Max distance between any locations, gives us an idea about the area covered
max(habibavisits$dists)

# check out the package vignette for an example of testing different radii:
# https://cran.r-project.org/web/packages/recurse/vignettes/recurse.html


#____________________________
### Recursion to polygon ####

# Very interesting also the option to get recursions to a polygon defined by the user
# works with a single sf convex polygon (not multiple poygons)
plot(habibaAEQD$geometry)
coords <- locator(4)
coords <- cbind(coords[[1]],coords[[2]])

poly <- st_polygon(list(rbind(coords, coords[1, ]))) |> 
  st_sfc(crs = st_crs(habibaAEQD_mv))

plot(poly, add=T, col="red")

# get recursions to made up polygon
polyVisits <- getRecursionsInPolygon(habibaAEQD, poly, timeunits = 'hours')

str(polyVisits)
polyVisits$revisitStats

# changing the threshold (in the same unit as timeunits) 
# allows to set a time threshold to ignore brief excursions outside the circle/radius or polygon. 
polyVisits <- getRecursionsInPolygon(habibaAEQD, poly, timeunits = 'hours',
                                     threshold = 27)
polyVisits$revisitStats
# Now we only have 2 instead of 4 visits to the polygon


#____________________________
### Population recursions ###

fishers <- mt_read(mt_example())
mapview::mapView(mt_track_lines(fishers), zcol="individual-local-identifier", legend=F) #as lines

fishers <- filter_track_data(fishers, unique(mt_track_id(fishers)) != c("M4","M5"))
mapview::mapView(mt_track_lines(fishers), zcol="individual-local-identifier", legend=F) #as lines

# Tracked around the same time and with similar timelags
table(mt_track_id(fishers))
group_by(fishers, mt_track_id(fishers)) %>% 
  summarise(startTime=min(mt_time(fishers)),endTime=max(mt_time(fishers)),
            meanTL=mean(mt_time_lags(fishers, units="minutes"), na.rm=T)) %>% print(width=Inf)

fishers <- filter(fishers, !st_is_empty(fishers))

# movement scale of fishers (about 100 m)
summary(mt_distance(fishers, units="m"))

# aequidistant projection
aeqd_crs2 <- mt_aeqd_crs(fishers, "center", "m") 
fishersAEQD <- sf::st_transform(fishers, aeqd_crs2)

## get recursions at all locations of all individuals
r = 500
fishersvisits <- getRecursions(fishersAEQD, radius = r, timeunits = "hours") #radius in metres

nrow(fishers)
str(fishersvisits)
head(fishersvisits$revisitStats, 20)

# how many revisits per radius/location
ggplot() +
  geom_sf(data=fishersAEQD, aes(color=fishersvisits$revisits)) +
  scale_color_viridis_c(option = "plasma")

# how many individuals revisited each radius
ids_per_radius <- fishersvisits$revisitStats %>%
  group_by(coordIdx, x, y) %>%
  summarise(nIds = length(unique(id)))
ids_per_radius

ggplot() +
  geom_point(data=ids_per_radius, aes(x=x, y=y, color=nIds)) +
  scale_color_viridis_c(option = "inferno")


#_________________________________________________
### Recursions to locations (e.g. grid points) ###


bb_sf <- st_as_sfc(st_bbox(fishersAEQD), crs = st_crs(fishersAEQD))
grid <- st_sf(st_make_grid(bb_sf, cellsize = r, square = TRUE))
cellCentroids <- st_centroid(grid)
ggplot() +
  geom_sf(data = grid, fill = NA, colour = "black", linewidth = 0.1) +
  geom_sf(data=mt_track_lines(fishersAEQD), aes(color=`individual-local-identifier`))
ggplot() +
  geom_sf(data = grid, fill = NA, colour = "black", linewidth = 0.1) +
  geom_sf(data = cellCentroids, colour = "black")

centroidsDF <- data.frame(st_coordinates(cellCentroids))

fishersPXLvisits <- getRecursionsAtLocations(fishersAEQD, 
                                             locations = centroidsDF,
                                             radius = r)
str(fishersPXLvisits)
length(fishersPXLvisits$revisits) == nrow(cellCentroids)

# N of visits and cumulative visit duration per pixel across all individuals
grid$Nvisits <- fishersPXLvisits$revisits
grid$residenceTime <- fishersPXLvisits$residenceTime
# We can also calculate the number of individuals that visited each pixel
head(fishersPXLvisits$revisitStats)
IndsPerPxl <- fishersPXLvisits$revisitStats %>%
  group_by(coordIdx) %>%
  summarise(nInds=length(unique(id)))
grid$coordIdx <- 1:nrow(grid)
grid <- merge(grid, IndsPerPxl, by="coordIdx", all.x=T)
grid$nInds[grid$Nvisits==0] <- 0

ggplot(grid) +
  geom_sf(aes(fill = Nvisits), color = NA) +
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal()

ggplot(grid) +
  geom_sf(aes(fill = residenceTime), color = NA) +
  scale_fill_viridis_c(option = "inferno") +
  theme_minimal()

ggplot(grid) +
  geom_sf(aes(fill = nInds), color = NA) +
  scale_fill_viridis_c(option = "viridis") +
  theme_minimal()

## Optional: in case you want to export this as a raster in Geotiff
library(terra)
gridRast <- rast(ext(grid),
                 resolution = r, 
                 crs = crs(grid), nlyr=3)
values(gridRast[[1]]) <- grid$Nvisits
values(gridRast[[2]]) <- grid$residenceTime
values(gridRast[[3]]) <- grid$nInds
names(gridRast) <- c("Nvisits","residenceTime","nInds")

writeRaster(gridRast, "fishers_revisits_raster.tif", overwrite = TRUE)

# You can also explore individual patterns
# and create one raster per individual using the values
# in the revisitStats dataframe

# Finally you can use this information to create spatial clusters
# of frequently visited areas
plot(quantile(fishersPXLvisits$revisits, seq(0,1,0.01)))
visitThreshold <- 20
highlyVisited <- centroidsDF[fishersPXLvisits$revisits > visitThreshold,]
popCluster <- kmeans(highlyVisited, centers = 3)

ggplot() +
  geom_sf(data = grid, fill = NA, colour = "black", linewidth = 0.1) +
  geom_point(data = highlyVisited, aes(X, Y, colour = as.character(popCluster$cluster))) +
  theme_minimal()



## You can do the procedure of extracting centroids also directly with terra package
# library(terra)
# ext <- ext(fishersAEQD)
# grid <- rast(extent = ext, resolution = r, crs=crs(fishersAEQD))
# ggplot() +
#   geom_sf(data = st_as_sf(as.polygons(grid)), fill = NA, colour = "black", linewidth = 0.1) +
#   geom_sf(data=mt_track_lines(fishersAEQD), aes(color=`individual-local-identifier`))
# centroids <- xyFromCell(grid, 1:ncell(grid))
# ggplot() +
#   geom_point(data=centroids, aes(x, y))

