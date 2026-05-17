#########################################################
###                  AniMove 2025                     ###    
### Script by Kami Safi, Martina Scacco & Anne Scharf ###
#########################################################
###  Intro to Move2, load & export data, manipulation ###
#########################################################

# install.packages("remotes")
# remotes::install_github("AniMoveCourse/animove_R_package")

library(move2)
library(dplyr)
library(sf)
library(vroom)

# set wd to the MovementAnalysis/data folder in your computer
setwd("/home/martina/ownCloud/Martina/Teaching/Animove/Animove2025_CostaRica/Animove2025_CostaRica_MaterialPreparation/MovementAnalysis/data/")

#______________________________
# STRUCTURE OF A MOVE2 OBJECT #
#______________________________

### Event and track attribute tables
mv2 <- mt_read(mt_example())
print(mv2)
class(mv2)

#_______________________________
# GETTING TRACKING DATA INTO R #
#_______________________________

###________________________________________________
### 1. Directly downloading data from Movebank ####

### First we need to store our movebank credentials in R
## you will be promped to set a keyring password
## by default this will be stored in the key list as service="movebank"
movebank_store_credentials("RBook", "Obstberg1")

## To check which accounts are stored in keyring:
keyring::key_list(service="movebank")

### Remove accounts:
## the default one:
movebank_remove_credentials()
keyring::key_list()
## a specific account with a key name:
movebank_remove_credentials(key_name = "movebank")
keyring::key_list()

## We restore it
movebank_store_credentials("RBook", "Obstberg1")

## More details about credentials here: https://bartk.gitlab.io/move2/articles/movebank.html

###___________
### For the actual download we will look at three functions:
## movebank_download_study_info() 
## movebank_download_deployment() 
## movebank_download_study()

### Browse the Movebank database
## list studies for which we have download access / you are collaborator of
movebank_download_study_info(i_have_download_access=T)
movebank_download_study_info(i_am_collaborator = T)

## select only columns of interest and studies about bats
movebank_download_study_info() %>%  
  select(id, name, number_of_deployed_locations) %>% 
  filter(grepl("Parti-colored bat", name))

## Get the metadata of a specific study (for study_id both name or number work)
movebank_download_study_info(study_id = 1918503) %>%  
  print(width = Inf) #all cols

### Download information about a specific Movebank study
## check for reference data of animals, deployments and tags
movebank_download_deployment(study_id = 1918503)
movebank_download_deployment(study_id = "Parti-colored bat") #same result

## what sensors available in the study?
unique(movebank_download_deployment(study_id = 1918503)$sensor_type_ids) # external_id of sensor
movebank_download_study_info(study_id = 1918503)$sensor_type_ids # name of sensor

## retrieve all sensor ids recognised by Movebank
(sens <- movebank_retrieve(entity_type = "tag_type"))


##### a. Download LOCATION data as a move2 object ----

### download all data from specific sensor ("external_id" or "id" from table above "sens")
## it is recommendable to ALWAYS SPECIFY THE SENSOR to ensure that all attributes associated 
## to the sensor get downloaded. If sensor is not specified, 
## only attributes of location data are downloaded
movebank_download_study(study_id = 1918503, 
                        sensor_type_id = "radio-transmitter")

### specify one or more animals, by specifying the individual_local_identifier (or the individual_id)
movebank_download_study(study_id = 1918503,
                        sensor_type_id = "radio-transmitter",
                        individual_local_identifier = c(239,360))

### for a specific time range
movebank_download_study(study_id = 1918503,
                        sensor_type_id = "radio-transmitter",
                        timestamp_start = as.POSIXct("2002-06-02 23:06:15", tz="UTC"),
                        timestamp_end = as.POSIXct("2002-06-11 22:18:25", tz="UTC"))

### download only a minimum amount of columns in event table (deployment/individual_local_identifier, timestamp, geometry) 
## ONLY FOR LOCATION DATA
movebank_download_study(study_id = 1918503,
                        sensor_type_id = "radio-transmitter",
                        attributes = NULL) #this keeps only the minimum columns, being deployment_id, timestamp, visible and geometry. It only acts on the event table, the track table is always downloaded in full

### download data including only specific attributes. Works for all sensor types
## get all attributes available for a specific study and sensor
movebank_retrieve(entity_type = "event",  #study, tag..
                  study_id = 1918503,
                  sensor_type_id = "radio-transmitter",
                  attributes = NULL)

##### b. Download NON-LOCATION data ----

### All options from section a. apply also to non-location data
## Note the empty geometry
(acc <- movebank_download_study(74496970,
                                sensor_type_id = "acceleration",
                                individual_local_identifier = "DER AR439"))
head(acc$geometry)
names(acc)

### Visualize and get basic stats of acceleration data (currently only for eObs tags)
## moveACC : https://gitlab.com/anneks/moveACC
## More with Hannah on Thursday... stay tuned!


###________________________________________________________
### 2. Reading in a .csv file downloaded from Movebank ####

### We read in this dataset downloaded from Movebank directly as a move2 object
bats <- mt_read("Parti-colored bat Safi Switzerland.csv")
names(bats)
## Note: pay attention to the change in column name!
## When data are downloaded as csv from Movebank the column names change their separator
## compared to when they are directly downloaded from the API
## This unfortunately depends on Movebank so we cannot change it for now

###_________________________________________________
### 3. Creating a move2 object from any data set ####

### Sierit the stork, we read the data as a data.frame
df <- read.csv("Sierit_DER AN858(eobs2561)_fromJan2020.csv")
str(df)

# first make sure the date/time is in POSIXct format
df$timestamp <- as.POSIXct(df$timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC")

# now create a move2 object from a data.frame
srk <- mt_as_move2(df, 
                   coords = c("location.long","location.lat"),
                   crs = "EPSG:4326",
                   time_column = "timestamp",
                   track_id_column = "individual.local.identifier",
                   track_attributes= c("sensor.type","individual.taxon.canonical.name",
                                       "tag.local.identifier","study.name"), ## optional!
                   na.fail = F) # allows or not empty coordinates

#__________________________
# CHECKING PROPERTIES,    #
# DUPLICATED TIMESTAMPS & #
# EMPTY LOCATIONS         #
#__________________________

stork <- mt_read("Sierit_DER AN858(eobs2561)_fromJan2020.csv")

### EMPTY COORDINATES: are there empty locations?
mt_has_no_empty_points(stork) 
## if FALSE in location data remove them. In non-location data points will be always empty!
stork <- dplyr::filter(stork, !sf::st_is_empty(stork))

### are tracks grouped, ie occur consecutively?
mt_is_track_id_cleaved(stork) 
### are timestamps ordered within tracks?
mt_is_time_ordered(stork) 
## if FALSE group them and arrange them
stork <- dplyr::arrange(stork, mt_track_id(stork), mt_time(stork))

### DUPLICATED TIMESTAMPS: does the data contain duplicated timestamps?
mt_has_unique_location_time_records(stork)
## if FALSE we can remove duplicates. 

## How many duplicated timestmaps?
table(duplicated(paste0(mt_track_id(stork),mt_time(stork))))

## Several options to remove them:
## 1. remove e.g. the 1st of each set of duplicates. No control over which one is taken. 
## see help for different options of argument "criterion"
stork <- mt_filter_unique(stork, criterion="first")
## 2. keep the entry with least number of columns with NA values
stork <- stork %>%
  mutate(n_na = rowSums(is.na(pick(everything())))) %>%
  dplyr::arrange(n_na) %>%
  mt_filter_unique(criterion='first') %>% # this always needs to be "first" because the duplicates get ordered according to the number of columns with NA. 
  dplyr::arrange(mt_track_id()) %>% ## data needs be reordered again
  dplyr::arrange(mt_track_id(),mt_time())

mt_has_unique_location_time_records(stork)


#___________________________
# EXTRACTING INFORMATION & #
# BASIC MANIPULATION       #
#___________________________

mv2 <- mt_read(mt_example())

#### Extracting information:

### Retrieve names of special columns
mt_time_column(mv2)
mt_track_id_column(mv2)

### Retrieve information
mt_track_data(mv2)
mt_time(mv2) %>% head()
mt_track_id(mv2) %>% unique()
mt_n_tracks(mv2)
table(mt_track_id(mv2))

# geometry information are retrieved using sf functions (they start with st_..)
sf::st_coordinates(mv2) %>% head()
sf::st_crs(mv2)
sf::st_transform(mv2, crs = "EPSG:32622") %>% sf::st_crs()

### Variables definitions of Movebank data
movebank_get_vocabulary(mv2)


#### Basic manipulation:

### Move attributes between track table and event table
mv2
?mt_as_event_attribute
mv2_2 <- mt_as_event_attribute(mv2, c("individual-taxon-canonical-name","tag-local-identifier"))
names(mv2_2)
names(mt_track_data(mv2_2))

### set a different track identifier - the chosen attribute should not group different individuals into one track
mv2_2 <- mt_set_track_id(mv2_2, value="tag-local-identifier")
mt_track_id_column(mv2_2)

### Manipulations on the TRACK TABLE

## dplyr verbs:
## select()** - on columns
## filter()** - on rows
## mutate()** - transform columns
## arrange() - sort rows
## summarize() - summarizes columns into one value
## group_by()** - groups data set into values of one column
## ** adapted function available for track data

# 1. Manipulating rows and columns in the TRACK attribute table

filter_track_data(mv2, .track_id = c("F1","F2"))
filter_track_data(mv2, `tag-local-identifier` == "1072")

select_track_data(mv2, `individual-local-identifier`) # keep one column

mv2 <- mutate_track_data(mv2, sex = substr(`individual-local-identifier`,1,1)) # add column sex to track data
mt_track_data(mv2)

# 2. Summarise variables by TRACK attribute
mv2 %>%
  group_by_track_data(sex) %>%
  summarize(min_time=min(mt_time()), max_time=max(mt_time()))

mv2 %>%
  group_by_track_data(sex) %>% summarize(nb_locs = n())

### Manipulations on the EVENT TABLE
# 1. Manipulating rows and columns in the event attribute table
dplyr::filter(mv2, visible == TRUE)

dplyr::select(mv2, `event-id`, visible, timestamp)

# 2. Summarise variables by EVENT attribute
mv2 %>% 
  group_by(`behavioural-classification`) %>% 
  summarize(nb_locs = n())

#__________________________________
# CONVERT BETWEEN MOVE & MOVE2 ####
#__________________________________

### This is still a transition phase and some methods we will present still require the "old" move object
mv <- to_move(mv2)
class(mv)

mv2_2 <- mt_as_move2(mv)
class(mv2_2)

## NOTE: "track_xyt" from "amt", and "telemetry"/"telemetry list" from "ctmm" can 
## be directly transformed into a move2 object with mt_as_move2()

#______________________
# OUTPUTTING DATA ####
#______________________

### Save the move2 object as RData file
save(mv2, file="fishers.Rdata")
load("fishers.Rdata") #cannot be assigned a new name, but can contain multiple objects

### Or as an rds file
saveRDS(mv2, file="fishers.rds")
mv2 <- readRDS("fishers.rds") # can only contain one object, but you can give it a name when loading


### Save as a GeoPackage
## as points
mv2_allattrb <- mt_as_event_attribute(mv2, everything())
mv2_allattrb <- mv2_allattrb %>% dplyr::filter(!sf::st_is_empty(.)) # ensure empty locations are removed
mv2_allattrb <- mv2_allattrb %>% select(where(~ !(all(is.na(.))))) # remove empty columns
sf::st_write(mv2_allattrb, dsn="fishers_pts.gpkg", driver="GPKG", delete_layer = TRUE) # for overwriting
sf::st_read("fishers_pts.gpkg")

## as segments
mv2_allattrb_seg <- mv2_allattrb
mv2_allattrb_seg$segments <- mt_segments(mv2_allattrb_seg)
mv2_allattrb_seg <-mv2_allattrb_seg %>%  dplyr::filter(st_geometry_type(mv2_allattrb_seg$segments) == "LINESTRING") # as the last one is a point, this has to be removed or it cannot be saved with st_write()
st_geometry(mv2_allattrb_seg) <- mv2_allattrb_seg$segments ## making the segments the geometry of the object
sf::st_write(mv2_allattrb_seg, dsn="fishers_seg.gpkg", driver="GPKG", delete_layer = TRUE) # for overwriting
sf::st_read("fishers_seg.gpkg")

## as lines
mv2_allattrb_lines <- mt_track_lines(mv2_allattrb) # returns one line per track, track attrb are kept
sf::st_write(mv2_allattrb_lines, dsn="fishers_lines.gpkg", driver="GPKG", delete_layer = TRUE) # for overwriting
sf::st_read("fishers_lines.gpkg")


### Save as kml (also as points, segments or lines)
sf::st_write(mv2_allattrb_lines, dsn="fishers_lines.kml", driver="GPKG", delete_layer = TRUE) # for overwriting
sf::st_read("fishers_lines.kml")


## Save as a Shapefile (also as points, segments or lines --> none can include all attributes)
mv2_sub <- dplyr::select(mv2, timestamp, geometry, `individual-local-identifier`)
sf::st_write(mv2_sub, getwd(), layer="fishers_pts", driver="ESRI Shapefile", delete_layer = TRUE) # for overwriting
sf::st_read("fishers_pts.shp")


### Save as csv file
## (downloading a "typical" dataset containing all the "problem" columns)
sierit <- movebank_download_study(study_id = 21231406,
                                  sensor_type_id = "gps",
                                  individual_local_identifier = "Sierit  / DER AN858 (eobs2561)",
                                  timestamp_start = as.POSIXct("2021-12-01 00:00:00",tz="UTC"))
sierit

#_____
#### steps to convert move2 into data frame without loosing info
## 1. move all track associated attributes to the event table
sierit <- mt_as_event_attribute(sierit, names(mt_track_data(sierit)))
sierit
head(sierit$geometry)
## 2. put coordinates in 2 columns
sierit <- dplyr::mutate(sierit, location_long=sf::st_coordinates(sierit)[,1],
                        location_lat=sf::st_coordinates(sierit)[,2]) 
## 3. remove the sf geometry column from the table                      
sierit <- sf::st_drop_geometry(sierit)
## This is especially important if we want to save the dataframe as csv:
## 4. make sure not to mess up timestamps, columns with class POSIXct get rounded when saves as csv.
# This ensures all timestamps have miliseconds and don't get rounded.
sierit_csv <- data.frame(sierit)
sierit_csv[,mt_time_column(sierit)] <- format(sierit_csv[,mt_time_column(sierit)],format="%Y-%m-%d %H:%M:%OS3") 
## 5. convert the e.g. "POINT" columns into characters, ie into WKT (Well-known text). st_as_sfc() can be used to convert these columns back to spacial
sierit$main_location[1]
sfc_cols <- names(sierit)[unlist(lapply(sierit, inherits, 'sfc'))] 
for(x in sfc_cols){ 
  sierit[[x]] <- st_as_text(sierit[[x]])
}
sierit$main_location[1]

write.table(sierit_csv, file="SieritStork.csv", sep=",", row.names = FALSE)



### NOTE: for more details see the vignettes https://cran.r-project.org/web/packages/move2/index.html
# and the articles in the website: https://bartk.gitlab.io/move2/

### To report issues with the package visit https://gitlab.com/bartk/move2/-/issues

### For users already familiar with move, we recommend the article 
# https://bartk.gitlab.io/move2/articles/convert.html 
# (which shows how to achieve a certain task in both packages)
