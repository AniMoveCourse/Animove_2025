
###### Convert Move2 to other useful classes #######
# Script by Anne Scharf edited by Martina Scacco
# Date: December 2025

## this script contains translators (mostly both directions) between the move2 object and the:
# - data.frame containing all event and track attributes: move2_TO_df() (mt_as_move2() reads in data.frame)
# - telemetry class of ctmm package: move2_TO_telemetry() (mt_as_move2() reads in telemetry & telemetry lists)
# - track_xyt from amt: move2_TO_track.xyt() (mt_as_move2() reads in track.xyt objects)
# - ltraj from adehabitatLT: move2_TO_ltraj() & ltraj_TO_move2()
# - to_move() & mt_as_move2() - between move and move2 objects
## Note: in the future some of the packages above will probably allow a move2 object as an input, and the move2 package will have functions to transform some of the classes above into a move2 object
## Note: most functions print a warning message: this is no error, this is to communicate important information and assumptions


######################################################
### data.frame with all event and track attributes ###
######################################################
library(sf)
move2_TO_df <- function(mv2){
  if(mt_is_move2(mv2)){
    ## 1. move all track associated attributes to the event table
    mv2 <- mt_as_event_attribute(mv2, names(mt_track_data(mv2)))
    ## 2. put coordinates in 2 columns
    mv2 <- dplyr::mutate(mv2, location_long=sf::st_coordinates(mv2)[,1],
                         location_lat=sf::st_coordinates(mv2)[,2])
    ## 3. convert the e.g. "POINT" columns into characters, ie into WKT (Well-known text). st_as_sfc() can be used to convert these columns back to spacial
    sfc_cols <- names(mv2)[unlist(lapply(mv2, inherits, 'sfc'))] 
    for(x in sfc_cols){ 
      mv2[[x]] <- st_as_text(mv2[[x]])
    }
    ## 4. remove the sf geometry column from the table    
    df <- data.frame(sf::st_drop_geometry(mv2))
    return(df)
  }
}


############################
### telemetry from ctmm ###
############################
library(move2)
library(ctmm)
library(sf)
move2_TO_telemetry <- function(mv2) {
  # needed columns: individual.local.identifier (or tag.local.identifier), timestamp, location.long and location.lat
  mv2 <- mt_as_event_attribute(mv2, names(mt_track_data(mv2)))
  mv2 <- dplyr::mutate(mv2, location.long=sf::st_coordinates(mv2)[,1],
                        location.lat=sf::st_coordinates(mv2)[,2])
  
  mv2df <- data.frame(mv2)
  ## as.telemetry expects the track id to be called "individual.local.identifier" this is a quick fix, it might need some more thought to it to make it nicer. HOPE THIS IS FIXED ONCE ctmm INTEGRATES READING IN move2
  # fix: idtrack colum gets the prefix "track_id:", individual.local.identifier gets the sufix "_original" to maintain this original information
  colnames(mv2df)[colnames(mv2df)%in%make.names(mt_track_id_column(mv2))] <- paste0("track_id:",make.names(mt_track_id_column(mv2)))
  colnames(mv2df)[colnames(mv2df)%in%c("individual.local.identifier","individual_local_identifier","individual-local-identifier")] <- paste0(colnames(mv2df)[colnames(mv2df)%in%c("individual.local.identifier","individual_local_identifier","individual-local-identifier")],"_original")
  mv2df$individual_local_identifier <-mt_track_id(mv2)
  mv2df$timestamp <- mt_time(mv2) # ensuring used timestamps are in the column "timestamp" as expected by as.telemetry()
  telem <- as.telemetry(mv2df,
                        timezone=tz(mt_time(mv2)),
                        projection= if(st_is_longlat(mv2)){NULL}else{projection(mv2)},
                        na.rm= "col",
                        keep=T)
  return(telem)
}

# telemetry_TO_move2 => mt_as_move2 reads in a telemetry /list telemetry object


############################
#### track_xyt from amt ####
############################
library(move2)
library(amt)
library(sf)
move2_TO_track.xyt <- function(mv2){
  if(mt_is_move2(mv2)){
    warning("!!INFO!!: only coordinates, timestamps and track IDs are retained")
    track(
      x=sf::st_coordinates(mv2)[,1],
      y=sf::st_coordinates(mv2)[,2],
      t=mt_time(mv2),
      id=mt_track_id(mv2),
      crs = sf::st_crs(mv2)
    )
  }
}

# track.xyt_TO_move2 => mt_as_move2 reads in a track.xyt object

###############################
### ltraj from adehabitatLT ###
###############################
library(move2)
library(adehabitatLT)
library(sf)
move2_TO_ltraj <- function(mv2){
  if(mt_is_move2(mv2)){
    warning("!!INFO!!: only coordinates, timestamps and track IDs are retained")
    warning("!!INFO!!: projection information is lost as the ltraj function still only accepts proj4 strings")
    if(sf::st_is_longlat(mv2)){
      warning("Converting a object in geographic coordinate system (lat/long) while the ltraj assums projected data")
    }
    adehabitatLT::as.ltraj(as.data.frame(sf::st_coordinates(mv2)), date = mt_time(mv2), id = as.character(mt_track_id(mv2)), typeII = T)#, proj4string=sf::st_crs(mv2))
  }
}

ltraj_TO_move2 <- function(ltj){
  if(is(ltj,"ltraj")){
    if (!attr(ltj, "typeII")) {
      stop("Can only work on typeII objects")
    }
    mv2_L <- lapply(1:length(ltj), function(i) {
      ltj2df <- data.frame(ltj[[i]])
      ltj2df <- data.frame(ltj2df,attr(ltj[[i]], "infolocs"))
      ltj2df$id <- attr(ltj[[i]], "id")
      ltj2df$burst <- attr(ltj[[i]], "burst")

      mt_as_move2(ltj2df,
                  coords = c("x", "y"),
                  time_column="date",
                  track_id_column="id",
                  crs= projection(ltj[[i]]))
    })

    mv2 <- mt_stack(mv2_L,.track_combine="rename")
  }
}

