
# load packages
library(momentuHMM)
library(moveHMM)
library(adehabitatLT)
library(lubridate)
library(ggplot2)


# simulate data and remove unneeded columns
data <- momentuHMM::simData(dist = list(step = "gamma", angle = "vm"), 
                            obsPerAnimal = 100,
                            Par = list(step = c(0.5, 5, 0.25, 2.5), 
                                       angle = c(0, 0, 0.5, 5)))
data$step <- NULL
data$angle <- NULL

# set time
data$time <- seq(from = ymd_hms("2025-01-01 00:00:00"), by = "hour", length.out = 100)
head(data)

# remove points (random few and large string/gap)
data <- data[-c(30, 40, 82, 95),]
data <- data[-c(60:80),]
summary(as.data.frame(data))


############################
## Approach 1: adehabitat ##
############################

# split the track where there are large temporal gaps
# maxGap is the maximum allowable gap before it gets split
# shortestTrack is the shortest allowable track - shorter tracks will be removed
# units indicates the time units of the previous arguments (default = "mins")
data <- moveHMM::splitAtGaps(data, 
                             maxGap = 10, 
                             shortestTrack = 10, 
                             units = "hours")
head(data, 40)
tail(data)
detach("package:moveHMM", unload = T)


# Create adehabitat trajectory padded with NAs
# need data to be in ltraj format for adehabitat
# data.ref is a reference date for the location times 
# (if your first location is "off" then data$time[1] would not be a good choice)
# dt is the time interval 
# tol is the tolerance around the regular times
# units indicates the time units for the previous arguments dt and tol
data_ade <- setNA(ltraj = as.ltraj(xy = data[, c("x", "y")], 
                                   date = data$time, 
                                   id = data$ID), 
                  date.ref = data$time[1], 
                  dt = 60, tol = 5, units = "min")

# Transform back to dataframe
data_na <- ld(data_ade)[, c("id", "x", "y", "date")]
colnames(data_na) <- c("ID", "x", "y", "time")
head(data_na, 40)
data_na[50:78,]

# prepare for HMM
data_hmm1 <- prepData(data_na, type = "UTM")
head(data_hmm1)
summary(data_hmm1)


#######################
## Approach 2: crawl ##
#######################

# Predict locations on 30-min grid using crawl 
# (through momentuHMM wrapper)
crw_out <- crawlWrap(obsData = data, 
                     timeStep = "60 min", 
                     Time.name = "time", 
                     coord = c("x", "y"))

data_hmm2 <- prepData(data = crw_out)
head(data_hmm2)
data_hmm2[50:78,]
summary(data_hmm2)
# data_hmm2 can be fitted with MIfitHMM() in the momentuHMM package



# plot data
ggplot() +
  geom_path(data = data_hmm1, aes(x = x, y = y, group = ID), color = "red") +
  geom_path(data = data_hmm2, aes(x = x, y = y, group = ID), color = "blue", linetype = "dashed") +
  geom_point(data = data_hmm1, aes(x = x, y = y), color = "red", size = 2) +
  geom_point(data = data_hmm2, aes(x = x, y = y), color = "blue") +
  coord_equal() + theme_bw()
 
