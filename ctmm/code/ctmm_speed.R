
###########################################################################
#' Day 3. Continuous-time movement models
#' Speed, distance, diffusion
#' See: Noonan et al. 2019. Movement Ecology, 7(1), 1–15.
###########################################################################

library(ctmm)

# Data preparation: -------------------------------------------------------

data(buffalo)

# North-up projection:
projection(buffalo) <- median(buffalo)

# Select only the first buffalo:
DATA <- buffalo[[1]]

# Units operator:
?`%#%`

1 %#% "day" # day in seconds
1 %#% "year" # year in seconds

# Consider only the first week of data:
DATA <- DATA[
  DATA$t <= DATA$t[1] + 1 %#% "week", ]
plot(DATA, col = color(DATA, by = "time"),
     error = FALSE)

# Select best fit:
guess <- ctmm.guess(DATA, interactive = FALSE)
fit <- ctmm.select(DATA, guess, trace = 2)

# Speed estimate here is RMS Gaussian:
summary(fit)

# Gaussian (regular speed, not RMS):
speed(fit)

# Non-parametric speed estimation:
ctsd <- speed(DATA, fit)
ctsd

# Impact of coarsening the data: ------------------------------------------

SUB <- DATA
fit.SUB <- fit

# Removing every other time:
SUB <- SUB[as.logical(1:nrow(SUB) %% 2), ]
plot(SUB, col = color(SUB, by = "time"), error = FALSE)
fit.SUB <- ctmm.select(SUB, fit.SUB, trace = 2)
# RMS Gaussian:
summary(fit)
summary(fit.SUB)
# Gaussian (regular speed - not RMS):
speed(fit)
speed(fit.SUB)
# Non-parametric speed estimation:
ctsd
speed(SUB, fit.SUB)
# repeat until data become too coarse

# keep in mind the stationary assumption of the model
# see the appendix of Noonan et al.

# Population-level inferences: --------------------------------------------

help("meta")

# Load in the fitted movement models:
load("data/fits_buffalo.rda")

# Estimate mean spead for each animal:
ctsdList <- list()
for (i in seq_along(length(buffalo))) {
  ctsdList[[i]] <- speed(buffalo[[i]], fitList[[i]])
}
names(ctsdList) <- names(buffalo)
# save(ctsdList, file = "data/ctsd_buffalo.rda")
load("data/ctsds_buffalo.rda")

meta(ctsdList, sort = TRUE)

# Instantaneous speeds: ---------------------------------------------------

inst_speeds <- speeds(buffalo[[1]], fitList[[1]])
head(inst_speeds) # standard units (m/s)
