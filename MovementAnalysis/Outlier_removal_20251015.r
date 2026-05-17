# OUTLIER DETECTION FOR ALL SPECIES 
# THRESHOLD ON 95% PERCENTILE, ABOVE FLAGGED AS OUTLIER - assumption that at least 5% data are outliers
# FF, 13 JUNE 2025, updated and corrected 14 October 2025

# follows the download code for move2 obj from the API - if needed, feel free to ask


##### 1: KS FF - OUTLIER DETECTION ####
# assumed that 5% of the points of EACH INDIVIDUAL are outlier based on prob of unusual step length, turning angle and ground speed from previous (above 99%)

#### FUNCTION ####
outlier_removal <- function(move_obj, threshold = 0.001, prob_type = "joint", 
                            remove = FALSE, plot = TRUE, drop_na = FALSE) {
  # Function arguments:
  # move_obj: A move2 object
  # threshold: The probability threshold (as percentile) for outlier identification
  # prob_type: Which probability to use - "joint", "step_turn", "delta_step", "delta_turn", or "custom"
  # remove: Whether to remove outliers or just flag them
  # plot: Whether to create diagnostic plots
  # drop_na: Whether to drop locations with NA probabilities
  
  # Check if input is a move2 object
  if (!inherits(move_obj, "move2")) {
    stop("Input must be a move2 object")
  }
  
  # Check threshold validity
  if (threshold <= 0 || threshold >= 1) {
    stop("Threshold must be between 0 and 1")
  }
  
  # Check prob_type validity
  valid_prob_types <- c("joint", "step_turn", "delta_step", "delta_turn", "custom")
  if (!prob_type %in% valid_prob_types) {
    stop(paste0("Invalid prob_type. Must be one of: ", paste(valid_prob_types, collapse = ", ")))
  }
  
  # Load required libraries
  suppressMessages({
    library(move2)
    library(sf)
    library(terra)
  })
  
  # Helper functions
  wrap <- function(x) {
    (x + pi) %% (2 * pi) - pi
  }
  
  add_noise <- function(coord, scale = 1e-6) {
    coord + runif(1, -scale, scale)
  }
  
  # Function to calculate 2D histogram of turning angle and step length
  TurnStepHist <- function(x, y, stand = TRUE, verbose = FALSE) {
    # get bandwidths for x and y direction
    bwx <- nclass.FD(x[!is.na(x)])
    bwy <- nclass.FD(y[!is.na(y)])
    # define based on that the number of bins
    nx <- max(bwx, 12)
    ny <- max(bwy, 12)
    # set up a raster with the appropriate number of bins
    test <- terra::rast(ncol = nx, nrow = ny, 
                        xmin = -pi, xmax = pi,
                        ymin = 0, ymax = 1.1 * (max(y, na.rm = TRUE)),
                        crs = "")
    # count the number of occurrences per bin
    xyRaster <- terra::rasterize(terra::vect(cbind(x, y)), test, fun = "count")
    # set the NA cells to empty
    xyRaster[is.na(xyRaster)] <- 0
    # standardise to sum the values to 1
    xyRaster <- xyRaster / sum(terra::values(xyRaster), na.rm = TRUE)
    
    if(verbose){
      message('x ', nrow(xyRaster), " y ", ncol(xyRaster))
    }
    
    # handle circular interpolation
    l <- xyRaster
    r <- xyRaster
    terra::ext(l) <- terra::ext(terra::ext(l)[1] - 2*pi, terra::ext(l)[2] - 2*pi, terra::ext(l)[3], terra::ext(l)[4])
    # terra::ext(r) <- terra::ext(terra::ext(r)[1] + 2*pi, terra::ext(l)[2] + 2*pi, terra::ext(r)[3], terra::ext(r)[4]) this makes the session abort! mixing r and l may create invalid boundaries.
    terra::ext(r) <- terra::ext(terra::ext(r)[1] + 2*pi, terra::ext(r)[2] + 2*pi, terra::ext(r)[3], terra::ext(r)[4])
    
    xyRaster <- terra::merge(l, xyRaster, r)
    
    # increase resolution through bilinear interpolation
    rasterXY <- terra::resample(xyRaster,
                                terra::rast(
                                  ncol = 150, nrow = 150, 
                                  xmin = -pi, xmax = pi,
                                  ymin = 0, ymax = max(y, na.rm = TRUE),
                                  crs = ""
                                ),
                                method = "bilinear")
    
    rasterXY[rasterXY < 0] <- 0
    
    # standardise to sum to 1
    rasterXY <- rasterXY / sum(terra::values(rasterXY), na.rm = TRUE)
    
    return(list(rasterXY, xyRaster)[[2 - stand]])
  }
  
  # Function to calculate probability distributions
  get.densities.2d <- function(stepLength, turningAngle, deltaStep, deltaTurn) {
    if(length(turningAngle) != length(stepLength)) {
      stop("Vector lengths of step length and turning angles do not match.")
    }
    rasterTS <- TurnStepHist(x = as.vector(turningAngle), y = as.vector(stepLength))
    # approximate the distributions
    autoS <- stats::approxfun(stats::density.default(deltaStep[!is.na(deltaStep)]))
    autoT <- stats::approxfun(stats::density.default(deltaTurn[!is.na(deltaTurn)]))
    return(list(TSRaster = rasterTS, autoT = autoT, autoS = autoS))
  }
  
  # Function to calculate joint movement probabilities
  get_joint_movement_probabilities <- function(stepLength, turningAngle, TwoDHist) {
    n <- length(stepLength)
    
    if (length(turningAngle) != n) {
      stop("stepLength and turningAngle must have the same length")
    }
    
    # Initialize probability vectors
    step_turn_probs <- rep(NA, n)
    delta_step_probs <- rep(NA, n)
    delta_turn_probs <- rep(NA, n)
    joint_probs <- rep(NA, n)
    
    # Get components
    rast <- TwoDHist$TSRaster
    auto_step_func <- TwoDHist$autoS
    auto_turn_func <- TwoDHist$autoT
    
    # Extract probabilities from the 2D histogram
    coords <- cbind(as.numeric(turningAngle), as.numeric(stepLength))
    probs_df <- terra::extract(rast, coords)
    step_turn_probs <- probs_df[, 1]  # Keep original indexing
    
    # Calculate differences
    delta_steps <- diff(c(NA, stepLength))
    delta_turns <- diff(c(NA, turningAngle))
    
    # Calculate probabilities
    for (i in 2:n) {
      if (!is.na(delta_steps[i])) {
        delta_step_probs[i] <- auto_step_func(delta_steps[i])
      }
      
      if (!is.na(delta_turns[i])) {
        wrapped_delta <- wrap(delta_turns[i])
        delta_turn_probs[i] <- auto_turn_func(wrapped_delta)
      }
      
      if (!is.na(step_turn_probs[i]) && !is.na(delta_step_probs[i]) && !is.na(delta_turn_probs[i])) {
        joint_probs[i] <- step_turn_probs[i] * delta_step_probs[i] * delta_turn_probs[i]
      }
    }
    
    return(list(
      step_turn_probs = step_turn_probs,
      delta_step_probs = delta_step_probs,
      delta_turn_probs = delta_turn_probs,
      joint_probs = joint_probs
    ))
  }
  
  # Calculate initial step lengths
  step_lengths <- mt_distance(move_obj)
  
  # ff -individual id
  ind <- unique(move_obj$individual_local_identifier)
  
  # Fix zero step lengths
  zero_indices <- which(as.vector(step_lengths) == 0)
  if (length(zero_indices) > 0) {
    message(paste0("Found ", length(zero_indices), " locations with zero step length. Adding small random noise."))
    
    geom <- st_geometry(move_obj)
    
    for (i in zero_indices) {
      if (i + 1 <= length(geom)) {
        point <- st_coordinates(geom[i + 1])
        new_point <- c(add_noise(point[1]), add_noise(point[2]))
        geom[i + 1] <- st_point(new_point)
      }
    }
    
    st_geometry(move_obj) <- geom
    step_lengths <- mt_distance(move_obj)
    
    remaining_zeros <- sum(as.vector(step_lengths) == 0, na.rm = TRUE)
    if (remaining_zeros > 0) {
      warning(paste0(remaining_zeros, " zero step lengths still remain after adding noise."))
    }
  }
  
  message("Calculating movement metrics...")
  
  # Calculate movement metrics
  stepLength <- mt_distance(move_obj, units = "m")
  turningAngle <- mt_turnangle(move_obj)
  
  if (length(stepLength) < 3) {
    stop("Not enough locations to calculate step length and turning angle differences. Need at least 3 points.")
  }
  
  deltaStep <- diff(as.numeric(stepLength))
  deltaTurn <- diff(as.numeric(turningAngle))
  
  message("Calculating probability distributions...")
  TwoDHist <- get.densities.2d(stepLength, turningAngle, deltaStep, deltaTurn)
  
  message("Calculating joint probabilities...")
  probability_components <- get_joint_movement_probabilities(
    as.numeric(stepLength), 
    as.numeric(turningAngle), 
    TwoDHist
  )
  
  # Add probability components to the move object
  move_obj$step_turn_prob <- probability_components$step_turn_probs
  move_obj$delta_step_prob <- probability_components$delta_step_probs
  move_obj$delta_turn_prob <- probability_components$delta_turn_probs
  move_obj$joint_prob <- probability_components$joint_probs
  
  # Determine which probability to use for outlier detection
  if (prob_type == "joint") {
    prob_col <- "joint_prob"
    prob_label <- "Joint probability"
  } else if (prob_type == "step_turn") {
    prob_col <- "step_turn_prob"
    prob_label <- "Step length & turning angle probability"
  } else if (prob_type == "delta_step") {
    prob_col <- "delta_step_prob"
    prob_label <- "Step length change probability"
  } else if (prob_type == "delta_turn") {
    prob_col <- "delta_turn_prob"
    prob_label <- "Turning angle change probability"
  } else if (prob_type == "custom") {
    prob_col <- "custom_prob"
    prob_label <- "Custom probability product"
    move_obj$custom_prob <- move_obj$step_turn_prob * move_obj$delta_step_prob
  }
  
  # Create log-transformed probability for visualization
  move_obj$log_prob <- log10(move_obj[[prob_col]])
  
  # Identify NA probabilities
  is_na_prob <- is.na(move_obj[[prob_col]])
  n_na <- sum(is_na_prob)
  
  # Identify outliers (only among non-NA values)
  message("Identifying outliers...")
  probs <- move_obj[[prob_col]]
  
  # Calculate outlier percentiles
  # Empirical Cumulative Distribution Function - creates a function that, for any value x, gives the proportion of my data points in probs that are less than or equal to x
  ecdf_func <- ecdf(probs[!is.na(probs)]) 
  # for each point, the fraction (between 0 and 1) of points in my data that are less than or equal to that value. This is the empirical percentile of that point
  # move_obj$outlier_percentile <- round(ecdf_func(probs) * 100, 2)
  # flip the logic to have high values for high chance outliers
  move_obj$outlier_percentile <- round((1 - ecdf_func(probs)) * 100, 2) # lowest probabilities (i.e., most unusual, most likely outliers) get the highest percentile values (close to 100)
  
  
  # Identify outliers based on percentile
  # is_outlier <- move_obj$outlier_percentile <= (threshold * 100)
  # flipped logic at line 256 so>=
  is_outlier <- move_obj$outlier_percentile >= (100 - threshold * 100)
  move_obj$is_outlier <- is_outlier
  
  ## before 12june: not percentile assigned so you can remove chunk 251-264, run these 2 lines instead and at 284 re-build 'outlier_threshold' into outlier message
  # outlier_threshold <- quantile(probs, threshold, na.rm = TRUE)
  # is_outlier <- probs < outlier_threshold & !is.na(probs)
  ##
  
  # Flag outliers and NA status
  move_obj$is_outlier <- is_outlier
  move_obj$is_na_prob <- is_na_prob
  
  # Count outliers and non-NA values
  n_outliers <- sum(is_outlier, na.rm = TRUE)
  n_total <- sum(!is.na(probs))
  
  # Create output message
  outlier_message <- paste0("Identified ", n_outliers, " outliers (", 
                            round(100 * n_outliers / n_total, 2), 
                            "% of locations with calculated probabilities) based on ", prob_label, ".\n",
                            "Using the ", threshold * 100, "th percentile as threshold (", 
                            format(threshold, scientific = TRUE, digits = 4), ").") # outlier_threshold if re/insert the above 2 lines
  
  if (n_na > 0) {
    na_message <- paste0("Found ", n_na, " locations (", 
                         round(100 * n_na / nrow(move_obj), 2), 
                         "% of all locations) with NA ", prob_label, ".")
    if (drop_na) {
      na_message <- paste0(na_message, " These will be removed.")
    } else {
      na_message <- paste0(na_message, " These will be kept.")
    }
    message(outlier_message)
    message(na_message)
  } else {
    message(outlier_message)
    message("All locations have calculated probabilities.")
  }
  
  # Create visualization if requested
  if (plot) {
    message("Creating visualization...")
    
    # Extract coordinates
    coords <- st_coordinates(move_obj)
    
    # Create visualization data frame
    plot_data <- data.frame(
      x = coords[, "X"],
      y = coords[, "Y"],
      log_prob = move_obj$log_prob,
      is_outlier = move_obj$is_outlier,
      is_na_prob = move_obj$is_na_prob
    )
    
    # Set up two-panel plot
    old_par <- par(no.readonly = TRUE)
    par(mfrow = c(1, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
    
    # Create color palette
    prob_colors <- colorRampPalette(c("red", "yellow", "green", "blue"))(100)
    
    # Panel 1: All locations with probability coloring
    plot(coords, type = "l", main = "All locations with probability", 
         col = "black", xlab = "Longitude", ylab = "Latitude", asp = 1)
    
    # Create a data frame for non-NA points, sorted by probability
    non_na_data <- plot_data[!plot_data$is_na_prob, ]
    if (nrow(non_na_data) > 0) {
      non_na_data <- non_na_data[order(non_na_data$log_prob, decreasing = TRUE), ]
      
      # Calculate color indices for non-NA points
      log_prob_range <- range(non_na_data$log_prob, na.rm = TRUE)
      color_indices <- ceiling(((non_na_data$log_prob - log_prob_range[1]) / 
                                  (log_prob_range[2] - log_prob_range[1])) * 99) + 1
      
      # Plot points from high to low probability
      points(non_na_data$x, 
             non_na_data$y, 
             col = prob_colors[color_indices], 
             pch = 19, 
             cex = 0.8)
    }
    
    # Plot points with NA probabilities in gray
    na_idx <- which(plot_data$is_na_prob)
    if (length(na_idx) > 0) {
      points(plot_data$x[na_idx], 
             plot_data$y[na_idx], 
             col = "gray", pch = 19, cex = 0.8)
    }
    
    # Add legend
    legend("topright", 
           legend = c("Low Prob", "", "", "High Prob", "NA Prob"), 
           col = c(prob_colors[c(1, 33, 66, 100)], "gray"), 
           pch = 19, 
           title = prob_label)
    
    # Panel 2: Track with outliers and NAs removed
    # Determine which points to remove
    to_remove <- is_outlier
    if (drop_na) {
      to_remove <- to_remove | is_na_prob
    }
    
    kept_idx <- which(!to_remove)
    removed_idx <- which(to_remove)
    
    # Plot the track
    plot(coords, type = "n", main = "Track with points removed", 
         xlab = "Longitude", ylab = "Latitude", asp = 1)
    
    # Draw track lines for kept points
    if (length(kept_idx) > 1) {
      lines(coords[kept_idx, ], col = "black")
    }
    
    # Plot kept points
    if (length(kept_idx) > 0) {
      points(coords[kept_idx, ], col = "blue", pch = 19, cex = 0.8)
    }
    
    # Plot removed points
    if (length(removed_idx) > 0) {
      points(coords[removed_idx, ], col = "red", pch = 19, cex = 0.8)
    }
    
    # Add legend
    legend("topright", 
           legend = c("Kept", "Removed"), 
           col = c("blue", "red"), 
           pch = 19)
    
    # Add title
    if (drop_na) {
      title_text <- paste0("Removing ", n_outliers, " outliers + ", n_na, " NAs (", 
                           round(100 * (n_outliers + n_na) / nrow(move_obj), 2), 
                           "% of locations)")
    } else {
      title_text <- paste0("Removing ", n_outliers, " outliers (", 
                           round(100 * n_outliers / n_total, 2), 
                           "% of locations with probabilities)",
                           " ind:", ind)
    }
    
    title(title_text, outer = TRUE)
    
    # Reset plot parameters
    par(old_par)
  }
  
  # Return step length and turning angle as already calculated
  move_obj$step_length_mv <- as.numeric(stepLength)
  move_obj$turning_angle_mv <- as.numeric(turningAngle)
  
  # Return filtered or original object
  if (remove) {
    # Determine which points to remove
    to_remove <- is_outlier
    if (drop_na) {
      to_remove <- to_remove | is_na_prob
    }
    
    # Filter the move object
    filtered_move_obj <- move_obj[!to_remove, ]
    return(filtered_move_obj)
  } else {
    return(move_obj)
  }
}

# Example usage
# # Load libraries
# library(move2)
# library(sf)
# library(terra)
# # specify account to use in the R session. 
# # options("move2_movebank_key_name" = "KamiMovebank") 
# cilla <- movebank_download_study(study_id = 1764627, individual_local_identifier = "Cilla")
# plot(cilla["timestamp"], type="l", max.plot=1)
# 
# # Flag outliers without removing them (returns original object with probability columns)
# result <- outlier_removal(cilla, threshold = 0.001, prob_type = "step_turn", remove = FALSE)
# 
# # Remove outliers (returns filtered object)
# filtered_cilla <- outlier_removal(cilla, threshold = 0.001, prob_type = "joint", remove = TRUE, drop_na=T)
# 
# # Try different probability metrics
# delta_step_filtered <- outlier_removal(cilla, threshold = 0.001, 
#                                        prob_type = "delta_step", remove = TRUE)


