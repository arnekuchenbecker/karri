# Converts number of seconds to MM:SS format.
convertToMMSS <- function(seconds) {
  seconds <- as.double(seconds)
  seconds <- round(seconds)
  minutes <- floor(seconds / 60)
  seconds <- seconds %% 60
  return(sprintf("%02d:%02d", minutes, seconds))
}

# Converts number of seconds to HH:MM format.
convertToHHMM <- function(seconds) {
  seconds <- as.double(seconds)
  seconds <- round(seconds)
  minutes <- floor(seconds / 60)
  hours <- floor(minutes / 60)
  minutes <- minutes %% 60
  return(sprintf("%02d:%02d", hours, minutes))
}


# Given the path to the result files of a KaRRi run (e.g. 
# "<output-dir>/Berlin-1pct_pedestrian/karri-col-simd_300_300"), 
# this function returns an overview over the solution quality of the assignments.
quality <- function(file_base) {
  
  asgnstats <- read.csv(paste0(file_base, ".assignmentquality.csv"))
  legstats <- read.csv(paste0(file_base, ".legstats.csv"))
  
  eventsimstats <- read.csv(paste0(file_base, ".eventsimulationstats.csv"))
  num.Vehicles <- sum(eventsimstats$type == "VehicleStartup")
  
  df <- data.frame(
             wait_time_avg = c(mean(asgnstats$wait_time) / 10), # avg wait time for each request
             wait_time_q95 = c(quantile(asgnstats$wait_time, 0.95) / 10), # q95 wait time for each request
             ride_time_avg = c(mean(asgnstats$ride_time) / 10), # avg ride time for each request
             ride_time_q95 = c(quantile(asgnstats$ride_time, 0.95) / 10), # q95 ride time for each request
             trip_time_avg = c(mean(asgnstats$trip_time) / 10), # avg trip time for each request
             trip_time_q95 = c(quantile(asgnstats$trip_time, 0.95) / 10), # q95 trip time for each request
             walk_to_pickup_avg=c(mean(asgnstats$walk_to_pickup_time) / 10), # avg walking time to pickup
             walk_to_pickup_q95=c(quantile(asgnstats$walk_to_pickup_time, 0.95) / 10), # q95 walking time to pickup
             walk_to_dropoff_avg=c(mean(asgnstats$walk_to_dropoff_time) / 10), # avg walking time to dropoff
             walk_to_dropoff_q95=c(quantile(asgnstats$walk_to_dropoff_time, 0.95) / 10), # q95 walking time to dropoff
             stop_time_avg = c(sum(legstats$stop_time) / num.Vehicles / 10), # avg total stop time for each vehicle
             empty_time_avg = c(sum(legstats[legstats$occupancy == 0, "drive_time"]) / num.Vehicles / 10), # avg time spent driving empty for each vehicle
             occ_time_avg = c(sum(legstats[legstats$occupancy > 0, "drive_time"]) / num.Vehicles / 10) # avg time spent driving occupied for each vehicle
             )
  
  df$op_time_avg <- df$stop_time_avg + df$empty_time_avg + df$occ_time_avg # avg total operation time for each vehicle
  df$occ_rate_avg <- sum(legstats$drive_time * legstats$occupancy) / sum(legstats$drive_time) # avg occupation rate while driving for each vehicle
  df$cost <- mean(asgnstats$cost) # avg cost (according to cost function used in KaRRi) for each vehicle
  
  # Reformat passenger times to MM:SS
  psg_time_cols <- c("wait_time_avg", "wait_time_q95", 
                     "ride_time_avg", "ride_time_q95", 
                     "trip_time_avg", "trip_time_q95",
                     "walk_to_pickup_avg", "walk_to_pickup_q95",
                     "walk_to_dropoff_avg", "walk_to_dropoff_q95"
                     )
  df[, colnames(df) %in% psg_time_cols] <- convertToMMSS(df[, colnames(df) %in% psg_time_cols])
  
  # Reformat vehicle times to HH:MM
  veh_time_cols <- c("stop_time_avg", "empty_time_avg", "occ_time_avg", "op_time_avg")
  df[, colnames(df) %in% veh_time_cols] <- convertToHHMM(df[, colnames(df) %in% veh_time_cols])
  
  print(df)
}

# Given the paths to the result files of two KaRRi runs, this functions checks
# whether all assignments are the same in both runs.
compareBestAssignments <- function(file1, file2) {
  bestins1 <- read.csv(paste0(file1, ".bestassignments.csv"))
  bestins2 <- read.csv(paste0(file2, ".bestassignments.csv"))
    
  bestins1 <- bestins1[order(bestins1$request_id),]
  bestins2 <- bestins2[order(bestins2$request_id),]
  
  # Get smallest row index where at least one value differs
  idx <- match(TRUE, rowSums(bestins1 != bestins2) > 0)
  if (is.na(idx)) {
    print("All best insertions are equal.")
  } else {
    print(bestins1[idx, "request_id"])
    row1 <- bestins1[idx,]
    row2 <- bestins2[idx,]
    View(rbind(row1, row2))
  }
}


perfStats <- function(file_base, type_name) {
  stats <- read.csv(paste0(file_base, ".perf_", type_name, ".csv"))
  stats <- stats[, ! colnames(stats) %in% c("request_id")]
  stats <- apply(stats, 2, mean)
  stats <- round(stats, 2)
  return(stats)
}

# Given the path to the result files of a KaRRi run, this function returns an 
# overview over the average runtime per request (in microseconds).
overallPerfStats <- function(file_base) {
  perfStats(file_base, "overall")
}

# Given the path to the result files of a KaRRi run, this function returns 
# performance statistics for the initialization phase.
initreqPerfStats <- function(file_base) {
  perfStats(file_base, "initreq")
}

# Given the path to the result files of a KaRRi run, this function returns 
# performance statistics for the elliptic BCH searches
ellipticBchPerfStats <- function(file_base) {
  perfStats(file_base, "ellipticbch")
}

# Given the path to the result files of a KaRRi run, this function returns 
# performance statistics for the computation of PD-distances.
pdDistancesPerfStats <- function(file_base) {
  perfStats(file_base, "pddistances")
}

# Given the path to the result files of a KaRRi run, this function returns 
# performance statistics for the ordinary insertions phase.
ordPerfStats <- function(file_base) {
  perfStats(file_base, "ord")
}

# Given the path to the result files of a KaRRi run, this function returns 
# performance statistics for the PBNS phase.
pbnsPerfStats <- function(file_base) {
  perfStats(file_base, "pbns")
}

# Given the path to the result files of a KaRRi run, this function returns 
# performance statistics for the PALS phase.
palsPerfStats <- function(file_base) {
  perfStats(file_base, "pals")
}

# Given the path to the result files of a KaRRi run, this function returns 
# performance statistics for the DALS phase.
dalsPerfStats <- function(file_base) {
  perfStats(file_base, "dals")
}

# Given the path to the result files of a KaRRi run, this function returns 
# performance statistics for updating the route state for each assignment.
updatePerfStats <- function(file_base) {
  perfStats(file_base, "update")
}


# Given the path to the result files of a KaRRi run, this function returns
# performance statistics for the different types of events in the event 
# simulation. request_receipt_event_time contains the main work of dispatching
# requests while all other times consist only of advancing the event 
# simulation by logging events and introducing new events.
eventSimulationPerfStats <- function(file_base) {
  
  stats <- read.csv(paste0(file_base, ".eventsimulationstats.csv"))
  df <- data.frame(
    vehicle_startup_event_time = c(mean(stats[stats$type=="VehicleStartupEvent", ]$running_time)),
    vehicle_arrival_event_time = c(mean(stats[stats$type=="VehicleArrivalEvent", ]$running_time)),
    vehicle_departure_event_time = c(mean(stats[stats$type=="VehicleDepartureEvent", ]$running_time)),
    vehicle_shutdown_event_time = c(mean(stats[stats$type=="VehicleShutdownEvent", ]$running_time)),
    request_receipt_event_time = c(mean(stats[stats$type=="RequestReceiptEvent",]$running_time)),
    request_pure_walking_arrival_event_time = c(mean(stats[stats$type=="PureWalkingArrivalEvent",]$running_time))
  )
  
  print(df)
  
}

getLowPDLocRows <- function(file_base) {
  overallStats <- read.csv(paste0(file_base, ".perf_", "overall", ".csv"))
  requestIDs <- overallStats[overallStats$num_pickups<5 | overallStats$num_dropoffs<5, colnames(overallStats) %in% c("request_id")]
  
  return(requestIDs)
}

getHighPDLocRows <- function(file_base) {
  overallStats <- read.csv(paste0(file_base, ".perf_", "overall", ".csv"))
  requestIDs <- overallStats[!(overallStats$num_pickups<5 | overallStats$num_dropoffs<5), colnames(overallStats) %in% c("request_id")]
  
  return(requestIDs)
}

getQuality <- function(file_base, suffix) {
  quality <- read.csv(paste0(file_base, ".assignmentquality.csv"))
  quality <- quality[, colnames(quality) %in% c("request_id", "trip_time")]
  names(quality)[names(quality)=="trip_time"] <- paste0("trip_time", suffix)
  return(quality)
}

compareLowPDLocs <- function(relative_file_base, absolute_file_base) {
  requestIDs <- getLowPDLocRows(relative_file_base)
  relativeQuality <- getQuality(relative_file_base, "_rel")
  absoluteQuality <- getQuality(absolute_file_base, "_abs")
  relativeQuality <- subset(relativeQuality, request_id %in% requestIDs)
  absoluteQuality <- subset(absoluteQuality, request_id %in% requestIDs)
  result <- merge(relativeQuality, absoluteQuality, by = "request_id")
  result <- result[, ! colnames(result) %in% c("request_id")]
  result <- result[,1] - result[,2]
  result <- sum(result)
  return(result)
}

compareHighPDLocs <- function(relative_file_base, absolute_file_base) {
  requestIDs <- getHighPDLocRows(relative_file_base)
  relativeQuality <- getQuality(relative_file_base, "_rel")
  relativeQuality <- getQuality(relative_file_base, "_rel")
  absoluteQuality <- getQuality(absolute_file_base, "_abs")
  relativeQuality <- subset(relativeQuality, request_id %in% requestIDs)
  absoluteQuality <- subset(absoluteQuality, request_id %in% requestIDs)
  result <- merge(relativeQuality, absoluteQuality, by = "request_id")
  result <- result[, ! colnames(result) %in% c("request_id")]
  result <- result[,1] - result[,2]
  result <- sum(result)
  return(result)
}

evaluatePointDispersion <- function(file_base) {
  points <- read.csv(paste0(file_base, ".filter_information.csv"))
  points <- points[, colnames(points) %in% c("filter_run", "x_coord", "y_coord")]
  points <- split(points, f = points$filter_run)
  
  octants <- lapply(points, calculateOctantNumbers)
  sums <- lapply(octants, sum)
  
  octants <- subset(octants, sums != 0)
  
  deviation <- sapply(octants, computeDeviation)
  zeroOctants <- sapply(octants, countZeroOctants)
  
  return(c("normalized deviation (avg)" = mean(deviation), "number of empty octants (avg)" = mean(zeroOctants)))
}

calculateOctantNumbers <- function(points) {
  points <- points[points$x_coord != 0 | points$y_coord != 0, ! colnames(points) %in% c("filter_run")]
  angles <- atan2(points$x_coord, points$y_coord) %% (2*pi)
  firstOctant <- (angles >= 0 & angles < pi/8) | (angles < 2*pi & angles >= (-pi/8) %% (2*pi)) # between  -pi/8 and   pi/8
  secondOctant <- angles >= pi/8 & angles < 3*pi/8                                             # between   pi/8 and  3pi/8
  thirdOctant <- angles >= 3*pi/8 & angles < 5*pi/8                                            # between  3pi/8 and  5pi/8
  fourthOctant <- angles >= 5*pi/8 & angles < 7*pi/8                                           # between  5pi/8 and  7pi/8
  fifthOctant <- angles >= 7*pi/8 & angles < 9*pi/8                                            # between  7pi/8 and  9pi/8
  sixthOctant <- angles >= 9*pi/8 & angles < 11*pi/8                                           # between  9pi/8 and 11pi/8
  seventhOctant <- angles >= 11*pi/8 & angles < 13*pi/8                                        # between 11pi/8 and 13pi/8
  eightOctant <- angles >= 13*pi/8 & angles < 15*pi/8                                          # between 13pi/8 and 15pi/8 (%% 2pi = -pi/8)
  octantNumbers <- c(sum(firstOctant), sum(secondOctant), sum(thirdOctant), sum(fourthOctant),
                     sum(fifthOctant), sum(sixthOctant), sum(seventhOctant), sum(eightOctant))
  return(octantNumbers)
}

computeDeviation <- function(octantNumbers) {
  totalPoints <- sum(octantNumbers)
  deviation <- sum((octantNumbers/totalPoints - 1/8)^2)
  
  return(deviation)
}

countZeroOctants <- function(octantNumbers) {
  zeroOctant <- octantNumbers == 0
  return(sum(zeroOctant))
}

plotComparisons <- function(run_types, date, noRuns) {
  for (run_type in run_types) {
    for (i in (1:noRuns)) {
      file_base <- paste0(date, "_", run_type, "_", i)
      qualityStats <- quality(file_base)
      performance <- overallPerfStats(file_base)
    }
  }
}

verifyHeuristics <- function(file_base) {
  ranks <- read.csv(paste0(file_base, ".ranks.csv"))
  bestAssignments <- read.csv(paste0(file_base, ".request_pdLocs.csv"))
  pickups <- bestAssignments[, ! colnames(bestAssignments) %in% c("dropoff", "origin", "destination")]
  dropoffs <- bestAssignments[, ! colnames(bestAssignments) %in% c("pickup", "origin", "destination")]
  origins <- bestAssignments[, ! colnames(bestAssignments) %in% c("pickup", "dropoff", "destination")]
  destinations <- bestAssignments[, ! colnames(bestAssignments) %in% c("pickup", "dropoff", "origin")]
  
  pickups <- merge(pickups, ranks, by.x = "pickup", by.y = "vertex_id", sort = FALSE)
  dropoffs <- merge(dropoffs, ranks, by.x = "dropoff", by.y = "vertex_id")
  origins <- merge(origins, ranks, by.x = "origin", by.y = "vertex_id")
  destinations <- merge(destinations, ranks, by.x = "destination", by.y = "vertex_id")
  
  colnames(origins)[colnames(origins) == "rank"] <- "passenger_rank"
  colnames(destinations)[colnames(destinations) == "rank"] <- "passenger_rank"
  
  avgRank <- apply(ranks, 2, mean)
  avgPickupRank <- apply(pickups, 2, mean)
  avgDropoffRank <- apply(dropoffs, 2, mean)
  
  print(paste0("Average Rank: ", avgRank[["rank"]]))
  print(paste0("Average Pickup Rank: ", avgPickupRank[["rank"]]))
  print(paste0("Average Dropoff Rank: ", avgDropoffRank[["rank"]]))
  
  print(sum(pickups[, colnames(pickups) == "rank"]))
  
  pickupProportion <- evaluatePDRankVersusPassengerRank(pickups[, ! colnames(pickups) %in% c("pickup")], origins[, ! colnames(origins) %in% c("origin")])
  dropoffProportion <- evaluatePDRankVersusPassengerRank(dropoffs[, ! colnames(dropoffs) %in% c("dropoff")], destinations[, ! colnames(destinations) %in% c("destination")])
  
  print(paste0("Proportion of Pickup locations with higher rank than origin: ", pickupProportion))
  print(paste0("Proportion of Dropoff locations with higher rank than destination: ", dropoffProportion))
  
  maxRanksAndDominators <- findMaxRanksAndDominators(file_base, ranks, pickups, dropoffs)
  
  evaluateMaxRankStats(maxRanksAndDominators[["pickups"]], "pickup locations")
  evaluateMaxRankStats(maxRanksAndDominators[["dropoffs"]], "dropoff locations")
}

findMaxRanksAndDominators <- function(file_base, ranks, selectedPickups, selectedDropoffs) {
  pdLocs <- read.csv(paste0(file_base, ".possible_pdLocs.csv"))
  pdLocs <- merge(pdLocs,ranks,by.x = "location", by.y = "vertex_id")
  
  splitPDLocs <- split(pdLocs, f = pdLocs$request_id)
  
  splitPickups <- lapply(splitPDLocs, FUN = (\(x) x[x$pd == "p",]))
  splitDropoffs <- lapply(splitPDLocs, FUN = (\(x) x[x$pd == "d",]))
  
  pickupRanks <- lapply(splitPickups, FUN = (\(x) x[,colnames(x) %in% c("rank", "request_id")]))
  dropoffRanks <- lapply(splitDropoffs, FUN = (\(x) x[,colnames(x) %in% c("rank", "request_id")]))
  
  pickupMaxima <- lapply(pickupRanks, calculateMaxAndDominators, selectedPickups)
  dropoffMaxima <- lapply(dropoffRanks, calculateMaxAndDominators, selectedDropoffs)
  return(list(pickups=pickupMaxima, dropoffs=dropoffMaxima))
}

calculateMaxAndDominators <- function(data, selectedLocs) {
  maximumRank <- max(data[,colnames(data) %in% c("rank")])
  requestID <- data[[1,1]]
  
  rank <- selectedLocs[selectedLocs$request_id == requestID, colnames(selectedLocs) %in% c("rank")]
  
  dominators <- length(data[data$rank >= rank,colnames(data) %in% c("rank")])
  
  return(c(request_id=requestID,max_rank=maximumRank,selected_rank=rank,higher_ranks=dominators))
}

evaluateMaxRankStats <- function(data, usage) {
  print(paste0("Statistics for ", usage))
  
  #How many (absolute) are higher or equal rank - avg, 80percentile, 90percentile, 95percentile
  dominators <- sapply(data, (\(x) x["higher_ranks"]))
  
  dominatorStats <- c(avg=mean(dominators), quantile(dominators, 0.8), quantile(dominators, 0.9), quantile(dominators,0.95))
  print("Parameters to be chosen for Absolute CH filter to ensure that on average / in 80/90/95 percent of the cases, the optimal solution is not filtered out:")
  print(dominatorStats)
  
  #Within what percentage of the maximum is the selected - avg, 80percentile, 90percentile, 95percentile
  percentages <- sapply(data, (\(x) 1 - x["selected_rank"] / x["max_rank"]))
  
  percentagesStats <- c(avg=mean(percentages), quantile(percentages, 0.8), quantile(percentages, 0.9), quantile(percentages,0.95))
  print("Parameters to be chosen for Relative CH filter to ensure that on average / in 80/90/95 percent of the cases, the optimal solution is not filtered out:")
  print(percentagesStats)
}

evaluatePDRankVersusPassengerRank <- function(selected, passenger) {
  combined <- merge(selected, passenger, by.x = "request_id", by.y = "request_id")
  
  above <- apply(combined, 1, (\(x) if (x[["rank"]] > x[["passenger_rank"]]) 1 else 0))
  
  return(mean(above))
}
