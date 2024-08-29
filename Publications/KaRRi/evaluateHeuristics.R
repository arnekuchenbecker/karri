source("~/Documents/Uni/Bachelorarbeit/KaRRi/Publications/KaRRi/eval.R")
source("~/Documents/Uni/Bachelorarbeit/KaRRi/Publications/KaRRi/outputAsTikz.R")

library(ggplot2)
library(scales)

strats <- c("ALL", "MAX_RAND", "CH_ABS", "CH_REL", "PARETO_SIMPLE", "PARETO_DIR", "CH_COVER")
run_params <- list(
  ALL = c(0), 
  MAX_RAND = c(1, 2, 3, 4, 5, 10, 15, 20, 25), 
  CH_ABS = c(1, 2, 3, 4, 5, 10, 15, 20, 25),
  CH_REL = c(1, 2, 3, 4, 5, 6, 8, 10, 12, 15, 20),
  PARETO_SIMPLE = c(1, 2, 3, 4, 5, 6, 7),
  PARETO_DIR = c(1, 2, 3, 4, 5, 6, 7),
  CH_COVER = c(0)
)
instances <- c("BerlinSmall", "BerlinSmallRed", "BerlinLarge", "RuhrSmall", "RuhrLarge")
radii <- c(300, 600)
y_lims <- list(
  BerlinSmall300 = list(
    trip_time_avg = c(930, 1030),
    op_time_avg = c(14300, 16400),
    empty_time_avg = c(1500, 2400)
  ),
  BerlinSmall600 = list(
    trip_time_avg = c(940, 1030),
    op_time_avg = c(14000, 16500),
    empty_time_avg = c(1500, 2500)
  ),
  RuhrSmall300 = list(
    trip_time_avg = c(1000, 1100),
    op_time_avg = c(14500, 17000),
    empty_time_avg = c(2400, 3500)
  )
)
x_lims <- list(
  BerlinSmall300 = c(350000, 1000000),
  BerlinSmall600 = c(350000, 4000000),
  RuhrSmall300 = c(350000, 950000)
)

plotComparison <- function(file_base, instance, radius, printFilterTiming) {
  if (! any(instances == instance)) {
    print("Error: Unknown instance name")
    return()
  }
  if(! any(radii == radius)) {
    print("Error: Unknown radius")
    return()
  }
  qualities <- getQualityComparison(file_base)
  stats <- getOverallPerfStatsComparison(file_base)
  
  path <- paste0(instance, radius, "/")
  
  plotQuality("trip_time_avg", qualities, stats, paste0(path, "avgTripTime"), "Average Trip Time [s]")
  plotQuality("op_time_avg", qualities, stats, paste0(path, "avgOpTime"), "Average Operation Time [s]")
  plotQuality("empty_time_avg", qualities, stats, paste0(path, "avgEmptyTime"), "Average Empty Time [s]")
  
  for (strategy in strats) {
    if (strategy != "ALL") {
      plotRuntimeSplit(stats, stats[["ALL"]], strategy)
    }
  }
  
  plotSpeedup(stats, paste0(path, "Speedup"))
  
  if(printFilterTiming) {
    runtimes <- getRunningTimes(file_base)
    plotFilterRunningTimes(stats, runtimes, paste0(path, "filter_runtimes"))
  }
}

plotVehicleAmountComparison <- function(first_file_base, first_vehicles, second_file_base, second_vehicles, strategies = c("CH_ABS", "PARETO_DIR")) {
  parameters <- c("trip_time_avg", "op_time_avg", "empty_time_avg", "occ_rate_avg")
  first_qualities <- getQualityComparison(first_file_base)
  second_qualities <- getQualityComparison(second_file_base)
  first_stats <- getOverallPerfStatsComparison(first_file_base)
  second_stats <- getOverallPerfStatsComparison(second_file_base)
  
  for(param in parameters) {
    plotVehicleComparison(first_qualities, first_stats, first_vehicles, second_qualities, second_stats, second_vehicles, strategies, param)
  }
}

getQualityComparison <- function(file_base) {
  result <- list()
  for (strat in strats) {
    strat_results <- list()
    for (k in run_params[[strat]]) {
      paramText <- ""
      if (k != 0) {
        paramText <- paste0("_", k)
      }
      run_file_base <- paste0(file_base, strat, paramText, "_KaRRi_run", 1)
      run_quality <- getQualityStats(run_file_base)
      strat_results[[paste0(k)]] <- run_quality
    }
    result[[strat]] <- strat_results
  }
  return(result)
}

getOverallPerfStatsComparison <- function(file_base) {
  result <- list()
  for (strat in strats) {
    strat_results <- list()
    for (k in run_params[[strat]]) {
      runStats <- getOverallPerfStatsForRun(1, file_base, strat, k)
      for (i in (2:5)) {
        runStats <- rbind(runStats, getOverallPerfStatsForRun(i, file_base, strat, k))
      }
      avgStats <- apply(runStats, 2, mean)
      avgStats["k"] <- k
      strat_results[[paste0(k)]] <- avgStats
    }
    result[[strat]] <- strat_results
  }
  return(result)
}

getOverallPerfStatsForRun <- function(run, file_base, strat, k) {
  paramText <- ""
  if (k != 0) {
    paramText <- paste0("_", k)
  }
  run_file_base <- paste0(file_base, strat, paramText, "_KaRRi_run", run)
  perfStats <- overallPerfStats(run_file_base)
  return(perfStats)
}

plotQuality <- function(parameter, qualities, stats, fileName, parameter_name, print_outside=FALSE) {
  df <- convertToLineplotFormat(stats, qualities, parameter)
  
  plt <- ggplot(df, aes(x=meeting_points, y=!!rlang::sym(parameter), group=strategy)) +
    scale_shape_manual(values=1:7) +
    geom_line(aes(color=strategy)) + 
    geom_point(aes(color=strategy, shape=strategy), size=3) + 
    theme_minimal()
  if(!print_outside) {
    plt <- plt + theme(
      legend.position = "inside",
      legend.justification.inside = c(1,1),
      legend.position.inside = c(1,1),
      legend.background = element_rect(fill = "white", colour = "black"),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10))
    )
  } else {
    plt <- plt + theme(
      legend.background = element_rect(fill = "white", colour = "black"),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10))
    )
  }
    
  plt <- plt + scale_x_continuous(labels = label_comma()) +
    #ggtitle(title) +
    labs(
      x="$\\numPickups{\\rho}{r} + \\numDropoffs{\\rho}{r}$",
      y=parameter_name,
      colour="Filter Strategies",
      shape="Filter Strategies"
    )
  
  outputAsTikz(plt, fileName, "1", height=3.5, width=5)
}

plotRuntimeSplit <- function(stats, all_stats, strategy) {
  runs <- stats[[strategy]]
  
  df <- as.data.frame(t(all_stats[[1]]))
  df[1,"k"] <- "ALL"
  if (length(runs) >= 1) {
    for(k in (1:length(runs))) {
      new_df <- runs[[k]]
      df <- rbind(df, new_df)
    }
  }
  df <- df[c(setdiff(seq_len(nrow(df)), 1), 1), ]
  df <- df[,! colnames(df) %in% c("num_pickups", "num_dropoffs", "total_time")]
  
  print_bar_chart(df, strategy)
}

convertToBarplotFormat <- function(df, x) {
  if (! x %in% colnames(df)) {
    print("Error: Unknown column name")
    return()
  }
  
  categories <- c()
  
  for (x_value in df[,x]) {
    categories <- c(categories, rep(x_value, ncol(df) - 1))
  }
  
  subcategories <- colnames(df[,! colnames(df) == x])
  subcategories <- rep(subcategories, nrow(df))
  
  values <- df[,! colnames(df) == x]
  values <- t(values)
  values <- as.vector(values)
  
  return_frame = data.frame(categories, subcategories, values)
  
  return(return_frame)
}

convertToLineplotFormat <- function(stats, qualities, parameter) {
  names <- c("strategy", "meeting_points", parameter)
  strategy_names = c(ALL = "\\texttt{All}", CH_ABS = "\\texttt{CH\\textsubscript{fix}}", CH_REL = "\\texttt{CH\\textsubscript{dyn}}", MAX_RAND = "\\texttt{Random}", PARETO_DIR = "\\texttt{Pareto\\textsubscript{dir}}", PARETO_SIMPLE = "\\texttt{Pareto}", CH_COVER = "\\texttt{RankCover}")
  all_stats <- stats[["ALL"]]
  all_stats <- all_stats[[1]]
  all_quality <- qualities[["ALL"]]
  all_quality <- all_quality[[1]]
  df <- setNames(data.frame(strategy_names[["ALL"]], all_stats["num_pickups"] + all_stats["num_dropoffs"], all_quality[parameter]), names)
  
  for(strategy in strats) {
    x <- sapply(stats[[strategy]], (\(run) run[["num_pickups"]] + run[["num_dropoffs"]]))
    y <- sapply(qualities[[strategy]], (\(run) run[[parameter]]))
    df <- rbind(df, setNames(data.frame(strategy_names[[strategy]], x, y), names))
  }
  return(df)
}

print_bar_chart <- function(df, title) {
  chart_df <- convertToBarplotFormat(df, "k")
  
  plt <- ggplot(chart_df, aes(x=categories,y=values,fill=subcategories)) +
    geom_col(position = "stack") +
    ggtitle(title) +
    scale_x_discrete(limits=chart_df$categories) +
    scale_y_continuous(expand=expansion(mult=c(0,.1))) +
    theme_linedraw() +
    labs(
      x = "",
      y = "time per request [ns]",
      fill = "algorithm step"
    )
  
  print(plt)
}

plotVehicleComparison <- function(first_qualities, first_stats, first_vehicles, second_qualities, second_stats, second_vehicles, strategies, parameter) {
  names <- c("strategy_veh", "total_time", parameter)
  
  # Add ALL point for first run
  first_all_stats <- first_stats[["ALL"]]
  first_all_stats <- first_all_stats[[1]]
  first_all_quality <- first_qualities[["ALL"]]
  first_all_quality <- first_all_quality[[1]]
  df <- setNames(data.frame(paste0("ALL_", first_vehicles), first_all_stats["total_time"], first_all_quality[parameter]), names)
  
  # Add ALL point for second run
  second_all_stats <- second_stats[["ALL"]]
  second_all_stats <- second_all_stats[[1]]
  second_all_quality <- second_qualities[["ALL"]]
  second_all_quality <- second_all_quality[[1]]
  df <- rbind(df, setNames(data.frame(paste0("ALL_", second_vehicles), second_all_stats["total_time"], second_all_quality[parameter]), names))
  
  for(strat in strategies) {
    x <- sapply(first_stats[[strat]], (\(run) run[["total_time"]]))
    y <- sapply(first_qualities[[strat]], (\(run) run[[parameter]]))
    df <- rbind(df, setNames(data.frame(paste0(strat, "_", first_vehicles), x, y), names))
    
    x <- sapply(second_stats[[strat]], (\(run) run[["total_time"]]))
    y <- sapply(second_qualities[[strat]], (\(run) run[[parameter]]))
    df <- rbind(df, setNames(data.frame(paste0(strat, "_", second_vehicles), x, y), names))
  }
  
  plt <- ggplot(df, aes(x=total_time, y=!!rlang::sym(parameter), group=strategy_veh)) +
    geom_line(aes(color=strategy_veh)) + 
    geom_point(aes(color=strategy_veh, shape=strategy_veh), size=3) + 
    theme_linedraw() +
    scale_x_continuous(labels = label_comma()) +
    labs(
      x="Runtime per Request [ns]",
      y=parameter,
      colour="Filter Strategy",
      shape="Filter Strategy"
    )
  
  print(plt)
}

compareCoverStrategy <- function(file_base_cover, file_base_abs, file_base_rand) {
  cover_quality_300 <- getQualityStats(paste0(file_base_cover, "_", 300))
  abs_quality_300 <- getQualityStats(paste0(file_base_abs, "_", 300))
  rand_quality_300 <- getQualityStats(paste0(file_base_rand, "_", 300))
  cover_quality_600 <- getQualityStats(paste0(file_base_cover, "_", 600))
  abs_quality_600 <- getQualityStats(paste0(file_base_abs, "_", 600))
  rand_quality_600 <- getQualityStats(paste0(file_base_rand, "_", 600))
  
  cover_stats_300 <- overallPerfStats(paste0(file_base_cover, "_", 300))
  abs_stats_300 <- overallPerfStats(paste0(file_base_abs, "_", 300))
  rand_stats_300 <- overallPerfStats(paste0(file_base_rand, "_", 300))
  cover_stats_600 <- overallPerfStats(paste0(file_base_cover, "_", 600))
  abs_stats_600 <- overallPerfStats(paste0(file_base_abs, "_", 600))
  rand_stats_600 <- overallPerfStats(paste0(file_base_rand, "_", 600))
  
  strats <- c("Cover", "Abs", "Rand", "Cover", "Abs", "Rand")
  radii <- c("300", "300", "300", "600", "600", "600")
  trip_times <- c(cover_quality_300$trip_time_avg, abs_quality_300$trip_time_avg, rand_quality_300$trip_time_avg,
                  cover_quality_600$trip_time_avg, abs_quality_600$trip_time_avg, rand_quality_600$trip_time_avg)
  op_times <- c(cover_quality_300$op_time_avg, abs_quality_300$op_time_avg, rand_quality_300$op_time_avg,
                cover_quality_600$op_time_avg, abs_quality_600$op_time_avg, rand_quality_600$op_time_avg)
  empty_times <- c(cover_quality_300$empty_time_avg, abs_quality_300$empty_time_avg, rand_quality_300$empty_time_avg,
                   cover_quality_600$empty_time_avg, abs_quality_600$empty_time_avg, rand_quality_600$empty_time_avg)
  times <- c(cover_stats_300[["total_time"]], abs_stats_300[["total_time"]], rand_stats_300[["total_time"]],
             cover_stats_600[["total_time"]], abs_stats_600[["total_time"]], rand_stats_600[["total_time"]])
  
  df <- data.frame(strats, trip_times, op_times, empty_times, times)
  
  plt_trip_time <- ggplot(df, aes(x=times, y=trip_times)) +
    geom_point(aes(color=strats, shape=radii), size=3) + 
    theme_linedraw() +
    scale_x_continuous(labels = label_comma()) +
    ggtitle("Avg Trip Time")
  
  print(plt_trip_time)
  
  plt_op_time <- ggplot(df, aes(x=times, y=op_times)) +
    geom_point(aes(color=strats, shape=radii), size=3) + 
    theme_linedraw() +
    scale_x_continuous(labels = label_comma()) +
    ggtitle("Avg Operation Time")
  
  print(plt_op_time)
  
  plt_empty_time <- ggplot(df, aes(x=times, y=empty_times)) +
    geom_point(aes(color=strats, shape=radii), size=3) + 
    theme_linedraw() +
    scale_x_continuous(labels = label_comma()) +
    ggtitle("Avg Empty Time")
  
  print(plt_empty_time)
}

plotSpeedup <- function(stats, fileName) {
  strategy_names = c(ALL = "\\texttt{All}", CH_ABS = "\\texttt{CH\\textsubscript{fix}}", CH_REL = "\\texttt{CH\\textsubscript{dyn}}", MAX_RAND = "\\texttt{Random}", PARETO_DIR = "\\texttt{Pareto\\textsubscript{dir}}", PARETO_SIMPLE = "\\texttt{Pareto}", CH_COVER = "\\texttt{RankCover}")
  allStats <- stats[["ALL"]]
  allStats <- allStats[[1]]
  allTime <- allStats["total_time"]
  allMeetingPoints <- allStats[["num_pickups"]] + allStats[["num_dropoffs"]]
  names <- c("strategy", "numMeetingPoints", "speedup")
  df <- setNames(data.frame(strategy_names[["ALL"]], allMeetingPoints, 1), names)
  for(strategy in strats) {
    if (strategy != "ALL") {
      x <- sapply(stats[[strategy]], (\(run) run[["num_pickups"]] + run[["num_dropoffs"]]))
      y <- sapply(stats[[strategy]], (\(run) allTime / run[["total_time"]]))
      df <- rbind(df, setNames(data.frame(strategy_names[[strategy]], x, y), names))
    }
  }
  
  plt <- ggplot(df, aes(x=numMeetingPoints, y=speedup, group=strategy)) +
    scale_shape_manual(values=1:7) +
    geom_line(aes(color=strategy)) + 
    geom_point(aes(color=strategy, shape=strategy), size=3) + 
    theme_minimal() +
    theme(
      legend.position = "inside",
      legend.justification.inside = c(1,1),
      legend.position.inside = c(1,1),
      legend.background = element_rect(fill = "white", colour = "black"),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10))
      ) +
    scale_x_continuous(labels = label_comma()) +
    labs(
      x="$\\numPickups{\\rho}{r} + \\numDropoffs{\\rho}{r}$",
      y="Speedup",
      colour="Filter Strategies",
      shape="Filter Strategies"
    )
  
  outputAsTikz(plt, fileName, "1", height = 3.1, width = 6)
}

getRunningTimes <- function(file_base) {
  result <- list()
  for (strat in strats) {
    strat_results <- list()
    for (k in run_params[[strat]]) {
      runStats <- getRunningTimesForRun(1, file_base, strat, k)
      for (i in (2:5)) {
        runStats <- rbind(runStats, getRunningTimesForRun(i, file_base, strat, k))
      }
      avgStats <- apply(runStats, 2, mean)
      avgStats["k"] <- k
      strat_results[[paste0(k)]] <- avgStats
    }
    result[[strat]] <- strat_results
  }
  return(result)
}

getRunningTimesForRun <- function(run, file_base, strat, k) {
  paramText <- ""
  if (k != 0) {
    paramText <- paste0("_", k)
  }
  run_file_base <- paste0(file_base, strat, paramText, "_KaRRi_run", run)
  runningTimes <- runningTimes(run_file_base)
  return(runningTimes)
}

runningTimes <- function(file_base) {
  stats <- read.csv(paste0(file_base, ".filter_time.csv"))
  stats <- apply(stats, 2, mean)
  stats <- round(stats, 2)
  return(stats)
}

plotFilterRunningTimes <- function(stats, runningTimes, fileName) {
  names <- c("strategy", "numMeetingPoints", "runningTime")
  strategy_names <- c(ALL = "\\texttt{All}", CH_ABS = "\\texttt{CH\\textsubscript{fix}}", CH_REL = "\\texttt{CH\\textsubscript{dyn}}", MAX_RAND = "\\texttt{Random}", PARETO_DIR = "\\texttt{Pareto\\textsubscript{dir}}", PARETO_SIMPLE = "\\texttt{Pareto}", CH_COVER = "\\texttt{RankCover}")
  allStats <- stats[["ALL"]]
  allStats <- allStats[[1]]
  allTime <- allStats["total_time"]
  allMeetingPoints <- allStats[["num_pickups"]] + allStats[["num_dropoffs"]]
  allRunningTime <- runningTimes[["ALL"]]
  allRunningTime <- allRunningTime[[1]]
  df <- setNames(data.frame(strategy_names[["ALL"]], allMeetingPoints, allRunningTime[["time"]] / 1000), names)
  
  for(strategy in strats) {
    if (strategy != "ALL") {
      x <- sapply(stats[[strategy]], (\(run) run[["num_pickups"]] + run[["num_dropoffs"]]))
      y <- sapply(runningTimes[[strategy]], (\(run) run[["time"]] / 1000))
      df <- rbind(df, setNames(data.frame(strategy_names[[strategy]], x, y), names))
    }
  }
  
  plt <- ggplot(df, aes(x=numMeetingPoints, y=runningTime, group=strategy)) +
    scale_shape_manual(values=1:7) +
    geom_line(aes(color=strategy)) + 
    geom_point(aes(color=strategy, shape=strategy), size=3) + 
    theme_minimal() +
    theme(
      legend.position = "inside",
      legend.justification.inside = c(1,1),
      legend.position.inside = c(1,1),
      legend.background = element_rect(fill = "white", colour = "black"),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10))
    ) +
    scale_x_continuous(labels = label_comma()) +
    labs(
      x="$\\numPickups{\\rho}{r} + \\numDropoffs{\\rho}{r}$",
      y="Running Time per request [${\\mu}s$]",
      colour="Filter Strategies",
      shape="Filter Strategies"
    )
  
  outputAsTikz(plt, fileName, "1", height = 3.1, width = 6)
}
