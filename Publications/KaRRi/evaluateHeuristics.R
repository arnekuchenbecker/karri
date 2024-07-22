source("~/Documents/Uni/Bachelorarbeit/KaRRi/Publications/KaRRi/eval.R")

library(ggplot2)
library(scales)

strats <- c("ALL", "MAX_RAND", "CH_ABS", "CH_REL", "PARETO_SIMPLE", "PARETO_DIR")
run_params <- list(
  ALL = c(0), 
  MAX_RAND = c(1, 2, 3, 4, 5, 10, 15, 20, 25), 
  CH_ABS = c(1, 2, 3, 4, 5, 10, 15, 20, 25),
  CH_REL = c(1, 2, 3, 4, 5, 6, 8, 10, 12, 15, 20),
  PARETO_SIMPLE = c(1, 2, 3, 4, 5, 6, 7),
  PARETO_DIR = c(1, 2, 3, 4, 5, 6, 7)
)
instances <- c("BerlinSmall", "BerlinLarge", "RuhrSmall", "RuhrLarge")
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

plotComparison <- function(file_base, instance, radius) {
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
  
  plotQuality("trip_time_avg", qualities, stats, "Average Trip Time", "Average Trip Time [s]")
  plotQuality("op_time_avg", qualities, stats, "Average Operation Time", "Averagy Operation Time [s]")
  plotQuality("empty_time_avg", qualities, stats, "Average Time Vehicles are Empty", "Average Empty Time [s]")
  
  for (strategy in strats) {
    if (strategy != "ALL") {
      plotRuntimeSplit(stats, stats[["ALL"]], strategy)
    }
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
  run_file_base <- paste0(file_base, strat, paramText, "_KaRRi_run", 1)
  perfStats <- overallPerfStats(run_file_base)
  return(perfStats)
}

plotQuality <- function(parameter, qualities, stats, title, parameter_name) {
  df <- convertToLineplotFormat(stats, qualities, parameter)
  
  plt <- ggplot(df, aes(x=total_time, y=!!rlang::sym(parameter), group=strategy)) +
    geom_line(aes(color=strategy)) + 
    geom_point(aes(color=strategy, shape=strategy), size=3) + 
    theme_linedraw() +
    scale_x_continuous(labels = label_comma()) +
    ggtitle(title) +
    labs(
      x="Runtime per Request [ns]",
      y=parameter_name,
      colour="Filter Strategy",
      shape="Filter Strategy"
    )
  
  print(plt)
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
  names <- c("strategy", "total_time", parameter)
  all_stats <- stats[["ALL"]]
  all_stats <- all_stats[[1]]
  all_quality <- qualities[["ALL"]]
  all_quality <- all_quality[[1]]
  df <- setNames(data.frame("ALL", all_stats["total_time"], all_quality[parameter]), names)
  
  # Add line for MAX_RAND
  x <- sapply(stats[["MAX_RAND"]], (\(run) run[["total_time"]]))
  y <- sapply(qualities[["MAX_RAND"]], (\(run) run[[parameter]]))
  df <- rbind(df, setNames(data.frame("MAX_RAND", x, y), names))
  
  # Add line for CH_ABS
  x <- sapply(stats[["CH_ABS"]], (\(run) run[["total_time"]]))
  y <- sapply(qualities[["CH_ABS"]], (\(run) run[[parameter]]))
  df <- rbind(df, setNames(data.frame("CH_ABS", x, y), names))
  
  # Add line for CH_REL
  x <- sapply(stats[["CH_REL"]], (\(run) run[["total_time"]]))
  y <- sapply(qualities[["CH_REL"]], (\(run) run[[parameter]]))
  df <- rbind(df, setNames(data.frame("CH_REL", x, y), names))
  
  # Add line for PARETO_SIMPLE
  x <- sapply(stats[["PARETO_SIMPLE"]], (\(run) run[["total_time"]]))
  y <- sapply(qualities[["PARETO_SIMPLE"]], (\(run) run[[parameter]]))
  df <- rbind(df, setNames(data.frame("PARETO_SIMPLE", x, y), names))
  
  # Add line for PARETO_DIR
  x <- sapply(stats[["PARETO_DIR"]], (\(run) run[["total_time"]]))
  y <- sapply(qualities[["PARETO_DIR"]], (\(run) run[[parameter]]))
  df <- rbind(df, setNames(data.frame("PARETO_DIR", x, y), names))
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
