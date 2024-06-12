source("~/Documents/Uni/Bachelorarbeit/KaRRi/Publications/KaRRi/eval.R")

runs <- c("ALL", "MAX_RAND", "CH_ABS", "CH_REL", "PARETO_SIMPLE", "PARETO_DIR")
run_params <- list(
  ALL = c(""), 
  MAX_RAND = c("1", "2", "3", "4", "5", "10", "15", "20", "25"), 
  CH_ABS = c("1", "2", "3", "4", "5", "10", "15", "20", "25"),
  CH_REL = c("1", "2", "3", "4", "5", "6", "8", "10", "12", "15", "20"),
  PARETO_SIMPLE = c("1", "2", "3", "4", "5", "6", "7"),
  PARETO_DIR = c("1", "2", "3", "4", "5", "6", "7")
)

compare_quality <- function(file_base) {
  result <- list()
  for (strat in runs) {
    strat_results <- list()
    for (k in run_params[[strat]]) {
      for (run in 1:5){
        if(run == 1) {
          run_file_base <- paste0(file_base, "/", strat, "_", k, "_KaRRi_run", run)
          run_quality <- quality(run_file_base)
          strat_results[[paste0(k)]] <- run_quality[["trip_time_avg"]]
        }
      }
    }
    result[[strat]] <- strat_results
  }
  return(result)
}