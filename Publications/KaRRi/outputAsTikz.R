library(tikzDevice)

current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
setTikzDefaults(overwrite = TRUE)
options(tikzLatexPackages= c(getOption("tikzLatexPackages"), 
                             "\\usepackage{amsmath}",
                             "\\input{\\string~/Documents/Uni/Bachelorarbeit/Plots/Commands/macros}"
))

# Output plots:
outputAsTikz <- function(plot, output_name, version_name, output_base = "~/Documents/Uni/Bachelorarbeit/Plots/", height=2, width=3, standAlone = FALSE) {
  path <- paste0(output_base, output_name)
  print(plot)
  if (!endsWith(path, ".tex")) path <- paste0(path, ".tex")
  print(path)
  tikz(path, height=height, width=width,standAlone = standAlone)
  print(plot)
  dev.off()
}

