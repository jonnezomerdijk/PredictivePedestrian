rm(list=ls())
source("Code/PredictivePedestrianA7.R")
source("Code/conflictingFunc.R")

# execution settings
jobs <- 1:4
n_cores <- 2
plotSim <- FALSE
# condition settings
n_iterations <- 900
max_caps <- 47
goalType <- "KnownOrdered"
randomness <- 1/200
interactionTime <- 2
goalStackSize <- 5
# source and output
script <- paste0("Code/playRetail",goalType,".R")
output_dir <- "outputInteraction"

# Run jobs
for (j in jobs) {
  for (c in 1:length(max_caps)) {
  nam <- paste0(goalType,"_IT",interactionTime,"_",max_caps[c],"_",j)
    cat(paste(
      "nam <- \"",nam,"\"\n",
      "max_cap <- ",max_caps[c],"\n",
      "output_dir <- \"",output_dir,"\"\n",
      "plotSim <- ",plotSim,"\n",
      "n_iterations <- ",n_iterations,"\n",
      "n_cores <- ",n_cores,"\n",
      "randomness <- ",randomness,"\n",
      "interactionTime <- ",interactionTime,"\n",
      "goalStackSize <- ",goalStackSize-1,"\n",
      "source(\"",script,"\")\n",
      sep=""
    ),file=paste0("Code/",nam,".R"))
    system(paste0("nohup R CMD BATCH ~/Jonne/Code/",nam,".R &"))
  }
}
