# SPLC22 Data processing code

library(ggplot2)
library(tidyverse)
library(hrbrthemes)


####################################################################################################


# Reads the participants data
experiment.complete.data <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/ExperimentCompleteDataSet.csv", 
                header=TRUE)
attach(experiment.complete.data)



