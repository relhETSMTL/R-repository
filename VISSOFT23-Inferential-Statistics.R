# IST Paper  - Inferential Statistics
# VISSOFT 2023

library("tidyverse")

## Loads the data files and completes the framework including T value
# Divides the responses according to t value
all.gaze.data <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/Analyzed-2023-09-18.csv", 
                          header=TRUE)

