# Transforming the computed transitions data for plotting

library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(dplyr)



## Example of computing the transistion data and plotting for participant 05
participant05.data <- read.csv(file ="../../Experiment-Data/Eye-tracking-data-samples/PartP05/P05-Transitions-Data.csv",
                               header=TRUE)
attach (participant05.data)

# For all the participant data, create a row with the form:
# Participant, QN, 1..6 AOI ID, Duration

raw.df <- data.frame()
raw.df <- raw.df %>% add_column(Participant="Participant", QN="QN", IDAOI="IDAOI", Duration="Duration")

for (i in 1:nrow(participant05.data)) {
  row.data <- participant05.data[i,]
  
  new.row <- c(row.data$Participant, row.data$QN, 0, row.data$Duration)  

#  raw.df <- raw.df %>% add_row(Participant=row.data$Participant,
#                     QN = row.data$QN,
#                     IDAOI = row.data$Index,
#                     Duration = row.data$Duration)

  # Answer=1, Buttons=2, CTC=3, FM=4, Legend=5, Question=6, Window=7
  aoi.fixation <- 0 
  num.selected.aois <-0
  
  # Indicated the selected AOI of the fixation
  if (row.data$Answer=="1") {
    aoi.fixation <- 1
    num.selected.aois <-  num.selected.aois + 1
  }
  if (row.data$Buttons=="1") {
    aoi.fixation <- 2
    num.selected.aois <-  num.selected.aois + 1
  }
  if (row.data$CTC=="1") {
    aoi.fixation <- 3
    num.selected.aois <-  num.selected.aois + 1
  }
  if (row.data$FM =="1") {
    aoi.fixation <- 4
    num.selected.aois <-  num.selected.aois + 1
  }
  if (row.data$Legend=="1") {
    aoi.fixation <- 5
    num.selected.aois <-  num.selected.aois + 1
  }
  if (row.data$Question=="1") {
    aoi.fixation <- 6
    num.selected.aois <-  num.selected.aois + 1
  }
  
  # if no AOI fixation was selected then mark it as Window, i.e. not in any AOI
  if (aoi.fixation == 0) {
    aoi.fixation <- 7
  }

  # Error checking
  if (row.data$Window != "1") {
    stop("Error in data")
  } 
    
  raw.df[i,] = c(row.data$Participant,row.data$QN,aoi.fixation,row.data$Duration)
  
  print (i)
  print (new.row)
}


# TODO 
# Keep the counter per question to add a sequencial number for each fixation in a question
# Transform the resulting columns to nuerical factors all the columns
# Explore first plotting examples