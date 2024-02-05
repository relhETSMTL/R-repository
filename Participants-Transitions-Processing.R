# Participants-Transitions-Processing.R
# This program performs the preprocessing of all the transitions files
# Outputs a single file with all the preprocessing information

# Transforming the computed transitions data for plotting

library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(dplyr)


# This functions computes the transitions of a participant based on the hits of the eye-tracker data
# Input: the transitions file of a participant
# Output: the transitions file of a participant in a condensed format
# Returns a frame to add
computeTransitionsParticipant <- function(inputFile, outputFile) {

  # Opens participant data file
  participant.data <- read.csv(file =inputFile, header=TRUE)
  attach (participant.data)
  
  # For all the participant data, create a row with the form:
  # Participant, QN, 1..6 AOI ID, FN fixation number, Duration
  
  raw.df <- data.frame()
  raw.df <- raw.df %>% add_column(Participant="Participant", QN="QN", IDAOI="IDAOI", FN ="FN", Duration="Duration")
  
  # Assumption that all the fixations of a question and participant are sequentially put together
  # Set current question to the first in the data frame
  current.qn <- participant.data$QN[1]
  
  # Keeps the counter of how many fixations per question
  fixation.counter <- 1
  
  # Loop for all the data of the participant
  for (i in 1:nrow(participant.data)) {
    row.data <- participant.data[i,]
    
    # if a new question number is found, set current question and reset fixation counter to 1
    if (row.data$QN != current.qn) {
      current.qn <- row.data$QN
      fixation.counter <- 1
    }
    
    
    # Answer=1, Buttons=2, CTC=3, FM=4, Legend=5, Question=6, Window=7
    aoi.fixation <- 0 
    num.selected.aois <-0
    
    # Indicated the selected AOI of the fixation
    if (row.data$Answer=="1") {
      aoi.fixation <- "Answer"
      num.selected.aois <-  num.selected.aois + 1
    }
    if (row.data$Buttons=="1") {
      aoi.fixation <- "Buttons"
      num.selected.aois <-  num.selected.aois + 1
    }
    if (row.data$CTC=="1") {
      aoi.fixation <- "CTC"
      num.selected.aois <-  num.selected.aois + 1
    }
    if (row.data$FM =="1") {
      aoi.fixation <- "FM"
      num.selected.aois <-  num.selected.aois + 1
    }
    if (row.data$Legend=="1") {
      aoi.fixation <- "Legend"
      num.selected.aois <-  num.selected.aois + 1
    }
    if (row.data$Question=="1") {
      aoi.fixation <- "Question"
      num.selected.aois <-  num.selected.aois + 1
    }
    
    # if no AOI fixation was selected then mark it as Window, i.e. not in any AOI
    if (aoi.fixation == 0) {
      aoi.fixation <- "Window"
    }
    
    # Error checking
    if (row.data$Window != "1") {
      stop("Error in data")
    } 
    
    # Adds the new row to the 
    raw.df[i,] = c(row.data$Participant,row.data$QN,aoi.fixation,fixation.counter, row.data$Duration)
    
    print (i)
    # print (new.row)
    
    # increments the row number
    fixation.counter <- fixation.counter + 1
    
  } # end of for all the data
  
  
  ## Transforming the columns into numerical values
  raw.df <- raw.df %>%
    mutate(Participant = as.numeric(Participant)) %>%
    mutate(QN = as.numeric(QN)) %>%
    mutate(IDAOI = as.factor(IDAOI)) %>%
    mutate(FN = as.numeric(FN)) %>%
    mutate(Duration = as.numeric(Duration))
  
  ## Saves the information of the raw data frame into the output file
  
  write.csv(raw.df, file = outputFile, row.names = FALSE)
  
  ## Returns the new data frame
  return (raw.df)
  
} # end of computeTransitionsParticipant

###############################################################################################################


## Calls the transitions processing on all the participants files
participant.p02 <- computeTransitionsParticipant("../../Experiment-Data/Eye-tracking-data-samples/PartP02/P02-Transitions-Data.csv",
                                                 "../../Experiment-Data/Eye-tracking-data-samples/PartP02/P02-Transitions-Curated-Data.csv")
participant.p03 <- computeTransitionsParticipant("../../Experiment-Data/Eye-tracking-data-samples/PartP03/P03-Transitions-Data.csv",
                                                 "../../Experiment-Data/Eye-tracking-data-samples/PartP03/P03-Transitions-Curated-Data.csv")
participant.p04 <- computeTransitionsParticipant("../../Experiment-Data/Eye-tracking-data-samples/PartP04/P04-Transitions-Data.csv",
                                                 "../../Experiment-Data/Eye-tracking-data-samples/PartP04/P04-Transitions-Curated-Data.csv")
participant.p05 <- computeTransitionsParticipant("../../Experiment-Data/Eye-tracking-data-samples/PartP05/P05-Transitions-Data.csv",
                                                 "../../Experiment-Data/Eye-tracking-data-samples/PartP05/P05-Transitions-Curated-Data.csv")

participant.p06 <- computeTransitionsParticipant("../../Experiment-Data/Eye-tracking-data-samples/PartP06/P06-Transitions-Data.csv",
                                                 "../../Experiment-Data/Eye-tracking-data-samples/PartP06/P06-Transitions-Curated-Data.csv")
participant.p07 <- computeTransitionsParticipant("../../Experiment-Data/Eye-tracking-data-samples/PartP07/P07-Transitions-Data.csv",
                                                 "../../Experiment-Data/Eye-tracking-data-samples/PartP07/P07-Transitions-Curated-Data.csv")
participant.p08 <- computeTransitionsParticipant("../../Experiment-Data/Eye-tracking-data-samples/PartP08/P08-Transitions-Data.csv",
                                                 "../../Experiment-Data/Eye-tracking-data-samples/PartP08/P08-Transitions-Curated-Data.csv")
participant.p09 <- computeTransitionsParticipant("../../Experiment-Data/Eye-tracking-data-samples/PartP09/P09-Transitions-Data.csv",
                                                 "../../Experiment-Data/Eye-tracking-data-samples/PartP09/P09-Transitions-Curated-Data.csv")
participant.p10 <- computeTransitionsParticipant("../../Experiment-Data/Eye-tracking-data-samples/PartP10/P10-Transitions-Data.csv",
                                                 "../../Experiment-Data/Eye-tracking-data-samples/PartP10/P10-Transitions-Curated-Data.csv")


participant.p11 <- computeTransitionsParticipant("../../Experiment-Data/Eye-tracking-data-samples/PartP11/P11-Transitions-Data.csv",
                                                 "../../Experiment-Data/Eye-tracking-data-samples/PartP11/P11-Transitions-Curated-Data.csv")
participant.p12 <- computeTransitionsParticipant("../../Experiment-Data/Eye-tracking-data-samples/PartP12/P12-Transitions-Data.csv",
                                                 "../../Experiment-Data/Eye-tracking-data-samples/PartP12/P12-Transitions-Curated-Data.csv")
participant.p13 <- computeTransitionsParticipant("../../Experiment-Data/Eye-tracking-data-samples/PartP13/P13-Transitions-Data.csv",
                                                 "../../Experiment-Data/Eye-tracking-data-samples/PartP13/P13-Transitions-Curated-Data.csv")
participant.p14 <- computeTransitionsParticipant("../../Experiment-Data/Eye-tracking-data-samples/PartP14/P14-Transitions-Data.csv",
                                                 "../../Experiment-Data/Eye-tracking-data-samples/PartP14/P14-Transitions-Curated-Data.csv")

participant.p16 <- computeTransitionsParticipant("../../Experiment-Data/Eye-tracking-data-samples/PartP16/P16-Transitions-Data.csv",
                                                 "../../Experiment-Data/Eye-tracking-data-samples/PartP16/P16-Transitions-Curated-Data.csv")
participant.p17 <- computeTransitionsParticipant("../../Experiment-Data/Eye-tracking-data-samples/PartP17/P17-Transitions-Data.csv",
                                                 "../../Experiment-Data/Eye-tracking-data-samples/PartP17/P17-Transitions-Curated-Data.csv")
participant.p18 <- computeTransitionsParticipant("../../Experiment-Data/Eye-tracking-data-samples/PartP18/P18-Transitions-Data.csv",
                                                 "../../Experiment-Data/Eye-tracking-data-samples/PartP18/P18-Transitions-Curated-Data.csv")
participant.p19 <- computeTransitionsParticipant("../../Experiment-Data/Eye-tracking-data-samples/PartP19/P19-Transitions-Data.csv",
                                                 "../../Experiment-Data/Eye-tracking-data-samples/PartP19/P19-Transitions-Curated-Data.csv")

# Creates a data frame with all the participants data

all.participants.transitions <- rbind(participant.p02, participant.p03, participant.p04, participant.p05,
                                     participant.p06, participant.p07, participant.p08, participant.p09, participant.p10,
                                     participant.p11, participant.p12, participant.p13, participant.p14,
                                     participant.p16, participant.p17, participant.p18, participant.p19)

write.csv(all.participants.transitions, 
          file = "../../Experiment-Data/Eye-tracking-data-samples/All-Participants-Transitions-Curated-Data.csv",
          row.names = FALSE)


# ## Example of computing the transistion data and plotting for participant 05
# participant05.data <- read.csv(file ="../../Experiment-Data/Eye-tracking-data-samples/PartP05/P05-Transitions-Data.csv",
#                                header=TRUE)
# attach (participant05.data)
# 
# # For all the participant data, create a row with the form:
# # Participant, QN, 1..6 AOI ID, FN fixation number, Duration
# 
# raw.df <- data.frame()
# raw.df <- raw.df %>% add_column(Participant="Participant", QN="QN", IDAOI="IDAOI", FN ="FN", Duration="Duration")
# 
# # Assumption that all the fixations of a question and participant are sequentially put together
# # Set current question to the first in the data frame
# current.qn <- participant05.data$QN[1]
# # Keeps the counter of how many fixations per question
# fixation.counter <- 1
# 
# # Loop for all the data of the participant
# for (i in 1:nrow(participant05.data)) {
#   row.data <- participant05.data[i,]
#   
#   # if a new question number is found, set current question and reset fixation counter to 1
#   if (row.data$QN != current.qn) {
#     current.qn <- row.data$QN
#     fixation.counter <- 1
#   }
#   
#   
#   # Answer=1, Buttons=2, CTC=3, FM=4, Legend=5, Question=6, Window=7
#   aoi.fixation <- 0 
#   num.selected.aois <-0
#   
#   # Indicated the selected AOI of the fixation
#   if (row.data$Answer=="1") {
#     aoi.fixation <- "Answer"
#     num.selected.aois <-  num.selected.aois + 1
#   }
#   if (row.data$Buttons=="1") {
#     aoi.fixation <- "Buttons"
#     num.selected.aois <-  num.selected.aois + 1
#   }
#   if (row.data$CTC=="1") {
#     aoi.fixation <- "CTC"
#     num.selected.aois <-  num.selected.aois + 1
#   }
#   if (row.data$FM =="1") {
#     aoi.fixation <- "FM"
#     num.selected.aois <-  num.selected.aois + 1
#   }
#   if (row.data$Legend=="1") {
#     aoi.fixation <- "Legend"
#     num.selected.aois <-  num.selected.aois + 1
#   }
#   if (row.data$Question=="1") {
#     aoi.fixation <- "Question"
#     num.selected.aois <-  num.selected.aois + 1
#   }
#   
#   # if no AOI fixation was selected then mark it as Window, i.e. not in any AOI
#   if (aoi.fixation == 0) {
#     aoi.fixation <- "Window"
#   }
#   
#   # Error checking
#   if (row.data$Window != "1") {
#     stop("Error in data")
#   } 
#   
#   # Adds the new row to the 
#   raw.df[i,] = c(row.data$Participant,row.data$QN,aoi.fixation,fixation.counter, row.data$Duration)
#   
#   print (i)
#   # print (new.row)
#   
#   # increments the row number
#   fixation.counter <- fixation.counter + 1
#   
# } # end of for all the data
# 
# 
# ## Transforming the columns into numerical values
# raw.df <- raw.df %>%
#   mutate(Participant = as.numeric(Participant)) %>%
#   mutate(QN = as.numeric(QN)) %>%
#   mutate(IDAOI = as.factor(IDAOI)) %>%
#   mutate(FN = as.numeric(FN)) %>%
#   mutate(Duration = as.numeric(Duration))

  
  