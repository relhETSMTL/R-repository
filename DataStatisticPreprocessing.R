# Objective: Reformatting the Feature Model data captured with the experiment interface 
# Project: Eye-Tracking Analysis of Feature Models Comprehension
# Ecole de technologie superieure
# VERITAS team
# Authors: Elmira Sepasi, Kambiz Belouchi, Roberto E. Lopez-Herrejon
# Last update: 2021-12-03

library(ggplot2)
library(tidyverse)
library(hrbrthemes)

# File reads the experiment data 
participantData <- read.csv(file = "../../Experiment-Data/Participant-2.csv", header=TRUE)
attach (participantData)

# Filters the Question number, Correct, and Elapsed Time
participantData <- participantData %>% select(QuestionNumer, Correct, ElapsedTime)

# Removes the first two rows
participantData <- participantData %>% filter(QuestionNumer > 0) 

# Adds the column of the participant ID 24 times, one for each question
participantData$ParticipantID <- rep(2,24)

# Adds dummy values for NoC and NOF columns
participantData$NoF <- rep(1,24)
participantData$NoC <- rep(1,24)

# Readjusts the positions
participantData <- participantData %>% relocate(NoF, .before = Correct)
participantData <- participantData %>% relocate(NoC, .before = Correct)
participantData <- participantData %>% relocate(ParticipantID, .before = Correct)

# Todo, substitute the values of NoF and NoC according to the question number

# for (question in 1:24) { print(question) }

# x <- 5
# if(x > 0){
#  print("Positive number")
# }


# For this study, we focus on two metrics: 
# NoF: Number of Features, NoC: number of cross-tree constraints
# We select the two metrics and the file name and the feature model name
# ExpData <- fmData %>% select(FileName, FeatureModel, NoF, NoC)