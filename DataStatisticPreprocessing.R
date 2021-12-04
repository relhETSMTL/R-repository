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

# Adds dummy values for NoC and NOF columns, nd combination number
participantData$NoF <- rep(1,24)
participantData$NoC <- rep(1,24)
participantData$CombNum <- rep(1,24) 

# Readjusts the positions of the columns for readibilty
participantData <- participantData %>% relocate(NoF, .before = Correct)
participantData <- participantData %>% relocate(NoC, .before = Correct)
participantData <- participantData %>% relocate(ParticipantID, .before = Correct)
participantData <- participantData %>% relocate(CombNum, .before = ParticipantID)

# Traverses the 24 questions of the data frame adjusting the  NOF, NoC, and CombNum
for (question in 1:24) {
 
  # Question 1
  if(participantData[question,]$QuestionNumer==1) {
    participantData[question,]$NoF <- 1
    participantData[question,]$NoC <- 1
    participantData[question,]$CombNum <- 1
  } 
  
  
  # Question 2
  if(participantData[question,]$QuestionNumer==2) {
    participantData[question,]$NoF <- 1
    participantData[question,]$NoC <- 1
    participantData[question,]$CombNum <- 2
  } 
  
  # Question 3
  if(participantData[question,]$QuestionNumer==3) {
    participantData[question,]$NoF <- 1
    participantData[question,]$NoC <- 2
    participantData[question,]$CombNum <- 1
  } 
  
  # Question 4
  if(participantData[question,]$QuestionNumer==4) {
    participantData[question,]$NoF <- 1
    participantData[question,]$NoC <- 2
    participantData[question,]$CombNum <- 2
  } 
  
  # Question 5
  if(participantData[question,]$QuestionNumer==5) {
    participantData[question,]$NoF <- 1
    participantData[question,]$NoC <- 3
    participantData[question,]$CombNum <- 1
  } 
  
  # Question 6
  if(participantData[question,]$QuestionNumer==6) {
    participantData[question,]$NoF <- 1
    participantData[question,]$NoC <- 3
    participantData[question,]$CombNum <- 2
  } 
  
  ###############
  
  # Question 7
  if(participantData[question,]$QuestionNumer==7) {
    participantData[question,]$NoF <- 2
    participantData[question,]$NoC <- 1
    participantData[question,]$CombNum <- 1
  } 
  
  
  # Question 8
  if(participantData[question,]$QuestionNumer==8) {
    participantData[question,]$NoF <- 2
    participantData[question,]$NoC <- 1
    participantData[question,]$CombNum <- 2
  } 
  
  # Question 9
  if(participantData[question,]$QuestionNumer==9) {
    participantData[question,]$NoF <- 2
    participantData[question,]$NoC <- 2
    participantData[question,]$CombNum <- 1
  } 
  
  # Question 10
  if(participantData[question,]$QuestionNumer==10) {
    participantData[question,]$NoF <- 2
    participantData[question,]$NoC <- 2
    participantData[question,]$CombNum <- 2
  } 
  
  # Question 11
  if(participantData[question,]$QuestionNumer==11) {
    participantData[question,]$NoF <- 2
    participantData[question,]$NoC <- 3
    participantData[question,]$CombNum <- 1
  } 
  
  # Question 12
  if(participantData[question,]$QuestionNumer==12) {
    participantData[question,]$NoF <- 2
    participantData[question,]$NoC <- 3
    participantData[question,]$CombNum <- 2
  } 
  
  ##############
  
  # Question 13
  if(participantData[question,]$QuestionNumer==13) {
    participantData[question,]$NoF <- 3
    participantData[question,]$NoC <- 1
    participantData[question,]$CombNum <- 1
  } 
  
  # Question 14
  if(participantData[question,]$QuestionNumer==14) {
    participantData[question,]$NoF <- 3
    participantData[question,]$NoC <- 1
    participantData[question,]$CombNum <- 2
  } 
  
  # Question 15
  if(participantData[question,]$QuestionNumer==15) {
    participantData[question,]$NoF <- 3
    participantData[question,]$NoC <- 2
    participantData[question,]$CombNum <- 1
  } 
  
  # Question 16
  if(participantData[question,]$QuestionNumer==16) {
    participantData[question,]$NoF <- 3
    participantData[question,]$NoC <- 2
    participantData[question,]$CombNum <- 2
  } 
  
  # Question 17
  if(participantData[question,]$QuestionNumer==17) {
    participantData[question,]$NoF <- 3
    participantData[question,]$NoC <- 3
    participantData[question,]$CombNum <- 1
  } 
  
  # Question 18
  if(participantData[question,]$QuestionNumer==18) {
    participantData[question,]$NoF <- 3
    participantData[question,]$NoC <- 3
    participantData[question,]$CombNum <- 2
  }
  
  ##############
  
  # Question 19
  if(participantData[question,]$QuestionNumer==19) {
    participantData[question,]$NoF <- 4
    participantData[question,]$NoC <- 1
    participantData[question,]$CombNum <- 1
  } 
    
  # Question 20
  if(participantData[question,]$QuestionNumer==20) {
    participantData[question,]$NoF <- 4
    participantData[question,]$NoC <- 1
    participantData[question,]$CombNum <- 2
  } 
  
  # Question 21
  if(participantData[question,]$QuestionNumer==21) {
    participantData[question,]$NoF <- 4
    participantData[question,]$NoC <- 2
    participantData[question,]$CombNum <- 1
  } 
  
  # Question 22
  if(participantData[question,]$QuestionNumer==22) {
    participantData[question,]$NoF <- 4
    participantData[question,]$NoC <- 2
    participantData[question,]$CombNum <- 2
  } 
  
  # Question 23
  if(participantData[question,]$QuestionNumer==23) {
    participantData[question,]$NoF <- 4
    participantData[question,]$NoC <- 3
    participantData[question,]$CombNum <- 1
  } 
  
  # Question 24
  if(participantData[question,]$QuestionNumer==24) {
    participantData[question,]$NoF <- 4
    participantData[question,]$NoC <- 3
    participantData[question,]$CombNum <- 2
  }
  
} # end of 24 loop for questions

