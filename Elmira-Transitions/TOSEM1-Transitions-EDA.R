# TOSEM 24 paper
# Transitions EDA, file computations and plotting

library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(dplyr)

# Sets the dir to the current place
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

################################################################################

################
## Auxiliary functions for string processing names

# If participant number is less than 10, add a leading zero to the string
leadingZeros <- function (participantNumber) {
  
  if (participantNumber<10) paste0("0",9)
  else
    as.character(participantNumber)
} # end of leadingZeros


#########################
## Function that computes the rectangle coordinates for the transitions scarf plots
# Returns a data frame with the information of the curated transition plus min, max for X and Y for drawing rectangle
# plots either for participants or questions
computeRectangleCoordinates <- function (raw.df, perParticipant) {

  # Creates a copy of the raw.df for an individual, adds the columns for the rectangle coordinates
  raw.rectangle.df <- raw.df
  raw.rectangle.df$Xmin <- rep(0,nrow(raw.rectangle.df))
  raw.rectangle.df$Xmax <- rep(0,nrow(raw.rectangle.df))
  raw.rectangle.df$Ymin <- rep(0,nrow(raw.rectangle.df))
  raw.rectangle.df$Ymax <- rep(0,nrow(raw.rectangle.df))

  # All the questions for each participant  
  if (perParticipant==TRUE) {

    # Assumption that all the fixations of a question and participant are sequentially put together
    # Set current row to be first question for the participant
    current.qn <- 1 # raw.rectangle.df$QN[1]
    
    # # Initializes the counters for the rectangle x, y coordinates
    x.accum <- 0
    y.accum <- 1
    
    # Loop for all the data of the participant
    for (i in 1:nrow(raw.rectangle.df)) {
      row.data <- raw.rectangle.df [i,]
      
      # Checks if it has reached a new question
      # If a new question number is found, set current question and reset fixation counter to 1
      if (row.data$QN != current.qn) {
        current.qn <- row.data$QN           # sets the new current row for comparison
        x.accum <- 0                        # resets x coordinate to zero
        y.accum <- y.accum + 1              # increments to the next question
      } #
      
      # Assigns coordinates to the new rectangle 
      raw.rectangle.df[i,]$Xmin <- x.accum                  # starting x accumulated
      x.accum <- x.accum + raw.rectangle.df[i,]$Duration    # increments x coordinate by width of duration
      raw.rectangle.df[i,]$Xmax <- x.accum                  # maximum is the new accumulated
      raw.rectangle.df[i,]$Ymin <- y.accum                  # starts from the current question
      raw.rectangle.df[i,]$Ymax <- y.accum + 1              # sets the maximum, one point higher
      
      print (c(i, 
               raw.rectangle.df[i,]$Xmin, raw.rectangle.df[i,]$Xmax, 
               raw.rectangle.df[i,]$Ymin, raw.rectangle.df[i,]$Ymax))
      
    } # end of for all the data
    
    
    
  } else { # computes the duration for the scarf plot per question
    
  }  # of the computation per question
  
  # returns the data frame with the computed rectangle plot coordinates
  return (raw.rectangle.df)
  
} # end of computeRectangleCoordinates

# # Creates a copy of the raw.df for an individual, adds the columns for the rectangle coordinates
# raw.rectangle.df <- raw.df
# raw.rectangle.df$Xmin <- rep(0,nrow(raw.rectangle.df))
# raw.rectangle.df$Xmax <- rep(0,nrow(raw.rectangle.df))
# raw.rectangle.df$Ymin <- rep(0,nrow(raw.rectangle.df))
# raw.rectangle.df$Ymax <- rep(0,nrow(raw.rectangle.df))

# # Assumption that all the fixations of a question and participant are sequentially put together
# # Set current row to be the first question in the data frame
# current.qn <- raw.rectangle.df$QN[1]

# # Initializes the counters for the rectangle coordinates
# x.accum <- 0
# y.accum <- 1
# 
# # Loop for all the data of the participant
# for (i in 1:nrow(raw.rectangle.df)) {
#   row.data <- raw.rectangle.df [i,]
#   
#   # Checks if it has reached a new question
#   # If a new question number is found, set current question and reset fixation counter to 1
#   if (row.data$QN != current.qn) {
#     current.qn <- row.data$QN           # sets the new current row for comparison
#     x.accum <- 0                        # resets x coordinate to zero
#     y.accum <- y.accum + 1              # increments to the next question
#   } #
#   
#   # Assigns coordinates to the new rectangle 
#   raw.rectangle.df[i,]$Xmin <- x.accum                  # starting x accumulated
#   x.accum <- x.accum + raw.rectangle.df[i,]$Duration    # increments x coordinate by width of duration
#   raw.rectangle.df[i,]$Xmax <- x.accum                  # maximum is the new accumulated
#   raw.rectangle.df[i,]$Ymin <- y.accum                  # starts from the current question
#   raw.rectangle.df[i,]$Ymax <- y.accum + 1              # sets the maximum, one point higher
#   
#   print (c(i, 
#            raw.rectangle.df[i,]$Xmin, raw.rectangle.df[i,]$Xmax, 
#            raw.rectangle.df[i,]$Ymin, raw.rectangle.df[i,]$Ymax))
#   
# } # end of for all the data




#####################
# Loads the entire set of curated transitions
all.participants.trans <- read.csv(file = "../../../Experiment-Data/All-Participants-Transitions-Curated-Data.csv", 
                                   header=TRUE)
attach (all.participants.trans)

# Computes a vector of participants and question numbers to iterate on
participants.ids <-unique(all.participants.trans$Participant) 
question.nums <-  unique(all.participants.trans$QN)


# input directory
input.dir <- "../../../Experiment-Data/Eye-tracking-data-samples/"

# Loops for all the participants and all the questions, loading the matrix of transition frequencies
for (participant.id in participants.ids) {
  
  # # Computes the complete path name of the file to load from
  # in.file.name <- paste(input.dir,"PartP",leadingZeros(participant.id),"/P",leadingZeros(participant.id),"-Transitions-Data.csv",sep="")
  # print(in.file.name)
  # 
  # # Loads the Transitions-Data.csv file for the corresponding participant's transitions
  # participant.transitions <- read.csv(file = in.file.name, header=TRUE)
  # attach(participant.transitions)

  
    
  for (question.num in question.nums) {
    cat("[",participant.id,"-",question.num,"]\n") 
    
  } # for all the questions
} # for all the participants


################################################################################
# Function that creates a file for each question




################################################################################
# 

# for all the participants

# for all the questions
