# Elmira Project Extension
# Computing the frequency of transitions for all participants and all questions

library(tidyverse)
library(dplyr)

# Sets the dir to the current place
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

# Loads the entire set of curated transitions
all.participants.trans <- read.csv(file = "../../../Experiment-Data/All-Participants-Transitions-Curated-Data.csv", 
                                   header=TRUE)
attach (all.participants.trans)

# Computes a vector of participants and question numbers to iterate on
participants.ids <-unique(all.participants.trans$Participant) 
question.nums <-  unique(all.participants.trans$QN)
# factors.aois <- as.factor(levels(as.factor(all.participants.trans$IDAOI)))
factors.aois <- unique(all.participants.trans$IDAOI)

# input directory
input.dir <- "../../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/"

# number of aois
num.aois <- length(factors.aois)

# Loops for all the participants and all the questions, loading the matrix of transition frequencies
for (participant.id in participants.ids) {
  
  for (question.num in question.nums) {
    cat("[",participant.id,"-",question.num,"]") 
    
    # write the output string including the Core suffix to indicate only the core rows
    in.file.name <- paste(input.dir,"P",participant.id,"-Q",question.num,"-Transitions-Matrix.csv",sep="")
    print(in.file.name)
    
    # loads the file of a participant and a question
    matrix.participant.question  <- read.csv(file = in.file.name, header=TRUE)
    
    # adds the corresponding entries to the collected data frame
    ## double loop from 1..7 of each AOI, index, get the value and the match in the AOIs
    for (i in  seq(1:num.aois)){
      for (j in  seq(1:num.aois)){
        cat(i," ", j, " ", factors.aois[i], " ", factors.aois[j])
        print(" ")
      } # for j
    } # for j
    
        
    
  } # all the questions
  
} # all the participants