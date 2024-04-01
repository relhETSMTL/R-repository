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

# input directory
input.dir <- "../../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/"

# Loops for all the participants and all the questions, loading the matrix of transition frequencies
for (participant.id in participants.ids) {
  
  for (question.num in question.nums) {
    cat("[",participant.id,"-",question.num,"]") 
    
    # write the output string including the Core suffix to indicate only the core rows
    out.file.name <- paste(input.dir,"P",participant.id,"-Q",question.num,"-Transitions-Matrix.csv",sep="")
    print(out.file.name)
    
  } # all the questions
  
} # all the participants