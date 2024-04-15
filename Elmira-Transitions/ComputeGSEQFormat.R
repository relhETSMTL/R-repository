# Transforms the data into GSEQ format for sequence analysis

library(tidyverse)

# Sets the dir to the current place
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

# Loads the entire set of curated transitions
all.participants.trans <- read.csv(file = "../../../Experiment-Data/All-Participants-Transitions-Curated-Data.csv", 
                                   header=TRUE)
attach (all.participants.trans)

# Read the merged data of transitions and information from the questions, NoF, NoC, number of features and type of 
# configuration


all.participants.collated <- 
  read.csv(file = "../../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/All-Participants-Transitions-Collated-Data.csv", 
           header=TRUE)
attach (all.participants.collated)

# Expected format for a question and participant
# <P2Q1> (2, Q1, 1, 1,True, 3 ,  Full)
# Buttons Legend Question Question Question Question Question Question Question Question Question Question 
#Question Question Question Question Question Question Question FM FM FM FM FM FM FM FM FM FM FM FM FM FM 
# FM FM FM FM Question Question Question Question Question Window FM FM FM FM FM FM FM FM Question Question 
# Question FM FM FM FM FM FM FM FM Legend Legend Window FM FM FM FM FM Window FM FM FM FM Legend FM Legend 
# Question Answer Question Question Question Window Answer Window FM FM FM FM FM FM FM FM Question Question 
# Answer Buttons/
  





# Testing searching the data for a participant and a question



# contains the data for a participant and a question 
participant.question.data <- all.participants.trans %>% filter(Participant==2 &	QN==1)

# computes the string with the aois in the sequence
aois.sequence <- paste(participant.question.data$IDAOI, collapse = " ")

# obtains the information of the question in place for the given participant
response.data <- all.participants.collated %>% filter(ParticipantID==2 & QNumber==1)

# creates the string describing the characteristics of the sequences in terms of its predictors
# the format is <P2Q1> (2, Q1, 1, 1,True, 3 ,  Full)
participant.sequence.descriptor <- toString(c(2,"Q1",1,1,"True",2,3,"Partial"))
sequence.header.descriptor <- paste("<P2Q1> (",participant.sequence.descriptor,")",sep="")

