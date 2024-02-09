# Transition Analysis Experiments
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(dplyr)

# Loads the entire set of curated transitions
all.participants.trans <- read.csv(file = "../../Experiment-Data/All-Participants-Transitions-Curated-Data.csv", 
                              header=TRUE)
attach (all.participants.trans)

# Making the IDAOIs factors instead of characters
all.participants.trans <- all.participants.trans %>%
  mutate(IDAOI=as.factor(IDAOI))

# Computes a vector of participants and question numbers to iterate on
participants.ids <-unique(all.participants.trans$Participant) 
question.nums <-  unique(all.participants.trans$QN)

## For all the participants
for (participant.id in participants.ids) {
  
  for (question.num in question.nums) {
    cat("[",participant.id,"-",question.num) 
    # print('done')
 
    ## Calls the procedure that filters the transition information for the participant and the question
    # Parameters: participant.id, question.num, transitions for that participant and question.num
    
    # Transition data for the participant and given question
    pq.data <- all.participants.trans %>%
      filter(Participant==participant.id & QN==question.num)
    
    # This is the number of transitions per participant and question  
    cat(",",nrow(pq.data),"]")
    
     
    } ## for all the questions
  
} ##  for all the participants
  
## For all the questions