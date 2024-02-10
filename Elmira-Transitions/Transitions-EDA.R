# Transition Analysis Experiments
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(dplyr)

########################################################################################
########################################################################################

  # Function that computes the transitions between FM, Question, CTC
  compute.transitions <- function (pid, qnum, participant.question.data) {
    
    # initializes the variables for the transition counters
    fm.question <- 0 
    fm.ctc <- 0
    question.fm <- 0
    question.ctc <-0
    ctc.fm <- 0
    ctc.question <- 0
    
    # initializes the current type
    current.type <- NULL
    
    number.transitions <- nrow(participant.question.data) 
    
    # Traverses all the fixations counting the transitions of different types
    for (aoi in pq.data[,3]) {
      
      # if first one, then set the current type
      if (is.null(current.type)) {
        current.type <- aoi
        
      } else {
        if (aoi=="Question") {
        
            
        } # Question
        
        if (aoi=="FM") {
          
        } # FM
        
        if (aoi=="CTC") {
          
        } # CTC
        
    

      } # else if not null
      
      print(aoi)
      
      
    } # traverses all the fixations
    
    # return a vector with the following 
    result <- c(pid, qnum, fm.question, fm.ctc, question.fm, question.ctc, ctc.fm, ctc.question)  
    
  } # of compute transitions
  

########################################################################################
########################################################################################

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
      filter(Participant==participant.id & QN==question.num &
               (IDAOI=="Question" | IDAOI=="FM" | IDAOI=="CTC") )
    
    # This is the number of transitions per participant and question  
    cat(",",nrow(pq.data),"]")
    
     
    pq.result <- compute.transitions(participant.id,question.num,pq.data)
    
    } ## for all the questions
  
} ##  for all the participants
  
## For all the questions