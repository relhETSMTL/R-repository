# Transition Analysis Experiments
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(dplyr)

########################################################################################
########################################################################################

  # Function that computes the transitions between FM, Question, CTC
  compute.transitions <- function (pid, qnum, participant.question.data, aois.factors, output.dir) {
    
    # initializes the variables for the transition counters
    counter <- 0 
    
    # initializes the current type
    current.type <- participant.question.data$IDAOI[1]  # The first entry in the transition matrix, AOI of the first transition
    index.from <- match(current.type, aois.factors)
    
    number.transitions <- nrow(participant.question.data) 
    
    # initializes the matrices of transitions
    trans.matrix <- matrix(rep(0,49), nrow=7, ncol = 7)
    
    # traverses all the fixations counting the transitions of different types
    for (aoi in participant.question.data[,3]) {
    
      # computes the index the to AOI
      index.to <- match(aoi, aois.factors)
      
      # increments the entry in the matrix by one
      trans.matrix[index.from, index.to] <- trans.matrix[index.from, index.to] + 1 
      
      # changes the index from to the current transition
      index.from <- index.to
    
      counter <- counter + 1   
      
      # print(aoi)
      # print(counter)
      
      
    } # traverses all the fixations
    
    # return a vector with the following 
    # result <- c(pid, qnum, fm.question, fm.ctc, question.fm, question.ctc, ctc.fm, ctc.question)  
    
    # changes the names of the rows and columns to the AOI names, e.g trans.matrix["Answer","CTC"]
    colnames(trans.matrix) <- aois.factors
    rownames(trans.matrix) <- aois.factors
    
    # write the output string
    out.file.name <- paste(output.dir,"P",pid,"-Q",qnum,"-Transitions-Matrix.csv",sep="")
    print(out.file.name)
    write.csv(trans.matrix, file = out.file.name, row.names = TRUE)
    
    return (trans.matrix)
    
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


# Testing a matrix transition call 
# pq.data.p2.q1 <- all.participants.trans %>% filter(Participant==2 & QN==1)
# factors.aois <- as.factor(levels(all.participants.trans$IDAOI))
mt.p2.q1 <- compute.transitions(2,1,pq.data.p2.q1,factors.aois, "../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/")
# colnames(mt.p2.q1) <- factors.aois
# rownames(mt.p2.q1) <- factors.aois
# cat ("../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/","P",2,"-Q",1,"-Transitions-Matrix.csv")
# paste("../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/","P",2,"-Q",1,"-Transitions-Matrix.csv",sep="")
# write.csv(mt.p2.q1, file = "../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/P02-Q1-Transitions-Matrix.csv", 
#           row.names = TRUE)


# Computes the factors of the AOIs names
factors.aois <- as.factor(levels(all.participants.trans$IDAOI))

## For all the participants
for (participant.id in participants.ids) {
  
  for (question.num in question.nums) {
    cat("[",participant.id,"-",question.num) 
    # print('done')
 
    ## Calls the procedure that filters the transition information for the participant and the question
    # Parameters: participant.id, question.num, transitions for that participant and question.num
    
    # Transition data for the participant and given question
    pq.data <- all.participants.trans %>%
      filter(Participant==participant.id & QN==question.num)  # & (IDAOI=="Question" | IDAOI=="FM" | IDAOI=="CTC") )
    
    # This is the number of transitions per participant and question  
    cat(",",nrow(pq.data),"]")
    
     
    pq.result <- compute.transitions(participant.id,question.num,pq.data,
                                     factors.aois,
                                     "../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/")
    
    } ## for all the questions
  
} ##  for all the participants
  
## For all the questions



# Some checks
pq.data.p19.q24 <- all.participants.trans %>% filter(Participant==19 & QN==24)
mt.p19.q24 <- compute.transitions(19,24,pq.data.p19.q24,factors.aois, "../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/")
