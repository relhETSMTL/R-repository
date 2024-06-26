# Transforms the data into GSEQ format for sequence analysis

library(tidyverse)

# Sets the dir to the current place
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

# Loads the entire set of curated transitions
all.participants.fixations <- read.csv(file = "../../../Experiment-Data/All-Participants-Transitions-Curated-Data.csv", 
                                   header=TRUE)
attach (all.participants.fixations)

# Read the merged data of transitions and information from the questions, NoF, NoC, number of features and type of 
# configuration
all.participants.collated <- 
  read.csv(file = "../../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/All-Participants-Transitions-Collated-Data.csv", 
           header=TRUE)
attach (all.participants.collated)


  
# Defines the output configuration file
file.path <- 'config.txt'

# writes the header
writeLines("Here goes the header", file.path)

# for all the participants and for all the questions create their characteristics descriptor and 
# its sequence of aois

# Expected format for the experiment characteristics
# Event % Data type declaration
# ($AOI = Answer Buttons CTC FM Legend Question Window) % Code declaration
# % Variable declaration
# *ParticipantID  (2 3 4 5 6 7 8 9 10 11 12 13 14 16 17 18 19)
# QNumber  (Q1 Q2 Q3 Q4 Q5 Q6 Q7 Q8 Q9 Q10 Q11 Q12 Q13 Q14 Q15 Q16 Q17 Q18 Q19 Q20 Q21 Q22 Q23 Q24 )
# NoF (1 2 3 4 )
# NoC (1 2 3 )
# Correct (True False) 
# FeaturesInQuestion (3 5 6 7 8 10 )
# ConfigurationType (Partial Full );

writeLines("Event % Data type declaration", file.path)
write("($AOI = Answer Buttons CTC FM Legend Question Window) % Code declaration", file.path, append=TRUE) 
write("% Variable declaration", file.path, append=TRUE) 
write("*ParticipantID  (2 3 4 5 6 7 8 9 10 11 12 13 14 16 17 18 19)", file.path, append=TRUE) 
write("QNumber  (Q1 Q2 Q3 Q4 Q5 Q6 Q7 Q8 Q9 Q10 Q11 Q12 Q13 Q14 Q15 Q16 Q17 Q18 Q19 Q20 Q21 Q22 Q23 Q24 )", file.path, append=TRUE) 
write("NoF (1 2 3 4 )", file.path, append=TRUE) 
write("NoC (1 2 3 )", file.path, append=TRUE) 
write("Correct (True False) ", file.path, append=TRUE) 
write("FeaturesInQuestion (3 5 6 7 8 10 )", file.path, append=TRUE) 
write("ConfigurationType (Partial Full );", file.path, append=TRUE) 

# Expected format for a question and participant

# <P2Q1> (2, Q1, 1, 1,True, 3 ,  Full)
# Buttons Legend Question Question Question Question Question Question Question Question Question Question 
#Question Question Question Question Question Question Question FM FM FM FM FM FM FM FM FM FM FM FM FM FM 
# FM FM FM FM Question Question Question Question Question Window FM FM FM FM FM FM FM FM Question Question 
# Question FM FM FM FM FM FM FM FM Legend Legend Window FM FM FM FM FM Window FM FM FM FM Legend FM Legend 
# Question Answer Question Question Question Window Answer Window FM FM FM FM FM FM FM FM Question Question 
# Answer Buttons/
participants.ids <-unique(all.participants.fixations$Participant) 
question.nums <-  unique(all.participants.fixations$QN)

## For all the participants
for (participant.id in participants.ids) {
  
  for (question.num in question.nums) {
    cat("[",participant.id,"-",question.num,"]") 
    
    # contains the fixations data for a participant and a question 
    participant.question.data <- all.participants.fixations %>% 
      filter(Participant==participant.id &	QN==question.num)
    
    # computes the string with the aois in the sequence
    aois.sequence <- paste(paste(participant.question.data$IDAOI, collapse = " "),"/",sep="")
    
    # obtains the information of the question in place for the given participant
    response.data <- all.participants.collated %>% filter(ParticipantID==participant.id & QNumber==question.num)
    
    # creates the string describing the characteristics of the sequences in terms of its predictors
    # the format is <P2Q1> (2, Q1, 1, 1,True, 3 ,  Full)
    QN <- paste("Q",question.num,sep="")
    NoF <-  response.data[1,"NoF"]
    NoC <-  response.data[1,"NoC"]
    Correct <- response.data[1,"Correct"]
    FeaturesInQuestion <- response.data[1,"FeaturesInQuestion"]
    ConfigurationType <- response.data[1,"ConfigurationType"]
    participant.sequence.descriptor <- 
      toString(c(participant.id,QN,NoF,NoC,Correct,FeaturesInQuestion,ConfigurationType))
    header <- paste("<P",participant.id,"Q",question.num,"> ",sep="")# <P2Q1>_ with extra white space at the end _
    sequence.header.descriptor <- paste(header,"(",participant.sequence.descriptor,")",sep="")
    
    # writes in the file the sequence descriptor
    write(sequence.header.descriptor, file.path, append=TRUE) 
  
    # write in the file the aois sequence
    write(aois.sequence, file.path, append=TRUE) 
    
  } # of all questions
  
} # of all participants

    


############################################
############################################
# 
# # Testing searching the data for a participant and a question
# 
# 
# # contains the data for a participant and a question 
# participant.question.data <- all.participants.trans %>% filter(Participant==2 &	QN==1)
# 
# # computes the string with the aois in the sequence
# aois.sequence <- paste(paste(participant.question.data$IDAOI, collapse = " "),"/",sep="")
# 
# # obtains the information of the question in place for the given participant
# response.data <- all.participants.collated %>% filter(ParticipantID==2 & QNumber==1)
# 
# 
# 
# # creates the string describing the characteristics of the sequences in terms of its predictors
# # the format is <P2Q1> (2, Q1, 1, 1,True, 3 ,  Full)
# participant.sequence.descriptor <- toString(c(2,"Q1",1,1,"True",3,"Partial"))
# sequence.header.descriptor <- paste("<P2Q1> (",participant.sequence.descriptor,")",sep="")
# 
# outFile <- file("config.txt")
# writeLines(sequence.header.descriptor, outFile)
# writeLines(aois.sequence, outFile)
# close(outFile)
# file.show("config.txt")
# 
# 
# 
# file.path <- 'config.txt'
# writeLines(sequence.header.descriptor, file.path)
# write(aois.sequence, file.path, append=TRUE) 
