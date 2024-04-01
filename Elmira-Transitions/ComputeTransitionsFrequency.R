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
factors.aois <- as.factor(levels(as.factor(all.participants.trans$IDAOI)))
# factors.aois <- unique(all.participants.trans$IDAOI)

# input directory
input.dir <- "../../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/"

# number of aois
num.aois <- length(factors.aois)

# TODO create the data frame for the output file
collated.transitions.count.df <- data.frame(matrix(ncol = 5, nrow = 0))
column.names <- c("ParticipantID", "QNumber", "From","To","FrequencyCount")
colnames(collated.transitions.count.df) <- column.names

#  define and initialize increment variable for each new entry
row.counter <- 1


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
        cat(i," ", j, " ", as.character(factors.aois[i]), " ", as.character(factors.aois[j]), " ")
        print("--")
        
        # adds the row, one extra index column because of column X with AOIs names 
        collated.transitions.count.df[row.counter,"ParticipantID"]= participant.id
        collated.transitions.count.df[row.counter,"QNumber"]= question.num
        collated.transitions.count.df[row.counter,"From"] = as.character(factors.aois[i])
        collated.transitions.count.df[row.counter,"To"] = as.character(factors.aois[j])
        collated.transitions.count.df[row.counter,"FrequencyCount"] = matrix.participant.question[i,j+1]
    

        # increments the row counter
        row.counter <- row.counter + 1

      } # for j
    } # for j
    
  } # all the questions
  
} # all the participants


# Loads the complete data set of interface data and eye tracker data to extract the NoF, NoC, and the correct
# response that will be later used for joining the tables

interface.transition.data <- read.csv(file = "../../../Experiment-Data/All-Participants-Curated-Data-Boolean.csv", 
                                   header=TRUE)
attach (interface.transition.data)

# Selects the required columns 
base.participant.data <- interface.transition.data %>%
  select(ParticipantID, QNumber, NoF, NoC, Correct)


# Performs the join based on ParticipantID and QNumber of the transition frequency and base data
complete.joined.data <- full_join(collated.transitions.count.df,base.participant.data)

# Saves the data file of the structure
# out.file.name <- "../../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/All-Participants-Transitions-Collated-Data.csv"
# write.csv(complete.joined.data, file = out.file.name, row.names = FALSE, quote = FALSE)

##########################
##########################

# Loads the description of the questions of the experiment
questions.data <- read.csv(file = "../../../Experiment-Data/experiment-questions.csv", header=TRUE)
attach (questions.data)

questions.data <- questions.data %>% select(QN,QText,QRightAnswer)

# number of features in each question, and adds the column
num.features <- c(3,3,6,6,6,6,6,8,7,10, 6,5,6,6,6,6,7,7,7,6 ,6,6,8,6)
questions.data$Features.In.Question <- num.features

# Partial and "Full" configurations
configuration.type <- factor(c("Partial","Partial","Partial","Full","Full","Partial","Full","Partial",
                             "Full","Partial","Partial","Partial","Partial","Partial","Partial","Full",
                             "Full","Partial","Full","Partial","Partial","Partial","Partial","Partial"))
questions.data$Configuration.Type <- configuration.type


# keeps only the question number, the number of features in the question and the type of configuration
questions.data <- questions.data %>% select(QN,Features.In.Question,Configuration.Type) %>%
  rename (QNumber=QN)

# writes out the file with the information
write.csv(questions.data, file = "../../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/Raw-Questions-Info.csv", 
          row.names = FALSE, quote = FALSE)

########################
#######################

## Merge of the joined data with the question data
final.data <- full_join(complete.joined.data, questions.data)

# Saves the data file of the structure
out.file.name <- "../../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/All-Participants-Transitions-Collated-Data.csv"
write.csv(final.data, file = out.file.name, row.names = FALSE, quote = FALSE)
