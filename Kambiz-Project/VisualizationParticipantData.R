# Kambiz project
# Participants response data

library("tidyverse")


# Loads original participants responses
participant.responses <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/ExperimentParticipantsResponses.csv", header=TRUE)
attach(participant.responses)


# Sorts the data frame by the ActualQuestionNumber, ranging from 1..16
participants.by.actualquestion <- participant.responses %>% arrange(ActualQuestionNumber)

# Fixes the column name problem of ParticipantsQuestionpNumber to ParticipantsQuestionNumber
participants.by.actualquestion <- participants.by.actualquestion %>% rename("ParticipantsQuestionNumber" = "ParticipantsQuestiopnNumber")

