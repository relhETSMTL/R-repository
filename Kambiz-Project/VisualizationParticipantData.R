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


#######
## loads all the pairs and triples files

fm01.pairs <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM01_2.csv", header=TRUE)
attach(fm01.pairs)
num.fm01.pairs <- nrow(fm01.pairs)

fm01.triplets <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM01_3.csv", header=TRUE)
attach(fm01.triplets)
num.fm01.triplets <- nrow(fm01.triplets)


fm02.pairs <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM02_2.csv", header=TRUE)
attach(fm02.pairs)
num.fm02.pairs <- nrow(fm02.pairs)

fm02.triplets <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM02_3.csv", header=TRUE)
attach(fm02.triplets)
num.fm02.triplets <- nrow(fm02.triplets)


fm03.pairs <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM03_2.csv", header=TRUE)
attach(fm03.pairs)
num.fm03.pairs <- nrow(fm03.pairs)

fm03.triplets <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM03_3.csv", header=TRUE)
attach(fm03.triplets)
num.fm03.triplets <- nrow(fm03.triplets)


fm04.pairs <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM04_2.csv", header=TRUE)
attach(fm04.pairs)
num.fm04.pairs <- nrow(fm04.pairs)

fm04.triplets <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM04_3.csv", header=TRUE)
attach(fm04.triplets)
num.fm04.triplets <- nrow(fm04.triplets)


fm05.pairs <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM05_2.csv", header=TRUE)
attach(fm05.pairs)
num.fm05.pairs <- nrow(fm05.pairs)

fm05.triplets <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM05_3.csv", header=TRUE)
attach(fm05.triplets)
num.fm05.triplets <- nrow(fm05.triplets)


fm06.pairs <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM06_2.csv", header=TRUE)
attach(fm06.pairs)
num.fm06.pairs <- nrow(fm06.pairs)

fm06.triplets <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM06_3.csv", header=TRUE)
attach(fm06.triplets)
num.fm06.triplets <- nrow(fm06.triplets)


fm07.pairs <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM07_2.csv", header=TRUE)
attach(fm07.pairs)
num.fm07.pairs <- nrow(fm07.pairs)

fm07.triplets <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM07_3.csv", header=TRUE)
attach(fm07.triplets)
num.fm07.triplets <- nrow(fm07.triplets)


fm08.pairs <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM08_2.csv", header=TRUE)
attach(fm08.pairs)
num.fm08.pairs <- nrow(fm08.pairs)

fm08.triplets <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM08_3.csv", header=TRUE)
attach(fm08.triplets)
num.fm08.triplets <- nrow(fm08.triplets)


fm09.pairs <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM09_2.csv", header=TRUE)
attach(fm09.pairs)
num.fm09.pairs <- nrow(fm09.pairs)

fm09.triplets <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM09_3.csv", header=TRUE)
attach(fm09.triplets)
num.fm09.triplets <- nrow(fm09.triplets)


fm10.pairs <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM10_2.csv", header=TRUE)
attach(fm10.pairs)
num.fm10.pairs <- nrow(fm10.pairs)

fm10.triplets <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM10_3.csv", header=TRUE)
attach(fm10.triplets)
num.fm10.triplets <- nrow(fm10.triplets)


fm11.pairs <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM11_2.csv", header=TRUE)
attach(fm11.pairs)
num.fm11.pairs <- nrow(fm11.pairs)

fm11.triplets <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM11_3.csv", header=TRUE)
attach(fm11.triplets)
num.fm11.triplets <- nrow(fm11.triplets)








# 
# Fm02Par2
# Fm05Par2
# Fm08Par2
# Fm07Par2
# 
# Fm09Scat2D
# Fm10Scat2D
# Fm08Scat2D
# Fm04Scat2D
# 
# Fm01Par3
# Fm09Par3
# Fm06Par3
# Fm11Par3
# 
# Fm09Scat3D
# Fm05Scat3D
# Fm06Scat3D
# Fm07Scat3D

