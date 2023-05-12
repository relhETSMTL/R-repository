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

##
fm01.pairs <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM01_2.csv", header=TRUE)
attach(fm01.pairs)
num.fm01.pairs <- nrow(fm01.pairs)
array.fm01.pairs <- ncol(fm01.pairs)-2


fm01.triplets <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM01_3.csv", header=TRUE)
attach(fm01.triplets)
num.fm01.triplets <- nrow(fm01.triplets)
array.fm01.triplets <- ncol(fm01.triplets)-3



##
fm02.pairs <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM02_2.csv", header=TRUE)
attach(fm02.pairs)
num.fm02.pairs <- nrow(fm02.pairs)
array.fm02.pairs <- ncol(fm02.pairs)-2



fm02.triplets <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM02_3.csv", header=TRUE)
attach(fm02.triplets)
num.fm02.triplets <- nrow(fm02.triplets)
array.fm02.triplets <- ncol(fm02.triplets)-3


##
fm03.pairs <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM03_2.csv", header=TRUE)
attach(fm03.pairs)
num.fm03.pairs <- nrow(fm03.pairs)
array.fm03.pairs <- ncol(fm03.pairs)-2

fm03.triplets <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM03_3.csv", header=TRUE)
attach(fm03.triplets)
num.fm03.triplets <- nrow(fm03.triplets)
array.fm03.triplets <- ncol(fm03.triplets)-3


##
fm04.pairs <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM04_2.csv", header=TRUE)
attach(fm04.pairs)
num.fm04.pairs <- nrow(fm04.pairs)
array.fm04.pairs <- ncol(fm04.pairs)-2

fm04.triplets <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM04_3.csv", header=TRUE)
attach(fm04.triplets)
num.fm04.triplets <- nrow(fm04.triplets)
array.fm04.triplets <- ncol(fm04.triplets)-3


##
fm05.pairs <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM05_2.csv", header=TRUE)
attach(fm05.pairs)
num.fm05.pairs <- nrow(fm05.pairs)
array.fm05.pairs <- ncol(fm05.pairs)-2

fm05.triplets <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM05_3.csv", header=TRUE)
attach(fm05.triplets)
num.fm05.triplets <- nrow(fm05.triplets)
array.fm05.triplets <- ncol(fm05.triplets)-3


##
fm06.pairs <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM06_2.csv", header=TRUE)
attach(fm06.pairs)
num.fm06.pairs <- nrow(fm06.pairs)
array.fm06.pairs <- ncol(fm06.pairs)-2

fm06.triplets <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM06_3.csv", header=TRUE)
attach(fm06.triplets)
num.fm06.triplets <- nrow(fm06.triplets)
array.fm06.triplets <- ncol(fm06.triplets)-3


##
fm07.pairs <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM07_2.csv", header=TRUE)
attach(fm07.pairs)
num.fm07.pairs <- nrow(fm07.pairs)
array.fm07.pairs <- ncol(fm07.pairs)-2


fm07.triplets <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM07_3.csv", header=TRUE)
attach(fm07.triplets)
num.fm07.triplets <- nrow(fm07.triplets)
array.fm07.triplets <- ncol(fm07.triplets)-3


##
fm08.pairs <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM08_2.csv", header=TRUE)
attach(fm08.pairs)
num.fm08.pairs <- nrow(fm08.pairs)
array.fm08.pairs <- ncol(fm08.pairs)-2

fm08.triplets <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM08_3.csv", header=TRUE)
attach(fm08.triplets)
num.fm08.triplets <- nrow(fm08.triplets)
array.fm08.triplets <- ncol(fm08.triplets)-3


##
fm09.pairs <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM09_2.csv", header=TRUE)
attach(fm09.pairs)
num.fm09.pairs <- nrow(fm09.pairs)
array.fm09.pairs <- ncol(fm09.pairs)-2


fm09.triplets <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM09_3.csv", header=TRUE)
attach(fm09.triplets)
num.fm09.triplets <- nrow(fm09.triplets)
array.fm09.triplets <- ncol(fm09.triplets)-3


##
fm10.pairs <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM10_2.csv", header=TRUE)
attach(fm10.pairs)
num.fm10.pairs <- nrow(fm10.pairs)
array.fm10.pairs <- ncol(fm10.pairs)-2


fm10.triplets <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM10_3.csv", header=TRUE)
attach(fm10.triplets)
num.fm10.triplets <- nrow(fm10.triplets)
array.fm10.triplets <- ncol(fm10.triplets)-3


##
fm11.pairs <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM11_2.csv", header=TRUE)
attach(fm11.pairs)
num.fm11.pairs <- nrow(fm11.pairs)
array.fm11.pairs <- ncol(fm11.pairs)-2


fm11.triplets <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/SolutionSetsRenamed/FM11_3.csv", header=TRUE)
attach(fm11.triplets)
num.fm11.triplets <- nrow(fm11.triplets)
array.fm11.triplets <- ncol(fm11.triplets)-3


# Creating the vectors for the feature models CVS files
feature.model <-c("FM01","FM02","FM03","FM04","FM05","FM06","FM07","FM08","FM09","FM10","FM11")
number.pairs <-c(num.fm01.pairs,num.fm02.pairs,num.fm03.pairs,num.fm04.pairs,num.fm05.pairs,num.fm06.pairs,
                 num.fm07.pairs,num.fm08.pairs,num.fm09.pairs,num.fm10.pairs,num.fm11.pairs)
number.triplets <- c(num.fm01.triplets,num.fm02.triplets,num.fm03.triplets,num.fm04.triplets,num.fm05.triplets,num.fm06.triplets,
                     num.fm07.triplets,num.fm08.triplets,num.fm09.triplets,num.fm10.triplets,num.fm11.triplets)
array.pairs.size <-c(array.fm01.pairs,array.fm02.pairs,array.fm03.pairs,array.fm04.pairs,array.fm05.pairs,array.fm06.pairs,
                     array.fm07.pairs,array.fm08.pairs,array.fm09.pairs,array.fm10.pairs,array.fm11.pairs)
array.triplets.size <-c(array.fm01.triplets,array.fm02.triplets,array.fm03.triplets,array.fm04.triplets,array.fm05.triplets,array.fm06.triplets,
                        array.fm07.triplets,array.fm08.triplets,array.fm09.triplets,array.fm10.triplets,array.fm11.triplets)


# Creates the data frame to contain the information of the feature models of the experiment
fm.dataframe <- data.frame(feature.model,number.pairs,number.triplets, array.pairs.size,array.triplets.size)

# Saves the dataframe into a file
write.csv(fm.dataframe,file = "../../../Eye-Tracking-Visualization/Experiment-Data/FeatureModelsData.csv", row.names = FALSE)


####################################
## Creation of the file Questions-MV-FM-Pairs

ActualQuestionNumber <-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
T <- c(2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3)
Visualization.Method <-c("2D-PD","2D-PD","2D-PD","2D-PD","2D-SP","2D-SP","2D-SP","2D-SP",
                         "3D-PD","3D-PD","3D-PD","3D-PD","3D-SP","3D-SP","3D-SP","3D-SP")
feature.model <-c("FM02","FM05","FM08","FM07","FM09","FM10","FM08","FM04",
                  "FM01","FM09","FM06","FM11","FM09","FM05","FM06","FM07")

NumberElements <- c(num.fm02.pairs,num.fm05.pairs,num.fm08.pairs,num.fm07.pairs, # Fm02Par2, Fm05Par2, Fm08Par2, Fm07Par2
                    num.fm09.pairs,num.fm10.pairs,num.fm08.pairs,num.fm04.pairs, # Fm09Scat2D, Fm10Scat2D, Fm08Scat2D, Fm04Scat2D
                    num.fm01.triplets, num.fm09.triplets,num.fm06.triplets,num.fm11.triplets, # Fm01Par3, Fm09Par3, Fm06Par3, Fm11Par3
                    num.fm09.triplets, num.fm05.triplets,num.fm06.triplets,num.fm07.triplets) # Fm09Scat3D, Fm05Scat3D, Fm06Scat3D, Fm07Scat3D

map.question.vm.fm <- data.frame(ActualQuestionNumber,T,Visualization.Method,feature.model,NumberElements)

# Saves the dataframe into a file
write.csv(map.question.vm.fm,file = "../../../Eye-Tracking-Visualization/Experiment-Data/MappingQuestions-T-VM-Size.csv", row.names = FALSE)


#####################################
## Performs the join between the mapped questions to VM-T-FM and the participants data
## The join is on the column ActualQuestionNumber

joinedData <- full_join(participants.by.actualquestion,map.question.vm.fm)

# Saves the joined dataframe into a file
write.csv(joinedData,file = "../../../Eye-Tracking-Visualization/Experiment-Data/CompleteParticipantsResponseData.csv", row.names = FALSE)


#####################################
# TODO
# Create the separated files for t=2 and t=3
# Create updated variable dictionary file
# Upload the files in the TransferETS repository


