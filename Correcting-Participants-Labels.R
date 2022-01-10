# Reads the computed data for the 17 participants, put them together in a single data frame, and joins the data
# with the Java interface data
# Corrects some of the mapping from data in eye-tracker to data in the Java interface

library(ggplot2)
library(tidyverse)
library(hrbrthemes)


####################################################################################################


# Reads the participants data
p02 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP02/P02.csv", header=TRUE)
attach(p02)
p03 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP03/P03.csv", header=TRUE)
attach(p03)
p04 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP04/P04.csv", header=TRUE)
attach(p04)
p05 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP05/P05.csv", header=TRUE)
attach(p05)
p06 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP06/P06.csv", header=TRUE)
attach(p06)
p07 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP07/P07.csv", header=TRUE)
attach(p07)
p08 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP08/P08.csv", header=TRUE)
attach(p08)
p09 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP09/P09.csv", header=TRUE)
attach(p09)
p10 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP10/P10.csv", header=TRUE)
attach(p10)
p11 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP11/P11.csv", header=TRUE)
attach(p11)
p12 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP12/P12.csv", header=TRUE)
attach(p12)
p13 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP13/P13.csv", header=TRUE)
attach(p13)
p14 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP14/P14.csv", header=TRUE)
attach(p14)
p15 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP15/P15.csv", header=TRUE)
attach(p15)
p16 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP16/P16.csv", header=TRUE)
attach(p16)
p17 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP17/P17.csv", header=TRUE)
attach(p17)
p18 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP18/P18.csv", header=TRUE)
attach(p18)



# Checking consistency in the data, if there is any NA in the participant data. So far only P05 has NA, 
# p02, p03, p04, p06, p07, p09, p10, p11, p12, and p13 (with mislabelled participant name) do not have NA
# p05 %>% filter_at(any_vars(is.na()))

# renaming participant 5
newp06 <- p05
newp06$ParticipantID <- rep (6,24)

# renaming participant 6
newp05 <- p06
newp05$ParticipantID <- rep (5,24)

# renaming participant 09
newp10 <-p09
newp10$ParticipantID <- rep (10,24)

# renaming participant 10
newp11 <- p10
newp11$ParticipantID <- rep(11,24)

# renaming participant 11
newp09 <- p11
newp09$ParticipantID <- rep(9,24)


# renaming participant 12
newp13 <- p12
newp13$ParticipantID <- rep(13,24)

# renaming participant 13
newp12 <- p13
newp12$ParticipantID <- rep(12,24)


# Collects only the analyzed data
partialEyeTrackerData <- rbind(p02,p03,p04,newp05,newp06,p07,newp09,newp10,newp11,newp12,newp13)

allInterfaceData <- read.csv(file = "../../Experiment-Data/All-Participants-Curated-Data.csv", header=TRUE)
attach (allInterfaceData)

## Performs the join of the table based on the ParticipantID and QNumber
joinedData <- full_join(allInterfaceData,partialEyeTrackerData)


# testing computing the total mutations
# checkFixations must be > 0 
# checkFMFixations, positive value means no overlapping hits, negative means overlappint hits or less FM hits
# checkpercFixations must be close to 1
testingData <- joinedData %>%
  mutate(checkFMFixations = fixations.FM - fixations.Containing - fixations.Navigating,
        partsFixations = fixations.Answer + fixations.Question + fixations.Legend + fixations.Buttons + fixations.FM + fixations.CTC,
        fixationsCheck = totalFixations - partsFixations,
        fixationsTimeCheck = totalFixationTime - (time.Answer + time.Question + time.Legend + time.Buttons + time.FM + time.CTC),
        timeJavaTobiiCheck = ElapsedTime - totalFixationTime,
        checkpercFixations = perc.fixations.Answer + perc.fixations.Question + perc.fixations.Legend + perc.fixations.Buttons + perc.fixations.FM + perc.fixations.CTC,
        checkpercFixationTime = perc.time.Answer + perc.time.Question + perc.time.Legend + perc.time.Buttons + perc.time.FM + perc.time.CTC
        ) %>% 
  # filter (timeCheck < 0) %>% 
  select(ParticipantID, QNumber, ElapsedTime, totalFixationTime, timeJavaTobiiCheck, fixationsTimeCheck, totalFixations,partsFixations, fixationsCheck, fixations.FM, fixations.Containing, fixations.Navigating, checkFMFixations, checkpercFixations, checkpercFixationTime)
#in total percent


#curatedData <- joinedData %>% na.omit()

write.csv(testingData,file = "../../Experiment-Data/Eye-tracking-data-samples/FixationTimesProblems.csv", row.names = FALSE)


###########################################################################################
###########################################################################################
###########################################################################################

# Joins the collected data from the Java interface



# Puts alls the data of the eye tracker into a single data frame
#allEyeTrackerData <- rbind(p02,p03,p04,p05,p06,p07,p08,p09,p10,p11,p12,p13,p14,p15,p16,p17,p18)

# Stores all the eye-tracker data into a file
write.csv(allEyeTrackerData,file = "../../Experiment-Data/Eye-tracking-data-samples/EyeTrackerData.csv", row.names = FALSE)

# Reads all the data from the Java interface
allInterfaceData <- read.csv(file = "../../Experiment-Data/All-Participants-Curated-Data.csv", header=TRUE)
attach (allInterfaceData)

## Performs the join of the table based on the ParticipantID and QNumber
joinedData <- full_join(allInterfaceData,allEyeTrackerData)


# Number of CTC for each of the 24 questions
numCTCQuestion <- c(0,0,2,2,6,6,0,0,1,1,7,4,0,0,1,2,6,4,0,0,2,1,5,6)

# Replicating the number of CTC for each of the 17 participants
numCTC <- rep (numCTCQuestion, each=17)

# Adding the column to the frame
joinedData$numCTC <- numCTC

# Puts the column of CTCs before all the columns of the eye-tracker data
joinedData <- joinedData %>% relocate(numCTC, .before = totalFixations)

# Writes the data farme
write.csv(joinedData,file = "../../Experiment-Data/Eye-tracking-data-samples/ExperimentCompleteDataSet.csv", row.names = FALSE)