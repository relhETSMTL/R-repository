# Reads the computed data for the 18 participants, put them together in a single data frame, and joins the data
# with the Java interface data

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
# participant with faulty data
# p15 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP15/P15.csv", header=TRUE)
# attach(p15)
p16 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP16/P16.csv", header=TRUE)
attach(p16)
p17 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP17/P17.csv", header=TRUE)
attach(p17)
p18 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP18/P18.csv", header=TRUE)
attach(p18)
p19 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP19/P19.csv", header=TRUE)
attach(p19)


# Puts alls the data of the eye tracker into a single data frame
allEyeTrackerData <- rbind(p02,p03,p04,p05,p06,p07,p08,p09,p10,p11,p12,p13,p14,p16,p17,p18,p19)

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
