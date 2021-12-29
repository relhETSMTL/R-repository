# Eye tracker data processing

# install.packages("xlsx")
library("xlsx")

library(ggplot2)
library(tidyverse)
library(hrbrthemes)

#read.xlsx(file, sheetIndex, header=TRUE)
#read.xlsx2(file, sheetIndex, header=TRUE)

# Too big to be read as excel file
# File reads the experiment data 
# participant03 <- read.xlsx2(file = "../../Experiment-Data/Eye-tracking-data-samples/Part03/P03-TOI-Q01-Act20-Data.xlsx", sheetName="Data", header=TRUE)
# attach (participant03)

participant03 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/Part03/P03-TOI-Q01-Act20-Data.csv", header=TRUE)
attach (participant03)

# Important data to be exported
#> levels(participant03$Eye.movement.type)
#[1] "EyesNotFound" "Fixation"     "Saccade"      "Unclassified"
#> participant03$Gaze.event.duration..ms

# About 250 types of eye movements?
# levels(as.factor(participant03$Eye.movement.type.index))

# Examples of areas of interest
# > sum(participant03$AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Answer)
# [1] 149
# > sum(participant03$AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.FMAOI) --> all feature model
# [1] 2500
# > sum(participant03$AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.NAOI.1.)
# [1] 61
#> sum(participant03$AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Window.) --> all window
#[1] 3888
#         AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Answer.,AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Window.)
#         AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Window.:AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Answer.)



##########################################################################################################
##########################################################################################################
##########################################################################################################


# Notes: 
# 1. Eliminating the NA rows dont help as 
# %>% select(-Column1,-Column2, -Column3, -Column4) %>% drop_na()  

# 2. Removing duplicates, where all the selected values are equal
# distinct()

#  filter(Sensor=="Eye Tracker") %>% 

curated03 <- participant03 %>% 
 filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
  distinct()

# What info is repeated?, 
repeated <- participant03 %>% 
  filter(Eye.movement.type.index=="570") %>% 
#  filter(Eye.movement.type=="Fixation" & Eye.movement.type.index=="566") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit"))



##########################################################################################################

### Question 20 processing
question20 <- curated03

# Totals
totalFixations <- nrow(question20)
totalFixationTime <- sum(question20$Gaze.event.duration..ms.)

# AOI Window
question20.Window <- question20 %>% filter(AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Window. =="1")
fixations.Window <-  sum(question20$AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Window.)
perc.fixations.Window <- fixations.Window / totalFixations
perc.time.Window <- sum(question20.Window$Gaze.event.duration..ms.)/totalFixationTime


  
# AOI Question
question20.Question <- question20 %>% filter(AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Question.=="1")
fixations.Question <- sum(question20$AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Question.)
perc.fixations.Question <- fixations.Question / totalFixations
time.Question <- sum(question20.Question$Gaze.event.duration..ms.)
perc.time.Question <- time.Question / totalFixationTime

# AOI Answer
question20.Answer <- question20 %>% filter(AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Answer.=="1")
fixations.Answer <- sum(question20$AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Answer.)
perc.fixations.Answer <- fixations.Answer / totalFixations
time.Answer <- sum(question20.Answer$Gaze.event.duration..ms.)
perc.time.Answer <- time.Answer / totalFixationTime

# AOI Legend
question20.Legend <- question20 %>% filter(AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Legend.=="1")
fixations.Legend <- sum(question20$AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Legend.)
perc.fixations.Legend <- fixations.Legend / totalFixations
time.Legend <- sum(question20.Legend$Gaze.event.duration..ms.)
perc.time.Legend <- time.Legend / totalFixationTime

# AOI Buttons
question20.Buttons <- question20 %>% filter(AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Buttons.=="1")
fixations.Buttons <- sum(question20$AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Buttons.)
perc.fixations.Buttons <- fixations.Buttons / totalFixations
time.Buttons <- sum(question20.Buttons$Gaze.event.duration..ms.)
perc.time.Buttons <- time.Buttons / totalFixationTime

# AOI FeatureModel
question20.FM <- question20 %>% filter(AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.FMAOI.=="1")
fixations.FM <- sum(question20$AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.FMAOI)
perc.fixations.FM <- fixations.FM / totalFixations
time.FM <- sum(question20.FM$Gaze.event.duration..ms.)
perc.time.FM <- time.FM / totalFixationTime

# AOI Containing
question20.Containing <- question20 %>% 
  filter(AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.CAOI.2.=="1" |
         AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.CAOI.3.=="1" |
           AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.CAOI.4.=="1" |
           AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.CAOI.5.=="1" |
           AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.CAOI.6.=="1")
fixations.Containing <- sum(question20.Containing %>% select(contains("CAOI")))
perc.fixations.Containing <- fixations.Containing / totalFixations
time.Containing <- sum(question20.Containing$Gaze.event.duration..ms.)
perc.time.Containing <- time.Containing / totalFixationTime


# AOI Navigating
question20.Navigating <- question20 %>% 
  filter(AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.NAOI.1.=="1")
fixations.Navigating <- sum(question20.Navigating %>% select(contains("NAOI")))
perc.fixations.Navigating <- fixations.Navigating / totalFixations
time.Navigating <- sum(question20.Navigating$Gaze.event.duration..ms.)
perc.time.Navigating <- time.Navigating / totalFixationTime

# AOI Intersection Containing and Navigating when FM < (Containing + Navigating)

# Creating the table now

ParticipantID <- 3
QNumber <- 20

df <- data.frame(ParticipantID, QNumber, totalFixations, totalFixationTime, 
                 fixations.Question, perc.fixations.Question, time.Question, perc.time.Question,
                 fixations.Answer, perc.fixations.Answer, time.Answer, perc.time.Answer,
                 fixations.Legend, perc.fixations.Legend, time.Legend, perc.time.Legend,
                 fixations.Buttons, perc.fixations.Buttons, time.Buttons, perc.time.Buttons,
                 fixations.FM, perc.fixations.FM, time.FM, perc.time.FM,
                 fixations.Containing, perc.fixations.Containing, time.Containing, perc.time.Containing,
                 fixations.Navigating, perc.fixations.Navigating, time.Navigating, perc.time.Navigating)
print(df)

write.csv(df,file = "../../Experiment-Data/Eye-tracking-data-samples/Part03/P03-Q20.csv", row.names = TRUE)


# Loads the Java interface data

allParticipantsData <- read.csv(file = "../../Experiment-Data/All-Participants-Curated-Data.csv", header=TRUE)
attach (allParticipantsData)

joinedData <- full_join(allParticipantsData,df)


####################################################################################################
### Testing the parametrization of process for the questions

### Question 20 processing
question20 <- curated03

# Totals
totalFixations <- nrow(question20)
totalFixationTime <- sum(question20$Gaze.event.duration..ms.)


# AOI Window
question20.Window <- question20 %>% filter(AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Window. =="1")
fixations.Window <-  sum(question20$AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Window.)
perc.fixations.Window <- fixations.Window / totalFixations
perc.time.Window <- sum(question20.Window$Gaze.event.duration..ms.)/totalFixationTime

# returns the column with all the values from the original loaded data file
# aoi.name <- AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Window.
aoi.name <- "AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Window."
question20 %>% filter(aoi.name =="1")

