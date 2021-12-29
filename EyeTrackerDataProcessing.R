# Eye tracker data processing

# install.packages("xlsx")
#library("xlsx")

#read.xlsx(file, sheetIndex, header=TRUE)
#read.xlsx2(file, sheetIndex, header=TRUE)

# Too big to be read as excel file
# File reads the experiment data 
# participant03 <- read.xlsx2(file = "../../Experiment-Data/Eye-tracking-data-samples/Part03/P03-TOI-Q01-Act20-Data.xlsx", sheetName="Data", header=TRUE)
# attach (participant03)

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

# What info is repeated?, 
#repeated <- participant03 %>% 
#  filter(Eye.movement.type.index=="570") %>% 
#  filter(Eye.movement.type=="Fixation" & Eye.movement.type.index=="566") %>% 
#  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit"))

##########################################################################################################
##########################################################################################################
##########################################################################################################
# Program start

library(ggplot2)
library(tidyverse)
library(hrbrthemes)



####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################

# Participant -> 03               --> Parameter to template
# Question Number -> 20
# Participant Question -> 01      --> Paramter to template


### Question 20 processing

participant03.Q20 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/Part03/P03-TOI-Q01-Act20-Data.csv", header=TRUE)
attach (participant03.Q20)

curated03.Q20 <- participant03.Q20 %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
  distinct()

question20 <- curated03.Q20

# Totals
totalFixations.Q20 <- nrow(question20)
totalFixationTime.Q20 <- sum(question20$Gaze.event.duration..ms.)

# AOI Window
question20.Window <- question20 %>% filter(AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Window. =="1")
fixations.Window.Q20 <-  sum(question20$AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Window.)
perc.fixations.Window.Q20 <- fixations.Window.Q20 / totalFixations.Q20
perc.time.Window.Q20 <- sum(question20.Window$Gaze.event.duration..ms.)/totalFixationTime.Q20

# AOI Question
question20.Question.Q20 <- question20 %>% filter(AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Question.=="1")
fixations.Question.Q20 <- sum(question20$AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Question.)
perc.fixations.Question.Q20 <- fixations.Question.Q20 / totalFixations.Q20
time.Question.Q20 <- sum(question20.Question$Gaze.event.duration..ms.)
perc.time.Question.Q20 <- time.Question.Q20 / totalFixationTime.Q20

# AOI Answer
question20.Answer <- question20 %>% filter(AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Answer.=="1")
fixations.Answer.Q20 <- sum(question20$AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Answer.)
perc.fixations.Answer.Q20 <- fixations.Answer.Q20 / totalFixations.Q20
time.Answer.Q20 <- sum(question20.Answer$Gaze.event.duration..ms.)
perc.time.Answer.Q20 <- time.Answer.Q20 / totalFixationTime.Q20

# AOI Legend
question20.Legend <- question20 %>% filter(AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Legend.=="1")
fixations.Legend.Q20 <- sum(question20$AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Legend.)
perc.fixations.Legend.Q20 <- fixations.Legend.Q20 / totalFixations.Q20
time.Legend.Q20 <- sum(question20.Legend$Gaze.event.duration..ms.)
perc.time.Legend.Q20 <- time.Legend.Q20 / totalFixationTime.Q20

# AOI Buttons
question20.Buttons <- question20 %>% filter(AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Buttons.=="1")
fixations.Buttons.Q20 <- sum(question20$AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Buttons.)
perc.fixations.Buttons.Q20 <- fixations.Buttons.Q20 / totalFixations.Q20
time.Buttons.Q20 <- sum(question20.Buttons$Gaze.event.duration..ms.)
perc.time.Buttons.Q20 <- time.Buttons.Q20 / totalFixationTime.Q20

# AOI FeatureModel
question20.FM <- question20 %>% filter(AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.FMAOI.=="1")
fixations.FM.Q20 <- sum(question20$AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.FMAOI)
perc.fixations.FM.Q20 <- fixations.FM.Q20 / totalFixations.Q20
time.FM.Q20 <- sum(question20.FM$Gaze.event.duration..ms.)
perc.time.FM.Q20 <- time.FM.Q20 / totalFixationTime.Q20

# AOI Containing
question20.Containing <- question20 %>% 
  filter(AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.CAOI.2.=="1" |
         AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.CAOI.3.=="1" |
           AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.CAOI.4.=="1" |
           AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.CAOI.5.=="1" |
           AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.CAOI.6.=="1")
fixations.Containing.Q20 <- sum(question20.Containing %>% select(contains("CAOI")))
perc.fixations.Containing.Q20 <- fixations.Containing.Q20 / totalFixations.Q20
time.Containing.Q20 <- sum(question20.Containing$Gaze.event.duration..ms.)
perc.time.Containing.Q20 <- time.Containing.Q20 / totalFixationTime.Q20


# AOI Navigating
question20.Navigating <- question20 %>% 
  filter(AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.NAOI.1.=="1")
fixations.Navigating.Q20 <- sum(question20.Navigating %>% select(contains("NAOI")))
perc.fixations.Navigating.Q20 <- fixations.Navigating.Q20 / totalFixations.Q20
time.Navigating.Q20 <- sum(question20.Navigating$Gaze.event.duration..ms.)
perc.time.Navigating.Q20 <- time.Navigating.Q20 / totalFixationTime.Q20

# AOI CTC - Question 20 does not have CTC 
fixations.CTC.Q20 <- 0
perc.fixations.CTC.Q20 <- 0
time.CTC.Q20 <- 0
perc.time.CTC.Q20 <- 0

# AOI Intersection Containing and Navigating when FM < (Containing + Navigating)
print(c(fixations.FM.Q20, fixations.Containing.Q20, fixations.Navigating.Q20))
print(c(fixations.Window.Q20, fixations.Question.Q20, fixations.Answer.Q20, fixations.Legend.Q20, fixations.Buttons.Q20,
        fixations.CTC.Q20))


# Creating the table now

ParticipantID <- 3
QNumber <- 20

df.Q20 <- data.frame(ParticipantID, QNumber, totalFixations.Q20, totalFixationTime.Q20, 
                 fixations.Question.Q20, perc.fixations.Question.Q20, time.Question.Q20, perc.time.Question.Q20,
                 fixations.Answer.Q20, perc.fixations.Answer.Q20, time.Answer.Q20, perc.time.Answer.Q20,
                 fixations.Legend.Q20, perc.fixations.Legend.Q20, time.Legend.Q20, perc.time.Legend.Q20,
                 fixations.Buttons.Q20, perc.fixations.Buttons.Q20, time.Buttons.Q20, perc.time.Buttons.Q20,
                 fixations.FM.Q20, perc.fixations.FM.Q20, time.FM.Q20, perc.time.FM.Q20,
                 fixations.Containing.Q20, perc.fixations.Containing.Q20, time.Containing.Q20, perc.time.Containing.Q20,
                 fixations.Navigating.Q20, perc.fixations.Navigating.Q20, time.Navigating.Q20, perc.time.Navigating.Q20,
                 fixations.CTC.Q20, perc.fixations.CTC.Q20, time.CTC.Q20, perc.time.CTC.Q20)
print(df.Q20)

write.csv(df.Q20,file = "../../Experiment-Data/Eye-tracking-data-samples/Part03/P03-Q20.csv", row.names = TRUE)


# Loads the Java interface data

allParticipantsData <- read.csv(file = "../../Experiment-Data/All-Participants-Curated-Data.csv", header=TRUE)
attach (allParticipantsData)

## Performs the join of the table based on the 
joinedData <- full_join(allParticipantsData,df)


####################################################################################################
### Testing the parametrization of process for the questions
### Note: Not possible to parameterize. Better use velocity templates for that.

### Question 20 processing
# question20 <- curated03

# Totals
# totalFixations <- nrow(question20)
# totalFixationTime <- sum(question20$Gaze.event.duration..ms.)


# AOI Window
# question20.Window <- question20 %>% filter(AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Window. =="1")
# fixations.Window <-  sum(question20$AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Window.)
# perc.fixations.Window <- fixations.Window / totalFixations
# perc.time.Window <- sum(question20.Window$Gaze.event.duration..ms.)/totalFixationTime

# returns the column with all the values from the original loaded data file
# aoi.name <- AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Window.
# aoi.name <- "AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Window."
# question20 %>% filter(aoi.name =="1")



####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################

# Participant -> 03               --> Parameter to template
# Question Number -> 23
# Participant Question -> 04      --> Paramter to template
participant03.Q23 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/Part03/P03-TOI-Q04-Act23-Data.csv", header=TRUE)
attach (participant03.Q23)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated03.Q23 <- participant03.Q23 %>% filter(!across(everything(), is.na)) %>% 
    filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
    select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
  distinct()


question23 <- curated03.Q23

# Totals
totalFixations.Q23 <- nrow(question23)
totalFixationTime.Q23 <- sum(question23$Gaze.event.duration..ms.)


# AOI Window - QNN for participant question
question23.Window <- question23 %>% filter(AOI.hit..P03.TOI.Q04.Act23.Snap...Q23.Window. =="1")
fixations.Window.Q23 <-  sum(question23$AOI.hit..P03.TOI.Q04.Act23.Snap...Q23.Window.)
perc.fixations.Window.Q23 <- fixations.Window.Q23 / totalFixations.Q23
perc.time.Window.Q23 <- sum(question23.Window$Gaze.event.duration..ms.)/totalFixationTime.Q23


# AOI Question
question23.Question <- question23 %>% filter(AOI.hit..P03.TOI.Q04.Act23.Snap...Q23.Question.=="1")
fixations.Question.Q23 <- sum(question23$AOI.hit..P03.TOI.Q04.Act23.Snap...Q23.Question.)
perc.fixations.Question.Q23 <- fixations.Question.Q23 / totalFixations.Q23
time.Question.Q23 <- sum(question23.Question$Gaze.event.duration..ms.)
perc.time.Question.Q23 <- time.Question.Q23 / totalFixationTime.Q23


# AOI Answer
question23.Answer <- question23 %>% filter(AOI.hit..P03.TOI.Q04.Act23.Snap...Q23.Answer.=="1")
fixations.Answer.Q23 <- sum(question23$AOI.hit..P03.TOI.Q04.Act23.Snap...Q23.Answer.)
perc.fixations.Answer.Q23 <- fixations.Answer.Q23 / totalFixations.Q23
time.Answer.Q23 <- sum(question23.Answer$Gaze.event.duration..ms.)
perc.time.Answer.Q23 <- time.Answer.Q23 / totalFixationTime.Q23

# AOI Legend
question23.Legend <- question23 %>% filter(AOI.hit..P03.TOI.Q04.Act23.Snap...Q23.Legend.=="1")
fixations.Legend.Q23 <- sum(question23$AOI.hit..P03.TOI.Q04.Act23.Snap...Q23.Legend.)
perc.fixations.Legend.Q23 <- fixations.Legend.Q23 / totalFixations.Q23
time.Legend.Q23 <- sum(question23.Legend$Gaze.event.duration..ms.)
perc.time.Legend.Q23 <- time.Legend.Q23 / totalFixationTime.Q23

# AOI Buttons
question23.Buttons <- question23 %>% filter(AOI.hit..P03.TOI.Q04.Act23.Snap...Q23.Buttons.=="1")
fixations.Buttons.Q23 <- sum(question23$AOI.hit..P03.TOI.Q04.Act23.Snap...Q23.Buttons.)
perc.fixations.Buttons.Q23 <- fixations.Buttons.Q23 / totalFixations.Q23
time.Buttons.Q23 <- sum(question23.Buttons$Gaze.event.duration..ms.)
perc.time.Buttons.Q23 <- time.Buttons.Q23 / totalFixationTime.Q23


# AOI FeatureModel
question23.FM <- question23 %>% filter(AOI.hit..P03.TOI.Q04.Act23.Snap...Q23.FMAOI.=="1")
fixations.FM.Q23 <- sum(question23$AOI.hit..P03.TOI.Q04.Act23.Snap...Q23.FMAOI.)
perc.fixations.FM.Q23 <- fixations.FM.Q23 / totalFixations.Q23
time.FM.Q23 <- sum(question23.FM$Gaze.event.duration..ms.)
perc.time.FM.Q23 <- time.FM.Q23 / totalFixationTime.Q23


# AOI Containing
question23.Containing <- question23 %>% 
  filter(AOI.hit..P03.TOI.Q04.Act23.Snap...Q23.CAOI.2.=="1" |
           AOI.hit..P03.TOI.Q04.Act23.Snap...Q23.CAOI.3.=="1" |
           AOI.hit..P03.TOI.Q04.Act23.Snap...Q23.CAOI.5.=="1")
fixations.Containing.Q23 <- sum(question23.Containing %>% select(contains("CAOI")))
perc.fixations.Containing.Q23 <- fixations.Containing.Q23 / totalFixations.Q23
time.Containing.Q23 <- sum(question23.Containing$Gaze.event.duration..ms.)
perc.time.Containing.Q23 <- time.Containing.Q23 / totalFixationTime.Q23

# AOI Navigating
question23.Navigating <- question23 %>% 
  filter(AOI.hit..P03.TOI.Q04.Act23.Snap...Q23.NAOI.1.=="1" |
           AOI.hit..P03.TOI.Q04.Act23.Snap...Q23.NAOI.4.=="1")
fixations.Navigating.Q23 <- sum(question23.Navigating %>% select(contains("NAOI")))
perc.fixations.Navigating.Q23 <- fixations.Navigating.Q23 / totalFixations.Q23
time.Navigating.Q23 <- sum(question23.Navigating$Gaze.event.duration..ms.)
perc.time.Navigating.Q23 <- time.Navigating.Q23 / totalFixationTime.Q23

# AOI CTC
question23.CTC <- question23 %>% filter(AOI.hit..P03.TOI.Q04.Act23.Snap...Q23.CTC.=="1")
fixations.CTC.Q23 <- sum(question23$AOI.hit..P03.TOI.Q04.Act23.Snap...Q23.CTC.)
perc.fixations.CTC.Q23 <- fixations.CTC.Q23 / totalFixations.Q23
time.CTC.Q23 <- sum(question23.CTC$Gaze.event.duration..ms.)
perc.time.CTC.Q23 <- time.CTC.Q23 / totalFixationTime.Q23




# AOI Intersection Containing and Navigating when FM < (Containing + Navigating)
print(c(fixations.FM.Q23, fixations.Containing.Q23, fixations.Navigating.Q23))
print(c(fixations.Window.Q23, fixations.Question.Q23, fixations.Answer.Q23, fixations.Legend.Q23, fixations.Buttons.Q23,
        fixations.CTC.Q23))

# TODO process the CTC fixations

# Creating the table now

ParticipantID <- 3
QNumber <- 23

df.Q23 <- data.frame(ParticipantID, QNumber, totalFixations.Q23, totalFixationTime.Q23, 
                 fixations.Question.Q23, perc.fixations.Question.Q23, time.Question.Q23, perc.time.Question.Q23,
                 fixations.Answer.Q23, perc.fixations.Answer.Q23, time.Answer.Q23, perc.time.Answer.Q23,
                 fixations.Legend.Q23, perc.fixations.Legend.Q23, time.Legend.Q23, perc.time.Legend.Q23,
                 fixations.Buttons.Q23, perc.fixations.Buttons.Q23, time.Buttons.Q23, perc.time.Buttons.Q23,
                 fixations.FM.Q23, perc.fixations.FM.Q23, time.FM.Q23, perc.time.FM.Q23,
                 fixations.Containing.Q23, perc.fixations.Containing.Q23, time.Containing.Q23, perc.time.Containing.Q23,
                 fixations.Navigating.Q23, perc.fixations.Navigating.Q23, time.Navigating.Q23, perc.time.Navigating.Q23,
                 fixations.CTC.Q23, perc.fixations.CTC.Q23, time.CTC.Q23, perc.time.CTC.Q23)
print(df.Q23)

############ CHECKED UP TO HERE
