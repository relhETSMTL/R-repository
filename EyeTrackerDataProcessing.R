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

### Question 17

participant03.Q17 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/Part03/P03-TOI-Q07-Act17-Data.csv", header=TRUE)
attach (participant03.Q17)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated03.Q17 <- participant03.Q17 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
  distinct()


question17 <- curated03.Q17

# Totals
totalFixations.Q17 <- nrow(question17)
totalFixationTime.Q17 <- sum(question17$Gaze.event.duration..ms.)


# AOI Window - QNN for participant question
question17.Window <- question17 %>% filter(AOI.hit..P03.TOI.Q07.Act17.Snap...Q17.Window. =="1")
fixations.Window.Q17 <-  sum(question17$AOI.hit..P03.TOI.Q07.Act17.Snap...Q17.Window.)
perc.fixations.Window.Q17 <- fixations.Window.Q17 / totalFixations.Q17
perc.time.Window.Q17 <- sum(question17.Window$Gaze.event.duration..ms.)/totalFixationTime.Q17


# AOI Question
question17.Question <- question17 %>% filter(AOI.hit..P03.TOI.Q07.Act17.Snap...Q17.Question.=="1")
fixations.Question.Q17 <- sum(question17$AOI.hit..P03.TOI.Q07.Act17.Snap...Q17.Question.)
perc.fixations.Question.Q17 <- fixations.Question.Q17 / totalFixations.Q17
time.Question.Q17 <- sum(question17.Question$Gaze.event.duration..ms.)
perc.time.Question.Q17 <- time.Question.Q17 / totalFixationTime.Q17


# AOI Answer
question17.Answer <- question17 %>% filter(AOI.hit..P03.TOI.Q07.Act17.Snap...Q17.Answer.=="1")
fixations.Answer.Q17 <- sum(question17$AOI.hit..P03.TOI.Q07.Act17.Snap...Q17.Answer.)
perc.fixations.Answer.Q17 <- fixations.Answer.Q17 / totalFixations.Q17
time.Answer.Q17 <- sum(question17.Answer$Gaze.event.duration..ms.)
perc.time.Answer.Q17 <- time.Answer.Q17 / totalFixationTime.Q17

# AOI Legend
question17.Legend <- question17 %>% filter(AOI.hit..P03.TOI.Q07.Act17.Snap...Q17.Legend.=="1")
fixations.Legend.Q17 <- sum(question17$AOI.hit..P03.TOI.Q07.Act17.Snap...Q17.Legend.)
perc.fixations.Legend.Q17 <- fixations.Legend.Q17 / totalFixations.Q17
time.Legend.Q17 <- sum(question17.Legend$Gaze.event.duration..ms.)
perc.time.Legend.Q17 <- time.Legend.Q17 / totalFixationTime.Q17

# AOI Buttons
question17.Buttons <- question17 %>% filter(AOI.hit..P03.TOI.Q07.Act17.Snap...Q17.Buttons.=="1")
fixations.Buttons.Q17 <- sum(question17$AOI.hit..P03.TOI.Q07.Act17.Snap...Q17.Buttons.)
perc.fixations.Buttons.Q17 <- fixations.Buttons.Q17 / totalFixations.Q17
time.Buttons.Q17 <- sum(question17.Buttons$Gaze.event.duration..ms.)
perc.time.Buttons.Q17 <- time.Buttons.Q17 / totalFixationTime.Q17


# AOI FeatureModel
question17.FM <- question17 %>% filter(AOI.hit..P03.TOI.Q07.Act17.Snap...Q17.FMAOI.=="1")
fixations.FM.Q17 <- sum(question17$AOI.hit..P03.TOI.Q07.Act17.Snap...Q17.FMAOI.)
perc.fixations.FM.Q17 <- fixations.FM.Q17 / totalFixations.Q17
time.FM.Q17 <- sum(question17.FM$Gaze.event.duration..ms.)
perc.time.FM.Q17 <- time.FM.Q17 / totalFixationTime.Q17


# AOI Containing
question17.Containing <- question17 %>% 
  filter(  AOI.hit..P03.TOI.Q07.Act17.Snap...Q17.CAOI.1.=="1" |	      
             AOI.hit..P03.TOI.Q07.Act17.Snap...Q17.CAOI.2.=="1" |	      
             AOI.hit..P03.TOI.Q07.Act17.Snap...Q17.CAOI.3.=="1" |	      
             AOI.hit..P03.TOI.Q07.Act17.Snap...Q17.CAOI.4.=="1") 

fixations.Containing.Q17 <- sum(question17.Containing %>% select(contains("CAOI")))
perc.fixations.Containing.Q17 <- fixations.Containing.Q17 / totalFixations.Q17
time.Containing.Q17 <- sum(question17.Containing$Gaze.event.duration..ms.)
perc.time.Containing.Q17 <- time.Containing.Q17 / totalFixationTime.Q17


fixations.Navigating.Q17 <- 0
perc.fixations.Navigating.Q17 <- 0
time.Navigating.Q17 <- 0
perc.time.Navigating.Q17 <- 0


# AOI CTC
question17.CTC <- question17 %>% filter(AOI.hit..P03.TOI.Q07.Act17.Snap...Q17.CTC.=="1")
fixations.CTC.Q17 <- sum(question17$AOI.hit..P03.TOI.Q07.Act17.Snap...Q17.CTC.)
perc.fixations.CTC.Q17 <- fixations.CTC.Q17 / totalFixations.Q17
time.CTC.Q17 <- sum(question17.CTC$Gaze.event.duration..ms.)
perc.time.CTC.Q17 <- time.CTC.Q17 / totalFixationTime.Q17

# AOI Intersection Containing and Navigating when FM < (Containing + Navigating)
print(c(fixations.FM.Q17, fixations.Containing.Q17, fixations.Navigating.Q17))
print(c(fixations.Window.Q17, fixations.Question.Q17, fixations.Answer.Q17, fixations.Legend.Q17, fixations.Buttons.Q17,
        fixations.CTC.Q17))



# Creating the table now

ParticipantID <- 3
QNumber <- 17

Q17.data <- c(ParticipantID, QNumber, totalFixations.Q17, totalFixationTime.Q17, 
              fixations.Question.Q17, perc.fixations.Question.Q17, time.Question.Q17, perc.time.Question.Q17,
              fixations.Answer.Q17, perc.fixations.Answer.Q17, time.Answer.Q17, perc.time.Answer.Q17,
              fixations.Legend.Q17, perc.fixations.Legend.Q17, time.Legend.Q17, perc.time.Legend.Q17,
              fixations.Buttons.Q17, perc.fixations.Buttons.Q17, time.Buttons.Q17, perc.time.Buttons.Q17,
              fixations.FM.Q17, perc.fixations.FM.Q17, time.FM.Q17, perc.time.FM.Q17,
              fixations.Containing.Q17, perc.fixations.Containing.Q17, time.Containing.Q17, perc.time.Containing.Q17,
              fixations.Navigating.Q17, perc.fixations.Navigating.Q17, time.Navigating.Q17, perc.time.Navigating.Q17,
              fixations.CTC.Q17, perc.fixations.CTC.Q17, time.CTC.Q17, perc.time.CTC.Q17)


####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################

### Question 18

participant03.Q18 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/Part03/P03-TOI-Q12-Act18-Data.csv", header=TRUE)
attach (participant03.Q18)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated03.Q18 <- participant03.Q18 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
  distinct()


question18 <- curated03.Q18

# Totals
totalFixations.Q18 <- nrow(question18)
totalFixationTime.Q18 <- sum(question18$Gaze.event.duration..ms.)


# AOI Window - QNN for participant question
question18.Window <- question18 %>% filter(AOI.hit..P03.TOI.Q12.Act18.Snap...Q18.Window. =="1")
fixations.Window.Q18 <-  sum(question18$AOI.hit..P03.TOI.Q12.Act18.Snap...Q18.Window.)
perc.fixations.Window.Q18 <- fixations.Window.Q18 / totalFixations.Q18
perc.time.Window.Q18 <- sum(question18.Window$Gaze.event.duration..ms.)/totalFixationTime.Q18


# AOI Question
question18.Question <- question18 %>% filter(AOI.hit..P03.TOI.Q12.Act18.Snap...Q18.Question.=="1")
fixations.Question.Q18 <- sum(question18$AOI.hit..P03.TOI.Q12.Act18.Snap...Q18.Question.)
perc.fixations.Question.Q18 <- fixations.Question.Q18 / totalFixations.Q18
time.Question.Q18 <- sum(question18.Question$Gaze.event.duration..ms.)
perc.time.Question.Q18 <- time.Question.Q18 / totalFixationTime.Q18


# AOI Answer
question18.Answer <- question18 %>% filter(AOI.hit..P03.TOI.Q12.Act18.Snap...Q18.Answer.=="1")
fixations.Answer.Q18 <- sum(question18$AOI.hit..P03.TOI.Q12.Act18.Snap...Q18.Answer.)
perc.fixations.Answer.Q18 <- fixations.Answer.Q18 / totalFixations.Q18
time.Answer.Q18 <- sum(question18.Answer$Gaze.event.duration..ms.)
perc.time.Answer.Q18 <- time.Answer.Q18 / totalFixationTime.Q18

# AOI Legend
question18.Legend <- question18 %>% filter(AOI.hit..P03.TOI.Q12.Act18.Snap...Q18.Legend.=="1")
fixations.Legend.Q18 <- sum(question18$AOI.hit..P03.TOI.Q12.Act18.Snap...Q18.Legend.)
perc.fixations.Legend.Q18 <- fixations.Legend.Q18 / totalFixations.Q18
time.Legend.Q18 <- sum(question18.Legend$Gaze.event.duration..ms.)
perc.time.Legend.Q18 <- time.Legend.Q18 / totalFixationTime.Q18

# AOI Buttons
question18.Buttons <- question18 %>% filter(AOI.hit..P03.TOI.Q12.Act18.Snap...Q18.Buttons.=="1")
fixations.Buttons.Q18 <- sum(question18$AOI.hit..P03.TOI.Q12.Act18.Snap...Q18.Buttons.)
perc.fixations.Buttons.Q18 <- fixations.Buttons.Q18 / totalFixations.Q18
time.Buttons.Q18 <- sum(question18.Buttons$Gaze.event.duration..ms.)
perc.time.Buttons.Q18 <- time.Buttons.Q18 / totalFixationTime.Q18


# AOI FeatureModel
question18.FM <- question18 %>% filter(AOI.hit..P03.TOI.Q12.Act18.Snap...Q18.FMAOI.=="1")
fixations.FM.Q18 <- sum(question18$AOI.hit..P03.TOI.Q12.Act18.Snap...Q18.FMAOI.)
perc.fixations.FM.Q18 <- fixations.FM.Q18 / totalFixations.Q18
time.FM.Q18 <- sum(question18.FM$Gaze.event.duration..ms.)
perc.time.FM.Q18 <- time.FM.Q18 / totalFixationTime.Q18


# AOI Containing
question18.Containing <- question18 %>% 
  filter(  AOI.hit..P03.TOI.Q12.Act18.Snap...Q18.CAOI.4.=="1") 

fixations.Containing.Q18 <- sum(question18.Containing %>% select(contains("CAOI")))
perc.fixations.Containing.Q18 <- fixations.Containing.Q18 / totalFixations.Q18
time.Containing.Q18 <- sum(question18.Containing$Gaze.event.duration..ms.)
perc.time.Containing.Q18 <- time.Containing.Q18 / totalFixationTime.Q18



# AOI Navigating
question18.Navigating <- question18 %>% 
  filter(  AOI.hit..P03.TOI.Q12.Act18.Snap...Q18.NAOI.1.=="1" | 	      
             AOI.hit..P03.TOI.Q12.Act18.Snap...Q18.NAOI.2.=="1" | 	      
             AOI.hit..P03.TOI.Q12.Act18.Snap...Q18.NAOI.3.=="1" | 	      
             AOI.hit..P03.TOI.Q12.Act18.Snap...Q18.NAOI.5.=="1" | 	      
             AOI.hit..P03.TOI.Q12.Act18.Snap...Q18.NAOI.6.=="1")


fixations.Navigating.Q18 <- sum(question18.Navigating %>% select(contains("NAOI")))
perc.fixations.Navigating.Q18 <- fixations.Navigating.Q18 / totalFixations.Q18
time.Navigating.Q18 <- sum(question18.Navigating$Gaze.event.duration..ms.)
perc.time.Navigating.Q18 <- time.Navigating.Q18 / totalFixationTime.Q18


# AOI CTC
question18.CTC <- question18 %>% filter(AOI.hit..P03.TOI.Q12.Act18.Snap...Q18.CTC.=="1")
fixations.CTC.Q18 <- sum(question18$AOI.hit..P03.TOI.Q12.Act18.Snap...Q18.CTC.)
perc.fixations.CTC.Q18 <- fixations.CTC.Q18 / totalFixations.Q18
time.CTC.Q18 <- sum(question18.CTC$Gaze.event.duration..ms.)
perc.time.CTC.Q18 <- time.CTC.Q18 / totalFixationTime.Q18

# AOI Intersection Containing and Navigating when FM < (Containing + Navigating)
print(c(fixations.FM.Q18, fixations.Containing.Q18, fixations.Navigating.Q18))
print(c(fixations.Window.Q18, fixations.Question.Q18, fixations.Answer.Q18, fixations.Legend.Q18, fixations.Buttons.Q18,
        fixations.CTC.Q18))



# Creating the table now

ParticipantID <- 3
QNumber <- 18

Q18.data <- c(ParticipantID, QNumber, totalFixations.Q18, totalFixationTime.Q18, 
              fixations.Question.Q18, perc.fixations.Question.Q18, time.Question.Q18, perc.time.Question.Q18,
              fixations.Answer.Q18, perc.fixations.Answer.Q18, time.Answer.Q18, perc.time.Answer.Q18,
              fixations.Legend.Q18, perc.fixations.Legend.Q18, time.Legend.Q18, perc.time.Legend.Q18,
              fixations.Buttons.Q18, perc.fixations.Buttons.Q18, time.Buttons.Q18, perc.time.Buttons.Q18,
              fixations.FM.Q18, perc.fixations.FM.Q18, time.FM.Q18, perc.time.FM.Q18,
              fixations.Containing.Q18, perc.fixations.Containing.Q18, time.Containing.Q18, perc.time.Containing.Q18,
              fixations.Navigating.Q18, perc.fixations.Navigating.Q18, time.Navigating.Q18, perc.time.Navigating.Q18,
              fixations.CTC.Q18, perc.fixations.CTC.Q18, time.CTC.Q18, perc.time.CTC.Q18)



####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################

### Question 19


participant03.Q19 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/Part03/P03-TOI-Q13-Act19-Data.csv", header=TRUE)
attach (participant03.Q19)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated03.Q19 <- participant03.Q19 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
  distinct()


question19 <- curated03.Q19

# Totals
totalFixations.Q19 <- nrow(question19)
totalFixationTime.Q19 <- sum(question19$Gaze.event.duration..ms.)


# AOI Window - QNN for participant question
question19.Window <- question19 %>% filter(AOI.hit..P03.TOI.Q13.Act19.Snap...Q19.Window. =="1")
fixations.Window.Q19 <-  sum(question19$AOI.hit..P03.TOI.Q13.Act19.Snap...Q19.Window.)
perc.fixations.Window.Q19 <- fixations.Window.Q19 / totalFixations.Q19
perc.time.Window.Q19 <- sum(question19.Window$Gaze.event.duration..ms.)/totalFixationTime.Q19


# AOI Question
question19.Question <- question19 %>% filter(AOI.hit..P03.TOI.Q13.Act19.Snap...Q19.Question.=="1")
fixations.Question.Q19 <- sum(question19$AOI.hit..P03.TOI.Q13.Act19.Snap...Q19.Question.)
perc.fixations.Question.Q19 <- fixations.Question.Q19 / totalFixations.Q19
time.Question.Q19 <- sum(question19.Question$Gaze.event.duration..ms.)
perc.time.Question.Q19 <- time.Question.Q19 / totalFixationTime.Q19


# AOI Answer
question19.Answer <- question19 %>% filter(AOI.hit..P03.TOI.Q13.Act19.Snap...Q19.Answer.=="1")
fixations.Answer.Q19 <- sum(question19$AOI.hit..P03.TOI.Q13.Act19.Snap...Q19.Answer.)
perc.fixations.Answer.Q19 <- fixations.Answer.Q19 / totalFixations.Q19
time.Answer.Q19 <- sum(question19.Answer$Gaze.event.duration..ms.)
perc.time.Answer.Q19 <- time.Answer.Q19 / totalFixationTime.Q19

# AOI Legend
question19.Legend <- question19 %>% filter(AOI.hit..P03.TOI.Q13.Act19.Snap...Q19.Legend.=="1")
fixations.Legend.Q19 <- sum(question19$AOI.hit..P03.TOI.Q13.Act19.Snap...Q19.Legend.)
perc.fixations.Legend.Q19 <- fixations.Legend.Q19 / totalFixations.Q19
time.Legend.Q19 <- sum(question19.Legend$Gaze.event.duration..ms.)
perc.time.Legend.Q19 <- time.Legend.Q19 / totalFixationTime.Q19

# AOI Buttons
question19.Buttons <- question19 %>% filter(AOI.hit..P03.TOI.Q13.Act19.Snap...Q19.Buttons.=="1")
fixations.Buttons.Q19 <- sum(question19$AOI.hit..P03.TOI.Q13.Act19.Snap...Q19.Buttons.)
perc.fixations.Buttons.Q19 <- fixations.Buttons.Q19 / totalFixations.Q19
time.Buttons.Q19 <- sum(question19.Buttons$Gaze.event.duration..ms.)
perc.time.Buttons.Q19 <- time.Buttons.Q19 / totalFixationTime.Q19


# AOI FeatureModel
question19.FM <- question19 %>% filter(AOI.hit..P03.TOI.Q13.Act19.Snap...Q19.FMAOI.=="1")
fixations.FM.Q19 <- sum(question19$AOI.hit..P03.TOI.Q13.Act19.Snap...Q19.FMAOI.)
perc.fixations.FM.Q19 <- fixations.FM.Q19 / totalFixations.Q19
time.FM.Q19 <- sum(question19.FM$Gaze.event.duration..ms.)
perc.time.FM.Q19 <- time.FM.Q19 / totalFixationTime.Q19


# AOI Containing
question19.Containing <- question19 %>% 
  filter(  AOI.hit..P03.TOI.Q13.Act19.Snap...Q19.CAOI.3.=="1" |	      
             AOI.hit..P03.TOI.Q13.Act19.Snap...Q19.CAOI.4.=="1" |	      
             AOI.hit..P03.TOI.Q13.Act19.Snap...Q19.CAOI.6.=="1" |	      
             AOI.hit..P03.TOI.Q13.Act19.Snap...Q19.CAOI.1.=="1") 

fixations.Containing.Q19 <- sum(question19.Containing %>% select(contains("CAOI")))
perc.fixations.Containing.Q19 <- fixations.Containing.Q19 / totalFixations.Q19
time.Containing.Q19 <- sum(question19.Containing$Gaze.event.duration..ms.)
perc.time.Containing.Q19 <- time.Containing.Q19 / totalFixationTime.Q19


fixations.Navigating.Q19 <- 0
perc.fixations.Navigating.Q19 <- 0
time.Navigating.Q19 <- 0
perc.time.Navigating.Q19 <- 0


fixations.CTC.Q19 <- 0
perc.fixations.CTC.Q19 <- 0
time.CTC.Q19 <- 0
perc.time.CTC.Q19 <- 0

# AOI Intersection Containing and Navigating when FM < (Containing + Navigating)
print(c(fixations.FM.Q19, fixations.Containing.Q19, fixations.Navigating.Q19))
print(c(fixations.Window.Q19, fixations.Question.Q19, fixations.Answer.Q19, fixations.Legend.Q19, fixations.Buttons.Q19,
        fixations.CTC.Q19))



# Creating the table now

ParticipantID <- 3
QNumber <- 19

Q19.data <- c(ParticipantID, QNumber, totalFixations.Q19, totalFixationTime.Q19, 
              fixations.Question.Q19, perc.fixations.Question.Q19, time.Question.Q19, perc.time.Question.Q19,
              fixations.Answer.Q19, perc.fixations.Answer.Q19, time.Answer.Q19, perc.time.Answer.Q19,
              fixations.Legend.Q19, perc.fixations.Legend.Q19, time.Legend.Q19, perc.time.Legend.Q19,
              fixations.Buttons.Q19, perc.fixations.Buttons.Q19, time.Buttons.Q19, perc.time.Buttons.Q19,
              fixations.FM.Q19, perc.fixations.FM.Q19, time.FM.Q19, perc.time.FM.Q19,
              fixations.Containing.Q19, perc.fixations.Containing.Q19, time.Containing.Q19, perc.time.Containing.Q19,
              fixations.Navigating.Q19, perc.fixations.Navigating.Q19, time.Navigating.Q19, perc.time.Navigating.Q19,
              fixations.CTC.Q19, perc.fixations.CTC.Q19, time.CTC.Q19, perc.time.CTC.Q19)


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
question20.Question <- question20 %>% filter(AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Question.=="1")
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

Q20.data <- c(ParticipantID, QNumber, totalFixations.Q20, totalFixationTime.Q20, 
              fixations.Question.Q20, perc.fixations.Question.Q20, time.Question.Q20, perc.time.Question.Q20,
              fixations.Answer.Q20, perc.fixations.Answer.Q20, time.Answer.Q20, perc.time.Answer.Q20,
              fixations.Legend.Q20, perc.fixations.Legend.Q20, time.Legend.Q20, perc.time.Legend.Q20,
              fixations.Buttons.Q20, perc.fixations.Buttons.Q20, time.Buttons.Q20, perc.time.Buttons.Q20,
              fixations.FM.Q20, perc.fixations.FM.Q20, time.FM.Q20, perc.time.FM.Q20,
              fixations.Containing.Q20, perc.fixations.Containing.Q20, time.Containing.Q20, perc.time.Containing.Q20,
              fixations.Navigating.Q20, perc.fixations.Navigating.Q20, time.Navigating.Q20, perc.time.Navigating.Q20,
              fixations.CTC.Q20, perc.fixations.CTC.Q20, time.CTC.Q20, perc.time.CTC.Q20)




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


participant03.Q21 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/Part03/P03-TOI-Q22-Act21-Data.csv", header=TRUE)
attach (participant03.Q21)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated03.Q21 <- participant03.Q21 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
  distinct()


question21 <- curated03.Q21

# Totals
totalFixations.Q21 <- nrow(question21)
totalFixationTime.Q21 <- sum(question21$Gaze.event.duration..ms.)


# AOI Window - QNN for participant question
question21.Window <- question21 %>% filter(AOI.hit..P03.TOI.Q22.Act21.Snap...Q21.Window. =="1")
fixations.Window.Q21 <-  sum(question21$AOI.hit..P03.TOI.Q22.Act21.Snap...Q21.Window.)
perc.fixations.Window.Q21 <- fixations.Window.Q21 / totalFixations.Q21
perc.time.Window.Q21 <- sum(question21.Window$Gaze.event.duration..ms.)/totalFixationTime.Q21


# AOI Question
question21.Question <- question21 %>% filter(AOI.hit..P03.TOI.Q22.Act21.Snap...Q21.Question.=="1")
fixations.Question.Q21 <- sum(question21$AOI.hit..P03.TOI.Q22.Act21.Snap...Q21.Question.)
perc.fixations.Question.Q21 <- fixations.Question.Q21 / totalFixations.Q21
time.Question.Q21 <- sum(question21.Question$Gaze.event.duration..ms.)
perc.time.Question.Q21 <- time.Question.Q21 / totalFixationTime.Q21


# AOI Answer
question21.Answer <- question21 %>% filter(AOI.hit..P03.TOI.Q22.Act21.Snap...Q21.Answer.=="1")
fixations.Answer.Q21 <- sum(question21$AOI.hit..P03.TOI.Q22.Act21.Snap...Q21.Answer.)
perc.fixations.Answer.Q21 <- fixations.Answer.Q21 / totalFixations.Q21
time.Answer.Q21 <- sum(question21.Answer$Gaze.event.duration..ms.)
perc.time.Answer.Q21 <- time.Answer.Q21 / totalFixationTime.Q21

# AOI Legend
question21.Legend <- question21 %>% filter(AOI.hit..P03.TOI.Q22.Act21.Snap...Q21.Legend.=="1")
fixations.Legend.Q21 <- sum(question21$AOI.hit..P03.TOI.Q22.Act21.Snap...Q21.Legend.)
perc.fixations.Legend.Q21 <- fixations.Legend.Q21 / totalFixations.Q21
time.Legend.Q21 <- sum(question21.Legend$Gaze.event.duration..ms.)
perc.time.Legend.Q21 <- time.Legend.Q21 / totalFixationTime.Q21

# AOI Buttons
question21.Buttons <- question21 %>% filter(AOI.hit..P03.TOI.Q22.Act21.Snap...Q21.Buttons.=="1")
fixations.Buttons.Q21 <- sum(question21$AOI.hit..P03.TOI.Q22.Act21.Snap...Q21.Buttons.)
perc.fixations.Buttons.Q21 <- fixations.Buttons.Q21 / totalFixations.Q21
time.Buttons.Q21 <- sum(question21.Buttons$Gaze.event.duration..ms.)
perc.time.Buttons.Q21 <- time.Buttons.Q21 / totalFixationTime.Q21


# AOI FeatureModel
question21.FM <- question21 %>% filter(AOI.hit..P03.TOI.Q22.Act21.Snap...Q21.FMAOI.=="1")
fixations.FM.Q21 <- sum(question21$AOI.hit..P03.TOI.Q22.Act21.Snap...Q21.FMAOI.)
perc.fixations.FM.Q21 <- fixations.FM.Q21 / totalFixations.Q21
time.FM.Q21 <- sum(question21.FM$Gaze.event.duration..ms.)
perc.time.FM.Q21 <- time.FM.Q21 / totalFixationTime.Q21


# AOI Containing
question21.Containing <- question21 %>% 
  filter(  AOI.hit..P03.TOI.Q22.Act21.Snap...Q21.CAOI.4.=="1" |	      
             AOI.hit..P03.TOI.Q22.Act21.Snap...Q21.CAOI.5.=="1" |	      
             AOI.hit..P03.TOI.Q22.Act21.Snap...Q21.CAOI.6.=="1" |	      
             AOI.hit..P03.TOI.Q22.Act21.Snap...Q21.CAOI.7.=="1") 

fixations.Containing.Q21 <- sum(question21.Containing %>% select(contains("CAOI")))
perc.fixations.Containing.Q21 <- fixations.Containing.Q21 / totalFixations.Q21
time.Containing.Q21 <- sum(question21.Containing$Gaze.event.duration..ms.)
perc.time.Containing.Q21 <- time.Containing.Q21 / totalFixationTime.Q21


# AOI Navigating
question21.Navigating <- question21 %>% 
  filter(  AOI.hit..P03.TOI.Q22.Act21.Snap...Q21.NAOI.1.=="1" | 	      
             AOI.hit..P03.TOI.Q22.Act21.Snap...Q21.NAOI.2.=="1" | 	      
             AOI.hit..P03.TOI.Q22.Act21.Snap...Q21.NAOI.3.=="1")

fixations.Navigating.Q21 <- sum(question21.Navigating %>% select(contains("NAOI")))
perc.fixations.Navigating.Q21 <- fixations.Navigating.Q21 / totalFixations.Q21
time.Navigating.Q21 <- sum(question21.Navigating$Gaze.event.duration..ms.)
perc.time.Navigating.Q21 <- time.Navigating.Q21 / totalFixationTime.Q21

# AOI CTC
question21.CTC <- question21 %>% filter(AOI.hit..P03.TOI.Q22.Act21.Snap...Q21.CTC.=="1")
fixations.CTC.Q21 <- sum(question21$AOI.hit..P03.TOI.Q22.Act21.Snap...Q21.CTC.)
perc.fixations.CTC.Q21 <- fixations.CTC.Q21 / totalFixations.Q21
time.CTC.Q21 <- sum(question21.CTC$Gaze.event.duration..ms.)
perc.time.CTC.Q21 <- time.CTC.Q21 / totalFixationTime.Q21

# AOI Intersection Containing and Navigating when FM < (Containing + Navigating)
print(c(fixations.FM.Q21, fixations.Containing.Q21, fixations.Navigating.Q21))
print(c(fixations.Window.Q21, fixations.Question.Q21, fixations.Answer.Q21, fixations.Legend.Q21, fixations.Buttons.Q21,
        fixations.CTC.Q21))

# TODO process the CTC fixations

# Creating the table now

ParticipantID <- 3
QNumber <- 21

Q21.data <- c(ParticipantID, QNumber, totalFixations.Q21, totalFixationTime.Q21, 
              fixations.Question.Q21, perc.fixations.Question.Q21, time.Question.Q21, perc.time.Question.Q21,
              fixations.Answer.Q21, perc.fixations.Answer.Q21, time.Answer.Q21, perc.time.Answer.Q21,
              fixations.Legend.Q21, perc.fixations.Legend.Q21, time.Legend.Q21, perc.time.Legend.Q21,
              fixations.Buttons.Q21, perc.fixations.Buttons.Q21, time.Buttons.Q21, perc.time.Buttons.Q21,
              fixations.FM.Q21, perc.fixations.FM.Q21, time.FM.Q21, perc.time.FM.Q21,
              fixations.Containing.Q21, perc.fixations.Containing.Q21, time.Containing.Q21, perc.time.Containing.Q21,
              fixations.Navigating.Q21, perc.fixations.Navigating.Q21, time.Navigating.Q21, perc.time.Navigating.Q21,
              fixations.CTC.Q21, perc.fixations.CTC.Q21, time.CTC.Q21, perc.time.CTC.Q21)




####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################

# Participant -> 03               --> Parameter to template
# Question Number -> 22
# Participant Question -> 19      --> Paramter to template

participant03.Q22 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/Part03/P03-TOI-Q19-Act22-Data.csv", header=TRUE)
attach (participant03.Q22)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated03.Q22 <- participant03.Q22 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
  distinct()


question22 <- curated03.Q22

#############################

participant03.Q22 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/Part03/P03-TOI-Q19-Act22-Data.csv", header=TRUE)
attach (participant03.Q22)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated03.Q22 <- participant03.Q22 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
  distinct()


question22 <- curated03.Q22

# Totals
totalFixations.Q22 <- nrow(question22)
totalFixationTime.Q22 <- sum(question22$Gaze.event.duration..ms.)


# AOI Window - QNN for participant question
question22.Window <- question22 %>% filter(AOI.hit..P03.TOI.Q19.Act22.Snap...Q22.Window. =="1")
fixations.Window.Q22 <-  sum(question22$AOI.hit..P03.TOI.Q19.Act22.Snap...Q22.Window.)
perc.fixations.Window.Q22 <- fixations.Window.Q22 / totalFixations.Q22
perc.time.Window.Q22 <- sum(question22.Window$Gaze.event.duration..ms.)/totalFixationTime.Q22


# AOI Question
question22.Question <- question22 %>% filter(AOI.hit..P03.TOI.Q19.Act22.Snap...Q22.Question.=="1")
fixations.Question.Q22 <- sum(question22$AOI.hit..P03.TOI.Q19.Act22.Snap...Q22.Question.)
perc.fixations.Question.Q22 <- fixations.Question.Q22 / totalFixations.Q22
time.Question.Q22 <- sum(question22.Question$Gaze.event.duration..ms.)
perc.time.Question.Q22 <- time.Question.Q22 / totalFixationTime.Q22


# AOI Answer
question22.Answer <- question22 %>% filter(AOI.hit..P03.TOI.Q19.Act22.Snap...Q22.Answer.=="1")
fixations.Answer.Q22 <- sum(question22$AOI.hit..P03.TOI.Q19.Act22.Snap...Q22.Answer.)
perc.fixations.Answer.Q22 <- fixations.Answer.Q22 / totalFixations.Q22
time.Answer.Q22 <- sum(question22.Answer$Gaze.event.duration..ms.)
perc.time.Answer.Q22 <- time.Answer.Q22 / totalFixationTime.Q22

# AOI Legend
question22.Legend <- question22 %>% filter(AOI.hit..P03.TOI.Q19.Act22.Snap...Q22.Legend.=="1")
fixations.Legend.Q22 <- sum(question22$AOI.hit..P03.TOI.Q19.Act22.Snap...Q22.Legend.)
perc.fixations.Legend.Q22 <- fixations.Legend.Q22 / totalFixations.Q22
time.Legend.Q22 <- sum(question22.Legend$Gaze.event.duration..ms.)
perc.time.Legend.Q22 <- time.Legend.Q22 / totalFixationTime.Q22

# AOI Buttons
question22.Buttons <- question22 %>% filter(AOI.hit..P03.TOI.Q19.Act22.Snap...Q22.Buttons.=="1")
fixations.Buttons.Q22 <- sum(question22$AOI.hit..P03.TOI.Q19.Act22.Snap...Q22.Buttons.)
perc.fixations.Buttons.Q22 <- fixations.Buttons.Q22 / totalFixations.Q22
time.Buttons.Q22 <- sum(question22.Buttons$Gaze.event.duration..ms.)
perc.time.Buttons.Q22 <- time.Buttons.Q22 / totalFixationTime.Q22


# AOI FeatureModel
question22.FM <- question22 %>% filter(AOI.hit..P03.TOI.Q19.Act22.Snap...Q22.FMAOI.=="1")
fixations.FM.Q22 <- sum(question22$AOI.hit..P03.TOI.Q19.Act22.Snap...Q22.FMAOI.)
perc.fixations.FM.Q22 <- fixations.FM.Q22 / totalFixations.Q22
time.FM.Q22 <- sum(question22.FM$Gaze.event.duration..ms.)
perc.time.FM.Q22 <- time.FM.Q22 / totalFixationTime.Q22


# AOI Containing
question22.Containing <- question22 %>% 
  filter(  AOI.hit..P03.TOI.Q19.Act22.Snap...Q22.CAOI.6.=="1" |	      
             AOI.hit..P03.TOI.Q19.Act22.Snap...Q22.CAOI.3.=="1" |	      
             AOI.hit..P03.TOI.Q19.Act22.Snap...Q22.CAOI.4.=="1" |	      
             AOI.hit..P03.TOI.Q19.Act22.Snap...Q22.CAOI.7.=="1") 

fixations.Containing.Q22 <- sum(question22.Containing %>% select(contains("CAOI")))
perc.fixations.Containing.Q22 <- fixations.Containing.Q22 / totalFixations.Q22
time.Containing.Q22 <- sum(question22.Containing$Gaze.event.duration..ms.)
perc.time.Containing.Q22 <- time.Containing.Q22 / totalFixationTime.Q22


# AOI Navigating
question22.Navigating <- question22 %>% 
  filter(  AOI.hit..P03.TOI.Q19.Act22.Snap...Q22.NAOI.2.=="1" | 	      
             AOI.hit..P03.TOI.Q19.Act22.Snap...Q22.NAOI.5.=="1" | 	      
             AOI.hit..P03.TOI.Q19.Act22.Snap...Q22.NAOI.1.=="1")

fixations.Navigating.Q22 <- sum(question22.Navigating %>% select(contains("NAOI")))
perc.fixations.Navigating.Q22 <- fixations.Navigating.Q22 / totalFixations.Q22
time.Navigating.Q22 <- sum(question22.Navigating$Gaze.event.duration..ms.)
perc.time.Navigating.Q22 <- time.Navigating.Q22 / totalFixationTime.Q22

# AOI CTC
question22.CTC <- question22 %>% filter(AOI.hit..P03.TOI.Q19.Act22.Snap...Q22.CTC.=="1")
fixations.CTC.Q22 <- sum(question22$AOI.hit..P03.TOI.Q19.Act22.Snap...Q22.CTC.)
perc.fixations.CTC.Q22 <- fixations.CTC.Q22 / totalFixations.Q22
time.CTC.Q22 <- sum(question22.CTC$Gaze.event.duration..ms.)
perc.time.CTC.Q22 <- time.CTC.Q22 / totalFixationTime.Q22

# AOI Intersection Containing and Navigating when FM < (Containing + Navigating)
print(c(fixations.FM.Q22, fixations.Containing.Q22, fixations.Navigating.Q22))
print(c(fixations.Window.Q22, fixations.Question.Q22, fixations.Answer.Q22, fixations.Legend.Q22, fixations.Buttons.Q22,
        fixations.CTC.Q22))

# TODO process the CTC fixations

# Creating the table now

ParticipantID <- 3
QNumber <- 22

Q22.data <- c(ParticipantID, QNumber, totalFixations.Q22, totalFixationTime.Q22, 
              fixations.Question.Q22, perc.fixations.Question.Q22, time.Question.Q22, perc.time.Question.Q22,
              fixations.Answer.Q22, perc.fixations.Answer.Q22, time.Answer.Q22, perc.time.Answer.Q22,
              fixations.Legend.Q22, perc.fixations.Legend.Q22, time.Legend.Q22, perc.time.Legend.Q22,
              fixations.Buttons.Q22, perc.fixations.Buttons.Q22, time.Buttons.Q22, perc.time.Buttons.Q22,
              fixations.FM.Q22, perc.fixations.FM.Q22, time.FM.Q22, perc.time.FM.Q22,
              fixations.Containing.Q22, perc.fixations.Containing.Q22, time.Containing.Q22, perc.time.Containing.Q22,
              fixations.Navigating.Q22, perc.fixations.Navigating.Q22, time.Navigating.Q22, perc.time.Navigating.Q22,
              fixations.CTC.Q22, perc.fixations.CTC.Q22, time.CTC.Q22, perc.time.CTC.Q22)


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
question23.Window <- question23 %>% filter(AOI.hit..P03.TOI.Q06.Act23.Snap...Q23.Window. =="1")
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

Q23.data <- c(ParticipantID, QNumber, totalFixations.Q23, totalFixationTime.Q23, 
              fixations.Question.Q23, perc.fixations.Question.Q23, time.Question.Q23, perc.time.Question.Q23,
              fixations.Answer.Q23, perc.fixations.Answer.Q23, time.Answer.Q23, perc.time.Answer.Q23,
              fixations.Legend.Q23, perc.fixations.Legend.Q23, time.Legend.Q23, perc.time.Legend.Q23,
              fixations.Buttons.Q23, perc.fixations.Buttons.Q23, time.Buttons.Q23, perc.time.Buttons.Q23,
              fixations.FM.Q23, perc.fixations.FM.Q23, time.FM.Q23, perc.time.FM.Q23,
              fixations.Containing.Q23, perc.fixations.Containing.Q23, time.Containing.Q23, perc.time.Containing.Q23,
              fixations.Navigating.Q23, perc.fixations.Navigating.Q23, time.Navigating.Q23, perc.time.Navigating.Q23,
              fixations.CTC.Q23, perc.fixations.CTC.Q23, time.CTC.Q23, perc.time.CTC.Q23)



####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################

# Participant -> 03               --> Parameter to template
# Question Number -> 24
# Participant Question -> 06      --> Paramter to template
participant03.Q24 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/Part03/P03-TOI-Q06-Act24-Data.csv", header=TRUE)
attach (participant03.Q24)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated03.Q24 <- participant03.Q24 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
  distinct()


question24 <- curated03.Q24

# Totals
totalFixations.Q24 <- nrow(question24)
totalFixationTime.Q24 <- sum(question24$Gaze.event.duration..ms.)


# AOI Window - QNN for participant question
question24.Window <- question24 %>% filter(AOI.hit..P03.TOI.Q06.Act24.Snap...Q24.Window. =="1")
fixations.Window.Q24 <-  sum(question24$AOI.hit..P03.TOI.Q06.Act24.Snap...Q24.Window.)
perc.fixations.Window.Q24 <- fixations.Window.Q24 / totalFixations.Q24
perc.time.Window.Q24 <- sum(question24.Window$Gaze.event.duration..ms.)/totalFixationTime.Q24


# AOI Question
question24.Question <- question24 %>% filter(AOI.hit..P03.TOI.Q06.Act24.Snap...Q24.Question.=="1")
fixations.Question.Q24 <- sum(question24$AOI.hit..P03.TOI.Q06.Act24.Snap...Q24.Question.)
perc.fixations.Question.Q24 <- fixations.Question.Q24 / totalFixations.Q24
time.Question.Q24 <- sum(question24.Question$Gaze.event.duration..ms.)
perc.time.Question.Q24 <- time.Question.Q24 / totalFixationTime.Q24


# AOI Answer
question24.Answer <- question24 %>% filter(AOI.hit..P03.TOI.Q06.Act24.Snap...Q24.Answer.=="1")
fixations.Answer.Q24 <- sum(question24$AOI.hit..P03.TOI.Q06.Act24.Snap...Q24.Answer.)
perc.fixations.Answer.Q24 <- fixations.Answer.Q24 / totalFixations.Q24
time.Answer.Q24 <- sum(question24.Answer$Gaze.event.duration..ms.)
perc.time.Answer.Q24 <- time.Answer.Q24 / totalFixationTime.Q24

# AOI Legend
question24.Legend <- question24 %>% filter(AOI.hit..P03.TOI.Q06.Act24.Snap...Q24.Legend.=="1")
fixations.Legend.Q24 <- sum(question24$AOI.hit..P03.TOI.Q06.Act24.Snap...Q24.Legend.)
perc.fixations.Legend.Q24 <- fixations.Legend.Q24 / totalFixations.Q24
time.Legend.Q24 <- sum(question24.Legend$Gaze.event.duration..ms.)
perc.time.Legend.Q24 <- time.Legend.Q24 / totalFixationTime.Q24

# AOI Buttons
question24.Buttons <- question24 %>% filter(AOI.hit..P03.TOI.Q06.Act24.Snap...Q24.Buttons.=="1")
fixations.Buttons.Q24 <- sum(question24$AOI.hit..P03.TOI.Q06.Act24.Snap...Q24.Buttons.)
perc.fixations.Buttons.Q24 <- fixations.Buttons.Q24 / totalFixations.Q24
time.Buttons.Q24 <- sum(question24.Buttons$Gaze.event.duration..ms.)
perc.time.Buttons.Q24 <- time.Buttons.Q24 / totalFixationTime.Q24


# AOI FeatureModel
question24.FM <- question24 %>% filter(AOI.hit..P03.TOI.Q06.Act24.Snap...Q24.FMAOI.=="1")
fixations.FM.Q24 <- sum(question24$AOI.hit..P03.TOI.Q06.Act24.Snap...Q24.FMAOI.)
perc.fixations.FM.Q24 <- fixations.FM.Q24 / totalFixations.Q24
time.FM.Q24 <- sum(question24.FM$Gaze.event.duration..ms.)
perc.time.FM.Q24 <- time.FM.Q24 / totalFixationTime.Q24


# AOI Containing
question24.Containing <- question24 %>% 
  filter(AOI.hit..P03.TOI.Q06.Act24.Snap...Q24.CAOI.2.=="1" |
           AOI.hit..P03.TOI.Q06.Act24.Snap...Q24.CAOI.5.=="1" |
           AOI.hit..P03.TOI.Q06.Act24.Snap...Q24.CAOI.6.=="1" |
           AOI.hit..P03.TOI.Q06.Act24.Snap...Q24.CAOI.7.=="1" |
           AOI.hit..P03.TOI.Q06.Act24.Snap...Q24.CAOI.9.=="1" |
           AOI.hit..P03.TOI.Q06.Act24.Snap...Q24.CAOI.10.=="1")
fixations.Containing.Q24 <- sum(question24.Containing %>% select(contains("CAOI")))
perc.fixations.Containing.Q24 <- fixations.Containing.Q24 / totalFixations.Q24
time.Containing.Q24 <- sum(question24.Containing$Gaze.event.duration..ms.)
perc.time.Containing.Q24 <- time.Containing.Q24 / totalFixationTime.Q24

# AOI Navigating
question24.Navigating <- question24 %>% 
  filter(AOI.hit..P03.TOI.Q06.Act24.Snap...Q24.NAOI.1.=="1" |
           AOI.hit..P03.TOI.Q06.Act24.Snap...Q24.NAOI.3.=="1" |
           AOI.hit..P03.TOI.Q06.Act24.Snap...Q24.NAOI.4.=="1" |
           AOI.hit..P03.TOI.Q06.Act24.Snap...Q24.NAOI.8.=="1")
fixations.Navigating.Q24 <- sum(question24.Navigating %>% select(contains("NAOI")))
perc.fixations.Navigating.Q24 <- fixations.Navigating.Q24 / totalFixations.Q24
time.Navigating.Q24 <- sum(question24.Navigating$Gaze.event.duration..ms.)
perc.time.Navigating.Q24 <- time.Navigating.Q24 / totalFixationTime.Q24

# AOI CTC
question24.CTC <- question24 %>% filter(AOI.hit..P03.TOI.Q06.Act24.Snap...Q24.CTC.=="1")
fixations.CTC.Q24 <- sum(question24$AOI.hit..P03.TOI.Q06.Act24.Snap...Q24.CTC.)
perc.fixations.CTC.Q24 <- fixations.CTC.Q24 / totalFixations.Q24
time.CTC.Q24 <- sum(question24.CTC$Gaze.event.duration..ms.)
perc.time.CTC.Q24 <- time.CTC.Q24 / totalFixationTime.Q24




# AOI Intersection Containing and Navigating when FM < (Containing + Navigating)
print(c(fixations.FM.Q24, fixations.Containing.Q24, fixations.Navigating.Q24))
print(c(fixations.Window.Q24, fixations.Question.Q24, fixations.Answer.Q24, fixations.Legend.Q24, fixations.Buttons.Q24,
        fixations.CTC.Q24))

# TODO process the CTC fixations

# Creating the table now

ParticipantID <- 3
QNumber <- 24

Q24.data <- c(ParticipantID, QNumber, totalFixations.Q24, totalFixationTime.Q24, 
              fixations.Question.Q24, perc.fixations.Question.Q24, time.Question.Q24, perc.time.Question.Q24,
              fixations.Answer.Q24, perc.fixations.Answer.Q24, time.Answer.Q24, perc.time.Answer.Q24,
              fixations.Legend.Q24, perc.fixations.Legend.Q24, time.Legend.Q24, perc.time.Legend.Q24,
              fixations.Buttons.Q24, perc.fixations.Buttons.Q24, time.Buttons.Q24, perc.time.Buttons.Q24,
              fixations.FM.Q24, perc.fixations.FM.Q24, time.FM.Q24, perc.time.FM.Q24,
              fixations.Containing.Q24, perc.fixations.Containing.Q24, time.Containing.Q24, perc.time.Containing.Q24,
              fixations.Navigating.Q24, perc.fixations.Navigating.Q24, time.Navigating.Q24, perc.time.Navigating.Q24,
              fixations.CTC.Q24, perc.fixations.CTC.Q24, time.CTC.Q24, perc.time.CTC.Q24)







####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################

# Creating the file with the data for the participant


column.names <- c("ParticipantID", "QNumber", "totalFixations", "totalFixationTime", 
"fixations.Question", "perc.fixations.Question", "time.Question", "perc.time.Question",
"fixations.Answer", "perc.fixations.Answer", "time.Answer", "perc.time.Answer",
"fixations.Legend", "perc.fixations.Legend", "time.Legend", "perc.time.Legend",
"fixations.Buttons", "perc.fixations.Buttons", "time.Buttons", "perc.time.Buttons",
"fixations.FM", "perc.fixations.FM", "time.FM", "perc.time.FM",
"fixations.Containing", "perc.fixations.Containing", "time.Containing", "perc.time.Containing",
"fixations.Navigating", "perc.fixations.Navigating", "time.Navigating", "perc.time.Navigating",
"fixations.CTC", "perc.fixations.CTC", "time.CTC", "perc.time.CTC")


allQuestions.data.frame <- 
  setNames(data.frame(matrix(ncol = 36, nrow = 0)), column.names)


# Changes the name of the columns
allQuestions.data.frame[nrow(allQuestions.data.frame) + 1,] = Q17.data
allQuestions.data.frame[nrow(allQuestions.data.frame) + 1,] = Q18.data
allQuestions.data.frame[nrow(allQuestions.data.frame) + 1,] = Q19.data
allQuestions.data.frame[nrow(allQuestions.data.frame) + 1,] = Q20.data
allQuestions.data.frame[nrow(allQuestions.data.frame) + 1,] = Q21.data
allQuestions.data.frame[nrow(allQuestions.data.frame) + 1,] = Q22.data
allQuestions.data.frame[nrow(allQuestions.data.frame) + 1,] = Q23.data
allQuestions.data.frame[nrow(allQuestions.data.frame) + 1,] = Q24.data


write.csv(allQuestions.data.frame,file = "../../Experiment-Data/Eye-tracking-data-samples/Part03/P03.csv", row.names = FALSE)


############ CHECKED UP TO HERE

####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################

### TODO: joined once all the participants data is ready

# df <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("name", "address", "date"))
# df

# Loads the Java interface data

# allParticipantsData <- read.csv(file = "../../Experiment-Data/All-Participants-Curated-Data.csv", header=TRUE)
# attach (allParticipantsData)

## Performs the join of the table based on the ParticipantID and QNumber
# joinedData <- full_join(allParticipantsData,df)

