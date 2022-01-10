# Alternative methods to compute and check the fixation time of a question
library(ggplot2)
library(tidyverse)
library(hrbrthemes)


# Participant 7 Question 7
participant07.Q07 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP07/P07-TOI-Q07-Act07-Data.csv", header=TRUE)
attach (participant07.Q07)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated07.Q07 <- participant07.Q07 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit"), Recording.timestamp..ms.) %>%
  distinct()


question07 <- curated07.Q07

# Totals
totalFixations.Q07 <- nrow(question07)
totalFixationTime.Q07 <- sum(question07$Gaze.event.duration..ms.)

------------------------------------------------
  
# Check with Participant 2

# Question 4

participant02.Q04 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP02/P02-TOI-Q03-Act04-Data.csv", header=TRUE)
attach (participant02.Q04)

# Finds the start of the question and the end of the question
startEnd <-  participant02.Q04 %>% filter (Event =="Q03-Start" | Event =="Q03-End")
startEnd$Recording.timestamp..ms.[2] - startEnd$Recording.timestamp..ms.[1]


# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated02.Q04 <- participant02.Q04 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit"), Recording.timestamp..ms.) %>%
  distinct()


question04 <- curated02.Q04

# Totals
totalFixations.Q04 <- nrow(question04)
totalFixationTime.Q04 <- sum(question04$Gaze.event.duration..ms.)


# AOI Window - QNN for participant question
question04.Window <- question04 %>% filter(AOI.hit..P02.TOI.Q03.Act04.Snap...Q04.Window. =="1")
fixations.Window.Q04 <-  sum(question04$AOI.hit..P02.TOI.Q03.Act04.Snap...Q04.Window.)
perc.fixations.Window.Q04 <- fixations.Window.Q04 / totalFixations.Q04
perc.time.Window.Q04 <- sum(question04.Window$Gaze.event.duration..ms.)/totalFixationTime.Q04


# AOI Question
question04.Question <- question04 %>% filter(AOI.hit..P02.TOI.Q03.Act04.Snap...Q04.Question.=="1")
fixations.Question.Q04 <- sum(question04$AOI.hit..P02.TOI.Q03.Act04.Snap...Q04.Question.)
perc.fixations.Question.Q04 <- fixations.Question.Q04 / totalFixations.Q04
time.Question.Q04 <- sum(question04.Question$Gaze.event.duration..ms.)
perc.time.Question.Q04 <- time.Question.Q04 / totalFixationTime.Q04


# AOI Answer
question04.Answer <- question04 %>% filter(AOI.hit..P02.TOI.Q03.Act04.Snap...Q04.Answer.=="1")
fixations.Answer.Q04 <- sum(question04$AOI.hit..P02.TOI.Q03.Act04.Snap...Q04.Answer.)
perc.fixations.Answer.Q04 <- fixations.Answer.Q04 / totalFixations.Q04
time.Answer.Q04 <- sum(question04.Answer$Gaze.event.duration..ms.)
perc.time.Answer.Q04 <- time.Answer.Q04 / totalFixationTime.Q04

# AOI Legend
question04.Legend <- question04 %>% filter(AOI.hit..P02.TOI.Q03.Act04.Snap...Q04.Legend.=="1")
fixations.Legend.Q04 <- sum(question04$AOI.hit..P02.TOI.Q03.Act04.Snap...Q04.Legend.)
perc.fixations.Legend.Q04 <- fixations.Legend.Q04 / totalFixations.Q04
time.Legend.Q04 <- sum(question04.Legend$Gaze.event.duration..ms.)
perc.time.Legend.Q04 <- time.Legend.Q04 / totalFixationTime.Q04

# AOI Buttons
question04.Buttons <- question04 %>% filter(AOI.hit..P02.TOI.Q03.Act04.Snap...Q04.Buttons.=="1")
fixations.Buttons.Q04 <- sum(question04$AOI.hit..P02.TOI.Q03.Act04.Snap...Q04.Buttons.)
perc.fixations.Buttons.Q04 <- fixations.Buttons.Q04 / totalFixations.Q04
time.Buttons.Q04 <- sum(question04.Buttons$Gaze.event.duration..ms.)
perc.time.Buttons.Q04 <- time.Buttons.Q04 / totalFixationTime.Q04


# AOI FeatureModel
question04.FM <- question04 %>% filter(AOI.hit..P02.TOI.Q03.Act04.Snap...Q04.FMAOI.=="1")
fixations.FM.Q04 <- sum(question04$AOI.hit..P02.TOI.Q03.Act04.Snap...Q04.FMAOI.)
perc.fixations.FM.Q04 <- fixations.FM.Q04 / totalFixations.Q04
time.FM.Q04 <- sum(question04.FM$Gaze.event.duration..ms.)
perc.time.FM.Q04 <- time.FM.Q04 / totalFixationTime.Q04


# AOI Containing
question04.Containing <- question04 %>% 
  filter(  AOI.hit..P02.TOI.Q03.Act04.Snap...Q04.CAOI.1.=="1") 


if (nrow(question04.Containing) !=0) {
  fixations.Containing.Q04 <- sum(question04.Containing %>% select(contains("CAOI")))
  perc.fixations.Containing.Q04 <- fixations.Containing.Q04 / totalFixations.Q04
  time.Containing.Q04 <- sum(question04.Containing$Gaze.event.duration..ms.)
  perc.time.Containing.Q04 <- time.Containing.Q04 / totalFixationTime.Q04
} else {
  fixations.Containing.Q04 <- 0
  perc.fixations.Containing.Q04 <- 0
  time.Containing.Q04 <- 0
  perc.time.Containing.Q04 <- 0
}

fixations.Navigating.Q04 <- 0
perc.fixations.Navigating.Q04 <- 0
time.Navigating.Q04 <- 0
perc.time.Navigating.Q04 <- 0


# AOI CTC
question04.CTC <- question04 %>% filter(AOI.hit..P02.TOI.Q03.Act04.Snap...Q04.CTC.=="1")
fixations.CTC.Q04 <- sum(question04$AOI.hit..P02.TOI.Q03.Act04.Snap...Q04.CTC.)
perc.fixations.CTC.Q04 <- fixations.CTC.Q04 / totalFixations.Q04
time.CTC.Q04 <- sum(question04.CTC$Gaze.event.duration..ms.)
perc.time.CTC.Q04 <- time.CTC.Q04 / totalFixationTime.Q04

# AOI Intersection Containing and Navigating when FM < (Containing + Navigating)
print(c(fixations.FM.Q04, fixations.Containing.Q04, fixations.Navigating.Q04))
print(c(fixations.Window.Q04, fixations.Question.Q04, fixations.Answer.Q04, fixations.Legend.Q04, fixations.Buttons.Q04,
        fixations.CTC.Q04))



# Creating the table now

ParticipantID <- 2
QNumber <- 4

Q04.data <- c(ParticipantID, QNumber, totalFixations.Q04, totalFixationTime.Q04, 
              fixations.Question.Q04, perc.fixations.Question.Q04, time.Question.Q04, perc.time.Question.Q04,
              fixations.Answer.Q04, perc.fixations.Answer.Q04, time.Answer.Q04, perc.time.Answer.Q04,
              fixations.Legend.Q04, perc.fixations.Legend.Q04, time.Legend.Q04, perc.time.Legend.Q04,
              fixations.Buttons.Q04, perc.fixations.Buttons.Q04, time.Buttons.Q04, perc.time.Buttons.Q04,
              fixations.FM.Q04, perc.fixations.FM.Q04, time.FM.Q04, perc.time.FM.Q04,
              fixations.Containing.Q04, perc.fixations.Containing.Q04, time.Containing.Q04, perc.time.Containing.Q04,
              fixations.Navigating.Q04, perc.fixations.Navigating.Q04, time.Navigating.Q04, perc.time.Navigating.Q04,
              fixations.CTC.Q04, perc.fixations.CTC.Q04, time.CTC.Q04, perc.time.CTC.Q04)  





# --------------------------------------------------------------------------------------------------
# Question 13



participant02.Q13 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP02/P02-TOI-Q13-Act13-Data.csv", header=TRUE)
attach (participant02.Q13)

# Finds the start of the question and the end of the question
startEnd <-  participant02.Q13 %>% filter (Event =="Q13-Start" | Event =="Q13-End")
startEnd$Recording.timestamp..ms.[2] - startEnd$Recording.timestamp..ms.[1]

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated02.Q13 <- participant02.Q13 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
  distinct()


question13 <- curated02.Q13

# Totals
totalFixations.Q13 <- nrow(question13)
totalFixationTime.Q13 <- sum(question13$Gaze.event.duration..ms.)


# AOI Window - QNN for participant question
question13.Window <- question13 %>% filter(AOI.hit..P02.TOI.Q13.Act13.Snap...Q13.Window. =="1")
fixations.Window.Q13 <-  sum(question13$AOI.hit..P02.TOI.Q13.Act13.Snap...Q13.Window.)
perc.fixations.Window.Q13 <- fixations.Window.Q13 / totalFixations.Q13
perc.time.Window.Q13 <- sum(question13.Window$Gaze.event.duration..ms.)/totalFixationTime.Q13


# AOI Question
question13.Question <- question13 %>% filter(AOI.hit..P02.TOI.Q13.Act13.Snap...Q13.Question.=="1")
fixations.Question.Q13 <- sum(question13$AOI.hit..P02.TOI.Q13.Act13.Snap...Q13.Question.)
perc.fixations.Question.Q13 <- fixations.Question.Q13 / totalFixations.Q13
time.Question.Q13 <- sum(question13.Question$Gaze.event.duration..ms.)
perc.time.Question.Q13 <- time.Question.Q13 / totalFixationTime.Q13


# AOI Answer
question13.Answer <- question13 %>% filter(AOI.hit..P02.TOI.Q13.Act13.Snap...Q13.Answer.=="1")
fixations.Answer.Q13 <- sum(question13$AOI.hit..P02.TOI.Q13.Act13.Snap...Q13.Answer.)
perc.fixations.Answer.Q13 <- fixations.Answer.Q13 / totalFixations.Q13
time.Answer.Q13 <- sum(question13.Answer$Gaze.event.duration..ms.)
perc.time.Answer.Q13 <- time.Answer.Q13 / totalFixationTime.Q13

# AOI Legend
question13.Legend <- question13 %>% filter(AOI.hit..P02.TOI.Q13.Act13.Snap...Q13.Legend.=="1")
fixations.Legend.Q13 <- sum(question13$AOI.hit..P02.TOI.Q13.Act13.Snap...Q13.Legend.)
perc.fixations.Legend.Q13 <- fixations.Legend.Q13 / totalFixations.Q13
time.Legend.Q13 <- sum(question13.Legend$Gaze.event.duration..ms.)
perc.time.Legend.Q13 <- time.Legend.Q13 / totalFixationTime.Q13

# AOI Buttons
question13.Buttons <- question13 %>% filter(AOI.hit..P02.TOI.Q13.Act13.Snap...Q13.Buttons.=="1")
fixations.Buttons.Q13 <- sum(question13$AOI.hit..P02.TOI.Q13.Act13.Snap...Q13.Buttons.)
perc.fixations.Buttons.Q13 <- fixations.Buttons.Q13 / totalFixations.Q13
time.Buttons.Q13 <- sum(question13.Buttons$Gaze.event.duration..ms.)
perc.time.Buttons.Q13 <- time.Buttons.Q13 / totalFixationTime.Q13


# AOI FeatureModel
question13.FM <- question13 %>% filter(AOI.hit..P02.TOI.Q13.Act13.Snap...Q13.FMAOI.=="1")
fixations.FM.Q13 <- sum(question13$AOI.hit..P02.TOI.Q13.Act13.Snap...Q13.FMAOI.)
perc.fixations.FM.Q13 <- fixations.FM.Q13 / totalFixations.Q13
time.FM.Q13 <- sum(question13.FM$Gaze.event.duration..ms.)
perc.time.FM.Q13 <- time.FM.Q13 / totalFixationTime.Q13


# AOI Containing
question13.Containing <- question13 %>% 
  filter(  AOI.hit..P02.TOI.Q13.Act13.Snap...Q13.CAOI.2.=="1" |	      
             AOI.hit..P02.TOI.Q13.Act13.Snap...Q13.CAOI.3.=="1") 


if (nrow(question13.Containing) !=0) {
  fixations.Containing.Q13 <- sum(question13.Containing %>% select(contains("CAOI")))
  perc.fixations.Containing.Q13 <- fixations.Containing.Q13 / totalFixations.Q13
  time.Containing.Q13 <- sum(question13.Containing$Gaze.event.duration..ms.)
  perc.time.Containing.Q13 <- time.Containing.Q13 / totalFixationTime.Q13
} else {
  fixations.Containing.Q13 <- 0
  perc.fixations.Containing.Q13 <- 0
  time.Containing.Q13 <- 0
  perc.time.Containing.Q13 <- 0
}


# AOI Navigating
question13.Navigating <- question13 %>% 
  filter(  AOI.hit..P02.TOI.Q13.Act13.Snap...Q13.NAOI.1.=="1")


if (nrow(question13.Navigating) !=0) {
  fixations.Navigating.Q13 <- sum(question13.Navigating %>% select(contains("NAOI")))
  perc.fixations.Navigating.Q13 <- fixations.Navigating.Q13 / totalFixations.Q13
  time.Navigating.Q13 <- sum(question13.Navigating$Gaze.event.duration..ms.)
  perc.time.Navigating.Q13 <- time.Navigating.Q13 / totalFixationTime.Q13
} else {
  fixations.Navigating.Q13 <- 0
  perc.fixations.Navigating.Q13 <- 0
  time.Navigating.Q13 <- 0
  perc.time.Navigating.Q13 <- 0 
}



fixations.CTC.Q13 <- 0
perc.fixations.CTC.Q13 <- 0
time.CTC.Q13 <- 0
perc.time.CTC.Q13 <- 0

# AOI Intersection Containing and Navigating when FM < (Containing + Navigating)
print(c(fixations.FM.Q13, fixations.Containing.Q13, fixations.Navigating.Q13))
print(c(fixations.Window.Q13, fixations.Question.Q13, fixations.Answer.Q13, fixations.Legend.Q13, fixations.Buttons.Q13,
        fixations.CTC.Q13))



# Creating the table now

ParticipantID <- 2
QNumber <- 13

Q13.data <- c(ParticipantID, QNumber, totalFixations.Q13, totalFixationTime.Q13, 
              fixations.Question.Q13, perc.fixations.Question.Q13, time.Question.Q13, perc.time.Question.Q13,
              fixations.Answer.Q13, perc.fixations.Answer.Q13, time.Answer.Q13, perc.time.Answer.Q13,
              fixations.Legend.Q13, perc.fixations.Legend.Q13, time.Legend.Q13, perc.time.Legend.Q13,
              fixations.Buttons.Q13, perc.fixations.Buttons.Q13, time.Buttons.Q13, perc.time.Buttons.Q13,
              fixations.FM.Q13, perc.fixations.FM.Q13, time.FM.Q13, perc.time.FM.Q13,
              fixations.Containing.Q13, perc.fixations.Containing.Q13, time.Containing.Q13, perc.time.Containing.Q13,
              fixations.Navigating.Q13, perc.fixations.Navigating.Q13, time.Navigating.Q13, perc.time.Navigating.Q13,
              fixations.CTC.Q13, perc.fixations.CTC.Q13, time.CTC.Q13, perc.time.CTC.Q13)  


# --------------------------------------------------------------------------------------------------
# Question 24



participant02.Q24 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP02/P02-TOI-Q19-Act24-Data.csv", header=TRUE)
attach (participant02.Q24)

# Finds the start of the question and the end of the question
startEnd <-  participant02.Q24 %>% filter (Event =="Q19-Start" | Event =="Q19-End")
startEnd$Recording.timestamp..ms.[2] - startEnd$Recording.timestamp..ms.[1]

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated02.Q24 <- participant02.Q24 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
  distinct()


question24 <- curated02.Q24
