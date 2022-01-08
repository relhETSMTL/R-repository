

library(ggplot2)
library(tidyverse)
library(hrbrthemes)




 


# --------------------------------------------------------------------------------------------------
# Question 1



participant02.Q01 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP02/P02-TOI-Q17-Act01-Data.csv", header=TRUE)
attach (participant02.Q01)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated02.Q01 <- participant02.Q01 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
  distinct()


question01 <- curated02.Q01

# Totals
totalFixations.Q01 <- nrow(question01)
totalFixationTime.Q01 <- sum(question01$Gaze.event.duration..ms.)


# AOI Window - QNN for participant question
question01.Window <- question01 %>% filter(AOI.hit..P02.TOI.Q17.Act01.Snap...Q01.Window. =="1")
fixations.Window.Q01 <-  sum(question01$AOI.hit..P02.TOI.Q17.Act01.Snap...Q01.Window.)
perc.fixations.Window.Q01 <- fixations.Window.Q01 / totalFixations.Q01
perc.time.Window.Q01 <- sum(question01.Window$Gaze.event.duration..ms.)/totalFixationTime.Q01


# AOI Question
question01.Question <- question01 %>% filter(AOI.hit..P02.TOI.Q17.Act01.Snap...Q01.Question.=="1")
fixations.Question.Q01 <- sum(question01$AOI.hit..P02.TOI.Q17.Act01.Snap...Q01.Question.)
perc.fixations.Question.Q01 <- fixations.Question.Q01 / totalFixations.Q01
time.Question.Q01 <- sum(question01.Question$Gaze.event.duration..ms.)
perc.time.Question.Q01 <- time.Question.Q01 / totalFixationTime.Q01


# AOI Answer
question01.Answer <- question01 %>% filter(AOI.hit..P02.TOI.Q17.Act01.Snap...Q01.Answer.=="1")
fixations.Answer.Q01 <- sum(question01$AOI.hit..P02.TOI.Q17.Act01.Snap...Q01.Answer.)
perc.fixations.Answer.Q01 <- fixations.Answer.Q01 / totalFixations.Q01
time.Answer.Q01 <- sum(question01.Answer$Gaze.event.duration..ms.)
perc.time.Answer.Q01 <- time.Answer.Q01 / totalFixationTime.Q01

# AOI Legend
question01.Legend <- question01 %>% filter(AOI.hit..P02.TOI.Q17.Act01.Snap...Q01.Legend.=="1")
fixations.Legend.Q01 <- sum(question01$AOI.hit..P02.TOI.Q17.Act01.Snap...Q01.Legend.)
perc.fixations.Legend.Q01 <- fixations.Legend.Q01 / totalFixations.Q01
time.Legend.Q01 <- sum(question01.Legend$Gaze.event.duration..ms.)
perc.time.Legend.Q01 <- time.Legend.Q01 / totalFixationTime.Q01

# AOI Buttons
question01.Buttons <- question01 %>% filter(AOI.hit..P02.TOI.Q17.Act01.Snap...Q01.Buttons.=="1")
fixations.Buttons.Q01 <- sum(question01$AOI.hit..P02.TOI.Q17.Act01.Snap...Q01.Buttons.)
perc.fixations.Buttons.Q01 <- fixations.Buttons.Q01 / totalFixations.Q01
time.Buttons.Q01 <- sum(question01.Buttons$Gaze.event.duration..ms.)
perc.time.Buttons.Q01 <- time.Buttons.Q01 / totalFixationTime.Q01


# AOI FeatureModel
question01.FM <- question01 %>% filter(AOI.hit..P02.TOI.Q17.Act01.Snap...Q01.FMAOI.=="1")
fixations.FM.Q01 <- sum(question01$AOI.hit..P02.TOI.Q17.Act01.Snap...Q01.FMAOI.)
perc.fixations.FM.Q01 <- fixations.FM.Q01 / totalFixations.Q01
time.FM.Q01 <- sum(question01.FM$Gaze.event.duration..ms.)
perc.time.FM.Q01 <- time.FM.Q01 / totalFixationTime.Q01


# AOI Containing
question01.Containing <- question01 %>% 
  filter(  AOI.hit..P02.TOI.Q17.Act01.Snap...Q01.CAOI.3.=="1" |	      
  AOI.hit..P02.TOI.Q17.Act01.Snap...Q01.CAOI.4.=="1" |	      
  AOI.hit..P02.TOI.Q17.Act01.Snap...Q01.CAOI.5.=="1") 
 

if (nrow(question01.Containing) !=0) {
fixations.Containing.Q01 <- sum(question01.Containing %>% select(contains("CAOI")))
perc.fixations.Containing.Q01 <- fixations.Containing.Q01 / totalFixations.Q01
time.Containing.Q01 <- sum(question01.Containing$Gaze.event.duration..ms.)
perc.time.Containing.Q01 <- time.Containing.Q01 / totalFixationTime.Q01
} else {
fixations.Containing.Q01 <- 0
perc.fixations.Containing.Q01 <- 0
time.Containing.Q01 <- 0
perc.time.Containing.Q01 <- 0
}


# AOI Navigating
question01.Navigating <- question01 %>% 
  filter(  AOI.hit..P02.TOI.Q17.Act01.Snap...Q01.NAOI.1.=="1" | 	      
  AOI.hit..P02.TOI.Q17.Act01.Snap...Q01.NAOI.2.=="1")
    
      
if (nrow(question01.Navigating) !=0) {
 fixations.Navigating.Q01 <- sum(question01.Navigating %>% select(contains("NAOI")))
 perc.fixations.Navigating.Q01 <- fixations.Navigating.Q01 / totalFixations.Q01
 time.Navigating.Q01 <- sum(question01.Navigating$Gaze.event.duration..ms.)
 perc.time.Navigating.Q01 <- time.Navigating.Q01 / totalFixationTime.Q01
} else {
  fixations.Navigating.Q01 <- 0
  perc.fixations.Navigating.Q01 <- 0
  time.Navigating.Q01 <- 0
  perc.time.Navigating.Q01 <- 0 
}



fixations.CTC.Q01 <- 0
perc.fixations.CTC.Q01 <- 0
time.CTC.Q01 <- 0
perc.time.CTC.Q01 <- 0

# AOI Intersection Containing and Navigating when FM < (Containing + Navigating)
print(c(fixations.FM.Q01, fixations.Containing.Q01, fixations.Navigating.Q01))
print(c(fixations.Window.Q01, fixations.Question.Q01, fixations.Answer.Q01, fixations.Legend.Q01, fixations.Buttons.Q01,
        fixations.CTC.Q01))



# Creating the table now

ParticipantID <- 2
QNumber <- 1

Q01.data <- c(ParticipantID, QNumber, totalFixations.Q01, totalFixationTime.Q01, 
              fixations.Question.Q01, perc.fixations.Question.Q01, time.Question.Q01, perc.time.Question.Q01,
              fixations.Answer.Q01, perc.fixations.Answer.Q01, time.Answer.Q01, perc.time.Answer.Q01,
              fixations.Legend.Q01, perc.fixations.Legend.Q01, time.Legend.Q01, perc.time.Legend.Q01,
              fixations.Buttons.Q01, perc.fixations.Buttons.Q01, time.Buttons.Q01, perc.time.Buttons.Q01,
              fixations.FM.Q01, perc.fixations.FM.Q01, time.FM.Q01, perc.time.FM.Q01,
              fixations.Containing.Q01, perc.fixations.Containing.Q01, time.Containing.Q01, perc.time.Containing.Q01,
              fixations.Navigating.Q01, perc.fixations.Navigating.Q01, time.Navigating.Q01, perc.time.Navigating.Q01,
              fixations.CTC.Q01, perc.fixations.CTC.Q01, time.CTC.Q01, perc.time.CTC.Q01)  

 


# --------------------------------------------------------------------------------------------------
# Question 2



participant02.Q02 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP02/P02-TOI-Q06-Act02-Data.csv", header=TRUE)
attach (participant02.Q02)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated02.Q02 <- participant02.Q02 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
  distinct()


question02 <- curated02.Q02

# Totals
totalFixations.Q02 <- nrow(question02)
totalFixationTime.Q02 <- sum(question02$Gaze.event.duration..ms.)


# AOI Window - QNN for participant question
question02.Window <- question02 %>% filter(AOI.hit..P02.TOI.Q06.Act02.Snap...Q02.Window. =="1")
fixations.Window.Q02 <-  sum(question02$AOI.hit..P02.TOI.Q06.Act02.Snap...Q02.Window.)
perc.fixations.Window.Q02 <- fixations.Window.Q02 / totalFixations.Q02
perc.time.Window.Q02 <- sum(question02.Window$Gaze.event.duration..ms.)/totalFixationTime.Q02


# AOI Question
question02.Question <- question02 %>% filter(AOI.hit..P02.TOI.Q06.Act02.Snap...Q02.Question.=="1")
fixations.Question.Q02 <- sum(question02$AOI.hit..P02.TOI.Q06.Act02.Snap...Q02.Question.)
perc.fixations.Question.Q02 <- fixations.Question.Q02 / totalFixations.Q02
time.Question.Q02 <- sum(question02.Question$Gaze.event.duration..ms.)
perc.time.Question.Q02 <- time.Question.Q02 / totalFixationTime.Q02


# AOI Answer
question02.Answer <- question02 %>% filter(AOI.hit..P02.TOI.Q06.Act02.Snap...Q02.Answer.=="1")
fixations.Answer.Q02 <- sum(question02$AOI.hit..P02.TOI.Q06.Act02.Snap...Q02.Answer.)
perc.fixations.Answer.Q02 <- fixations.Answer.Q02 / totalFixations.Q02
time.Answer.Q02 <- sum(question02.Answer$Gaze.event.duration..ms.)
perc.time.Answer.Q02 <- time.Answer.Q02 / totalFixationTime.Q02

# AOI Legend
question02.Legend <- question02 %>% filter(AOI.hit..P02.TOI.Q06.Act02.Snap...Q02.Legend.=="1")
fixations.Legend.Q02 <- sum(question02$AOI.hit..P02.TOI.Q06.Act02.Snap...Q02.Legend.)
perc.fixations.Legend.Q02 <- fixations.Legend.Q02 / totalFixations.Q02
time.Legend.Q02 <- sum(question02.Legend$Gaze.event.duration..ms.)
perc.time.Legend.Q02 <- time.Legend.Q02 / totalFixationTime.Q02

# AOI Buttons
question02.Buttons <- question02 %>% filter(AOI.hit..P02.TOI.Q06.Act02.Snap...Q02.Buttons.=="1")
fixations.Buttons.Q02 <- sum(question02$AOI.hit..P02.TOI.Q06.Act02.Snap...Q02.Buttons.)
perc.fixations.Buttons.Q02 <- fixations.Buttons.Q02 / totalFixations.Q02
time.Buttons.Q02 <- sum(question02.Buttons$Gaze.event.duration..ms.)
perc.time.Buttons.Q02 <- time.Buttons.Q02 / totalFixationTime.Q02


# AOI FeatureModel
question02.FM <- question02 %>% filter(AOI.hit..P02.TOI.Q06.Act02.Snap...Q02.FMAOI.=="1")
fixations.FM.Q02 <- sum(question02$AOI.hit..P02.TOI.Q06.Act02.Snap...Q02.FMAOI.)
perc.fixations.FM.Q02 <- fixations.FM.Q02 / totalFixations.Q02
time.FM.Q02 <- sum(question02.FM$Gaze.event.duration..ms.)
perc.time.FM.Q02 <- time.FM.Q02 / totalFixationTime.Q02


# AOI Containing
question02.Containing <- question02 %>% 
  filter(  AOI.hit..P02.TOI.Q06.Act02.Snap...Q02.CAOI.2.=="1" |	      
  AOI.hit..P02.TOI.Q06.Act02.Snap...Q02.CAOI.3.=="1") 
 

if (nrow(question02.Containing) !=0) {
fixations.Containing.Q02 <- sum(question02.Containing %>% select(contains("CAOI")))
perc.fixations.Containing.Q02 <- fixations.Containing.Q02 / totalFixations.Q02
time.Containing.Q02 <- sum(question02.Containing$Gaze.event.duration..ms.)
perc.time.Containing.Q02 <- time.Containing.Q02 / totalFixationTime.Q02
} else {
fixations.Containing.Q02 <- 0
perc.fixations.Containing.Q02 <- 0
time.Containing.Q02 <- 0
perc.time.Containing.Q02 <- 0
}


# AOI Navigating
question02.Navigating <- question02 %>% 
  filter(  AOI.hit..P02.TOI.Q06.Act02.Snap...Q02.NAOI.1.=="1")
    
      
if (nrow(question02.Navigating) !=0) {
 fixations.Navigating.Q02 <- sum(question02.Navigating %>% select(contains("NAOI")))
 perc.fixations.Navigating.Q02 <- fixations.Navigating.Q02 / totalFixations.Q02
 time.Navigating.Q02 <- sum(question02.Navigating$Gaze.event.duration..ms.)
 perc.time.Navigating.Q02 <- time.Navigating.Q02 / totalFixationTime.Q02
} else {
  fixations.Navigating.Q02 <- 0
  perc.fixations.Navigating.Q02 <- 0
  time.Navigating.Q02 <- 0
  perc.time.Navigating.Q02 <- 0 
}



fixations.CTC.Q02 <- 0
perc.fixations.CTC.Q02 <- 0
time.CTC.Q02 <- 0
perc.time.CTC.Q02 <- 0

# AOI Intersection Containing and Navigating when FM < (Containing + Navigating)
print(c(fixations.FM.Q02, fixations.Containing.Q02, fixations.Navigating.Q02))
print(c(fixations.Window.Q02, fixations.Question.Q02, fixations.Answer.Q02, fixations.Legend.Q02, fixations.Buttons.Q02,
        fixations.CTC.Q02))



# Creating the table now

ParticipantID <- 2
QNumber <- 2

Q02.data <- c(ParticipantID, QNumber, totalFixations.Q02, totalFixationTime.Q02, 
              fixations.Question.Q02, perc.fixations.Question.Q02, time.Question.Q02, perc.time.Question.Q02,
              fixations.Answer.Q02, perc.fixations.Answer.Q02, time.Answer.Q02, perc.time.Answer.Q02,
              fixations.Legend.Q02, perc.fixations.Legend.Q02, time.Legend.Q02, perc.time.Legend.Q02,
              fixations.Buttons.Q02, perc.fixations.Buttons.Q02, time.Buttons.Q02, perc.time.Buttons.Q02,
              fixations.FM.Q02, perc.fixations.FM.Q02, time.FM.Q02, perc.time.FM.Q02,
              fixations.Containing.Q02, perc.fixations.Containing.Q02, time.Containing.Q02, perc.time.Containing.Q02,
              fixations.Navigating.Q02, perc.fixations.Navigating.Q02, time.Navigating.Q02, perc.time.Navigating.Q02,
              fixations.CTC.Q02, perc.fixations.CTC.Q02, time.CTC.Q02, perc.time.CTC.Q02)  

 


# --------------------------------------------------------------------------------------------------
# Question 3



participant02.Q03 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP02/P02-TOI-Q22-Act03-Data.csv", header=TRUE)
attach (participant02.Q03)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated02.Q03 <- participant02.Q03 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
  distinct()


question03 <- curated02.Q03

# Totals
totalFixations.Q03 <- nrow(question03)
totalFixationTime.Q03 <- sum(question03$Gaze.event.duration..ms.)


# AOI Window - QNN for participant question
question03.Window <- question03 %>% filter(AOI.hit..P02.TOI.Q22.Act03.Snap...Q03.Window. =="1")
fixations.Window.Q03 <-  sum(question03$AOI.hit..P02.TOI.Q22.Act03.Snap...Q03.Window.)
perc.fixations.Window.Q03 <- fixations.Window.Q03 / totalFixations.Q03
perc.time.Window.Q03 <- sum(question03.Window$Gaze.event.duration..ms.)/totalFixationTime.Q03


# AOI Question
question03.Question <- question03 %>% filter(AOI.hit..P02.TOI.Q22.Act03.Snap...Q03.Question.=="1")
fixations.Question.Q03 <- sum(question03$AOI.hit..P02.TOI.Q22.Act03.Snap...Q03.Question.)
perc.fixations.Question.Q03 <- fixations.Question.Q03 / totalFixations.Q03
time.Question.Q03 <- sum(question03.Question$Gaze.event.duration..ms.)
perc.time.Question.Q03 <- time.Question.Q03 / totalFixationTime.Q03


# AOI Answer
question03.Answer <- question03 %>% filter(AOI.hit..P02.TOI.Q22.Act03.Snap...Q03.Answer.=="1")
fixations.Answer.Q03 <- sum(question03$AOI.hit..P02.TOI.Q22.Act03.Snap...Q03.Answer.)
perc.fixations.Answer.Q03 <- fixations.Answer.Q03 / totalFixations.Q03
time.Answer.Q03 <- sum(question03.Answer$Gaze.event.duration..ms.)
perc.time.Answer.Q03 <- time.Answer.Q03 / totalFixationTime.Q03

# AOI Legend
question03.Legend <- question03 %>% filter(AOI.hit..P02.TOI.Q22.Act03.Snap...Q03.Legend.=="1")
fixations.Legend.Q03 <- sum(question03$AOI.hit..P02.TOI.Q22.Act03.Snap...Q03.Legend.)
perc.fixations.Legend.Q03 <- fixations.Legend.Q03 / totalFixations.Q03
time.Legend.Q03 <- sum(question03.Legend$Gaze.event.duration..ms.)
perc.time.Legend.Q03 <- time.Legend.Q03 / totalFixationTime.Q03

# AOI Buttons
question03.Buttons <- question03 %>% filter(AOI.hit..P02.TOI.Q22.Act03.Snap...Q03.Buttons.=="1")
fixations.Buttons.Q03 <- sum(question03$AOI.hit..P02.TOI.Q22.Act03.Snap...Q03.Buttons.)
perc.fixations.Buttons.Q03 <- fixations.Buttons.Q03 / totalFixations.Q03
time.Buttons.Q03 <- sum(question03.Buttons$Gaze.event.duration..ms.)
perc.time.Buttons.Q03 <- time.Buttons.Q03 / totalFixationTime.Q03


# AOI FeatureModel
question03.FM <- question03 %>% filter(AOI.hit..P02.TOI.Q22.Act03.Snap...Q03.FMAOI.=="1")
fixations.FM.Q03 <- sum(question03$AOI.hit..P02.TOI.Q22.Act03.Snap...Q03.FMAOI.)
perc.fixations.FM.Q03 <- fixations.FM.Q03 / totalFixations.Q03
time.FM.Q03 <- sum(question03.FM$Gaze.event.duration..ms.)
perc.time.FM.Q03 <- time.FM.Q03 / totalFixationTime.Q03


# AOI Containing
question03.Containing <- question03 %>% 
  filter(  AOI.hit..P02.TOI.Q22.Act03.Snap...Q03.CAOI.1.=="1" |	      
  AOI.hit..P02.TOI.Q22.Act03.Snap...Q03.CAOI.2.=="1" |	      
  AOI.hit..P02.TOI.Q22.Act03.Snap...Q03.CAOI.3.=="1" |	      
  AOI.hit..P02.TOI.Q22.Act03.Snap...Q03.CAOI.4.=="1") 
 

if (nrow(question03.Containing) !=0) {
fixations.Containing.Q03 <- sum(question03.Containing %>% select(contains("CAOI")))
perc.fixations.Containing.Q03 <- fixations.Containing.Q03 / totalFixations.Q03
time.Containing.Q03 <- sum(question03.Containing$Gaze.event.duration..ms.)
perc.time.Containing.Q03 <- time.Containing.Q03 / totalFixationTime.Q03
} else {
fixations.Containing.Q03 <- 0
perc.fixations.Containing.Q03 <- 0
time.Containing.Q03 <- 0
perc.time.Containing.Q03 <- 0
}

fixations.Navigating.Q03 <- 0
perc.fixations.Navigating.Q03 <- 0
time.Navigating.Q03 <- 0
perc.time.Navigating.Q03 <- 0


# AOI CTC
question03.CTC <- question03 %>% filter(AOI.hit..P02.TOI.Q22.Act03.Snap...Q03.CTC.=="1")
fixations.CTC.Q03 <- sum(question03$AOI.hit..P02.TOI.Q22.Act03.Snap...Q03.CTC.)
perc.fixations.CTC.Q03 <- fixations.CTC.Q03 / totalFixations.Q03
time.CTC.Q03 <- sum(question03.CTC$Gaze.event.duration..ms.)
perc.time.CTC.Q03 <- time.CTC.Q03 / totalFixationTime.Q03

# AOI Intersection Containing and Navigating when FM < (Containing + Navigating)
print(c(fixations.FM.Q03, fixations.Containing.Q03, fixations.Navigating.Q03))
print(c(fixations.Window.Q03, fixations.Question.Q03, fixations.Answer.Q03, fixations.Legend.Q03, fixations.Buttons.Q03,
        fixations.CTC.Q03))



# Creating the table now

ParticipantID <- 2
QNumber <- 3

Q03.data <- c(ParticipantID, QNumber, totalFixations.Q03, totalFixationTime.Q03, 
              fixations.Question.Q03, perc.fixations.Question.Q03, time.Question.Q03, perc.time.Question.Q03,
              fixations.Answer.Q03, perc.fixations.Answer.Q03, time.Answer.Q03, perc.time.Answer.Q03,
              fixations.Legend.Q03, perc.fixations.Legend.Q03, time.Legend.Q03, perc.time.Legend.Q03,
              fixations.Buttons.Q03, perc.fixations.Buttons.Q03, time.Buttons.Q03, perc.time.Buttons.Q03,
              fixations.FM.Q03, perc.fixations.FM.Q03, time.FM.Q03, perc.time.FM.Q03,
              fixations.Containing.Q03, perc.fixations.Containing.Q03, time.Containing.Q03, perc.time.Containing.Q03,
              fixations.Navigating.Q03, perc.fixations.Navigating.Q03, time.Navigating.Q03, perc.time.Navigating.Q03,
              fixations.CTC.Q03, perc.fixations.CTC.Q03, time.CTC.Q03, perc.time.CTC.Q03)  

 


# --------------------------------------------------------------------------------------------------
# Question 4



participant02.Q04 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP02/P02-TOI-Q03-Act04-Data.csv", header=TRUE)
attach (participant02.Q04)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated02.Q04 <- participant02.Q04 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
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
# Question 5



participant02.Q05 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP02/P02-TOI-Q20-Act05-Data.csv", header=TRUE)
attach (participant02.Q05)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated02.Q05 <- participant02.Q05 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
  distinct()


question05 <- curated02.Q05

# Totals
totalFixations.Q05 <- nrow(question05)
totalFixationTime.Q05 <- sum(question05$Gaze.event.duration..ms.)


# AOI Window - QNN for participant question
question05.Window <- question05 %>% filter(AOI.hit..P02.TOI.Q20.Act05.Snap...Q05.Window. =="1")
fixations.Window.Q05 <-  sum(question05$AOI.hit..P02.TOI.Q20.Act05.Snap...Q05.Window.)
perc.fixations.Window.Q05 <- fixations.Window.Q05 / totalFixations.Q05
perc.time.Window.Q05 <- sum(question05.Window$Gaze.event.duration..ms.)/totalFixationTime.Q05


# AOI Question
question05.Question <- question05 %>% filter(AOI.hit..P02.TOI.Q20.Act05.Snap...Q05.Question.=="1")
fixations.Question.Q05 <- sum(question05$AOI.hit..P02.TOI.Q20.Act05.Snap...Q05.Question.)
perc.fixations.Question.Q05 <- fixations.Question.Q05 / totalFixations.Q05
time.Question.Q05 <- sum(question05.Question$Gaze.event.duration..ms.)
perc.time.Question.Q05 <- time.Question.Q05 / totalFixationTime.Q05


# AOI Answer
question05.Answer <- question05 %>% filter(AOI.hit..P02.TOI.Q20.Act05.Snap...Q05.Answer.=="1")
fixations.Answer.Q05 <- sum(question05$AOI.hit..P02.TOI.Q20.Act05.Snap...Q05.Answer.)
perc.fixations.Answer.Q05 <- fixations.Answer.Q05 / totalFixations.Q05
time.Answer.Q05 <- sum(question05.Answer$Gaze.event.duration..ms.)
perc.time.Answer.Q05 <- time.Answer.Q05 / totalFixationTime.Q05

# AOI Legend
question05.Legend <- question05 %>% filter(AOI.hit..P02.TOI.Q20.Act05.Snap...Q05.Legend.=="1")
fixations.Legend.Q05 <- sum(question05$AOI.hit..P02.TOI.Q20.Act05.Snap...Q05.Legend.)
perc.fixations.Legend.Q05 <- fixations.Legend.Q05 / totalFixations.Q05
time.Legend.Q05 <- sum(question05.Legend$Gaze.event.duration..ms.)
perc.time.Legend.Q05 <- time.Legend.Q05 / totalFixationTime.Q05

# AOI Buttons
question05.Buttons <- question05 %>% filter(AOI.hit..P02.TOI.Q20.Act05.Snap...Q05.Buttons.=="1")
fixations.Buttons.Q05 <- sum(question05$AOI.hit..P02.TOI.Q20.Act05.Snap...Q05.Buttons.)
perc.fixations.Buttons.Q05 <- fixations.Buttons.Q05 / totalFixations.Q05
time.Buttons.Q05 <- sum(question05.Buttons$Gaze.event.duration..ms.)
perc.time.Buttons.Q05 <- time.Buttons.Q05 / totalFixationTime.Q05


# AOI FeatureModel
question05.FM <- question05 %>% filter(AOI.hit..P02.TOI.Q20.Act05.Snap...Q05.FMAOI.=="1")
fixations.FM.Q05 <- sum(question05$AOI.hit..P02.TOI.Q20.Act05.Snap...Q05.FMAOI.)
perc.fixations.FM.Q05 <- fixations.FM.Q05 / totalFixations.Q05
time.FM.Q05 <- sum(question05.FM$Gaze.event.duration..ms.)
perc.time.FM.Q05 <- time.FM.Q05 / totalFixationTime.Q05


# AOI Containing
question05.Containing <- question05 %>% 
  filter(  AOI.hit..P02.TOI.Q20.Act05.Snap...Q05.CAOI.1.=="1" |	      
  AOI.hit..P02.TOI.Q20.Act05.Snap...Q05.CAOI.2.=="1" |	      
  AOI.hit..P02.TOI.Q20.Act05.Snap...Q05.CAOI.3.=="1") 
 

if (nrow(question05.Containing) !=0) {
fixations.Containing.Q05 <- sum(question05.Containing %>% select(contains("CAOI")))
perc.fixations.Containing.Q05 <- fixations.Containing.Q05 / totalFixations.Q05
time.Containing.Q05 <- sum(question05.Containing$Gaze.event.duration..ms.)
perc.time.Containing.Q05 <- time.Containing.Q05 / totalFixationTime.Q05
} else {
fixations.Containing.Q05 <- 0
perc.fixations.Containing.Q05 <- 0
time.Containing.Q05 <- 0
perc.time.Containing.Q05 <- 0
}

fixations.Navigating.Q05 <- 0
perc.fixations.Navigating.Q05 <- 0
time.Navigating.Q05 <- 0
perc.time.Navigating.Q05 <- 0


# AOI CTC
question05.CTC <- question05 %>% filter(AOI.hit..P02.TOI.Q20.Act05.Snap...Q05.CTC.=="1")
fixations.CTC.Q05 <- sum(question05$AOI.hit..P02.TOI.Q20.Act05.Snap...Q05.CTC.)
perc.fixations.CTC.Q05 <- fixations.CTC.Q05 / totalFixations.Q05
time.CTC.Q05 <- sum(question05.CTC$Gaze.event.duration..ms.)
perc.time.CTC.Q05 <- time.CTC.Q05 / totalFixationTime.Q05

# AOI Intersection Containing and Navigating when FM < (Containing + Navigating)
print(c(fixations.FM.Q05, fixations.Containing.Q05, fixations.Navigating.Q05))
print(c(fixations.Window.Q05, fixations.Question.Q05, fixations.Answer.Q05, fixations.Legend.Q05, fixations.Buttons.Q05,
        fixations.CTC.Q05))



# Creating the table now

ParticipantID <- 2
QNumber <- 5

Q05.data <- c(ParticipantID, QNumber, totalFixations.Q05, totalFixationTime.Q05, 
              fixations.Question.Q05, perc.fixations.Question.Q05, time.Question.Q05, perc.time.Question.Q05,
              fixations.Answer.Q05, perc.fixations.Answer.Q05, time.Answer.Q05, perc.time.Answer.Q05,
              fixations.Legend.Q05, perc.fixations.Legend.Q05, time.Legend.Q05, perc.time.Legend.Q05,
              fixations.Buttons.Q05, perc.fixations.Buttons.Q05, time.Buttons.Q05, perc.time.Buttons.Q05,
              fixations.FM.Q05, perc.fixations.FM.Q05, time.FM.Q05, perc.time.FM.Q05,
              fixations.Containing.Q05, perc.fixations.Containing.Q05, time.Containing.Q05, perc.time.Containing.Q05,
              fixations.Navigating.Q05, perc.fixations.Navigating.Q05, time.Navigating.Q05, perc.time.Navigating.Q05,
              fixations.CTC.Q05, perc.fixations.CTC.Q05, time.CTC.Q05, perc.time.CTC.Q05)  

 


# --------------------------------------------------------------------------------------------------
# Question 6



participant02.Q06 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP02/P02-TOI-Q23-Act06-Data.csv", header=TRUE)
attach (participant02.Q06)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated02.Q06 <- participant02.Q06 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
  distinct()


question06 <- curated02.Q06

# Totals
totalFixations.Q06 <- nrow(question06)
totalFixationTime.Q06 <- sum(question06$Gaze.event.duration..ms.)


# AOI Window - QNN for participant question
question06.Window <- question06 %>% filter(AOI.hit..P02.TOI.Q23.Act06.Snap...Q06.Window. =="1")
fixations.Window.Q06 <-  sum(question06$AOI.hit..P02.TOI.Q23.Act06.Snap...Q06.Window.)
perc.fixations.Window.Q06 <- fixations.Window.Q06 / totalFixations.Q06
perc.time.Window.Q06 <- sum(question06.Window$Gaze.event.duration..ms.)/totalFixationTime.Q06


# AOI Question
question06.Question <- question06 %>% filter(AOI.hit..P02.TOI.Q23.Act06.Snap...Q06.Question.=="1")
fixations.Question.Q06 <- sum(question06$AOI.hit..P02.TOI.Q23.Act06.Snap...Q06.Question.)
perc.fixations.Question.Q06 <- fixations.Question.Q06 / totalFixations.Q06
time.Question.Q06 <- sum(question06.Question$Gaze.event.duration..ms.)
perc.time.Question.Q06 <- time.Question.Q06 / totalFixationTime.Q06


# AOI Answer
question06.Answer <- question06 %>% filter(AOI.hit..P02.TOI.Q23.Act06.Snap...Q06.Answer.=="1")
fixations.Answer.Q06 <- sum(question06$AOI.hit..P02.TOI.Q23.Act06.Snap...Q06.Answer.)
perc.fixations.Answer.Q06 <- fixations.Answer.Q06 / totalFixations.Q06
time.Answer.Q06 <- sum(question06.Answer$Gaze.event.duration..ms.)
perc.time.Answer.Q06 <- time.Answer.Q06 / totalFixationTime.Q06

# AOI Legend
question06.Legend <- question06 %>% filter(AOI.hit..P02.TOI.Q23.Act06.Snap...Q06.Legend.=="1")
fixations.Legend.Q06 <- sum(question06$AOI.hit..P02.TOI.Q23.Act06.Snap...Q06.Legend.)
perc.fixations.Legend.Q06 <- fixations.Legend.Q06 / totalFixations.Q06
time.Legend.Q06 <- sum(question06.Legend$Gaze.event.duration..ms.)
perc.time.Legend.Q06 <- time.Legend.Q06 / totalFixationTime.Q06

# AOI Buttons
question06.Buttons <- question06 %>% filter(AOI.hit..P02.TOI.Q23.Act06.Snap...Q06.Buttons.=="1")
fixations.Buttons.Q06 <- sum(question06$AOI.hit..P02.TOI.Q23.Act06.Snap...Q06.Buttons.)
perc.fixations.Buttons.Q06 <- fixations.Buttons.Q06 / totalFixations.Q06
time.Buttons.Q06 <- sum(question06.Buttons$Gaze.event.duration..ms.)
perc.time.Buttons.Q06 <- time.Buttons.Q06 / totalFixationTime.Q06


# AOI FeatureModel
question06.FM <- question06 %>% filter(AOI.hit..P02.TOI.Q23.Act06.Snap...Q06.FMAOI.=="1")
fixations.FM.Q06 <- sum(question06$AOI.hit..P02.TOI.Q23.Act06.Snap...Q06.FMAOI.)
perc.fixations.FM.Q06 <- fixations.FM.Q06 / totalFixations.Q06
time.FM.Q06 <- sum(question06.FM$Gaze.event.duration..ms.)
perc.time.FM.Q06 <- time.FM.Q06 / totalFixationTime.Q06


# AOI Containing
question06.Containing <- question06 %>% 
  filter(  AOI.hit..P02.TOI.Q23.Act06.Snap...Q06.CAOI.2.=="1" |	      
  AOI.hit..P02.TOI.Q23.Act06.Snap...Q06.CAOI.3.=="1" |	      
  AOI.hit..P02.TOI.Q23.Act06.Snap...Q06.CAOI.4.=="1") 
 

if (nrow(question06.Containing) !=0) {
fixations.Containing.Q06 <- sum(question06.Containing %>% select(contains("CAOI")))
perc.fixations.Containing.Q06 <- fixations.Containing.Q06 / totalFixations.Q06
time.Containing.Q06 <- sum(question06.Containing$Gaze.event.duration..ms.)
perc.time.Containing.Q06 <- time.Containing.Q06 / totalFixationTime.Q06
} else {
fixations.Containing.Q06 <- 0
perc.fixations.Containing.Q06 <- 0
time.Containing.Q06 <- 0
perc.time.Containing.Q06 <- 0
}


# AOI Navigating
question06.Navigating <- question06 %>% 
  filter(  AOI.hit..P02.TOI.Q23.Act06.Snap...Q06.NAOI.1.=="1")
    
      
if (nrow(question06.Navigating) !=0) {
 fixations.Navigating.Q06 <- sum(question06.Navigating %>% select(contains("NAOI")))
 perc.fixations.Navigating.Q06 <- fixations.Navigating.Q06 / totalFixations.Q06
 time.Navigating.Q06 <- sum(question06.Navigating$Gaze.event.duration..ms.)
 perc.time.Navigating.Q06 <- time.Navigating.Q06 / totalFixationTime.Q06
} else {
  fixations.Navigating.Q06 <- 0
  perc.fixations.Navigating.Q06 <- 0
  time.Navigating.Q06 <- 0
  perc.time.Navigating.Q06 <- 0 
}



# AOI CTC
question06.CTC <- question06 %>% filter(AOI.hit..P02.TOI.Q23.Act06.Snap...Q06.CTC.=="1")
fixations.CTC.Q06 <- sum(question06$AOI.hit..P02.TOI.Q23.Act06.Snap...Q06.CTC.)
perc.fixations.CTC.Q06 <- fixations.CTC.Q06 / totalFixations.Q06
time.CTC.Q06 <- sum(question06.CTC$Gaze.event.duration..ms.)
perc.time.CTC.Q06 <- time.CTC.Q06 / totalFixationTime.Q06

# AOI Intersection Containing and Navigating when FM < (Containing + Navigating)
print(c(fixations.FM.Q06, fixations.Containing.Q06, fixations.Navigating.Q06))
print(c(fixations.Window.Q06, fixations.Question.Q06, fixations.Answer.Q06, fixations.Legend.Q06, fixations.Buttons.Q06,
        fixations.CTC.Q06))



# Creating the table now

ParticipantID <- 2
QNumber <- 6

Q06.data <- c(ParticipantID, QNumber, totalFixations.Q06, totalFixationTime.Q06, 
              fixations.Question.Q06, perc.fixations.Question.Q06, time.Question.Q06, perc.time.Question.Q06,
              fixations.Answer.Q06, perc.fixations.Answer.Q06, time.Answer.Q06, perc.time.Answer.Q06,
              fixations.Legend.Q06, perc.fixations.Legend.Q06, time.Legend.Q06, perc.time.Legend.Q06,
              fixations.Buttons.Q06, perc.fixations.Buttons.Q06, time.Buttons.Q06, perc.time.Buttons.Q06,
              fixations.FM.Q06, perc.fixations.FM.Q06, time.FM.Q06, perc.time.FM.Q06,
              fixations.Containing.Q06, perc.fixations.Containing.Q06, time.Containing.Q06, perc.time.Containing.Q06,
              fixations.Navigating.Q06, perc.fixations.Navigating.Q06, time.Navigating.Q06, perc.time.Navigating.Q06,
              fixations.CTC.Q06, perc.fixations.CTC.Q06, time.CTC.Q06, perc.time.CTC.Q06)  

 


# --------------------------------------------------------------------------------------------------
# Question 7



participant02.Q07 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP02/P02-TOI-Q02-Act07-Data.csv", header=TRUE)
attach (participant02.Q07)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated02.Q07 <- participant02.Q07 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
  distinct()


question07 <- curated02.Q07

# Totals
totalFixations.Q07 <- nrow(question07)
totalFixationTime.Q07 <- sum(question07$Gaze.event.duration..ms.)


# AOI Window - QNN for participant question
question07.Window <- question07 %>% filter(AOI.hit..P02.TOI.Q02.Act07.Snap...Q07.Window. =="1")
fixations.Window.Q07 <-  sum(question07$AOI.hit..P02.TOI.Q02.Act07.Snap...Q07.Window.)
perc.fixations.Window.Q07 <- fixations.Window.Q07 / totalFixations.Q07
perc.time.Window.Q07 <- sum(question07.Window$Gaze.event.duration..ms.)/totalFixationTime.Q07


# AOI Question
question07.Question <- question07 %>% filter(AOI.hit..P02.TOI.Q02.Act07.Snap...Q07.Question.=="1")
fixations.Question.Q07 <- sum(question07$AOI.hit..P02.TOI.Q02.Act07.Snap...Q07.Question.)
perc.fixations.Question.Q07 <- fixations.Question.Q07 / totalFixations.Q07
time.Question.Q07 <- sum(question07.Question$Gaze.event.duration..ms.)
perc.time.Question.Q07 <- time.Question.Q07 / totalFixationTime.Q07


# AOI Answer
question07.Answer <- question07 %>% filter(AOI.hit..P02.TOI.Q02.Act07.Snap...Q07.Answer.=="1")
fixations.Answer.Q07 <- sum(question07$AOI.hit..P02.TOI.Q02.Act07.Snap...Q07.Answer.)
perc.fixations.Answer.Q07 <- fixations.Answer.Q07 / totalFixations.Q07
time.Answer.Q07 <- sum(question07.Answer$Gaze.event.duration..ms.)
perc.time.Answer.Q07 <- time.Answer.Q07 / totalFixationTime.Q07

# AOI Legend
question07.Legend <- question07 %>% filter(AOI.hit..P02.TOI.Q02.Act07.Snap...Q07.Legend.=="1")
fixations.Legend.Q07 <- sum(question07$AOI.hit..P02.TOI.Q02.Act07.Snap...Q07.Legend.)
perc.fixations.Legend.Q07 <- fixations.Legend.Q07 / totalFixations.Q07
time.Legend.Q07 <- sum(question07.Legend$Gaze.event.duration..ms.)
perc.time.Legend.Q07 <- time.Legend.Q07 / totalFixationTime.Q07

# AOI Buttons
question07.Buttons <- question07 %>% filter(AOI.hit..P02.TOI.Q02.Act07.Snap...Q07.Buttons.=="1")
fixations.Buttons.Q07 <- sum(question07$AOI.hit..P02.TOI.Q02.Act07.Snap...Q07.Buttons.)
perc.fixations.Buttons.Q07 <- fixations.Buttons.Q07 / totalFixations.Q07
time.Buttons.Q07 <- sum(question07.Buttons$Gaze.event.duration..ms.)
perc.time.Buttons.Q07 <- time.Buttons.Q07 / totalFixationTime.Q07


# AOI FeatureModel
question07.FM <- question07 %>% filter(AOI.hit..P02.TOI.Q02.Act07.Snap...Q07.FMAOI.=="1")
fixations.FM.Q07 <- sum(question07$AOI.hit..P02.TOI.Q02.Act07.Snap...Q07.FMAOI.)
perc.fixations.FM.Q07 <- fixations.FM.Q07 / totalFixations.Q07
time.FM.Q07 <- sum(question07.FM$Gaze.event.duration..ms.)
perc.time.FM.Q07 <- time.FM.Q07 / totalFixationTime.Q07


# AOI Containing
question07.Containing <- question07 %>% 
  filter(  AOI.hit..P02.TOI.Q02.Act07.Snap...Q07.CAOI.1.=="1" |	      
  AOI.hit..P02.TOI.Q02.Act07.Snap...Q07.CAOI.2.=="1" |	      
  AOI.hit..P02.TOI.Q02.Act07.Snap...Q07.CAOI.3.=="1" |	      
  AOI.hit..P02.TOI.Q02.Act07.Snap...Q07.CAOI.4.=="1") 
 

if (nrow(question07.Containing) !=0) {
fixations.Containing.Q07 <- sum(question07.Containing %>% select(contains("CAOI")))
perc.fixations.Containing.Q07 <- fixations.Containing.Q07 / totalFixations.Q07
time.Containing.Q07 <- sum(question07.Containing$Gaze.event.duration..ms.)
perc.time.Containing.Q07 <- time.Containing.Q07 / totalFixationTime.Q07
} else {
fixations.Containing.Q07 <- 0
perc.fixations.Containing.Q07 <- 0
time.Containing.Q07 <- 0
perc.time.Containing.Q07 <- 0
}

fixations.Navigating.Q07 <- 0
perc.fixations.Navigating.Q07 <- 0
time.Navigating.Q07 <- 0
perc.time.Navigating.Q07 <- 0


fixations.CTC.Q07 <- 0
perc.fixations.CTC.Q07 <- 0
time.CTC.Q07 <- 0
perc.time.CTC.Q07 <- 0

# AOI Intersection Containing and Navigating when FM < (Containing + Navigating)
print(c(fixations.FM.Q07, fixations.Containing.Q07, fixations.Navigating.Q07))
print(c(fixations.Window.Q07, fixations.Question.Q07, fixations.Answer.Q07, fixations.Legend.Q07, fixations.Buttons.Q07,
        fixations.CTC.Q07))



# Creating the table now

ParticipantID <- 2
QNumber <- 7

Q07.data <- c(ParticipantID, QNumber, totalFixations.Q07, totalFixationTime.Q07, 
              fixations.Question.Q07, perc.fixations.Question.Q07, time.Question.Q07, perc.time.Question.Q07,
              fixations.Answer.Q07, perc.fixations.Answer.Q07, time.Answer.Q07, perc.time.Answer.Q07,
              fixations.Legend.Q07, perc.fixations.Legend.Q07, time.Legend.Q07, perc.time.Legend.Q07,
              fixations.Buttons.Q07, perc.fixations.Buttons.Q07, time.Buttons.Q07, perc.time.Buttons.Q07,
              fixations.FM.Q07, perc.fixations.FM.Q07, time.FM.Q07, perc.time.FM.Q07,
              fixations.Containing.Q07, perc.fixations.Containing.Q07, time.Containing.Q07, perc.time.Containing.Q07,
              fixations.Navigating.Q07, perc.fixations.Navigating.Q07, time.Navigating.Q07, perc.time.Navigating.Q07,
              fixations.CTC.Q07, perc.fixations.CTC.Q07, time.CTC.Q07, perc.time.CTC.Q07)  

 


# --------------------------------------------------------------------------------------------------
# Question 8



participant02.Q08 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP02/P02-TOI-Q18-Act08-Data.csv", header=TRUE)
attach (participant02.Q08)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated02.Q08 <- participant02.Q08 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
  distinct()


question08 <- curated02.Q08

# Totals
totalFixations.Q08 <- nrow(question08)
totalFixationTime.Q08 <- sum(question08$Gaze.event.duration..ms.)


# AOI Window - QNN for participant question
question08.Window <- question08 %>% filter(AOI.hit..P02.TOI.Q18.Act08.Snap...Q08.Window. =="1")
fixations.Window.Q08 <-  sum(question08$AOI.hit..P02.TOI.Q18.Act08.Snap...Q08.Window.)
perc.fixations.Window.Q08 <- fixations.Window.Q08 / totalFixations.Q08
perc.time.Window.Q08 <- sum(question08.Window$Gaze.event.duration..ms.)/totalFixationTime.Q08


# AOI Question
question08.Question <- question08 %>% filter(AOI.hit..P02.TOI.Q18.Act08.Snap...Q08.Question.=="1")
fixations.Question.Q08 <- sum(question08$AOI.hit..P02.TOI.Q18.Act08.Snap...Q08.Question.)
perc.fixations.Question.Q08 <- fixations.Question.Q08 / totalFixations.Q08
time.Question.Q08 <- sum(question08.Question$Gaze.event.duration..ms.)
perc.time.Question.Q08 <- time.Question.Q08 / totalFixationTime.Q08


# AOI Answer
question08.Answer <- question08 %>% filter(AOI.hit..P02.TOI.Q18.Act08.Snap...Q08.Answer.=="1")
fixations.Answer.Q08 <- sum(question08$AOI.hit..P02.TOI.Q18.Act08.Snap...Q08.Answer.)
perc.fixations.Answer.Q08 <- fixations.Answer.Q08 / totalFixations.Q08
time.Answer.Q08 <- sum(question08.Answer$Gaze.event.duration..ms.)
perc.time.Answer.Q08 <- time.Answer.Q08 / totalFixationTime.Q08

# AOI Legend
question08.Legend <- question08 %>% filter(AOI.hit..P02.TOI.Q18.Act08.Snap...Q08.Legend.=="1")
fixations.Legend.Q08 <- sum(question08$AOI.hit..P02.TOI.Q18.Act08.Snap...Q08.Legend.)
perc.fixations.Legend.Q08 <- fixations.Legend.Q08 / totalFixations.Q08
time.Legend.Q08 <- sum(question08.Legend$Gaze.event.duration..ms.)
perc.time.Legend.Q08 <- time.Legend.Q08 / totalFixationTime.Q08

# AOI Buttons
question08.Buttons <- question08 %>% filter(AOI.hit..P02.TOI.Q18.Act08.Snap...Q08.Buttons.=="1")
fixations.Buttons.Q08 <- sum(question08$AOI.hit..P02.TOI.Q18.Act08.Snap...Q08.Buttons.)
perc.fixations.Buttons.Q08 <- fixations.Buttons.Q08 / totalFixations.Q08
time.Buttons.Q08 <- sum(question08.Buttons$Gaze.event.duration..ms.)
perc.time.Buttons.Q08 <- time.Buttons.Q08 / totalFixationTime.Q08


# AOI FeatureModel
question08.FM <- question08 %>% filter(AOI.hit..P02.TOI.Q18.Act08.Snap...Q08.FMAOI.=="1")
fixations.FM.Q08 <- sum(question08$AOI.hit..P02.TOI.Q18.Act08.Snap...Q08.FMAOI.)
perc.fixations.FM.Q08 <- fixations.FM.Q08 / totalFixations.Q08
time.FM.Q08 <- sum(question08.FM$Gaze.event.duration..ms.)
perc.time.FM.Q08 <- time.FM.Q08 / totalFixationTime.Q08


# AOI Containing
question08.Containing <- question08 %>% 
  filter(  AOI.hit..P02.TOI.Q18.Act08.Snap...Q08.CAOI.4.=="1" |	      
  AOI.hit..P02.TOI.Q18.Act08.Snap...Q08.CAOI.5.=="1" |	      
  AOI.hit..P02.TOI.Q18.Act08.Snap...Q08.CAOI.6.=="1" |	      
  AOI.hit..P02.TOI.Q18.Act08.Snap...Q08.CAOI.7.=="1") 
 

if (nrow(question08.Containing) !=0) {
fixations.Containing.Q08 <- sum(question08.Containing %>% select(contains("CAOI")))
perc.fixations.Containing.Q08 <- fixations.Containing.Q08 / totalFixations.Q08
time.Containing.Q08 <- sum(question08.Containing$Gaze.event.duration..ms.)
perc.time.Containing.Q08 <- time.Containing.Q08 / totalFixationTime.Q08
} else {
fixations.Containing.Q08 <- 0
perc.fixations.Containing.Q08 <- 0
time.Containing.Q08 <- 0
perc.time.Containing.Q08 <- 0
}


# AOI Navigating
question08.Navigating <- question08 %>% 
  filter(  AOI.hit..P02.TOI.Q18.Act08.Snap...Q08.NAOI.1.=="1" | 	      
  AOI.hit..P02.TOI.Q18.Act08.Snap...Q08.NAOI.2.=="1" | 	      
  AOI.hit..P02.TOI.Q18.Act08.Snap...Q08.NAOI.3.=="1")
    
      
if (nrow(question08.Navigating) !=0) {
 fixations.Navigating.Q08 <- sum(question08.Navigating %>% select(contains("NAOI")))
 perc.fixations.Navigating.Q08 <- fixations.Navigating.Q08 / totalFixations.Q08
 time.Navigating.Q08 <- sum(question08.Navigating$Gaze.event.duration..ms.)
 perc.time.Navigating.Q08 <- time.Navigating.Q08 / totalFixationTime.Q08
} else {
  fixations.Navigating.Q08 <- 0
  perc.fixations.Navigating.Q08 <- 0
  time.Navigating.Q08 <- 0
  perc.time.Navigating.Q08 <- 0 
}



fixations.CTC.Q08 <- 0
perc.fixations.CTC.Q08 <- 0
time.CTC.Q08 <- 0
perc.time.CTC.Q08 <- 0

# AOI Intersection Containing and Navigating when FM < (Containing + Navigating)
print(c(fixations.FM.Q08, fixations.Containing.Q08, fixations.Navigating.Q08))
print(c(fixations.Window.Q08, fixations.Question.Q08, fixations.Answer.Q08, fixations.Legend.Q08, fixations.Buttons.Q08,
        fixations.CTC.Q08))



# Creating the table now

ParticipantID <- 2
QNumber <- 8

Q08.data <- c(ParticipantID, QNumber, totalFixations.Q08, totalFixationTime.Q08, 
              fixations.Question.Q08, perc.fixations.Question.Q08, time.Question.Q08, perc.time.Question.Q08,
              fixations.Answer.Q08, perc.fixations.Answer.Q08, time.Answer.Q08, perc.time.Answer.Q08,
              fixations.Legend.Q08, perc.fixations.Legend.Q08, time.Legend.Q08, perc.time.Legend.Q08,
              fixations.Buttons.Q08, perc.fixations.Buttons.Q08, time.Buttons.Q08, perc.time.Buttons.Q08,
              fixations.FM.Q08, perc.fixations.FM.Q08, time.FM.Q08, perc.time.FM.Q08,
              fixations.Containing.Q08, perc.fixations.Containing.Q08, time.Containing.Q08, perc.time.Containing.Q08,
              fixations.Navigating.Q08, perc.fixations.Navigating.Q08, time.Navigating.Q08, perc.time.Navigating.Q08,
              fixations.CTC.Q08, perc.fixations.CTC.Q08, time.CTC.Q08, perc.time.CTC.Q08)  

 


# --------------------------------------------------------------------------------------------------
# Question 9



participant02.Q09 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP02/P02-TOI-Q16-Act09-Data.csv", header=TRUE)
attach (participant02.Q09)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated02.Q09 <- participant02.Q09 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
  distinct()


question09 <- curated02.Q09

# Totals
totalFixations.Q09 <- nrow(question09)
totalFixationTime.Q09 <- sum(question09$Gaze.event.duration..ms.)


# AOI Window - QNN for participant question
question09.Window <- question09 %>% filter(AOI.hit..P02.TOI.Q16.Act09.Snap...Q09.Window. =="1")
fixations.Window.Q09 <-  sum(question09$AOI.hit..P02.TOI.Q16.Act09.Snap...Q09.Window.)
perc.fixations.Window.Q09 <- fixations.Window.Q09 / totalFixations.Q09
perc.time.Window.Q09 <- sum(question09.Window$Gaze.event.duration..ms.)/totalFixationTime.Q09


# AOI Question
question09.Question <- question09 %>% filter(AOI.hit..P02.TOI.Q16.Act09.Snap...Q09.Question.=="1")
fixations.Question.Q09 <- sum(question09$AOI.hit..P02.TOI.Q16.Act09.Snap...Q09.Question.)
perc.fixations.Question.Q09 <- fixations.Question.Q09 / totalFixations.Q09
time.Question.Q09 <- sum(question09.Question$Gaze.event.duration..ms.)
perc.time.Question.Q09 <- time.Question.Q09 / totalFixationTime.Q09


# AOI Answer
question09.Answer <- question09 %>% filter(AOI.hit..P02.TOI.Q16.Act09.Snap...Q09.Answer.=="1")
fixations.Answer.Q09 <- sum(question09$AOI.hit..P02.TOI.Q16.Act09.Snap...Q09.Answer.)
perc.fixations.Answer.Q09 <- fixations.Answer.Q09 / totalFixations.Q09
time.Answer.Q09 <- sum(question09.Answer$Gaze.event.duration..ms.)
perc.time.Answer.Q09 <- time.Answer.Q09 / totalFixationTime.Q09

# AOI Legend
question09.Legend <- question09 %>% filter(AOI.hit..P02.TOI.Q16.Act09.Snap...Q09.Legend.=="1")
fixations.Legend.Q09 <- sum(question09$AOI.hit..P02.TOI.Q16.Act09.Snap...Q09.Legend.)
perc.fixations.Legend.Q09 <- fixations.Legend.Q09 / totalFixations.Q09
time.Legend.Q09 <- sum(question09.Legend$Gaze.event.duration..ms.)
perc.time.Legend.Q09 <- time.Legend.Q09 / totalFixationTime.Q09

# AOI Buttons
question09.Buttons <- question09 %>% filter(AOI.hit..P02.TOI.Q16.Act09.Snap...Q09.Buttons.=="1")
fixations.Buttons.Q09 <- sum(question09$AOI.hit..P02.TOI.Q16.Act09.Snap...Q09.Buttons.)
perc.fixations.Buttons.Q09 <- fixations.Buttons.Q09 / totalFixations.Q09
time.Buttons.Q09 <- sum(question09.Buttons$Gaze.event.duration..ms.)
perc.time.Buttons.Q09 <- time.Buttons.Q09 / totalFixationTime.Q09


# AOI FeatureModel
question09.FM <- question09 %>% filter(AOI.hit..P02.TOI.Q16.Act09.Snap...Q09.FMAOI.=="1")
fixations.FM.Q09 <- sum(question09$AOI.hit..P02.TOI.Q16.Act09.Snap...Q09.FMAOI.)
perc.fixations.FM.Q09 <- fixations.FM.Q09 / totalFixations.Q09
time.FM.Q09 <- sum(question09.FM$Gaze.event.duration..ms.)
perc.time.FM.Q09 <- time.FM.Q09 / totalFixationTime.Q09


# AOI Containing
question09.Containing <- question09 %>% 
  filter(  AOI.hit..P02.TOI.Q16.Act09.Snap...Q09.CAOI.1.=="1" |	      
  AOI.hit..P02.TOI.Q16.Act09.Snap...Q09.CAOI.2.=="1" |	      
  AOI.hit..P02.TOI.Q16.Act09.Snap...Q09.CAOI.3.=="1" |	      
  AOI.hit..P02.TOI.Q16.Act09.Snap...Q09.CAOI.4.=="1") 
 

if (nrow(question09.Containing) !=0) {
fixations.Containing.Q09 <- sum(question09.Containing %>% select(contains("CAOI")))
perc.fixations.Containing.Q09 <- fixations.Containing.Q09 / totalFixations.Q09
time.Containing.Q09 <- sum(question09.Containing$Gaze.event.duration..ms.)
perc.time.Containing.Q09 <- time.Containing.Q09 / totalFixationTime.Q09
} else {
fixations.Containing.Q09 <- 0
perc.fixations.Containing.Q09 <- 0
time.Containing.Q09 <- 0
perc.time.Containing.Q09 <- 0
}

fixations.Navigating.Q09 <- 0
perc.fixations.Navigating.Q09 <- 0
time.Navigating.Q09 <- 0
perc.time.Navigating.Q09 <- 0


# AOI CTC
question09.CTC <- question09 %>% filter(AOI.hit..P02.TOI.Q16.Act09.Snap...Q09.CTC.=="1")
fixations.CTC.Q09 <- sum(question09$AOI.hit..P02.TOI.Q16.Act09.Snap...Q09.CTC.)
perc.fixations.CTC.Q09 <- fixations.CTC.Q09 / totalFixations.Q09
time.CTC.Q09 <- sum(question09.CTC$Gaze.event.duration..ms.)
perc.time.CTC.Q09 <- time.CTC.Q09 / totalFixationTime.Q09

# AOI Intersection Containing and Navigating when FM < (Containing + Navigating)
print(c(fixations.FM.Q09, fixations.Containing.Q09, fixations.Navigating.Q09))
print(c(fixations.Window.Q09, fixations.Question.Q09, fixations.Answer.Q09, fixations.Legend.Q09, fixations.Buttons.Q09,
        fixations.CTC.Q09))



# Creating the table now

ParticipantID <- 2
QNumber <- 9

Q09.data <- c(ParticipantID, QNumber, totalFixations.Q09, totalFixationTime.Q09, 
              fixations.Question.Q09, perc.fixations.Question.Q09, time.Question.Q09, perc.time.Question.Q09,
              fixations.Answer.Q09, perc.fixations.Answer.Q09, time.Answer.Q09, perc.time.Answer.Q09,
              fixations.Legend.Q09, perc.fixations.Legend.Q09, time.Legend.Q09, perc.time.Legend.Q09,
              fixations.Buttons.Q09, perc.fixations.Buttons.Q09, time.Buttons.Q09, perc.time.Buttons.Q09,
              fixations.FM.Q09, perc.fixations.FM.Q09, time.FM.Q09, perc.time.FM.Q09,
              fixations.Containing.Q09, perc.fixations.Containing.Q09, time.Containing.Q09, perc.time.Containing.Q09,
              fixations.Navigating.Q09, perc.fixations.Navigating.Q09, time.Navigating.Q09, perc.time.Navigating.Q09,
              fixations.CTC.Q09, perc.fixations.CTC.Q09, time.CTC.Q09, perc.time.CTC.Q09)  

 


# --------------------------------------------------------------------------------------------------
# Question 10



participant02.Q10 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP02/P02-TOI-Q21-Act10-Data.csv", header=TRUE)
attach (participant02.Q10)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated02.Q10 <- participant02.Q10 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
  distinct()


question10 <- curated02.Q10

# Totals
totalFixations.Q10 <- nrow(question10)
totalFixationTime.Q10 <- sum(question10$Gaze.event.duration..ms.)


# AOI Window - QNN for participant question
question10.Window <- question10 %>% filter(AOI.hit..P02.TOI.Q21.Act10.Snap...Q10.Window. =="1")
fixations.Window.Q10 <-  sum(question10$AOI.hit..P02.TOI.Q21.Act10.Snap...Q10.Window.)
perc.fixations.Window.Q10 <- fixations.Window.Q10 / totalFixations.Q10
perc.time.Window.Q10 <- sum(question10.Window$Gaze.event.duration..ms.)/totalFixationTime.Q10


# AOI Question
question10.Question <- question10 %>% filter(AOI.hit..P02.TOI.Q21.Act10.Snap...Q10.Question.=="1")
fixations.Question.Q10 <- sum(question10$AOI.hit..P02.TOI.Q21.Act10.Snap...Q10.Question.)
perc.fixations.Question.Q10 <- fixations.Question.Q10 / totalFixations.Q10
time.Question.Q10 <- sum(question10.Question$Gaze.event.duration..ms.)
perc.time.Question.Q10 <- time.Question.Q10 / totalFixationTime.Q10


# AOI Answer
question10.Answer <- question10 %>% filter(AOI.hit..P02.TOI.Q21.Act10.Snap...Q10.Answer.=="1")
fixations.Answer.Q10 <- sum(question10$AOI.hit..P02.TOI.Q21.Act10.Snap...Q10.Answer.)
perc.fixations.Answer.Q10 <- fixations.Answer.Q10 / totalFixations.Q10
time.Answer.Q10 <- sum(question10.Answer$Gaze.event.duration..ms.)
perc.time.Answer.Q10 <- time.Answer.Q10 / totalFixationTime.Q10

# AOI Legend
question10.Legend <- question10 %>% filter(AOI.hit..P02.TOI.Q21.Act10.Snap...Q10.Legend.=="1")
fixations.Legend.Q10 <- sum(question10$AOI.hit..P02.TOI.Q21.Act10.Snap...Q10.Legend.)
perc.fixations.Legend.Q10 <- fixations.Legend.Q10 / totalFixations.Q10
time.Legend.Q10 <- sum(question10.Legend$Gaze.event.duration..ms.)
perc.time.Legend.Q10 <- time.Legend.Q10 / totalFixationTime.Q10

# AOI Buttons
question10.Buttons <- question10 %>% filter(AOI.hit..P02.TOI.Q21.Act10.Snap...Q10.Buttons.=="1")
fixations.Buttons.Q10 <- sum(question10$AOI.hit..P02.TOI.Q21.Act10.Snap...Q10.Buttons.)
perc.fixations.Buttons.Q10 <- fixations.Buttons.Q10 / totalFixations.Q10
time.Buttons.Q10 <- sum(question10.Buttons$Gaze.event.duration..ms.)
perc.time.Buttons.Q10 <- time.Buttons.Q10 / totalFixationTime.Q10


# AOI FeatureModel
question10.FM <- question10 %>% filter(AOI.hit..P02.TOI.Q21.Act10.Snap...Q10.FMAOI.=="1")
fixations.FM.Q10 <- sum(question10$AOI.hit..P02.TOI.Q21.Act10.Snap...Q10.FMAOI.)
perc.fixations.FM.Q10 <- fixations.FM.Q10 / totalFixations.Q10
time.FM.Q10 <- sum(question10.FM$Gaze.event.duration..ms.)
perc.time.FM.Q10 <- time.FM.Q10 / totalFixationTime.Q10


# AOI Containing
question10.Containing <- question10 %>% 
  filter(  AOI.hit..P02.TOI.Q21.Act10.Snap...Q10.CAOI.1.=="1" |	      
  AOI.hit..P02.TOI.Q21.Act10.Snap...Q10.CAOI.2.=="1") 
 

if (nrow(question10.Containing) !=0) {
fixations.Containing.Q10 <- sum(question10.Containing %>% select(contains("CAOI")))
perc.fixations.Containing.Q10 <- fixations.Containing.Q10 / totalFixations.Q10
time.Containing.Q10 <- sum(question10.Containing$Gaze.event.duration..ms.)
perc.time.Containing.Q10 <- time.Containing.Q10 / totalFixationTime.Q10
} else {
fixations.Containing.Q10 <- 0
perc.fixations.Containing.Q10 <- 0
time.Containing.Q10 <- 0
perc.time.Containing.Q10 <- 0
}

fixations.Navigating.Q10 <- 0
perc.fixations.Navigating.Q10 <- 0
time.Navigating.Q10 <- 0
perc.time.Navigating.Q10 <- 0


# AOI CTC
question10.CTC <- question10 %>% filter(AOI.hit..P02.TOI.Q21.Act10.Snap...Q10.CTC.=="1")
fixations.CTC.Q10 <- sum(question10$AOI.hit..P02.TOI.Q21.Act10.Snap...Q10.CTC.)
perc.fixations.CTC.Q10 <- fixations.CTC.Q10 / totalFixations.Q10
time.CTC.Q10 <- sum(question10.CTC$Gaze.event.duration..ms.)
perc.time.CTC.Q10 <- time.CTC.Q10 / totalFixationTime.Q10

# AOI Intersection Containing and Navigating when FM < (Containing + Navigating)
print(c(fixations.FM.Q10, fixations.Containing.Q10, fixations.Navigating.Q10))
print(c(fixations.Window.Q10, fixations.Question.Q10, fixations.Answer.Q10, fixations.Legend.Q10, fixations.Buttons.Q10,
        fixations.CTC.Q10))



# Creating the table now

ParticipantID <- 2
QNumber <- 10

Q10.data <- c(ParticipantID, QNumber, totalFixations.Q10, totalFixationTime.Q10, 
              fixations.Question.Q10, perc.fixations.Question.Q10, time.Question.Q10, perc.time.Question.Q10,
              fixations.Answer.Q10, perc.fixations.Answer.Q10, time.Answer.Q10, perc.time.Answer.Q10,
              fixations.Legend.Q10, perc.fixations.Legend.Q10, time.Legend.Q10, perc.time.Legend.Q10,
              fixations.Buttons.Q10, perc.fixations.Buttons.Q10, time.Buttons.Q10, perc.time.Buttons.Q10,
              fixations.FM.Q10, perc.fixations.FM.Q10, time.FM.Q10, perc.time.FM.Q10,
              fixations.Containing.Q10, perc.fixations.Containing.Q10, time.Containing.Q10, perc.time.Containing.Q10,
              fixations.Navigating.Q10, perc.fixations.Navigating.Q10, time.Navigating.Q10, perc.time.Navigating.Q10,
              fixations.CTC.Q10, perc.fixations.CTC.Q10, time.CTC.Q10, perc.time.CTC.Q10)  

 


# --------------------------------------------------------------------------------------------------
# Question 11



participant02.Q11 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP02/P02-TOI-Q14-Act11-Data.csv", header=TRUE)
attach (participant02.Q11)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated02.Q11 <- participant02.Q11 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
  distinct()


question11 <- curated02.Q11

# Totals
totalFixations.Q11 <- nrow(question11)
totalFixationTime.Q11 <- sum(question11$Gaze.event.duration..ms.)


# AOI Window - QNN for participant question
question11.Window <- question11 %>% filter(AOI.hit..P02.TOI.Q14.Act11.Snap...Q11.Window. =="1")
fixations.Window.Q11 <-  sum(question11$AOI.hit..P02.TOI.Q14.Act11.Snap...Q11.Window.)
perc.fixations.Window.Q11 <- fixations.Window.Q11 / totalFixations.Q11
perc.time.Window.Q11 <- sum(question11.Window$Gaze.event.duration..ms.)/totalFixationTime.Q11


# AOI Question
question11.Question <- question11 %>% filter(AOI.hit..P02.TOI.Q14.Act11.Snap...Q11.Question.=="1")
fixations.Question.Q11 <- sum(question11$AOI.hit..P02.TOI.Q14.Act11.Snap...Q11.Question.)
perc.fixations.Question.Q11 <- fixations.Question.Q11 / totalFixations.Q11
time.Question.Q11 <- sum(question11.Question$Gaze.event.duration..ms.)
perc.time.Question.Q11 <- time.Question.Q11 / totalFixationTime.Q11


# AOI Answer
question11.Answer <- question11 %>% filter(AOI.hit..P02.TOI.Q14.Act11.Snap...Q11.Answer.=="1")
fixations.Answer.Q11 <- sum(question11$AOI.hit..P02.TOI.Q14.Act11.Snap...Q11.Answer.)
perc.fixations.Answer.Q11 <- fixations.Answer.Q11 / totalFixations.Q11
time.Answer.Q11 <- sum(question11.Answer$Gaze.event.duration..ms.)
perc.time.Answer.Q11 <- time.Answer.Q11 / totalFixationTime.Q11

# AOI Legend
question11.Legend <- question11 %>% filter(AOI.hit..P02.TOI.Q14.Act11.Snap...Q11.Legend.=="1")
fixations.Legend.Q11 <- sum(question11$AOI.hit..P02.TOI.Q14.Act11.Snap...Q11.Legend.)
perc.fixations.Legend.Q11 <- fixations.Legend.Q11 / totalFixations.Q11
time.Legend.Q11 <- sum(question11.Legend$Gaze.event.duration..ms.)
perc.time.Legend.Q11 <- time.Legend.Q11 / totalFixationTime.Q11

# AOI Buttons
question11.Buttons <- question11 %>% filter(AOI.hit..P02.TOI.Q14.Act11.Snap...Q11.Buttons.=="1")
fixations.Buttons.Q11 <- sum(question11$AOI.hit..P02.TOI.Q14.Act11.Snap...Q11.Buttons.)
perc.fixations.Buttons.Q11 <- fixations.Buttons.Q11 / totalFixations.Q11
time.Buttons.Q11 <- sum(question11.Buttons$Gaze.event.duration..ms.)
perc.time.Buttons.Q11 <- time.Buttons.Q11 / totalFixationTime.Q11


# AOI FeatureModel
question11.FM <- question11 %>% filter(AOI.hit..P02.TOI.Q14.Act11.Snap...Q11.FMAOI.=="1")
fixations.FM.Q11 <- sum(question11$AOI.hit..P02.TOI.Q14.Act11.Snap...Q11.FMAOI.)
perc.fixations.FM.Q11 <- fixations.FM.Q11 / totalFixations.Q11
time.FM.Q11 <- sum(question11.FM$Gaze.event.duration..ms.)
perc.time.FM.Q11 <- time.FM.Q11 / totalFixationTime.Q11


# AOI Containing
question11.Containing <- question11 %>% 
  filter(  AOI.hit..P02.TOI.Q14.Act11.Snap...Q11.CAOI.3.=="1" |	      
  AOI.hit..P02.TOI.Q14.Act11.Snap...Q11.CAOI.5.=="1" |	      
  AOI.hit..P02.TOI.Q14.Act11.Snap...Q11.CAOI.6.=="1" |	      
  AOI.hit..P02.TOI.Q14.Act11.Snap...Q11.CAOI.7.=="1" |	      
  AOI.hit..P02.TOI.Q14.Act11.Snap...Q11.CAOI.8.=="1") 
 

if (nrow(question11.Containing) !=0) {
fixations.Containing.Q11 <- sum(question11.Containing %>% select(contains("CAOI")))
perc.fixations.Containing.Q11 <- fixations.Containing.Q11 / totalFixations.Q11
time.Containing.Q11 <- sum(question11.Containing$Gaze.event.duration..ms.)
perc.time.Containing.Q11 <- time.Containing.Q11 / totalFixationTime.Q11
} else {
fixations.Containing.Q11 <- 0
perc.fixations.Containing.Q11 <- 0
time.Containing.Q11 <- 0
perc.time.Containing.Q11 <- 0
}


# AOI Navigating
question11.Navigating <- question11 %>% 
  filter(  AOI.hit..P02.TOI.Q14.Act11.Snap...Q11.NAOI.1.=="1" | 	      
  AOI.hit..P02.TOI.Q14.Act11.Snap...Q11.NAOI.2.=="1" | 	      
  AOI.hit..P02.TOI.Q14.Act11.Snap...Q11.NAOI.4.=="1")
    
      
if (nrow(question11.Navigating) !=0) {
 fixations.Navigating.Q11 <- sum(question11.Navigating %>% select(contains("NAOI")))
 perc.fixations.Navigating.Q11 <- fixations.Navigating.Q11 / totalFixations.Q11
 time.Navigating.Q11 <- sum(question11.Navigating$Gaze.event.duration..ms.)
 perc.time.Navigating.Q11 <- time.Navigating.Q11 / totalFixationTime.Q11
} else {
  fixations.Navigating.Q11 <- 0
  perc.fixations.Navigating.Q11 <- 0
  time.Navigating.Q11 <- 0
  perc.time.Navigating.Q11 <- 0 
}



# AOI CTC
question11.CTC <- question11 %>% filter(AOI.hit..P02.TOI.Q14.Act11.Snap...Q11.CTC.=="1")
fixations.CTC.Q11 <- sum(question11$AOI.hit..P02.TOI.Q14.Act11.Snap...Q11.CTC.)
perc.fixations.CTC.Q11 <- fixations.CTC.Q11 / totalFixations.Q11
time.CTC.Q11 <- sum(question11.CTC$Gaze.event.duration..ms.)
perc.time.CTC.Q11 <- time.CTC.Q11 / totalFixationTime.Q11

# AOI Intersection Containing and Navigating when FM < (Containing + Navigating)
print(c(fixations.FM.Q11, fixations.Containing.Q11, fixations.Navigating.Q11))
print(c(fixations.Window.Q11, fixations.Question.Q11, fixations.Answer.Q11, fixations.Legend.Q11, fixations.Buttons.Q11,
        fixations.CTC.Q11))



# Creating the table now

ParticipantID <- 2
QNumber <- 11

Q11.data <- c(ParticipantID, QNumber, totalFixations.Q11, totalFixationTime.Q11, 
              fixations.Question.Q11, perc.fixations.Question.Q11, time.Question.Q11, perc.time.Question.Q11,
              fixations.Answer.Q11, perc.fixations.Answer.Q11, time.Answer.Q11, perc.time.Answer.Q11,
              fixations.Legend.Q11, perc.fixations.Legend.Q11, time.Legend.Q11, perc.time.Legend.Q11,
              fixations.Buttons.Q11, perc.fixations.Buttons.Q11, time.Buttons.Q11, perc.time.Buttons.Q11,
              fixations.FM.Q11, perc.fixations.FM.Q11, time.FM.Q11, perc.time.FM.Q11,
              fixations.Containing.Q11, perc.fixations.Containing.Q11, time.Containing.Q11, perc.time.Containing.Q11,
              fixations.Navigating.Q11, perc.fixations.Navigating.Q11, time.Navigating.Q11, perc.time.Navigating.Q11,
              fixations.CTC.Q11, perc.fixations.CTC.Q11, time.CTC.Q11, perc.time.CTC.Q11)  

 


# --------------------------------------------------------------------------------------------------
# Question 12



participant02.Q12 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP02/P02-TOI-Q15-Act12-Data.csv", header=TRUE)
attach (participant02.Q12)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated02.Q12 <- participant02.Q12 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
  distinct()


question12 <- curated02.Q12

# Totals
totalFixations.Q12 <- nrow(question12)
totalFixationTime.Q12 <- sum(question12$Gaze.event.duration..ms.)


# AOI Window - QNN for participant question
question12.Window <- question12 %>% filter(AOI.hit..P02.TOI.Q15.Act12.Snap...Q12.Window. =="1")
fixations.Window.Q12 <-  sum(question12$AOI.hit..P02.TOI.Q15.Act12.Snap...Q12.Window.)
perc.fixations.Window.Q12 <- fixations.Window.Q12 / totalFixations.Q12
perc.time.Window.Q12 <- sum(question12.Window$Gaze.event.duration..ms.)/totalFixationTime.Q12


# AOI Question
question12.Question <- question12 %>% filter(AOI.hit..P02.TOI.Q15.Act12.Snap...Q12.Question.=="1")
fixations.Question.Q12 <- sum(question12$AOI.hit..P02.TOI.Q15.Act12.Snap...Q12.Question.)
perc.fixations.Question.Q12 <- fixations.Question.Q12 / totalFixations.Q12
time.Question.Q12 <- sum(question12.Question$Gaze.event.duration..ms.)
perc.time.Question.Q12 <- time.Question.Q12 / totalFixationTime.Q12


# AOI Answer
question12.Answer <- question12 %>% filter(AOI.hit..P02.TOI.Q15.Act12.Snap...Q12.Answer.=="1")
fixations.Answer.Q12 <- sum(question12$AOI.hit..P02.TOI.Q15.Act12.Snap...Q12.Answer.)
perc.fixations.Answer.Q12 <- fixations.Answer.Q12 / totalFixations.Q12
time.Answer.Q12 <- sum(question12.Answer$Gaze.event.duration..ms.)
perc.time.Answer.Q12 <- time.Answer.Q12 / totalFixationTime.Q12

# AOI Legend
question12.Legend <- question12 %>% filter(AOI.hit..P02.TOI.Q15.Act12.Snap...Q12.Legend.=="1")
fixations.Legend.Q12 <- sum(question12$AOI.hit..P02.TOI.Q15.Act12.Snap...Q12.Legend.)
perc.fixations.Legend.Q12 <- fixations.Legend.Q12 / totalFixations.Q12
time.Legend.Q12 <- sum(question12.Legend$Gaze.event.duration..ms.)
perc.time.Legend.Q12 <- time.Legend.Q12 / totalFixationTime.Q12

# AOI Buttons
question12.Buttons <- question12 %>% filter(AOI.hit..P02.TOI.Q15.Act12.Snap...Q12.Buttons.=="1")
fixations.Buttons.Q12 <- sum(question12$AOI.hit..P02.TOI.Q15.Act12.Snap...Q12.Buttons.)
perc.fixations.Buttons.Q12 <- fixations.Buttons.Q12 / totalFixations.Q12
time.Buttons.Q12 <- sum(question12.Buttons$Gaze.event.duration..ms.)
perc.time.Buttons.Q12 <- time.Buttons.Q12 / totalFixationTime.Q12


# AOI FeatureModel
question12.FM <- question12 %>% filter(AOI.hit..P02.TOI.Q15.Act12.Snap...Q12.FMAOI.=="1")
fixations.FM.Q12 <- sum(question12$AOI.hit..P02.TOI.Q15.Act12.Snap...Q12.FMAOI.)
perc.fixations.FM.Q12 <- fixations.FM.Q12 / totalFixations.Q12
time.FM.Q12 <- sum(question12.FM$Gaze.event.duration..ms.)
perc.time.FM.Q12 <- time.FM.Q12 / totalFixationTime.Q12


# AOI Containing
question12.Containing <- question12 %>% 
  filter(  AOI.hit..P02.TOI.Q15.Act12.Snap...Q12.CAOI.2.=="1" |	      
  AOI.hit..P02.TOI.Q15.Act12.Snap...Q12.CAOI.5.=="1" |	      
  AOI.hit..P02.TOI.Q15.Act12.Snap...Q12.CAOI.6.=="1") 
 

if (nrow(question12.Containing) !=0) {
fixations.Containing.Q12 <- sum(question12.Containing %>% select(contains("CAOI")))
perc.fixations.Containing.Q12 <- fixations.Containing.Q12 / totalFixations.Q12
time.Containing.Q12 <- sum(question12.Containing$Gaze.event.duration..ms.)
perc.time.Containing.Q12 <- time.Containing.Q12 / totalFixationTime.Q12
} else {
fixations.Containing.Q12 <- 0
perc.fixations.Containing.Q12 <- 0
time.Containing.Q12 <- 0
perc.time.Containing.Q12 <- 0
}


# AOI Navigating
question12.Navigating <- question12 %>% 
  filter(  AOI.hit..P02.TOI.Q15.Act12.Snap...Q12.NAOI.1.=="1" | 	      
  AOI.hit..P02.TOI.Q15.Act12.Snap...Q12.NAOI.3.=="1" | 	      
  AOI.hit..P02.TOI.Q15.Act12.Snap...Q12.NAOI.4.=="1")
    
      
if (nrow(question12.Navigating) !=0) {
 fixations.Navigating.Q12 <- sum(question12.Navigating %>% select(contains("NAOI")))
 perc.fixations.Navigating.Q12 <- fixations.Navigating.Q12 / totalFixations.Q12
 time.Navigating.Q12 <- sum(question12.Navigating$Gaze.event.duration..ms.)
 perc.time.Navigating.Q12 <- time.Navigating.Q12 / totalFixationTime.Q12
} else {
  fixations.Navigating.Q12 <- 0
  perc.fixations.Navigating.Q12 <- 0
  time.Navigating.Q12 <- 0
  perc.time.Navigating.Q12 <- 0 
}



# AOI CTC
question12.CTC <- question12 %>% filter(AOI.hit..P02.TOI.Q15.Act12.Snap...Q12.CTC.=="1")
fixations.CTC.Q12 <- sum(question12$AOI.hit..P02.TOI.Q15.Act12.Snap...Q12.CTC.)
perc.fixations.CTC.Q12 <- fixations.CTC.Q12 / totalFixations.Q12
time.CTC.Q12 <- sum(question12.CTC$Gaze.event.duration..ms.)
perc.time.CTC.Q12 <- time.CTC.Q12 / totalFixationTime.Q12

# AOI Intersection Containing and Navigating when FM < (Containing + Navigating)
print(c(fixations.FM.Q12, fixations.Containing.Q12, fixations.Navigating.Q12))
print(c(fixations.Window.Q12, fixations.Question.Q12, fixations.Answer.Q12, fixations.Legend.Q12, fixations.Buttons.Q12,
        fixations.CTC.Q12))



# Creating the table now

ParticipantID <- 2
QNumber <- 12

Q12.data <- c(ParticipantID, QNumber, totalFixations.Q12, totalFixationTime.Q12, 
              fixations.Question.Q12, perc.fixations.Question.Q12, time.Question.Q12, perc.time.Question.Q12,
              fixations.Answer.Q12, perc.fixations.Answer.Q12, time.Answer.Q12, perc.time.Answer.Q12,
              fixations.Legend.Q12, perc.fixations.Legend.Q12, time.Legend.Q12, perc.time.Legend.Q12,
              fixations.Buttons.Q12, perc.fixations.Buttons.Q12, time.Buttons.Q12, perc.time.Buttons.Q12,
              fixations.FM.Q12, perc.fixations.FM.Q12, time.FM.Q12, perc.time.FM.Q12,
              fixations.Containing.Q12, perc.fixations.Containing.Q12, time.Containing.Q12, perc.time.Containing.Q12,
              fixations.Navigating.Q12, perc.fixations.Navigating.Q12, time.Navigating.Q12, perc.time.Navigating.Q12,
              fixations.CTC.Q12, perc.fixations.CTC.Q12, time.CTC.Q12, perc.time.CTC.Q12)  

 


# --------------------------------------------------------------------------------------------------
# Question 13



participant02.Q13 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP02/P02-TOI-Q13-Act13-Data.csv", header=TRUE)
attach (participant02.Q13)

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
# Question 14



participant02.Q14 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP02/P02-TOI-Q05-Act14-Data.csv", header=TRUE)
attach (participant02.Q14)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated02.Q14 <- participant02.Q14 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
  distinct()


question14 <- curated02.Q14

# Totals
totalFixations.Q14 <- nrow(question14)
totalFixationTime.Q14 <- sum(question14$Gaze.event.duration..ms.)


# AOI Window - QNN for participant question
question14.Window <- question14 %>% filter(AOI.hit..P02.TOI.Q05.Act14.Snap...Q14.Window. =="1")
fixations.Window.Q14 <-  sum(question14$AOI.hit..P02.TOI.Q05.Act14.Snap...Q14.Window.)
perc.fixations.Window.Q14 <- fixations.Window.Q14 / totalFixations.Q14
perc.time.Window.Q14 <- sum(question14.Window$Gaze.event.duration..ms.)/totalFixationTime.Q14


# AOI Question
question14.Question <- question14 %>% filter(AOI.hit..P02.TOI.Q05.Act14.Snap...Q14.Question.=="1")
fixations.Question.Q14 <- sum(question14$AOI.hit..P02.TOI.Q05.Act14.Snap...Q14.Question.)
perc.fixations.Question.Q14 <- fixations.Question.Q14 / totalFixations.Q14
time.Question.Q14 <- sum(question14.Question$Gaze.event.duration..ms.)
perc.time.Question.Q14 <- time.Question.Q14 / totalFixationTime.Q14


# AOI Answer
question14.Answer <- question14 %>% filter(AOI.hit..P02.TOI.Q05.Act14.Snap...Q14.Answer.=="1")
fixations.Answer.Q14 <- sum(question14$AOI.hit..P02.TOI.Q05.Act14.Snap...Q14.Answer.)
perc.fixations.Answer.Q14 <- fixations.Answer.Q14 / totalFixations.Q14
time.Answer.Q14 <- sum(question14.Answer$Gaze.event.duration..ms.)
perc.time.Answer.Q14 <- time.Answer.Q14 / totalFixationTime.Q14

# AOI Legend
question14.Legend <- question14 %>% filter(AOI.hit..P02.TOI.Q05.Act14.Snap...Q14.Legend.=="1")
fixations.Legend.Q14 <- sum(question14$AOI.hit..P02.TOI.Q05.Act14.Snap...Q14.Legend.)
perc.fixations.Legend.Q14 <- fixations.Legend.Q14 / totalFixations.Q14
time.Legend.Q14 <- sum(question14.Legend$Gaze.event.duration..ms.)
perc.time.Legend.Q14 <- time.Legend.Q14 / totalFixationTime.Q14

# AOI Buttons
question14.Buttons <- question14 %>% filter(AOI.hit..P02.TOI.Q05.Act14.Snap...Q14.Buttons.=="1")
fixations.Buttons.Q14 <- sum(question14$AOI.hit..P02.TOI.Q05.Act14.Snap...Q14.Buttons.)
perc.fixations.Buttons.Q14 <- fixations.Buttons.Q14 / totalFixations.Q14
time.Buttons.Q14 <- sum(question14.Buttons$Gaze.event.duration..ms.)
perc.time.Buttons.Q14 <- time.Buttons.Q14 / totalFixationTime.Q14


# AOI FeatureModel
question14.FM <- question14 %>% filter(AOI.hit..P02.TOI.Q05.Act14.Snap...Q14.FMAOI.=="1")
fixations.FM.Q14 <- sum(question14$AOI.hit..P02.TOI.Q05.Act14.Snap...Q14.FMAOI.)
perc.fixations.FM.Q14 <- fixations.FM.Q14 / totalFixations.Q14
time.FM.Q14 <- sum(question14.FM$Gaze.event.duration..ms.)
perc.time.FM.Q14 <- time.FM.Q14 / totalFixationTime.Q14


# AOI Containing
question14.Containing <- question14 %>% 
  filter(  AOI.hit..P02.TOI.Q05.Act14.Snap...Q14.CAOI.2.=="1" |	      
  AOI.hit..P02.TOI.Q05.Act14.Snap...Q14.CAOI.3.=="1" |	      
  AOI.hit..P02.TOI.Q05.Act14.Snap...Q14.CAOI.5.=="1") 
 

if (nrow(question14.Containing) !=0) {
fixations.Containing.Q14 <- sum(question14.Containing %>% select(contains("CAOI")))
perc.fixations.Containing.Q14 <- fixations.Containing.Q14 / totalFixations.Q14
time.Containing.Q14 <- sum(question14.Containing$Gaze.event.duration..ms.)
perc.time.Containing.Q14 <- time.Containing.Q14 / totalFixationTime.Q14
} else {
fixations.Containing.Q14 <- 0
perc.fixations.Containing.Q14 <- 0
time.Containing.Q14 <- 0
perc.time.Containing.Q14 <- 0
}


# AOI Navigating
question14.Navigating <- question14 %>% 
  filter(  AOI.hit..P02.TOI.Q05.Act14.Snap...Q14.NAOI.1.=="1" | 	      
  AOI.hit..P02.TOI.Q05.Act14.Snap...Q14.NAOI.4.=="1")
    
      
if (nrow(question14.Navigating) !=0) {
 fixations.Navigating.Q14 <- sum(question14.Navigating %>% select(contains("NAOI")))
 perc.fixations.Navigating.Q14 <- fixations.Navigating.Q14 / totalFixations.Q14
 time.Navigating.Q14 <- sum(question14.Navigating$Gaze.event.duration..ms.)
 perc.time.Navigating.Q14 <- time.Navigating.Q14 / totalFixationTime.Q14
} else {
  fixations.Navigating.Q14 <- 0
  perc.fixations.Navigating.Q14 <- 0
  time.Navigating.Q14 <- 0
  perc.time.Navigating.Q14 <- 0 
}



fixations.CTC.Q14 <- 0
perc.fixations.CTC.Q14 <- 0
time.CTC.Q14 <- 0
perc.time.CTC.Q14 <- 0

# AOI Intersection Containing and Navigating when FM < (Containing + Navigating)
print(c(fixations.FM.Q14, fixations.Containing.Q14, fixations.Navigating.Q14))
print(c(fixations.Window.Q14, fixations.Question.Q14, fixations.Answer.Q14, fixations.Legend.Q14, fixations.Buttons.Q14,
        fixations.CTC.Q14))



# Creating the table now

ParticipantID <- 2
QNumber <- 14

Q14.data <- c(ParticipantID, QNumber, totalFixations.Q14, totalFixationTime.Q14, 
              fixations.Question.Q14, perc.fixations.Question.Q14, time.Question.Q14, perc.time.Question.Q14,
              fixations.Answer.Q14, perc.fixations.Answer.Q14, time.Answer.Q14, perc.time.Answer.Q14,
              fixations.Legend.Q14, perc.fixations.Legend.Q14, time.Legend.Q14, perc.time.Legend.Q14,
              fixations.Buttons.Q14, perc.fixations.Buttons.Q14, time.Buttons.Q14, perc.time.Buttons.Q14,
              fixations.FM.Q14, perc.fixations.FM.Q14, time.FM.Q14, perc.time.FM.Q14,
              fixations.Containing.Q14, perc.fixations.Containing.Q14, time.Containing.Q14, perc.time.Containing.Q14,
              fixations.Navigating.Q14, perc.fixations.Navigating.Q14, time.Navigating.Q14, perc.time.Navigating.Q14,
              fixations.CTC.Q14, perc.fixations.CTC.Q14, time.CTC.Q14, perc.time.CTC.Q14)  

 


# --------------------------------------------------------------------------------------------------
# Question 15



participant02.Q15 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP02/P02-TOI-Q08-Act15-Data.csv", header=TRUE)
attach (participant02.Q15)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated02.Q15 <- participant02.Q15 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
  distinct()


question15 <- curated02.Q15

# Totals
totalFixations.Q15 <- nrow(question15)
totalFixationTime.Q15 <- sum(question15$Gaze.event.duration..ms.)


# AOI Window - QNN for participant question
question15.Window <- question15 %>% filter(AOI.hit..P02.TOI.Q08.Act15.Snap...Q15.Window. =="1")
fixations.Window.Q15 <-  sum(question15$AOI.hit..P02.TOI.Q08.Act15.Snap...Q15.Window.)
perc.fixations.Window.Q15 <- fixations.Window.Q15 / totalFixations.Q15
perc.time.Window.Q15 <- sum(question15.Window$Gaze.event.duration..ms.)/totalFixationTime.Q15


# AOI Question
question15.Question <- question15 %>% filter(AOI.hit..P02.TOI.Q08.Act15.Snap...Q15.Question.=="1")
fixations.Question.Q15 <- sum(question15$AOI.hit..P02.TOI.Q08.Act15.Snap...Q15.Question.)
perc.fixations.Question.Q15 <- fixations.Question.Q15 / totalFixations.Q15
time.Question.Q15 <- sum(question15.Question$Gaze.event.duration..ms.)
perc.time.Question.Q15 <- time.Question.Q15 / totalFixationTime.Q15


# AOI Answer
question15.Answer <- question15 %>% filter(AOI.hit..P02.TOI.Q08.Act15.Snap...Q15.Answer.=="1")
fixations.Answer.Q15 <- sum(question15$AOI.hit..P02.TOI.Q08.Act15.Snap...Q15.Answer.)
perc.fixations.Answer.Q15 <- fixations.Answer.Q15 / totalFixations.Q15
time.Answer.Q15 <- sum(question15.Answer$Gaze.event.duration..ms.)
perc.time.Answer.Q15 <- time.Answer.Q15 / totalFixationTime.Q15

# AOI Legend
question15.Legend <- question15 %>% filter(AOI.hit..P02.TOI.Q08.Act15.Snap...Q15.Legend.=="1")
fixations.Legend.Q15 <- sum(question15$AOI.hit..P02.TOI.Q08.Act15.Snap...Q15.Legend.)
perc.fixations.Legend.Q15 <- fixations.Legend.Q15 / totalFixations.Q15
time.Legend.Q15 <- sum(question15.Legend$Gaze.event.duration..ms.)
perc.time.Legend.Q15 <- time.Legend.Q15 / totalFixationTime.Q15

# AOI Buttons
question15.Buttons <- question15 %>% filter(AOI.hit..P02.TOI.Q08.Act15.Snap...Q15.Buttons.=="1")
fixations.Buttons.Q15 <- sum(question15$AOI.hit..P02.TOI.Q08.Act15.Snap...Q15.Buttons.)
perc.fixations.Buttons.Q15 <- fixations.Buttons.Q15 / totalFixations.Q15
time.Buttons.Q15 <- sum(question15.Buttons$Gaze.event.duration..ms.)
perc.time.Buttons.Q15 <- time.Buttons.Q15 / totalFixationTime.Q15


# AOI FeatureModel
question15.FM <- question15 %>% filter(AOI.hit..P02.TOI.Q08.Act15.Snap...Q15.FMAOI.=="1")
fixations.FM.Q15 <- sum(question15$AOI.hit..P02.TOI.Q08.Act15.Snap...Q15.FMAOI.)
perc.fixations.FM.Q15 <- fixations.FM.Q15 / totalFixations.Q15
time.FM.Q15 <- sum(question15.FM$Gaze.event.duration..ms.)
perc.time.FM.Q15 <- time.FM.Q15 / totalFixationTime.Q15


# AOI Containing
question15.Containing <- question15 %>% 
  filter(  AOI.hit..P02.TOI.Q08.Act15.Snap...Q15.CAOI.3.=="1" |	      
  AOI.hit..P02.TOI.Q08.Act15.Snap...Q15.CAOI.4.=="1" |	      
  AOI.hit..P02.TOI.Q08.Act15.Snap...Q15.CAOI.5.=="1" |	      
  AOI.hit..P02.TOI.Q08.Act15.Snap...Q15.CAOI.7.=="1") 
 

if (nrow(question15.Containing) !=0) {
fixations.Containing.Q15 <- sum(question15.Containing %>% select(contains("CAOI")))
perc.fixations.Containing.Q15 <- fixations.Containing.Q15 / totalFixations.Q15
time.Containing.Q15 <- sum(question15.Containing$Gaze.event.duration..ms.)
perc.time.Containing.Q15 <- time.Containing.Q15 / totalFixationTime.Q15
} else {
fixations.Containing.Q15 <- 0
perc.fixations.Containing.Q15 <- 0
time.Containing.Q15 <- 0
perc.time.Containing.Q15 <- 0
}


# AOI Navigating
question15.Navigating <- question15 %>% 
  filter(  AOI.hit..P02.TOI.Q08.Act15.Snap...Q15.NAOI.1.=="1" | 	      
  AOI.hit..P02.TOI.Q08.Act15.Snap...Q15.NAOI.2.=="1" | 	      
  AOI.hit..P02.TOI.Q08.Act15.Snap...Q15.NAOI.6.=="1")
    
      
if (nrow(question15.Navigating) !=0) {
 fixations.Navigating.Q15 <- sum(question15.Navigating %>% select(contains("NAOI")))
 perc.fixations.Navigating.Q15 <- fixations.Navigating.Q15 / totalFixations.Q15
 time.Navigating.Q15 <- sum(question15.Navigating$Gaze.event.duration..ms.)
 perc.time.Navigating.Q15 <- time.Navigating.Q15 / totalFixationTime.Q15
} else {
  fixations.Navigating.Q15 <- 0
  perc.fixations.Navigating.Q15 <- 0
  time.Navigating.Q15 <- 0
  perc.time.Navigating.Q15 <- 0 
}



# AOI CTC
question15.CTC <- question15 %>% filter(AOI.hit..P02.TOI.Q08.Act15.Snap...Q15.CTC.=="1")
fixations.CTC.Q15 <- sum(question15$AOI.hit..P02.TOI.Q08.Act15.Snap...Q15.CTC.)
perc.fixations.CTC.Q15 <- fixations.CTC.Q15 / totalFixations.Q15
time.CTC.Q15 <- sum(question15.CTC$Gaze.event.duration..ms.)
perc.time.CTC.Q15 <- time.CTC.Q15 / totalFixationTime.Q15

# AOI Intersection Containing and Navigating when FM < (Containing + Navigating)
print(c(fixations.FM.Q15, fixations.Containing.Q15, fixations.Navigating.Q15))
print(c(fixations.Window.Q15, fixations.Question.Q15, fixations.Answer.Q15, fixations.Legend.Q15, fixations.Buttons.Q15,
        fixations.CTC.Q15))



# Creating the table now

ParticipantID <- 2
QNumber <- 15

Q15.data <- c(ParticipantID, QNumber, totalFixations.Q15, totalFixationTime.Q15, 
              fixations.Question.Q15, perc.fixations.Question.Q15, time.Question.Q15, perc.time.Question.Q15,
              fixations.Answer.Q15, perc.fixations.Answer.Q15, time.Answer.Q15, perc.time.Answer.Q15,
              fixations.Legend.Q15, perc.fixations.Legend.Q15, time.Legend.Q15, perc.time.Legend.Q15,
              fixations.Buttons.Q15, perc.fixations.Buttons.Q15, time.Buttons.Q15, perc.time.Buttons.Q15,
              fixations.FM.Q15, perc.fixations.FM.Q15, time.FM.Q15, perc.time.FM.Q15,
              fixations.Containing.Q15, perc.fixations.Containing.Q15, time.Containing.Q15, perc.time.Containing.Q15,
              fixations.Navigating.Q15, perc.fixations.Navigating.Q15, time.Navigating.Q15, perc.time.Navigating.Q15,
              fixations.CTC.Q15, perc.fixations.CTC.Q15, time.CTC.Q15, perc.time.CTC.Q15)  

 


# --------------------------------------------------------------------------------------------------
# Question 16



participant02.Q16 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP02/P02-TOI-Q07-Act16-Data.csv", header=TRUE)
attach (participant02.Q16)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated02.Q16 <- participant02.Q16 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
  distinct()


question16 <- curated02.Q16

# Totals
totalFixations.Q16 <- nrow(question16)
totalFixationTime.Q16 <- sum(question16$Gaze.event.duration..ms.)


# AOI Window - QNN for participant question
question16.Window <- question16 %>% filter(AOI.hit..P02.TOI.Q07.Act16.Snap...Q16.Window. =="1")
fixations.Window.Q16 <-  sum(question16$AOI.hit..P02.TOI.Q07.Act16.Snap...Q16.Window.)
perc.fixations.Window.Q16 <- fixations.Window.Q16 / totalFixations.Q16
perc.time.Window.Q16 <- sum(question16.Window$Gaze.event.duration..ms.)/totalFixationTime.Q16


# AOI Question
question16.Question <- question16 %>% filter(AOI.hit..P02.TOI.Q07.Act16.Snap...Q16.Question.=="1")
fixations.Question.Q16 <- sum(question16$AOI.hit..P02.TOI.Q07.Act16.Snap...Q16.Question.)
perc.fixations.Question.Q16 <- fixations.Question.Q16 / totalFixations.Q16
time.Question.Q16 <- sum(question16.Question$Gaze.event.duration..ms.)
perc.time.Question.Q16 <- time.Question.Q16 / totalFixationTime.Q16


# AOI Answer
question16.Answer <- question16 %>% filter(AOI.hit..P02.TOI.Q07.Act16.Snap...Q16.Answer.=="1")
fixations.Answer.Q16 <- sum(question16$AOI.hit..P02.TOI.Q07.Act16.Snap...Q16.Answer.)
perc.fixations.Answer.Q16 <- fixations.Answer.Q16 / totalFixations.Q16
time.Answer.Q16 <- sum(question16.Answer$Gaze.event.duration..ms.)
perc.time.Answer.Q16 <- time.Answer.Q16 / totalFixationTime.Q16

# AOI Legend
question16.Legend <- question16 %>% filter(AOI.hit..P02.TOI.Q07.Act16.Snap...Q16.Legend.=="1")
fixations.Legend.Q16 <- sum(question16$AOI.hit..P02.TOI.Q07.Act16.Snap...Q16.Legend.)
perc.fixations.Legend.Q16 <- fixations.Legend.Q16 / totalFixations.Q16
time.Legend.Q16 <- sum(question16.Legend$Gaze.event.duration..ms.)
perc.time.Legend.Q16 <- time.Legend.Q16 / totalFixationTime.Q16

# AOI Buttons
question16.Buttons <- question16 %>% filter(AOI.hit..P02.TOI.Q07.Act16.Snap...Q16.Buttons.=="1")
fixations.Buttons.Q16 <- sum(question16$AOI.hit..P02.TOI.Q07.Act16.Snap...Q16.Buttons.)
perc.fixations.Buttons.Q16 <- fixations.Buttons.Q16 / totalFixations.Q16
time.Buttons.Q16 <- sum(question16.Buttons$Gaze.event.duration..ms.)
perc.time.Buttons.Q16 <- time.Buttons.Q16 / totalFixationTime.Q16


# AOI FeatureModel
question16.FM <- question16 %>% filter(AOI.hit..P02.TOI.Q07.Act16.Snap...Q16.FMAOI.=="1")
fixations.FM.Q16 <- sum(question16$AOI.hit..P02.TOI.Q07.Act16.Snap...Q16.FMAOI.)
perc.fixations.FM.Q16 <- fixations.FM.Q16 / totalFixations.Q16
time.FM.Q16 <- sum(question16.FM$Gaze.event.duration..ms.)
perc.time.FM.Q16 <- time.FM.Q16 / totalFixationTime.Q16


# AOI Containing
question16.Containing <- question16 %>% 
  filter(  AOI.hit..P02.TOI.Q07.Act16.Snap...Q16.CAOI.1.=="1" |	      
  AOI.hit..P02.TOI.Q07.Act16.Snap...Q16.CAOI.2.=="1" |	      
  AOI.hit..P02.TOI.Q07.Act16.Snap...Q16.CAOI.3.=="1" |	      
  AOI.hit..P02.TOI.Q07.Act16.Snap...Q16.CAOI.4.=="1") 
 

if (nrow(question16.Containing) !=0) {
fixations.Containing.Q16 <- sum(question16.Containing %>% select(contains("CAOI")))
perc.fixations.Containing.Q16 <- fixations.Containing.Q16 / totalFixations.Q16
time.Containing.Q16 <- sum(question16.Containing$Gaze.event.duration..ms.)
perc.time.Containing.Q16 <- time.Containing.Q16 / totalFixationTime.Q16
} else {
fixations.Containing.Q16 <- 0
perc.fixations.Containing.Q16 <- 0
time.Containing.Q16 <- 0
perc.time.Containing.Q16 <- 0
}

fixations.Navigating.Q16 <- 0
perc.fixations.Navigating.Q16 <- 0
time.Navigating.Q16 <- 0
perc.time.Navigating.Q16 <- 0


# AOI CTC
question16.CTC <- question16 %>% filter(AOI.hit..P02.TOI.Q07.Act16.Snap...Q16.CTC.=="1")
fixations.CTC.Q16 <- sum(question16$AOI.hit..P02.TOI.Q07.Act16.Snap...Q16.CTC.)
perc.fixations.CTC.Q16 <- fixations.CTC.Q16 / totalFixations.Q16
time.CTC.Q16 <- sum(question16.CTC$Gaze.event.duration..ms.)
perc.time.CTC.Q16 <- time.CTC.Q16 / totalFixationTime.Q16

# AOI Intersection Containing and Navigating when FM < (Containing + Navigating)
print(c(fixations.FM.Q16, fixations.Containing.Q16, fixations.Navigating.Q16))
print(c(fixations.Window.Q16, fixations.Question.Q16, fixations.Answer.Q16, fixations.Legend.Q16, fixations.Buttons.Q16,
        fixations.CTC.Q16))



# Creating the table now

ParticipantID <- 2
QNumber <- 16

Q16.data <- c(ParticipantID, QNumber, totalFixations.Q16, totalFixationTime.Q16, 
              fixations.Question.Q16, perc.fixations.Question.Q16, time.Question.Q16, perc.time.Question.Q16,
              fixations.Answer.Q16, perc.fixations.Answer.Q16, time.Answer.Q16, perc.time.Answer.Q16,
              fixations.Legend.Q16, perc.fixations.Legend.Q16, time.Legend.Q16, perc.time.Legend.Q16,
              fixations.Buttons.Q16, perc.fixations.Buttons.Q16, time.Buttons.Q16, perc.time.Buttons.Q16,
              fixations.FM.Q16, perc.fixations.FM.Q16, time.FM.Q16, perc.time.FM.Q16,
              fixations.Containing.Q16, perc.fixations.Containing.Q16, time.Containing.Q16, perc.time.Containing.Q16,
              fixations.Navigating.Q16, perc.fixations.Navigating.Q16, time.Navigating.Q16, perc.time.Navigating.Q16,
              fixations.CTC.Q16, perc.fixations.CTC.Q16, time.CTC.Q16, perc.time.CTC.Q16)  

 


# --------------------------------------------------------------------------------------------------
# Question 17



participant02.Q17 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP02/P02-TOI-Q24-Act17-Data.csv", header=TRUE)
attach (participant02.Q17)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated02.Q17 <- participant02.Q17 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
  distinct()


question17 <- curated02.Q17

# Totals
totalFixations.Q17 <- nrow(question17)
totalFixationTime.Q17 <- sum(question17$Gaze.event.duration..ms.)


# AOI Window - QNN for participant question
question17.Window <- question17 %>% filter(AOI.hit..P02.TOI.Q24.Act17.Snap...Q17.Window. =="1")
fixations.Window.Q17 <-  sum(question17$AOI.hit..P02.TOI.Q24.Act17.Snap...Q17.Window.)
perc.fixations.Window.Q17 <- fixations.Window.Q17 / totalFixations.Q17
perc.time.Window.Q17 <- sum(question17.Window$Gaze.event.duration..ms.)/totalFixationTime.Q17


# AOI Question
question17.Question <- question17 %>% filter(AOI.hit..P02.TOI.Q24.Act17.Snap...Q17.Question.=="1")
fixations.Question.Q17 <- sum(question17$AOI.hit..P02.TOI.Q24.Act17.Snap...Q17.Question.)
perc.fixations.Question.Q17 <- fixations.Question.Q17 / totalFixations.Q17
time.Question.Q17 <- sum(question17.Question$Gaze.event.duration..ms.)
perc.time.Question.Q17 <- time.Question.Q17 / totalFixationTime.Q17


# AOI Answer
question17.Answer <- question17 %>% filter(AOI.hit..P02.TOI.Q24.Act17.Snap...Q17.Answer.=="1")
fixations.Answer.Q17 <- sum(question17$AOI.hit..P02.TOI.Q24.Act17.Snap...Q17.Answer.)
perc.fixations.Answer.Q17 <- fixations.Answer.Q17 / totalFixations.Q17
time.Answer.Q17 <- sum(question17.Answer$Gaze.event.duration..ms.)
perc.time.Answer.Q17 <- time.Answer.Q17 / totalFixationTime.Q17

# AOI Legend
question17.Legend <- question17 %>% filter(AOI.hit..P02.TOI.Q24.Act17.Snap...Q17.Legend.=="1")
fixations.Legend.Q17 <- sum(question17$AOI.hit..P02.TOI.Q24.Act17.Snap...Q17.Legend.)
perc.fixations.Legend.Q17 <- fixations.Legend.Q17 / totalFixations.Q17
time.Legend.Q17 <- sum(question17.Legend$Gaze.event.duration..ms.)
perc.time.Legend.Q17 <- time.Legend.Q17 / totalFixationTime.Q17

# AOI Buttons
question17.Buttons <- question17 %>% filter(AOI.hit..P02.TOI.Q24.Act17.Snap...Q17.Buttons.=="1")
fixations.Buttons.Q17 <- sum(question17$AOI.hit..P02.TOI.Q24.Act17.Snap...Q17.Buttons.)
perc.fixations.Buttons.Q17 <- fixations.Buttons.Q17 / totalFixations.Q17
time.Buttons.Q17 <- sum(question17.Buttons$Gaze.event.duration..ms.)
perc.time.Buttons.Q17 <- time.Buttons.Q17 / totalFixationTime.Q17


# AOI FeatureModel
question17.FM <- question17 %>% filter(AOI.hit..P02.TOI.Q24.Act17.Snap...Q17.FMAOI.=="1")
fixations.FM.Q17 <- sum(question17$AOI.hit..P02.TOI.Q24.Act17.Snap...Q17.FMAOI.)
perc.fixations.FM.Q17 <- fixations.FM.Q17 / totalFixations.Q17
time.FM.Q17 <- sum(question17.FM$Gaze.event.duration..ms.)
perc.time.FM.Q17 <- time.FM.Q17 / totalFixationTime.Q17


# AOI Containing
question17.Containing <- question17 %>% 
  filter(  AOI.hit..P02.TOI.Q24.Act17.Snap...Q17.CAOI.1.=="1" |	      
  AOI.hit..P02.TOI.Q24.Act17.Snap...Q17.CAOI.2.=="1" |	      
  AOI.hit..P02.TOI.Q24.Act17.Snap...Q17.CAOI.3.=="1" |	      
  AOI.hit..P02.TOI.Q24.Act17.Snap...Q17.CAOI.4.=="1") 
 

if (nrow(question17.Containing) !=0) {
fixations.Containing.Q17 <- sum(question17.Containing %>% select(contains("CAOI")))
perc.fixations.Containing.Q17 <- fixations.Containing.Q17 / totalFixations.Q17
time.Containing.Q17 <- sum(question17.Containing$Gaze.event.duration..ms.)
perc.time.Containing.Q17 <- time.Containing.Q17 / totalFixationTime.Q17
} else {
fixations.Containing.Q17 <- 0
perc.fixations.Containing.Q17 <- 0
time.Containing.Q17 <- 0
perc.time.Containing.Q17 <- 0
}

fixations.Navigating.Q17 <- 0
perc.fixations.Navigating.Q17 <- 0
time.Navigating.Q17 <- 0
perc.time.Navigating.Q17 <- 0


# AOI CTC
question17.CTC <- question17 %>% filter(AOI.hit..P02.TOI.Q24.Act17.Snap...Q17.CTC.=="1")
fixations.CTC.Q17 <- sum(question17$AOI.hit..P02.TOI.Q24.Act17.Snap...Q17.CTC.)
perc.fixations.CTC.Q17 <- fixations.CTC.Q17 / totalFixations.Q17
time.CTC.Q17 <- sum(question17.CTC$Gaze.event.duration..ms.)
perc.time.CTC.Q17 <- time.CTC.Q17 / totalFixationTime.Q17

# AOI Intersection Containing and Navigating when FM < (Containing + Navigating)
print(c(fixations.FM.Q17, fixations.Containing.Q17, fixations.Navigating.Q17))
print(c(fixations.Window.Q17, fixations.Question.Q17, fixations.Answer.Q17, fixations.Legend.Q17, fixations.Buttons.Q17,
        fixations.CTC.Q17))



# Creating the table now

ParticipantID <- 2
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

 


# --------------------------------------------------------------------------------------------------
# Question 18



participant02.Q18 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP02/P02-TOI-Q12-Act18-Data.csv", header=TRUE)
attach (participant02.Q18)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated02.Q18 <- participant02.Q18 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
  distinct()


question18 <- curated02.Q18

# Totals
totalFixations.Q18 <- nrow(question18)
totalFixationTime.Q18 <- sum(question18$Gaze.event.duration..ms.)


# AOI Window - QNN for participant question
question18.Window <- question18 %>% filter(AOI.hit..P02.TOI.Q12.Act18.Snap...Q18.Window. =="1")
fixations.Window.Q18 <-  sum(question18$AOI.hit..P02.TOI.Q12.Act18.Snap...Q18.Window.)
perc.fixations.Window.Q18 <- fixations.Window.Q18 / totalFixations.Q18
perc.time.Window.Q18 <- sum(question18.Window$Gaze.event.duration..ms.)/totalFixationTime.Q18


# AOI Question
question18.Question <- question18 %>% filter(AOI.hit..P02.TOI.Q12.Act18.Snap...Q18.Question.=="1")
fixations.Question.Q18 <- sum(question18$AOI.hit..P02.TOI.Q12.Act18.Snap...Q18.Question.)
perc.fixations.Question.Q18 <- fixations.Question.Q18 / totalFixations.Q18
time.Question.Q18 <- sum(question18.Question$Gaze.event.duration..ms.)
perc.time.Question.Q18 <- time.Question.Q18 / totalFixationTime.Q18


# AOI Answer
question18.Answer <- question18 %>% filter(AOI.hit..P02.TOI.Q12.Act18.Snap...Q18.Answer.=="1")
fixations.Answer.Q18 <- sum(question18$AOI.hit..P02.TOI.Q12.Act18.Snap...Q18.Answer.)
perc.fixations.Answer.Q18 <- fixations.Answer.Q18 / totalFixations.Q18
time.Answer.Q18 <- sum(question18.Answer$Gaze.event.duration..ms.)
perc.time.Answer.Q18 <- time.Answer.Q18 / totalFixationTime.Q18

# AOI Legend
question18.Legend <- question18 %>% filter(AOI.hit..P02.TOI.Q12.Act18.Snap...Q18.Legend.=="1")
fixations.Legend.Q18 <- sum(question18$AOI.hit..P02.TOI.Q12.Act18.Snap...Q18.Legend.)
perc.fixations.Legend.Q18 <- fixations.Legend.Q18 / totalFixations.Q18
time.Legend.Q18 <- sum(question18.Legend$Gaze.event.duration..ms.)
perc.time.Legend.Q18 <- time.Legend.Q18 / totalFixationTime.Q18

# AOI Buttons
question18.Buttons <- question18 %>% filter(AOI.hit..P02.TOI.Q12.Act18.Snap...Q18.Buttons.=="1")
fixations.Buttons.Q18 <- sum(question18$AOI.hit..P02.TOI.Q12.Act18.Snap...Q18.Buttons.)
perc.fixations.Buttons.Q18 <- fixations.Buttons.Q18 / totalFixations.Q18
time.Buttons.Q18 <- sum(question18.Buttons$Gaze.event.duration..ms.)
perc.time.Buttons.Q18 <- time.Buttons.Q18 / totalFixationTime.Q18


# AOI FeatureModel
question18.FM <- question18 %>% filter(AOI.hit..P02.TOI.Q12.Act18.Snap...Q18.FMAOI.=="1")
fixations.FM.Q18 <- sum(question18$AOI.hit..P02.TOI.Q12.Act18.Snap...Q18.FMAOI.)
perc.fixations.FM.Q18 <- fixations.FM.Q18 / totalFixations.Q18
time.FM.Q18 <- sum(question18.FM$Gaze.event.duration..ms.)
perc.time.FM.Q18 <- time.FM.Q18 / totalFixationTime.Q18


# AOI Containing
question18.Containing <- question18 %>% 
  filter(  AOI.hit..P02.TOI.Q12.Act18.Snap...Q18.CAOI.4.=="1") 
 

if (nrow(question18.Containing) !=0) {
fixations.Containing.Q18 <- sum(question18.Containing %>% select(contains("CAOI")))
perc.fixations.Containing.Q18 <- fixations.Containing.Q18 / totalFixations.Q18
time.Containing.Q18 <- sum(question18.Containing$Gaze.event.duration..ms.)
perc.time.Containing.Q18 <- time.Containing.Q18 / totalFixationTime.Q18
} else {
fixations.Containing.Q18 <- 0
perc.fixations.Containing.Q18 <- 0
time.Containing.Q18 <- 0
perc.time.Containing.Q18 <- 0
}


# AOI Navigating
question18.Navigating <- question18 %>% 
  filter(  AOI.hit..P02.TOI.Q12.Act18.Snap...Q18.NAOI.1.=="1" | 	      
  AOI.hit..P02.TOI.Q12.Act18.Snap...Q18.NAOI.2.=="1" | 	      
  AOI.hit..P02.TOI.Q12.Act18.Snap...Q18.NAOI.3.=="1" | 	      
  AOI.hit..P02.TOI.Q12.Act18.Snap...Q18.NAOI.5.=="1" | 	      
  AOI.hit..P02.TOI.Q12.Act18.Snap...Q18.NAOI.6.=="1")
    
      
if (nrow(question18.Navigating) !=0) {
 fixations.Navigating.Q18 <- sum(question18.Navigating %>% select(contains("NAOI")))
 perc.fixations.Navigating.Q18 <- fixations.Navigating.Q18 / totalFixations.Q18
 time.Navigating.Q18 <- sum(question18.Navigating$Gaze.event.duration..ms.)
 perc.time.Navigating.Q18 <- time.Navigating.Q18 / totalFixationTime.Q18
} else {
  fixations.Navigating.Q18 <- 0
  perc.fixations.Navigating.Q18 <- 0
  time.Navigating.Q18 <- 0
  perc.time.Navigating.Q18 <- 0 
}



# AOI CTC
question18.CTC <- question18 %>% filter(AOI.hit..P02.TOI.Q12.Act18.Snap...Q18.CTC.=="1")
fixations.CTC.Q18 <- sum(question18$AOI.hit..P02.TOI.Q12.Act18.Snap...Q18.CTC.)
perc.fixations.CTC.Q18 <- fixations.CTC.Q18 / totalFixations.Q18
time.CTC.Q18 <- sum(question18.CTC$Gaze.event.duration..ms.)
perc.time.CTC.Q18 <- time.CTC.Q18 / totalFixationTime.Q18

# AOI Intersection Containing and Navigating when FM < (Containing + Navigating)
print(c(fixations.FM.Q18, fixations.Containing.Q18, fixations.Navigating.Q18))
print(c(fixations.Window.Q18, fixations.Question.Q18, fixations.Answer.Q18, fixations.Legend.Q18, fixations.Buttons.Q18,
        fixations.CTC.Q18))



# Creating the table now

ParticipantID <- 2
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

 


# --------------------------------------------------------------------------------------------------
# Question 19



participant02.Q19 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP02/P02-TOI-Q01-Act19-Data.csv", header=TRUE)
attach (participant02.Q19)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated02.Q19 <- participant02.Q19 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
  distinct()


question19 <- curated02.Q19

# Totals
totalFixations.Q19 <- nrow(question19)
totalFixationTime.Q19 <- sum(question19$Gaze.event.duration..ms.)


# AOI Window - QNN for participant question
question19.Window <- question19 %>% filter(AOI.hit..P02.TOI.Q01.Act19.Snap...Q19.Window. =="1")
fixations.Window.Q19 <-  sum(question19$AOI.hit..P02.TOI.Q01.Act19.Snap...Q19.Window.)
perc.fixations.Window.Q19 <- fixations.Window.Q19 / totalFixations.Q19
perc.time.Window.Q19 <- sum(question19.Window$Gaze.event.duration..ms.)/totalFixationTime.Q19


# AOI Question
question19.Question <- question19 %>% filter(AOI.hit..P02.TOI.Q01.Act19.Snap...Q19.Question.=="1")
fixations.Question.Q19 <- sum(question19$AOI.hit..P02.TOI.Q01.Act19.Snap...Q19.Question.)
perc.fixations.Question.Q19 <- fixations.Question.Q19 / totalFixations.Q19
time.Question.Q19 <- sum(question19.Question$Gaze.event.duration..ms.)
perc.time.Question.Q19 <- time.Question.Q19 / totalFixationTime.Q19


# AOI Answer
question19.Answer <- question19 %>% filter(AOI.hit..P02.TOI.Q01.Act19.Snap...Q19.Answer.=="1")
fixations.Answer.Q19 <- sum(question19$AOI.hit..P02.TOI.Q01.Act19.Snap...Q19.Answer.)
perc.fixations.Answer.Q19 <- fixations.Answer.Q19 / totalFixations.Q19
time.Answer.Q19 <- sum(question19.Answer$Gaze.event.duration..ms.)
perc.time.Answer.Q19 <- time.Answer.Q19 / totalFixationTime.Q19

# AOI Legend
question19.Legend <- question19 %>% filter(AOI.hit..P02.TOI.Q01.Act19.Snap...Q19.Legend.=="1")
fixations.Legend.Q19 <- sum(question19$AOI.hit..P02.TOI.Q01.Act19.Snap...Q19.Legend.)
perc.fixations.Legend.Q19 <- fixations.Legend.Q19 / totalFixations.Q19
time.Legend.Q19 <- sum(question19.Legend$Gaze.event.duration..ms.)
perc.time.Legend.Q19 <- time.Legend.Q19 / totalFixationTime.Q19

# AOI Buttons
question19.Buttons <- question19 %>% filter(AOI.hit..P02.TOI.Q01.Act19.Snap...Q19.Buttons.=="1")
fixations.Buttons.Q19 <- sum(question19$AOI.hit..P02.TOI.Q01.Act19.Snap...Q19.Buttons.)
perc.fixations.Buttons.Q19 <- fixations.Buttons.Q19 / totalFixations.Q19
time.Buttons.Q19 <- sum(question19.Buttons$Gaze.event.duration..ms.)
perc.time.Buttons.Q19 <- time.Buttons.Q19 / totalFixationTime.Q19


# AOI FeatureModel
question19.FM <- question19 %>% filter(AOI.hit..P02.TOI.Q01.Act19.Snap...Q19.FMAOI.=="1")
fixations.FM.Q19 <- sum(question19$AOI.hit..P02.TOI.Q01.Act19.Snap...Q19.FMAOI.)
perc.fixations.FM.Q19 <- fixations.FM.Q19 / totalFixations.Q19
time.FM.Q19 <- sum(question19.FM$Gaze.event.duration..ms.)
perc.time.FM.Q19 <- time.FM.Q19 / totalFixationTime.Q19


# AOI Containing
question19.Containing <- question19 %>% 
  filter(  AOI.hit..P02.TOI.Q01.Act19.Snap...Q19.CAOI.3.=="1" |	      
  AOI.hit..P02.TOI.Q01.Act19.Snap...Q19.CAOI.4.=="1" |	      
  AOI.hit..P02.TOI.Q01.Act19.Snap...Q19.CAOI.2.=="1" |	      
  AOI.hit..P02.TOI.Q01.Act19.Snap...Q19.CAOI.1.=="1") 
 

if (nrow(question19.Containing) !=0) {
fixations.Containing.Q19 <- sum(question19.Containing %>% select(contains("CAOI")))
perc.fixations.Containing.Q19 <- fixations.Containing.Q19 / totalFixations.Q19
time.Containing.Q19 <- sum(question19.Containing$Gaze.event.duration..ms.)
perc.time.Containing.Q19 <- time.Containing.Q19 / totalFixationTime.Q19
} else {
fixations.Containing.Q19 <- 0
perc.fixations.Containing.Q19 <- 0
time.Containing.Q19 <- 0
perc.time.Containing.Q19 <- 0
}

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

ParticipantID <- 2
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

 


# --------------------------------------------------------------------------------------------------
# Question 20



participant02.Q20 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP02/P02-TOI-Q04-Act20-Data.csv", header=TRUE)
attach (participant02.Q20)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated02.Q20 <- participant02.Q20 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
  distinct()


question20 <- curated02.Q20

# Totals
totalFixations.Q20 <- nrow(question20)
totalFixationTime.Q20 <- sum(question20$Gaze.event.duration..ms.)


# AOI Window - QNN for participant question
question20.Window <- question20 %>% filter(AOI.hit..P02.TOI.Q04.Act20.Snap...Q20.Window. =="1")
fixations.Window.Q20 <-  sum(question20$AOI.hit..P02.TOI.Q04.Act20.Snap...Q20.Window.)
perc.fixations.Window.Q20 <- fixations.Window.Q20 / totalFixations.Q20
perc.time.Window.Q20 <- sum(question20.Window$Gaze.event.duration..ms.)/totalFixationTime.Q20


# AOI Question
question20.Question <- question20 %>% filter(AOI.hit..P02.TOI.Q04.Act20.Snap...Q20.Question.=="1")
fixations.Question.Q20 <- sum(question20$AOI.hit..P02.TOI.Q04.Act20.Snap...Q20.Question.)
perc.fixations.Question.Q20 <- fixations.Question.Q20 / totalFixations.Q20
time.Question.Q20 <- sum(question20.Question$Gaze.event.duration..ms.)
perc.time.Question.Q20 <- time.Question.Q20 / totalFixationTime.Q20


# AOI Answer
question20.Answer <- question20 %>% filter(AOI.hit..P02.TOI.Q04.Act20.Snap...Q20.Answer.=="1")
fixations.Answer.Q20 <- sum(question20$AOI.hit..P02.TOI.Q04.Act20.Snap...Q20.Answer.)
perc.fixations.Answer.Q20 <- fixations.Answer.Q20 / totalFixations.Q20
time.Answer.Q20 <- sum(question20.Answer$Gaze.event.duration..ms.)
perc.time.Answer.Q20 <- time.Answer.Q20 / totalFixationTime.Q20

# AOI Legend
question20.Legend <- question20 %>% filter(AOI.hit..P02.TOI.Q04.Act20.Snap...Q20.Legend.=="1")
fixations.Legend.Q20 <- sum(question20$AOI.hit..P02.TOI.Q04.Act20.Snap...Q20.Legend.)
perc.fixations.Legend.Q20 <- fixations.Legend.Q20 / totalFixations.Q20
time.Legend.Q20 <- sum(question20.Legend$Gaze.event.duration..ms.)
perc.time.Legend.Q20 <- time.Legend.Q20 / totalFixationTime.Q20

# AOI Buttons
question20.Buttons <- question20 %>% filter(AOI.hit..P02.TOI.Q04.Act20.Snap...Q20.Buttons.=="1")
fixations.Buttons.Q20 <- sum(question20$AOI.hit..P02.TOI.Q04.Act20.Snap...Q20.Buttons.)
perc.fixations.Buttons.Q20 <- fixations.Buttons.Q20 / totalFixations.Q20
time.Buttons.Q20 <- sum(question20.Buttons$Gaze.event.duration..ms.)
perc.time.Buttons.Q20 <- time.Buttons.Q20 / totalFixationTime.Q20


# AOI FeatureModel
question20.FM <- question20 %>% filter(AOI.hit..P02.TOI.Q04.Act20.Snap...Q20.FMAOI.=="1")
fixations.FM.Q20 <- sum(question20$AOI.hit..P02.TOI.Q04.Act20.Snap...Q20.FMAOI.)
perc.fixations.FM.Q20 <- fixations.FM.Q20 / totalFixations.Q20
time.FM.Q20 <- sum(question20.FM$Gaze.event.duration..ms.)
perc.time.FM.Q20 <- time.FM.Q20 / totalFixationTime.Q20


# AOI Containing
question20.Containing <- question20 %>% 
  filter(  AOI.hit..P02.TOI.Q04.Act20.Snap...Q20.CAOI.2.=="1" |	      
  AOI.hit..P02.TOI.Q04.Act20.Snap...Q20.CAOI.3.=="1" |	      
  AOI.hit..P02.TOI.Q04.Act20.Snap...Q20.CAOI.4.=="1" |	      
  AOI.hit..P02.TOI.Q04.Act20.Snap...Q20.CAOI.5.=="1" |	      
  AOI.hit..P02.TOI.Q04.Act20.Snap...Q20.CAOI.6.=="1") 
 

if (nrow(question20.Containing) !=0) {
fixations.Containing.Q20 <- sum(question20.Containing %>% select(contains("CAOI")))
perc.fixations.Containing.Q20 <- fixations.Containing.Q20 / totalFixations.Q20
time.Containing.Q20 <- sum(question20.Containing$Gaze.event.duration..ms.)
perc.time.Containing.Q20 <- time.Containing.Q20 / totalFixationTime.Q20
} else {
fixations.Containing.Q20 <- 0
perc.fixations.Containing.Q20 <- 0
time.Containing.Q20 <- 0
perc.time.Containing.Q20 <- 0
}


# AOI Navigating
question20.Navigating <- question20 %>% 
  filter(  AOI.hit..P02.TOI.Q04.Act20.Snap...Q20.NAOI.1.=="1")
    
      
if (nrow(question20.Navigating) !=0) {
 fixations.Navigating.Q20 <- sum(question20.Navigating %>% select(contains("NAOI")))
 perc.fixations.Navigating.Q20 <- fixations.Navigating.Q20 / totalFixations.Q20
 time.Navigating.Q20 <- sum(question20.Navigating$Gaze.event.duration..ms.)
 perc.time.Navigating.Q20 <- time.Navigating.Q20 / totalFixationTime.Q20
} else {
  fixations.Navigating.Q20 <- 0
  perc.fixations.Navigating.Q20 <- 0
  time.Navigating.Q20 <- 0
  perc.time.Navigating.Q20 <- 0 
}



fixations.CTC.Q20 <- 0
perc.fixations.CTC.Q20 <- 0
time.CTC.Q20 <- 0
perc.time.CTC.Q20 <- 0

# AOI Intersection Containing and Navigating when FM < (Containing + Navigating)
print(c(fixations.FM.Q20, fixations.Containing.Q20, fixations.Navigating.Q20))
print(c(fixations.Window.Q20, fixations.Question.Q20, fixations.Answer.Q20, fixations.Legend.Q20, fixations.Buttons.Q20,
        fixations.CTC.Q20))



# Creating the table now

ParticipantID <- 2
QNumber <- 20

Q20.data <- c(ParticipantID, QNumber, totalFixations.Q20, totalFixationTime.Q20, 
              fixations.Question.Q20, perc.fixations.Question.Q20, time.Question.Q20, perc.time.Question.Q20,
              fixations.Answer.Q20, perc.fixations.Answer.Q20, time.Answer.Q20, perc.time.Answer.Q20,
              fixations.Legend.Q20, perc.fixations.Legend.Q20, time.Legend.Q20, perc.time.Legend.Q20,
              fixations.Buttons.Q20, perc.fixations.Buttons.Q20, time.Buttons.Q20, perc.time.Buttons.Q20,
              fixations.FM.Q20, perc.fixations.FM.Q20, time.FM.Q20, perc.time.FM.Q20,
              fixations.Containing.Q20, perc.fixations.Containing.Q20, time.Containing.Q20, perc.time.Containing.Q20,
              fixations.Navigating.Q20, perc.fixations.Navigating.Q20, time.Navigating.Q20, perc.time.Navigating.Q20,
              fixations.CTC.Q20, perc.fixations.CTC.Q20, time.CTC.Q20, perc.time.CTC.Q20)  

 


# --------------------------------------------------------------------------------------------------
# Question 21



participant02.Q21 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP02/P02-TOI-Q09-Act21-Data.csv", header=TRUE)
attach (participant02.Q21)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated02.Q21 <- participant02.Q21 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
  distinct()


question21 <- curated02.Q21

# Totals
totalFixations.Q21 <- nrow(question21)
totalFixationTime.Q21 <- sum(question21$Gaze.event.duration..ms.)


# AOI Window - QNN for participant question
question21.Window <- question21 %>% filter(AOI.hit..P02.TOI.Q09.Act21.Snap...Q21.Window. =="1")
fixations.Window.Q21 <-  sum(question21$AOI.hit..P02.TOI.Q09.Act21.Snap...Q21.Window.)
perc.fixations.Window.Q21 <- fixations.Window.Q21 / totalFixations.Q21
perc.time.Window.Q21 <- sum(question21.Window$Gaze.event.duration..ms.)/totalFixationTime.Q21


# AOI Question
question21.Question <- question21 %>% filter(AOI.hit..P02.TOI.Q09.Act21.Snap...Q21.Question.=="1")
fixations.Question.Q21 <- sum(question21$AOI.hit..P02.TOI.Q09.Act21.Snap...Q21.Question.)
perc.fixations.Question.Q21 <- fixations.Question.Q21 / totalFixations.Q21
time.Question.Q21 <- sum(question21.Question$Gaze.event.duration..ms.)
perc.time.Question.Q21 <- time.Question.Q21 / totalFixationTime.Q21


# AOI Answer
question21.Answer <- question21 %>% filter(AOI.hit..P02.TOI.Q09.Act21.Snap...Q21.Answer.=="1")
fixations.Answer.Q21 <- sum(question21$AOI.hit..P02.TOI.Q09.Act21.Snap...Q21.Answer.)
perc.fixations.Answer.Q21 <- fixations.Answer.Q21 / totalFixations.Q21
time.Answer.Q21 <- sum(question21.Answer$Gaze.event.duration..ms.)
perc.time.Answer.Q21 <- time.Answer.Q21 / totalFixationTime.Q21

# AOI Legend
question21.Legend <- question21 %>% filter(AOI.hit..P02.TOI.Q09.Act21.Snap...Q21.Legend.=="1")
fixations.Legend.Q21 <- sum(question21$AOI.hit..P02.TOI.Q09.Act21.Snap...Q21.Legend.)
perc.fixations.Legend.Q21 <- fixations.Legend.Q21 / totalFixations.Q21
time.Legend.Q21 <- sum(question21.Legend$Gaze.event.duration..ms.)
perc.time.Legend.Q21 <- time.Legend.Q21 / totalFixationTime.Q21

# AOI Buttons
question21.Buttons <- question21 %>% filter(AOI.hit..P02.TOI.Q09.Act21.Snap...Q21.Buttons.=="1")
fixations.Buttons.Q21 <- sum(question21$AOI.hit..P02.TOI.Q09.Act21.Snap...Q21.Buttons.)
perc.fixations.Buttons.Q21 <- fixations.Buttons.Q21 / totalFixations.Q21
time.Buttons.Q21 <- sum(question21.Buttons$Gaze.event.duration..ms.)
perc.time.Buttons.Q21 <- time.Buttons.Q21 / totalFixationTime.Q21


# AOI FeatureModel
question21.FM <- question21 %>% filter(AOI.hit..P02.TOI.Q09.Act21.Snap...Q21.FMAOI.=="1")
fixations.FM.Q21 <- sum(question21$AOI.hit..P02.TOI.Q09.Act21.Snap...Q21.FMAOI.)
perc.fixations.FM.Q21 <- fixations.FM.Q21 / totalFixations.Q21
time.FM.Q21 <- sum(question21.FM$Gaze.event.duration..ms.)
perc.time.FM.Q21 <- time.FM.Q21 / totalFixationTime.Q21


# AOI Containing
question21.Containing <- question21 %>% 
  filter(  AOI.hit..P02.TOI.Q09.Act21.Snap...Q21.CAOI.4.=="1" |	      
  AOI.hit..P02.TOI.Q09.Act21.Snap...Q21.CAOI.5.=="1" |	      
  AOI.hit..P02.TOI.Q09.Act21.Snap...Q21.CAOI.6.=="1" |	      
  AOI.hit..P02.TOI.Q09.Act21.Snap...Q21.CAOI.7.=="1") 
 

if (nrow(question21.Containing) !=0) {
fixations.Containing.Q21 <- sum(question21.Containing %>% select(contains("CAOI")))
perc.fixations.Containing.Q21 <- fixations.Containing.Q21 / totalFixations.Q21
time.Containing.Q21 <- sum(question21.Containing$Gaze.event.duration..ms.)
perc.time.Containing.Q21 <- time.Containing.Q21 / totalFixationTime.Q21
} else {
fixations.Containing.Q21 <- 0
perc.fixations.Containing.Q21 <- 0
time.Containing.Q21 <- 0
perc.time.Containing.Q21 <- 0
}


# AOI Navigating
question21.Navigating <- question21 %>% 
  filter(  AOI.hit..P02.TOI.Q09.Act21.Snap...Q21.NAOI.1.=="1" | 	      
  AOI.hit..P02.TOI.Q09.Act21.Snap...Q21.NAOI.2.=="1" | 	      
  AOI.hit..P02.TOI.Q09.Act21.Snap...Q21.NAOI.3.=="1")
    
      
if (nrow(question21.Navigating) !=0) {
 fixations.Navigating.Q21 <- sum(question21.Navigating %>% select(contains("NAOI")))
 perc.fixations.Navigating.Q21 <- fixations.Navigating.Q21 / totalFixations.Q21
 time.Navigating.Q21 <- sum(question21.Navigating$Gaze.event.duration..ms.)
 perc.time.Navigating.Q21 <- time.Navigating.Q21 / totalFixationTime.Q21
} else {
  fixations.Navigating.Q21 <- 0
  perc.fixations.Navigating.Q21 <- 0
  time.Navigating.Q21 <- 0
  perc.time.Navigating.Q21 <- 0 
}



# AOI CTC
question21.CTC <- question21 %>% filter(AOI.hit..P02.TOI.Q09.Act21.Snap...Q21.CTC.=="1")
fixations.CTC.Q21 <- sum(question21$AOI.hit..P02.TOI.Q09.Act21.Snap...Q21.CTC.)
perc.fixations.CTC.Q21 <- fixations.CTC.Q21 / totalFixations.Q21
time.CTC.Q21 <- sum(question21.CTC$Gaze.event.duration..ms.)
perc.time.CTC.Q21 <- time.CTC.Q21 / totalFixationTime.Q21

# AOI Intersection Containing and Navigating when FM < (Containing + Navigating)
print(c(fixations.FM.Q21, fixations.Containing.Q21, fixations.Navigating.Q21))
print(c(fixations.Window.Q21, fixations.Question.Q21, fixations.Answer.Q21, fixations.Legend.Q21, fixations.Buttons.Q21,
        fixations.CTC.Q21))



# Creating the table now

ParticipantID <- 2
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

 


# --------------------------------------------------------------------------------------------------
# Question 22



participant02.Q22 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP02/P02-TOI-Q11-Act22-Data.csv", header=TRUE)
attach (participant02.Q22)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated02.Q22 <- participant02.Q22 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
  distinct()


question22 <- curated02.Q22

# Totals
totalFixations.Q22 <- nrow(question22)
totalFixationTime.Q22 <- sum(question22$Gaze.event.duration..ms.)


# AOI Window - QNN for participant question
question22.Window <- question22 %>% filter(AOI.hit..P02.TOI.Q11.Act22.Snap...Q22.Window. =="1")
fixations.Window.Q22 <-  sum(question22$AOI.hit..P02.TOI.Q11.Act22.Snap...Q22.Window.)
perc.fixations.Window.Q22 <- fixations.Window.Q22 / totalFixations.Q22
perc.time.Window.Q22 <- sum(question22.Window$Gaze.event.duration..ms.)/totalFixationTime.Q22


# AOI Question
question22.Question <- question22 %>% filter(AOI.hit..P02.TOI.Q11.Act22.Snap...Q22.Question.=="1")
fixations.Question.Q22 <- sum(question22$AOI.hit..P02.TOI.Q11.Act22.Snap...Q22.Question.)
perc.fixations.Question.Q22 <- fixations.Question.Q22 / totalFixations.Q22
time.Question.Q22 <- sum(question22.Question$Gaze.event.duration..ms.)
perc.time.Question.Q22 <- time.Question.Q22 / totalFixationTime.Q22


# AOI Answer
question22.Answer <- question22 %>% filter(AOI.hit..P02.TOI.Q11.Act22.Snap...Q22.Answer.=="1")
fixations.Answer.Q22 <- sum(question22$AOI.hit..P02.TOI.Q11.Act22.Snap...Q22.Answer.)
perc.fixations.Answer.Q22 <- fixations.Answer.Q22 / totalFixations.Q22
time.Answer.Q22 <- sum(question22.Answer$Gaze.event.duration..ms.)
perc.time.Answer.Q22 <- time.Answer.Q22 / totalFixationTime.Q22

# AOI Legend
question22.Legend <- question22 %>% filter(AOI.hit..P02.TOI.Q11.Act22.Snap...Q22.Legend.=="1")
fixations.Legend.Q22 <- sum(question22$AOI.hit..P02.TOI.Q11.Act22.Snap...Q22.Legend.)
perc.fixations.Legend.Q22 <- fixations.Legend.Q22 / totalFixations.Q22
time.Legend.Q22 <- sum(question22.Legend$Gaze.event.duration..ms.)
perc.time.Legend.Q22 <- time.Legend.Q22 / totalFixationTime.Q22

# AOI Buttons
question22.Buttons <- question22 %>% filter(AOI.hit..P02.TOI.Q11.Act22.Snap...Q22.Buttons.=="1")
fixations.Buttons.Q22 <- sum(question22$AOI.hit..P02.TOI.Q11.Act22.Snap...Q22.Buttons.)
perc.fixations.Buttons.Q22 <- fixations.Buttons.Q22 / totalFixations.Q22
time.Buttons.Q22 <- sum(question22.Buttons$Gaze.event.duration..ms.)
perc.time.Buttons.Q22 <- time.Buttons.Q22 / totalFixationTime.Q22


# AOI FeatureModel
question22.FM <- question22 %>% filter(AOI.hit..P02.TOI.Q11.Act22.Snap...Q22.FMAOI.=="1")
fixations.FM.Q22 <- sum(question22$AOI.hit..P02.TOI.Q11.Act22.Snap...Q22.FMAOI.)
perc.fixations.FM.Q22 <- fixations.FM.Q22 / totalFixations.Q22
time.FM.Q22 <- sum(question22.FM$Gaze.event.duration..ms.)
perc.time.FM.Q22 <- time.FM.Q22 / totalFixationTime.Q22


# AOI Containing
question22.Containing <- question22 %>% 
  filter(  AOI.hit..P02.TOI.Q11.Act22.Snap...Q22.CAOI.6.=="1" |	      
  AOI.hit..P02.TOI.Q11.Act22.Snap...Q22.CAOI.3.=="1" |	      
  AOI.hit..P02.TOI.Q11.Act22.Snap...Q22.CAOI.4.=="1" |	      
  AOI.hit..P02.TOI.Q11.Act22.Snap...Q22.CAOI.7.=="1") 
 

if (nrow(question22.Containing) !=0) {
fixations.Containing.Q22 <- sum(question22.Containing %>% select(contains("CAOI")))
perc.fixations.Containing.Q22 <- fixations.Containing.Q22 / totalFixations.Q22
time.Containing.Q22 <- sum(question22.Containing$Gaze.event.duration..ms.)
perc.time.Containing.Q22 <- time.Containing.Q22 / totalFixationTime.Q22
} else {
fixations.Containing.Q22 <- 0
perc.fixations.Containing.Q22 <- 0
time.Containing.Q22 <- 0
perc.time.Containing.Q22 <- 0
}


# AOI Navigating
question22.Navigating <- question22 %>% 
  filter(  AOI.hit..P02.TOI.Q11.Act22.Snap...Q22.NAOI.2.=="1" | 	      
  AOI.hit..P02.TOI.Q11.Act22.Snap...Q22.NAOI.5.=="1" | 	      
  AOI.hit..P02.TOI.Q11.Act22.Snap...Q22.NAOI.1.=="1")
    
      
if (nrow(question22.Navigating) !=0) {
 fixations.Navigating.Q22 <- sum(question22.Navigating %>% select(contains("NAOI")))
 perc.fixations.Navigating.Q22 <- fixations.Navigating.Q22 / totalFixations.Q22
 time.Navigating.Q22 <- sum(question22.Navigating$Gaze.event.duration..ms.)
 perc.time.Navigating.Q22 <- time.Navigating.Q22 / totalFixationTime.Q22
} else {
  fixations.Navigating.Q22 <- 0
  perc.fixations.Navigating.Q22 <- 0
  time.Navigating.Q22 <- 0
  perc.time.Navigating.Q22 <- 0 
}



# AOI CTC
question22.CTC <- question22 %>% filter(AOI.hit..P02.TOI.Q11.Act22.Snap...Q22.CTC.=="1")
fixations.CTC.Q22 <- sum(question22$AOI.hit..P02.TOI.Q11.Act22.Snap...Q22.CTC.)
perc.fixations.CTC.Q22 <- fixations.CTC.Q22 / totalFixations.Q22
time.CTC.Q22 <- sum(question22.CTC$Gaze.event.duration..ms.)
perc.time.CTC.Q22 <- time.CTC.Q22 / totalFixationTime.Q22

# AOI Intersection Containing and Navigating when FM < (Containing + Navigating)
print(c(fixations.FM.Q22, fixations.Containing.Q22, fixations.Navigating.Q22))
print(c(fixations.Window.Q22, fixations.Question.Q22, fixations.Answer.Q22, fixations.Legend.Q22, fixations.Buttons.Q22,
        fixations.CTC.Q22))



# Creating the table now

ParticipantID <- 2
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

 


# --------------------------------------------------------------------------------------------------
# Question 23



participant02.Q23 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP02/P02-TOI-Q10-Act23-Data.csv", header=TRUE)
attach (participant02.Q23)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated02.Q23 <- participant02.Q23 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
  distinct()


question23 <- curated02.Q23

# Totals
totalFixations.Q23 <- nrow(question23)
totalFixationTime.Q23 <- sum(question23$Gaze.event.duration..ms.)


# AOI Window - QNN for participant question
question23.Window <- question23 %>% filter(AOI.hit..P02.TOI.Q10.Act23.Snap...Q23.Window. =="1")
fixations.Window.Q23 <-  sum(question23$AOI.hit..P02.TOI.Q10.Act23.Snap...Q23.Window.)
perc.fixations.Window.Q23 <- fixations.Window.Q23 / totalFixations.Q23
perc.time.Window.Q23 <- sum(question23.Window$Gaze.event.duration..ms.)/totalFixationTime.Q23


# AOI Question
question23.Question <- question23 %>% filter(AOI.hit..P02.TOI.Q10.Act23.Snap...Q23.Question.=="1")
fixations.Question.Q23 <- sum(question23$AOI.hit..P02.TOI.Q10.Act23.Snap...Q23.Question.)
perc.fixations.Question.Q23 <- fixations.Question.Q23 / totalFixations.Q23
time.Question.Q23 <- sum(question23.Question$Gaze.event.duration..ms.)
perc.time.Question.Q23 <- time.Question.Q23 / totalFixationTime.Q23


# AOI Answer
question23.Answer <- question23 %>% filter(AOI.hit..P02.TOI.Q10.Act23.Snap...Q23.Answer.=="1")
fixations.Answer.Q23 <- sum(question23$AOI.hit..P02.TOI.Q10.Act23.Snap...Q23.Answer.)
perc.fixations.Answer.Q23 <- fixations.Answer.Q23 / totalFixations.Q23
time.Answer.Q23 <- sum(question23.Answer$Gaze.event.duration..ms.)
perc.time.Answer.Q23 <- time.Answer.Q23 / totalFixationTime.Q23

# AOI Legend
question23.Legend <- question23 %>% filter(AOI.hit..P02.TOI.Q10.Act23.Snap...Q23.Legend.=="1")
fixations.Legend.Q23 <- sum(question23$AOI.hit..P02.TOI.Q10.Act23.Snap...Q23.Legend.)
perc.fixations.Legend.Q23 <- fixations.Legend.Q23 / totalFixations.Q23
time.Legend.Q23 <- sum(question23.Legend$Gaze.event.duration..ms.)
perc.time.Legend.Q23 <- time.Legend.Q23 / totalFixationTime.Q23

# AOI Buttons
question23.Buttons <- question23 %>% filter(AOI.hit..P02.TOI.Q10.Act23.Snap...Q23.Buttons.=="1")
fixations.Buttons.Q23 <- sum(question23$AOI.hit..P02.TOI.Q10.Act23.Snap...Q23.Buttons.)
perc.fixations.Buttons.Q23 <- fixations.Buttons.Q23 / totalFixations.Q23
time.Buttons.Q23 <- sum(question23.Buttons$Gaze.event.duration..ms.)
perc.time.Buttons.Q23 <- time.Buttons.Q23 / totalFixationTime.Q23


# AOI FeatureModel
question23.FM <- question23 %>% filter(AOI.hit..P02.TOI.Q10.Act23.Snap...Q23.FMAOI.=="1")
fixations.FM.Q23 <- sum(question23$AOI.hit..P02.TOI.Q10.Act23.Snap...Q23.FMAOI.)
perc.fixations.FM.Q23 <- fixations.FM.Q23 / totalFixations.Q23
time.FM.Q23 <- sum(question23.FM$Gaze.event.duration..ms.)
perc.time.FM.Q23 <- time.FM.Q23 / totalFixationTime.Q23


# AOI Containing
question23.Containing <- question23 %>% 
  filter(  AOI.hit..P02.TOI.Q10.Act23.Snap...Q23.CAOI.2.=="1" |	      
  AOI.hit..P02.TOI.Q10.Act23.Snap...Q23.CAOI.3.=="1" |	      
  AOI.hit..P02.TOI.Q10.Act23.Snap...Q23.CAOI.5.=="1") 
 

if (nrow(question23.Containing) !=0) {
fixations.Containing.Q23 <- sum(question23.Containing %>% select(contains("CAOI")))
perc.fixations.Containing.Q23 <- fixations.Containing.Q23 / totalFixations.Q23
time.Containing.Q23 <- sum(question23.Containing$Gaze.event.duration..ms.)
perc.time.Containing.Q23 <- time.Containing.Q23 / totalFixationTime.Q23
} else {
fixations.Containing.Q23 <- 0
perc.fixations.Containing.Q23 <- 0
time.Containing.Q23 <- 0
perc.time.Containing.Q23 <- 0
}


# AOI Navigating
question23.Navigating <- question23 %>% 
  filter(  AOI.hit..P02.TOI.Q10.Act23.Snap...Q23.NAOI.1.=="1" | 	      
  AOI.hit..P02.TOI.Q10.Act23.Snap...Q23.NAOI.4.=="1")
    
      
if (nrow(question23.Navigating) !=0) {
 fixations.Navigating.Q23 <- sum(question23.Navigating %>% select(contains("NAOI")))
 perc.fixations.Navigating.Q23 <- fixations.Navigating.Q23 / totalFixations.Q23
 time.Navigating.Q23 <- sum(question23.Navigating$Gaze.event.duration..ms.)
 perc.time.Navigating.Q23 <- time.Navigating.Q23 / totalFixationTime.Q23
} else {
  fixations.Navigating.Q23 <- 0
  perc.fixations.Navigating.Q23 <- 0
  time.Navigating.Q23 <- 0
  perc.time.Navigating.Q23 <- 0 
}



# AOI CTC
question23.CTC <- question23 %>% filter(AOI.hit..P02.TOI.Q10.Act23.Snap...Q23.CTC.=="1")
fixations.CTC.Q23 <- sum(question23$AOI.hit..P02.TOI.Q10.Act23.Snap...Q23.CTC.)
perc.fixations.CTC.Q23 <- fixations.CTC.Q23 / totalFixations.Q23
time.CTC.Q23 <- sum(question23.CTC$Gaze.event.duration..ms.)
perc.time.CTC.Q23 <- time.CTC.Q23 / totalFixationTime.Q23

# AOI Intersection Containing and Navigating when FM < (Containing + Navigating)
print(c(fixations.FM.Q23, fixations.Containing.Q23, fixations.Navigating.Q23))
print(c(fixations.Window.Q23, fixations.Question.Q23, fixations.Answer.Q23, fixations.Legend.Q23, fixations.Buttons.Q23,
        fixations.CTC.Q23))



# Creating the table now

ParticipantID <- 2
QNumber <- 23

Q23.data <- c(ParticipantID, QNumber, totalFixations.Q23, totalFixationTime.Q23, 
              fixations.Question.Q23, perc.fixations.Question.Q23, time.Question.Q23, perc.time.Question.Q23,
              fixations.Answer.Q23, perc.fixations.Answer.Q23, time.Answer.Q23, perc.time.Answer.Q23,
              fixations.Legend.Q23, perc.fixations.Legend.Q23, time.Legend.Q23, perc.time.Legend.Q23,
              fixations.Buttons.Q23, perc.fixations.Buttons.Q23, time.Buttons.Q23, perc.time.Buttons.Q23,
              fixations.FM.Q23, perc.fixations.FM.Q23, time.FM.Q23, perc.time.FM.Q23,
              fixations.Containing.Q23, perc.fixations.Containing.Q23, time.Containing.Q23, perc.time.Containing.Q23,
              fixations.Navigating.Q23, perc.fixations.Navigating.Q23, time.Navigating.Q23, perc.time.Navigating.Q23,
              fixations.CTC.Q23, perc.fixations.CTC.Q23, time.CTC.Q23, perc.time.CTC.Q23)  

 


# --------------------------------------------------------------------------------------------------
# Question 24



participant02.Q24 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP02/P02-TOI-Q19-Act24-Data.csv", header=TRUE)
attach (participant02.Q24)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated02.Q24 <- participant02.Q24 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
  distinct()


question24 <- curated02.Q24

# Totals
totalFixations.Q24 <- nrow(question24)
totalFixationTime.Q24 <- sum(question24$Gaze.event.duration..ms.)


# AOI Window - QNN for participant question
question24.Window <- question24 %>% filter(AOI.hit..P02.TOI.Q19.Act24.Snap...Q24.Window. =="1")
fixations.Window.Q24 <-  sum(question24$AOI.hit..P02.TOI.Q19.Act24.Snap...Q24.Window.)
perc.fixations.Window.Q24 <- fixations.Window.Q24 / totalFixations.Q24
perc.time.Window.Q24 <- sum(question24.Window$Gaze.event.duration..ms.)/totalFixationTime.Q24


# AOI Question
question24.Question <- question24 %>% filter(AOI.hit..P02.TOI.Q19.Act24.Snap...Q24.Question.=="1")
fixations.Question.Q24 <- sum(question24$AOI.hit..P02.TOI.Q19.Act24.Snap...Q24.Question.)
perc.fixations.Question.Q24 <- fixations.Question.Q24 / totalFixations.Q24
time.Question.Q24 <- sum(question24.Question$Gaze.event.duration..ms.)
perc.time.Question.Q24 <- time.Question.Q24 / totalFixationTime.Q24


# AOI Answer
question24.Answer <- question24 %>% filter(AOI.hit..P02.TOI.Q19.Act24.Snap...Q24.Answer.=="1")
fixations.Answer.Q24 <- sum(question24$AOI.hit..P02.TOI.Q19.Act24.Snap...Q24.Answer.)
perc.fixations.Answer.Q24 <- fixations.Answer.Q24 / totalFixations.Q24
time.Answer.Q24 <- sum(question24.Answer$Gaze.event.duration..ms.)
perc.time.Answer.Q24 <- time.Answer.Q24 / totalFixationTime.Q24

# AOI Legend
question24.Legend <- question24 %>% filter(AOI.hit..P02.TOI.Q19.Act24.Snap...Q24.Legend.=="1")
fixations.Legend.Q24 <- sum(question24$AOI.hit..P02.TOI.Q19.Act24.Snap...Q24.Legend.)
perc.fixations.Legend.Q24 <- fixations.Legend.Q24 / totalFixations.Q24
time.Legend.Q24 <- sum(question24.Legend$Gaze.event.duration..ms.)
perc.time.Legend.Q24 <- time.Legend.Q24 / totalFixationTime.Q24

# AOI Buttons
question24.Buttons <- question24 %>% filter(AOI.hit..P02.TOI.Q19.Act24.Snap...Q24.Buttons.=="1")
fixations.Buttons.Q24 <- sum(question24$AOI.hit..P02.TOI.Q19.Act24.Snap...Q24.Buttons.)
perc.fixations.Buttons.Q24 <- fixations.Buttons.Q24 / totalFixations.Q24
time.Buttons.Q24 <- sum(question24.Buttons$Gaze.event.duration..ms.)
perc.time.Buttons.Q24 <- time.Buttons.Q24 / totalFixationTime.Q24


# AOI FeatureModel
question24.FM <- question24 %>% filter(AOI.hit..P02.TOI.Q19.Act24.Snap...Q24.FMAOI.=="1")
fixations.FM.Q24 <- sum(question24$AOI.hit..P02.TOI.Q19.Act24.Snap...Q24.FMAOI.)
perc.fixations.FM.Q24 <- fixations.FM.Q24 / totalFixations.Q24
time.FM.Q24 <- sum(question24.FM$Gaze.event.duration..ms.)
perc.time.FM.Q24 <- time.FM.Q24 / totalFixationTime.Q24


# AOI Containing
question24.Containing <- question24 %>% 
  filter(  AOI.hit..P02.TOI.Q19.Act24.Snap...Q24.CAOI.2.=="1" |	      
  AOI.hit..P02.TOI.Q19.Act24.Snap...Q24.CAOI.5.=="1" |	      
  AOI.hit..P02.TOI.Q19.Act24.Snap...Q24.CAOI.6.=="1" |	      
  AOI.hit..P02.TOI.Q19.Act24.Snap...Q24.CAOI.7.=="1" |	      
  AOI.hit..P02.TOI.Q19.Act24.Snap...Q24.CAOI.9.=="1" |	      
  AOI.hit..P02.TOI.Q19.Act24.Snap...Q24.CAOI.10.=="1") 
 

if (nrow(question24.Containing) !=0) {
fixations.Containing.Q24 <- sum(question24.Containing %>% select(contains("CAOI")))
perc.fixations.Containing.Q24 <- fixations.Containing.Q24 / totalFixations.Q24
time.Containing.Q24 <- sum(question24.Containing$Gaze.event.duration..ms.)
perc.time.Containing.Q24 <- time.Containing.Q24 / totalFixationTime.Q24
} else {
fixations.Containing.Q24 <- 0
perc.fixations.Containing.Q24 <- 0
time.Containing.Q24 <- 0
perc.time.Containing.Q24 <- 0
}


# AOI Navigating
question24.Navigating <- question24 %>% 
  filter(  AOI.hit..P02.TOI.Q19.Act24.Snap...Q24.NAOI.1.=="1" | 	      
  AOI.hit..P02.TOI.Q19.Act24.Snap...Q24.NAOI.3.=="1" | 	      
  AOI.hit..P02.TOI.Q19.Act24.Snap...Q24.NAOI.4.=="1" | 	      
  AOI.hit..P02.TOI.Q19.Act24.Snap...Q24.NAOI.8.=="1")
    
      
if (nrow(question24.Navigating) !=0) {
 fixations.Navigating.Q24 <- sum(question24.Navigating %>% select(contains("NAOI")))
 perc.fixations.Navigating.Q24 <- fixations.Navigating.Q24 / totalFixations.Q24
 time.Navigating.Q24 <- sum(question24.Navigating$Gaze.event.duration..ms.)
 perc.time.Navigating.Q24 <- time.Navigating.Q24 / totalFixationTime.Q24
} else {
  fixations.Navigating.Q24 <- 0
  perc.fixations.Navigating.Q24 <- 0
  time.Navigating.Q24 <- 0
  perc.time.Navigating.Q24 <- 0 
}



# AOI CTC
question24.CTC <- question24 %>% filter(AOI.hit..P02.TOI.Q19.Act24.Snap...Q24.CTC.=="1")
fixations.CTC.Q24 <- sum(question24$AOI.hit..P02.TOI.Q19.Act24.Snap...Q24.CTC.)
perc.fixations.CTC.Q24 <- fixations.CTC.Q24 / totalFixations.Q24
time.CTC.Q24 <- sum(question24.CTC$Gaze.event.duration..ms.)
perc.time.CTC.Q24 <- time.CTC.Q24 / totalFixationTime.Q24

# AOI Intersection Containing and Navigating when FM < (Containing + Navigating)
print(c(fixations.FM.Q24, fixations.Containing.Q24, fixations.Navigating.Q24))
print(c(fixations.Window.Q24, fixations.Question.Q24, fixations.Answer.Q24, fixations.Legend.Q24, fixations.Buttons.Q24,
        fixations.CTC.Q24))



# Creating the table now

ParticipantID <- 2
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


# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------

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
allQuestions.data.frame[nrow(allQuestions.data.frame) + 1,] = Q01.data
allQuestions.data.frame[nrow(allQuestions.data.frame) + 1,] = Q02.data
allQuestions.data.frame[nrow(allQuestions.data.frame) + 1,] = Q03.data
allQuestions.data.frame[nrow(allQuestions.data.frame) + 1,] = Q04.data
allQuestions.data.frame[nrow(allQuestions.data.frame) + 1,] = Q05.data
allQuestions.data.frame[nrow(allQuestions.data.frame) + 1,] = Q06.data
allQuestions.data.frame[nrow(allQuestions.data.frame) + 1,] = Q07.data
allQuestions.data.frame[nrow(allQuestions.data.frame) + 1,] = Q08.data
allQuestions.data.frame[nrow(allQuestions.data.frame) + 1,] = Q09.data
allQuestions.data.frame[nrow(allQuestions.data.frame) + 1,] = Q10.data
allQuestions.data.frame[nrow(allQuestions.data.frame) + 1,] = Q11.data
allQuestions.data.frame[nrow(allQuestions.data.frame) + 1,] = Q12.data
allQuestions.data.frame[nrow(allQuestions.data.frame) + 1,] = Q13.data
allQuestions.data.frame[nrow(allQuestions.data.frame) + 1,] = Q14.data
allQuestions.data.frame[nrow(allQuestions.data.frame) + 1,] = Q15.data
allQuestions.data.frame[nrow(allQuestions.data.frame) + 1,] = Q16.data
allQuestions.data.frame[nrow(allQuestions.data.frame) + 1,] = Q17.data
allQuestions.data.frame[nrow(allQuestions.data.frame) + 1,] = Q18.data
allQuestions.data.frame[nrow(allQuestions.data.frame) + 1,] = Q19.data
allQuestions.data.frame[nrow(allQuestions.data.frame) + 1,] = Q20.data
allQuestions.data.frame[nrow(allQuestions.data.frame) + 1,] = Q21.data
allQuestions.data.frame[nrow(allQuestions.data.frame) + 1,] = Q22.data
allQuestions.data.frame[nrow(allQuestions.data.frame) + 1,] = Q23.data
allQuestions.data.frame[nrow(allQuestions.data.frame) + 1,] = Q24.data


write.csv(allQuestions.data.frame,file = "../../Experiment-Data/Eye-tracking-data-samples/PartP02/P02.csv", row.names = FALSE)
