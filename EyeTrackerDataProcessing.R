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

# Notes: 
# 1. Eliminating the NA rows dont help as 
# %>% select(-Column1,-Column2, -Column3, -Column4) %>% drop_na()  


curated03 <- participant03 %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit"))
#         AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Answer.,AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Window.)
#         AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Window.:AOI.hit..P03.TOI.Q01.Act20.Snap...Q20.Answer.)

# What info is repeated?, 
repeated <- participant03 %>% 
  filter(Eye.movement.type.index=="570") %>% 
#  filter(Eye.movement.type=="Fixation" & Eye.movement.type.index=="566") %>% 
  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit"))



### Question 20 processing
question20 <- curated03

numFixations <- nrow(question20)
totalTime <- sum(question20$Gaze.event.duration..ms.)
