# Processing data for answer eye transitions
# This file tests the code that is needed to produce the data for computing the eye transitions

library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(dplyr)



# --------------------------------------------------------------------------------------------------
# Question 1

# This question does not contain CTC
participant05.Q01 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP05/P05-TOI-Q01-Act01-Data.csv", header=TRUE)
attach (participant05.Q01)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated05.Q01 <- participant05.Q01 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type.index, Gaze.event.duration..ms., starts_with("AOI.hit")) %>%
  distinct() %>%
  # renames the column names to be standard across the answers
  rename(Index=Eye.movement.type.index) %>%
  rename(Duration=Gaze.event.duration..ms.) %>%
  rename(Answer=AOI.hit..P05.TOI.Q01.Act01.Snap...Q01.Answer.) %>%
  rename(Buttons=AOI.hit..P05.TOI.Q01.Act01.Snap...Q01.Buttons.) %>% 
  rename(FM=AOI.hit..P05.TOI.Q01.Act01.Snap...Q01.FMAOI.) %>%
  rename(Legend=AOI.hit..P05.TOI.Q01.Act01.Snap...Q01.Legend.) %>% 
  rename(Question=AOI.hit..P05.TOI.Q01.Act01.Snap...Q01.Question.) %>%
  rename(Window=AOI.hit..P05.TOI.Q01.Act01.Snap...Q01.Window.) %>%
  filter(Window=="1") %>%
  select(Index,Duration,Answer,Buttons,FM,Legend,Question,Window)
  

# If the question lacks CTCs, add the default empty CTC and put it after Buttons
curated05.Q01$CTC <- rep(0,nrow(curated05.Q01))
curated05.Q01 <- curated05.Q01 %>% relocate(CTC, .before = FM)

question01 <- curated05.Q01


# --------------------------------------------------------------------------------------------------
# Question 3

participant05.Q03 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP05/P05-TOI-Q23-Act03-Data.csv", header=TRUE)
attach (participant05.Q03)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
# curated05.Q03 <- participant05.Q03 %>% filter(!across(everything(), is.na)) %>% 
#  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
#  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
#  distinct()


# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated05.Q03 <- participant05.Q03 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type.index, Gaze.event.duration..ms., starts_with("AOI.hit")) %>%
  distinct() %>%
  # renames the column names to be standard across the answers
  rename(Index=Eye.movement.type.index) %>%
  rename(Duration=Gaze.event.duration..ms.) %>%
  rename(Answer=AOI.hit..P05.TOI.Q23.Act03.Snap...Q03.Answer.) %>%
  rename(Buttons=AOI.hit..P05.TOI.Q23.Act03.Snap...Q03.Buttons.) %>% 
  rename(CTC=AOI.hit..P05.TOI.Q23.Act03.Snap...Q03.CTC.) %>%  
  rename(FM=AOI.hit..P05.TOI.Q23.Act03.Snap...Q03.FMAOI.) %>%
  rename(Legend=AOI.hit..P05.TOI.Q23.Act03.Snap...Q03.Legend.) %>% 
  rename(Question=AOI.hit..P05.TOI.Q23.Act03.Snap...Q03.Question.) %>%
  rename(Window=AOI.hit..P05.TOI.Q23.Act03.Snap...Q03.Window.) %>%
  filter(Window=="1") %>%
  select(Index,Duration,Answer,Buttons,CTC,FM,Legend,Question,Window)

question03 <- curated05.Q03


# [1] "Eye.movement.type.index"                         "Gaze.event.duration..ms."                       
# [3] "AOI.hit..P05.TOI.Q23.Act03.Snap...Q03.Answer."   "AOI.hit..P05.TOI.Q23.Act03.Snap...Q03.Buttons." 
# [5] "AOI.hit..P05.TOI.Q23.Act03.Snap...Q03.CAOI.1."   "AOI.hit..P05.TOI.Q23.Act03.Snap...Q03.CAOI.2."  
# [7] "AOI.hit..P05.TOI.Q23.Act03.Snap...Q03.CAOI.3."   "AOI.hit..P05.TOI.Q23.Act03.Snap...Q03.CAOI.4."  
# [9] "AOI.hit..P05.TOI.Q23.Act03.Snap...Q03.CTC."      "AOI.hit..P05.TOI.Q23.Act03.Snap...Q03.FMAOI."   
# [11] "AOI.hit..P05.TOI.Q23.Act03.Snap...Q03.Legend."   "AOI.hit..P05.TOI.Q23.Act03.Snap...Q03.Question."
# [13] "AOI.hit..P05.TOI.Q23.Act03.Snap...Q03.Window."  

