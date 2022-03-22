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
# Question 2

participant05.Q02 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP05/P05-TOI-Q10-Act02-Data.csv", header=TRUE)
attach (participant05.Q02)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated05.Q02 <- participant05.Q02 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type.index, Gaze.event.duration..ms., starts_with("AOI.hit")) %>%
  distinct() %>%
  # renames the column names to be standard across the answers
  rename(Index=Eye.movement.type.index) %>%
  rename(Duration=Gaze.event.duration..ms.) %>%
  rename(Answer=AOI.hit..P05.TOI.Q10.Act02.Snap...Q02.Answer.) %>%
  rename(Buttons=AOI.hit..P05.TOI.Q10.Act02.Snap...Q02.Buttons.) %>% 
#  rename(CTC=AOI.hit..P05.TOI.Q10.Act02.Snap...Q02.CTC.) %>%  
  rename(FM=AOI.hit..P05.TOI.Q10.Act02.Snap...Q02.FMAOI.) %>%
  rename(Legend=AOI.hit..P05.TOI.Q10.Act02.Snap...Q02.Legend.) %>% 
  rename(Question=AOI.hit..P05.TOI.Q10.Act02.Snap...Q02.Question.) %>%
  rename(Window=AOI.hit..P05.TOI.Q10.Act02.Snap...Q02.Window.) %>%
  filter(Window=="1") %>%
  select(Index,Duration,Answer,Buttons,FM,Legend,Question,Window)

  curated05.Q02$CTC <- rep(0,nrow(curated05.Q02))
  curated05.Q02 <- curated05.Q02 %>% relocate(CTC, .before = FM)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
# curated05.Q02 <- participant05.Q02 %>% filter(!across(everything(), is.na)) %>% 
#  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
#  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
#  distinct()

question02 <- curated05.Q02


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


# --------------------------------------------------------------------------------------------------
# Question 4

participant05.Q04 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP05/P05-TOI-Q04-Act04-Data.csv", header=TRUE)
attach (participant05.Q04)


# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated05.Q04 <- participant05.Q04 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type.index, Gaze.event.duration..ms., starts_with("AOI.hit")) %>%
  distinct() %>%
  # renames the column names to be standard across the answers
  rename(Index=Eye.movement.type.index) %>%
  rename(Duration=Gaze.event.duration..ms.) %>%
  rename(Answer=AOI.hit..P05.TOI.Q04.Act04.Snap...Q04.Answer.) %>%
  rename(Buttons=AOI.hit..P05.TOI.Q04.Act04.Snap...Q04.Buttons.) %>% 
  rename(CTC=AOI.hit..P05.TOI.Q04.Act04.Snap...Q04.CTC.) %>%  
  rename(FM=AOI.hit..P05.TOI.Q04.Act04.Snap...Q04.FMAOI.) %>%
  rename(Legend=AOI.hit..P05.TOI.Q04.Act04.Snap...Q04.Legend.) %>% 
  rename(Question=AOI.hit..P05.TOI.Q04.Act04.Snap...Q04.Question.) %>%
  rename(Window=AOI.hit..P05.TOI.Q04.Act04.Snap...Q04.Window.) %>%
  filter(Window=="1") %>%
  select(Index,Duration,Answer,Buttons,CTC,FM,Legend,Question,Window)

question04 <- curated05.Q04



# --------------------------------------------------------------------------------------------------
# Question 5


participant05.Q05 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP05/P05-TOI-Q02-Act05-Data.csv", header=TRUE)
attach (participant05.Q05)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated05.Q05 <- participant05.Q05 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type.index, Gaze.event.duration..ms., starts_with("AOI.hit")) %>%
  distinct() %>%
  # renames the column names to be standard across the answers
  rename(Index=Eye.movement.type.index) %>%
  rename(Duration=Gaze.event.duration..ms.) %>%
  rename(Answer=AOI.hit..P05.TOI.Q02.Act05.Snap...Q05.Answer.) %>%
  rename(Buttons=AOI.hit..P05.TOI.Q02.Act05.Snap...Q05.Buttons.) %>% 
  rename(CTC=AOI.hit..P05.TOI.Q02.Act05.Snap...Q05.CTC.) %>%  
  rename(FM=AOI.hit..P05.TOI.Q02.Act05.Snap...Q05.FMAOI.) %>%
  rename(Legend=AOI.hit..P05.TOI.Q02.Act05.Snap...Q05.Legend.) %>% 
  rename(Question=AOI.hit..P05.TOI.Q02.Act05.Snap...Q05.Question.) %>%
  rename(Window=AOI.hit..P05.TOI.Q02.Act05.Snap...Q05.Window.) %>%
  filter(Window=="1") %>%
  select(Index,Duration,Answer,Buttons,CTC,FM,Legend,Question,Window)

question05 <- curated05.Q05



# --------------------------------------------------------------------------------------------------
# Question 6


participant05.Q06 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP05/P05-TOI-Q11-Act06-Data.csv", header=TRUE)
attach (participant05.Q06)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated05.Q06 <- participant05.Q06 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type.index, Gaze.event.duration..ms., starts_with("AOI.hit")) %>%
  distinct() %>%
  # renames the column names to be standard across the answers
  rename(Index=Eye.movement.type.index) %>%
  rename(Duration=Gaze.event.duration..ms.) %>%
  rename(Answer=AOI.hit..P05.TOI.Q11.Act06.Snap...Q06.Answer.) %>%
  rename(Buttons=AOI.hit..P05.TOI.Q11.Act06.Snap...Q06.Buttons.) %>% 
  rename(CTC=AOI.hit..P05.TOI.Q11.Act06.Snap...Q06.CTC.) %>%  
  rename(FM=AOI.hit..P05.TOI.Q11.Act06.Snap...Q06.FMAOI.) %>%
  rename(Legend=AOI.hit..P05.TOI.Q11.Act06.Snap...Q06.Legend.) %>% 
  rename(Question=AOI.hit..P05.TOI.Q11.Act06.Snap...Q06.Question.) %>%
  rename(Window=AOI.hit..P05.TOI.Q11.Act06.Snap...Q06.Window.) %>%
  filter(Window=="1") %>%
  select(Index,Duration,Answer,Buttons,CTC,FM,Legend,Question,Window)

question06 <- curated05.Q06


# --------------------------------------------------------------------------------------------------
# Question 7

participant05.Q07 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP05/P05-TOI-Q24-Act07-Data.csv", header=TRUE)
attach (participant05.Q07)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated05.Q07 <- participant05.Q07 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type.index, Gaze.event.duration..ms., starts_with("AOI.hit")) %>%
  distinct() %>%
  # renames the column names to be standard across the answers
  rename(Index=Eye.movement.type.index) %>%
  rename(Duration=Gaze.event.duration..ms.) %>%
  rename(Answer=AOI.hit..P05.TOI.Q24.Act07.Snap...Q07.Answer.) %>%
  rename(Buttons=AOI.hit..P05.TOI.Q24.Act07.Snap...Q07.Buttons.) %>% 
#  rename(CTC=AOI.hit..P05.TOI.Q24.Act07.Snap...Q07.CTC.) %>%  
  rename(FM=AOI.hit..P05.TOI.Q24.Act07.Snap...Q07.FMAOI.) %>%
  rename(Legend=AOI.hit..P05.TOI.Q24.Act07.Snap...Q07.Legend.) %>% 
  rename(Question=AOI.hit..P05.TOI.Q24.Act07.Snap...Q07.Question.) %>%
  rename(Window=AOI.hit..P05.TOI.Q24.Act07.Snap...Q07.Window.) %>%
  filter(Window=="1") %>%
  select(Index,Duration,Answer,Buttons,FM,Legend,Question,Window)


# If the question lacks CTCs, add the default empty CTC and put it after Buttons
curated05.Q07$CTC <- rep(0,nrow(curated05.Q07))
curated05.Q07 <- curated05.Q07 %>% relocate(CTC, .before = FM)


question07 <- curated05.Q07


# --------------------------------------------------------------------------------------------------
# Question 8

participant05.Q08 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP05/P05-TOI-Q14-Act08-Data.csv", header=TRUE)
attach (participant05.Q08)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated05.Q08 <- participant05.Q08 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type.index, Gaze.event.duration..ms., starts_with("AOI.hit")) %>%
  distinct() %>%
  # renames the column names to be standard across the answers
  rename(Index=Eye.movement.type.index) %>%
  rename(Duration=Gaze.event.duration..ms.) %>%
  rename(Answer=AOI.hit..P05.TOI.Q14.Act08.Snap...Q08.Answer.) %>%
  rename(Buttons=AOI.hit..P05.TOI.Q14.Act08.Snap...Q08.Buttons.) %>% 
# rename(CTC=AOI.hit..P05.TOI.Q14.Act08.Snap...Q08.CTC.) %>%  
  rename(FM=AOI.hit..P05.TOI.Q14.Act08.Snap...Q08.FMAOI.) %>%
  rename(Legend=AOI.hit..P05.TOI.Q14.Act08.Snap...Q08.Legend.) %>% 
  rename(Question=AOI.hit..P05.TOI.Q14.Act08.Snap...Q08.Question.) %>%
  rename(Window=AOI.hit..P05.TOI.Q14.Act08.Snap...Q08.Window.) %>%
  filter(Window=="1") %>%
  select(Index,Duration,Answer,Buttons,FM,Legend,Question,Window)


# If the question lacks CTCs, add the default empty CTC and put it after Buttons
curated05.Q08$CTC <- rep(0,nrow(curated05.Q08))
curated05.Q08 <- curated05.Q08 %>% relocate(CTC, .before = FM)

question08 <- curated05.Q08


