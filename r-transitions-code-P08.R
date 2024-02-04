# Processing data for answer eye transitions
# This file tests the code that is needed to produce the data for computing the eye transitions

library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(dplyr)


# Example of the code that needs to be generated for each participant to compute the transitions

# --------------------------------------------------------------------------------------------------
# Question 1

# This question does not contain CTC
participant08.Q01 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP08/P08-TOI-Q22-Act01-Data.csv", header=TRUE)
attach (participant08.Q01)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated08.Q01 <- participant08.Q01 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type.index, Gaze.event.duration..ms., starts_with("AOI.hit")) %>%
  distinct() %>%
  # renames the column names to be standard across the answers
  rename(Index=Eye.movement.type.index) %>%
  rename(Duration=Gaze.event.duration..ms.) %>%
  rename(Answer=AOI.hit..P08.TOI.Q22.Act01.Snap...Q01.Answer.) %>%
  rename(Buttons=AOI.hit..P08.TOI.Q22.Act01.Snap...Q01.Buttons.) %>% 
  rename(FM=AOI.hit..P08.TOI.Q22.Act01.Snap...Q01.FMAOI.) %>%
  rename(Legend=AOI.hit..P08.TOI.Q22.Act01.Snap...Q01.Legend.) %>% 
  rename(Question=AOI.hit..P08.TOI.Q22.Act01.Snap...Q01.Question.) %>%
  rename(Window=AOI.hit..P08.TOI.Q22.Act01.Snap...Q01.Window.) %>%
  filter(Window=="1") %>%
  select(Index,Duration,Answer,Buttons,FM,Legend,Question,Window)
  

# If the question lacks CTCs, add the default empty CTC and put it after Buttons
curated08.Q01$CTC <- rep(0,nrow(curated08.Q01))
curated08.Q01 <- curated08.Q01 %>% relocate(CTC, .before = FM)

question01 <- curated08.Q01


# --------------------------------------------------------------------------------------------------
# Question 2

participant08.Q02 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP08/P08-TOI-Q02-Act02-Data.csv", header=TRUE)
attach (participant08.Q02)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated08.Q02 <- participant08.Q02 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type.index, Gaze.event.duration..ms., starts_with("AOI.hit")) %>%
  distinct() %>%
  # renames the column names to be standard across the answers
  rename(Index=Eye.movement.type.index) %>%
  rename(Duration=Gaze.event.duration..ms.) %>%
  rename(Answer=AOI.hit..P08.TOI.Q02.Act02.Snap...Q02.Answer.) %>%
  rename(Buttons=AOI.hit..P08.TOI.Q02.Act02.Snap...Q02.Buttons.) %>% 
#  rename(CTC=AOI.hit..P08.TOI.Q02.Act02.Snap...Q02.CTC.) %>%  
  rename(FM=AOI.hit..P08.TOI.Q02.Act02.Snap...Q02.FMAOI.) %>%
  rename(Legend=AOI.hit..P08.TOI.Q02.Act02.Snap...Q02.Legend.) %>% 
  rename(Question=AOI.hit..P08.TOI.Q02.Act02.Snap...Q02.Question.) %>%
  rename(Window=AOI.hit..P08.TOI.Q02.Act02.Snap...Q02.Window.) %>%
  filter(Window=="1") %>%
  select(Index,Duration,Answer,Buttons,FM,Legend,Question,Window)

  curated08.Q02$CTC <- rep(0,nrow(curated08.Q02))
  curated08.Q02 <- curated08.Q02 %>% relocate(CTC, .before = FM)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
# curated08.Q02 <- participant08.Q02 %>% filter(!across(everything(), is.na)) %>% 
#  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
#  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
#  distinct()

question02 <- curated08.Q02


# --------------------------------------------------------------------------------------------------
# Question 3

participant08.Q03 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP08/P08-TOI-Q04-Act03-Data.csv", header=TRUE)
attach (participant08.Q03)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
# curated08.Q03 <- participant08.Q03 %>% filter(!across(everything(), is.na)) %>% 
#  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
#  select(Eye.movement.type, Eye.movement.type.index, Gaze.event.duration..ms., Sensor, starts_with("AOI.hit")) %>%
#  distinct()


# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated08.Q03 <- participant08.Q03 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type.index, Gaze.event.duration..ms., starts_with("AOI.hit")) %>%
  distinct() %>%
  # renames the column names to be standard across the answers
  rename(Index=Eye.movement.type.index) %>%
  rename(Duration=Gaze.event.duration..ms.) %>%
  rename(Answer=AOI.hit..P08.TOI.Q04.Act03.Snap...Q03.Answer.) %>%
  rename(Buttons=AOI.hit..P08.TOI.Q04.Act03.Snap...Q03.Buttons.) %>% 
  rename(CTC=AOI.hit..P08.TOI.Q04.Act03.Snap...Q03.CTC.) %>%  
  rename(FM=AOI.hit..P08.TOI.Q04.Act03.Snap...Q03.FMAOI.) %>%
  rename(Legend=AOI.hit..P08.TOI.Q04.Act03.Snap...Q03.Legend.) %>% 
  rename(Question=AOI.hit..P08.TOI.Q04.Act03.Snap...Q03.Question.) %>%
  rename(Window=AOI.hit..P08.TOI.Q04.Act03.Snap...Q03.Window.) %>%
  filter(Window=="1") %>%
  select(Index,Duration,Answer,Buttons,CTC,FM,Legend,Question,Window)

question03 <- curated08.Q03


# --------------------------------------------------------------------------------------------------
# Question 4

participant08.Q04 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP08/P08-TOI-Q20-Act04-Data.csv", header=TRUE)
attach (participant08.Q04)


# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated08.Q04 <- participant08.Q04 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type.index, Gaze.event.duration..ms., starts_with("AOI.hit")) %>%
  distinct() %>%
  # renames the column names to be standard across the answers
  rename(Index=Eye.movement.type.index) %>%
  rename(Duration=Gaze.event.duration..ms.) %>%
  rename(Answer=AOI.hit..P08.TOI.Q20.Act04.Snap...Q04.Answer.) %>%
  rename(Buttons=AOI.hit..P08.TOI.Q20.Act04.Snap...Q04.Buttons.) %>% 
  rename(CTC=AOI.hit..P08.TOI.Q20.Act04.Snap...Q04.CTC.) %>%  
  rename(FM=AOI.hit..P08.TOI.Q20.Act04.Snap...Q04.FMAOI.) %>%
  rename(Legend=AOI.hit..P08.TOI.Q20.Act04.Snap...Q04.Legend.) %>% 
  rename(Question=AOI.hit..P08.TOI.Q20.Act04.Snap...Q04.Question.) %>%
  rename(Window=AOI.hit..P08.TOI.Q20.Act04.Snap...Q04.Window.) %>%
  filter(Window=="1") %>%
  select(Index,Duration,Answer,Buttons,CTC,FM,Legend,Question,Window)

question04 <- curated08.Q04



# --------------------------------------------------------------------------------------------------
# Question 5


participant08.Q05 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP08/P08-TOI-Q16-Act05-Data.csv", header=TRUE)
attach (participant08.Q05)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated08.Q05 <- participant08.Q05 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type.index, Gaze.event.duration..ms., starts_with("AOI.hit")) %>%
  distinct() %>%
  # renames the column names to be standard across the answers
  rename(Index=Eye.movement.type.index) %>%
  rename(Duration=Gaze.event.duration..ms.) %>%
  rename(Answer=AOI.hit..P08.TOI.Q16.Act05.Snap...Q05.Answer.) %>%
  rename(Buttons=AOI.hit..P08.TOI.Q16.Act05.Snap...Q05.Buttons.) %>% 
  rename(CTC=AOI.hit..P08.TOI.Q16.Act05.Snap...Q05.CTC.) %>%  
  rename(FM=AOI.hit..P08.TOI.Q16.Act05.Snap...Q05.FMAOI.) %>%
  rename(Legend=AOI.hit..P08.TOI.Q16.Act05.Snap...Q05.Legend.) %>% 
  rename(Question=AOI.hit..P08.TOI.Q16.Act05.Snap...Q05.Question.) %>%
  rename(Window=AOI.hit..P08.TOI.Q16.Act05.Snap...Q05.Window.) %>%
  filter(Window=="1") %>%
  select(Index,Duration,Answer,Buttons,CTC,FM,Legend,Question,Window)

question05 <- curated08.Q05



# --------------------------------------------------------------------------------------------------
# Question 6


participant08.Q06 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP08/P08-TOI-Q13-Act06-Data.csv", header=TRUE)
attach (participant08.Q06)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated08.Q06 <- participant08.Q06 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type.index, Gaze.event.duration..ms., starts_with("AOI.hit")) %>%
  distinct() %>%
  # renames the column names to be standard across the answers
  rename(Index=Eye.movement.type.index) %>%
  rename(Duration=Gaze.event.duration..ms.) %>%
  rename(Answer=AOI.hit..P08.TOI.Q13.Act06.Snap...Q06.Answer.) %>%
  rename(Buttons=AOI.hit..P08.TOI.Q13.Act06.Snap...Q06.Buttons.) %>% 
  rename(CTC=AOI.hit..P08.TOI.Q13.Act06.Snap...Q06.CTC.) %>%  
  rename(FM=AOI.hit..P08.TOI.Q13.Act06.Snap...Q06.FMAOI.) %>%
  rename(Legend=AOI.hit..P08.TOI.Q13.Act06.Snap...Q06.Legend.) %>% 
  rename(Question=AOI.hit..P08.TOI.Q13.Act06.Snap...Q06.Question.) %>%
  rename(Window=AOI.hit..P08.TOI.Q13.Act06.Snap...Q06.Window.) %>%
  filter(Window=="1") %>%
  select(Index,Duration,Answer,Buttons,CTC,FM,Legend,Question,Window)

question06 <- curated08.Q06


# --------------------------------------------------------------------------------------------------
# Question 7

participant08.Q07 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP08/P08-TOI-Q07-Act07-Data.csv", header=TRUE)
attach (participant08.Q07)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated08.Q07 <- participant08.Q07 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type.index, Gaze.event.duration..ms., starts_with("AOI.hit")) %>%
  distinct() %>%
  # renames the column names to be standard across the answers
  rename(Index=Eye.movement.type.index) %>%
  rename(Duration=Gaze.event.duration..ms.) %>%
  rename(Answer=AOI.hit..P08.TOI.Q07.Act07.Snap...Q07.Answer.) %>%
  rename(Buttons=AOI.hit..P08.TOI.Q07.Act07.Snap...Q07.Buttons.) %>% 
#  rename(CTC=AOI.hit..P08.TOI.Q07.Act07.Snap...Q07.CTC.) %>%  
  rename(FM=AOI.hit..P08.TOI.Q07.Act07.Snap...Q07.FMAOI.) %>%
  rename(Legend=AOI.hit..P08.TOI.Q07.Act07.Snap...Q07.Legend.) %>% 
  rename(Question=AOI.hit..P08.TOI.Q07.Act07.Snap...Q07.Question.) %>%
  rename(Window=AOI.hit..P08.TOI.Q07.Act07.Snap...Q07.Window.) %>%
  filter(Window=="1") %>%
  select(Index,Duration,Answer,Buttons,FM,Legend,Question,Window)


# If the question lacks CTCs, add the default empty CTC and put it after Buttons
curated08.Q07$CTC <- rep(0,nrow(curated08.Q07))
curated08.Q07 <- curated08.Q07 %>% relocate(CTC, .before = FM)


question07 <- curated08.Q07


# --------------------------------------------------------------------------------------------------
# Question 8

participant08.Q08 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP08/P08-TOI-Q12-Act08-Data.csv", header=TRUE)
attach (participant08.Q08)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated08.Q08 <- participant08.Q08 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type.index, Gaze.event.duration..ms., starts_with("AOI.hit")) %>%
  distinct() %>%
  # renames the column names to be standard across the answers
  rename(Index=Eye.movement.type.index) %>%
  rename(Duration=Gaze.event.duration..ms.) %>%
  rename(Answer=AOI.hit..P08.TOI.Q12.Act08.Snap...Q08.Answer.) %>%
  rename(Buttons=AOI.hit..P08.TOI.Q12.Act08.Snap...Q08.Buttons.) %>% 
# rename(CTC=AOI.hit..P08.TOI.Q12.Act08.Snap...Q08.CTC.) %>%  
  rename(FM=AOI.hit..P08.TOI.Q12.Act08.Snap...Q08.FMAOI.) %>%
  rename(Legend=AOI.hit..P08.TOI.Q12.Act08.Snap...Q08.Legend.) %>% 
  rename(Question=AOI.hit..P08.TOI.Q12.Act08.Snap...Q08.Question.) %>%
  rename(Window=AOI.hit..P08.TOI.Q12.Act08.Snap...Q08.Window.) %>%
  filter(Window=="1") %>%
  select(Index,Duration,Answer,Buttons,FM,Legend,Question,Window)


# If the question lacks CTCs, add the default empty CTC and put it after Buttons
curated08.Q08$CTC <- rep(0,nrow(curated08.Q08))
curated08.Q08 <- curated08.Q08 %>% relocate(CTC, .before = FM)

question08 <- curated08.Q08


# --------------------------------------------------------------------------------------------------
# Question 9

participant08.Q09 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP08/P08-TOI-Q24-Act09-Data.csv", header=TRUE)
attach (participant08.Q09)


# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated08.Q09 <- participant08.Q09 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type.index, Gaze.event.duration..ms., starts_with("AOI.hit")) %>%
  distinct() %>%
  # renames the column names to be standard across the answers
  rename(Index=Eye.movement.type.index) %>%
  rename(Duration=Gaze.event.duration..ms.) %>%
  rename(Answer=AOI.hit..P08.TOI.Q24.Act09.Snap...Q09.Answer.) %>%
  rename(Buttons=AOI.hit..P08.TOI.Q24.Act09.Snap...Q09.Buttons.) %>% 
  rename(CTC=AOI.hit..P08.TOI.Q24.Act09.Snap...Q09.CTC.) %>%  
  rename(FM=AOI.hit..P08.TOI.Q24.Act09.Snap...Q09.FMAOI.) %>%
  rename(Legend=AOI.hit..P08.TOI.Q24.Act09.Snap...Q09.Legend.) %>% 
  rename(Question=AOI.hit..P08.TOI.Q24.Act09.Snap...Q09.Question.) %>%
  rename(Window=AOI.hit..P08.TOI.Q24.Act09.Snap...Q09.Window.) %>%
  filter(Window=="1") %>%
  select(Index,Duration,Answer,Buttons,CTC,FM,Legend,Question,Window)


question09 <- curated08.Q09


# --------------------------------------------------------------------------------------------------
# Question 10

participant08.Q10 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP08/P08-TOI-Q08-Act10-Data.csv", header=TRUE)
attach (participant08.Q10)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated08.Q10 <- participant08.Q10 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type.index, Gaze.event.duration..ms., starts_with("AOI.hit")) %>%
  distinct() %>%
  # renames the column names to be standard across the answers
  rename(Index=Eye.movement.type.index) %>%
  rename(Duration=Gaze.event.duration..ms.) %>%
  rename(Answer=AOI.hit..P08.TOI.Q08.Act10.Snap...Q10.Answer.) %>%
  rename(Buttons=AOI.hit..P08.TOI.Q08.Act10.Snap...Q10.Buttons.) %>% 
  rename(CTC=AOI.hit..P08.TOI.Q08.Act10.Snap...Q10.CTC.) %>%  
  rename(FM=AOI.hit..P08.TOI.Q08.Act10.Snap...Q10.FMAOI.) %>%
  rename(Legend=AOI.hit..P08.TOI.Q08.Act10.Snap...Q10.Legend.) %>% 
  rename(Question=AOI.hit..P08.TOI.Q08.Act10.Snap...Q10.Question.) %>%
  rename(Window=AOI.hit..P08.TOI.Q08.Act10.Snap...Q10.Window.) %>%
  filter(Window=="1") %>%
  select(Index,Duration,Answer,Buttons,CTC,FM,Legend,Question,Window)

question10 <- curated08.Q10


# --------------------------------------------------------------------------------------------------
# Question 11

participant08.Q11 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP08/P08-TOI-Q05-Act11-Data.csv", header=TRUE)
attach (participant08.Q11)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated08.Q11 <- participant08.Q11 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type.index, Gaze.event.duration..ms., starts_with("AOI.hit")) %>%
  distinct() %>%
  # renames the column names to be standard across the answers
  rename(Index=Eye.movement.type.index) %>%
  rename(Duration=Gaze.event.duration..ms.) %>%
  rename(Answer=AOI.hit..P08.TOI.Q05.Act11.Snap...Q11.Answer.) %>%
  rename(Buttons=AOI.hit..P08.TOI.Q05.Act11.Snap...Q11.Buttons.) %>% 
  rename(CTC=AOI.hit..P08.TOI.Q05.Act11.Snap...Q11.CTC.) %>%  
  rename(FM=AOI.hit..P08.TOI.Q05.Act11.Snap...Q11.FMAOI.) %>%
  rename(Legend=AOI.hit..P08.TOI.Q05.Act11.Snap...Q11.Legend.) %>% 
  rename(Question=AOI.hit..P08.TOI.Q05.Act11.Snap...Q11.Question.) %>%
  rename(Window=AOI.hit..P08.TOI.Q05.Act11.Snap...Q11.Window.) %>%
  filter(Window=="1") %>%
  select(Index,Duration,Answer,Buttons,CTC,FM,Legend,Question,Window)

question11 <- curated08.Q11


# --------------------------------------------------------------------------------------------------
# Question 12

participant08.Q12 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP08/P08-TOI-Q11-Act12-Data.csv", header=TRUE)
attach (participant08.Q12)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated08.Q12 <- participant08.Q12 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type.index, Gaze.event.duration..ms., starts_with("AOI.hit")) %>%
  distinct() %>%
  # renames the column names to be standard across the answers
  rename(Index=Eye.movement.type.index) %>%
  rename(Duration=Gaze.event.duration..ms.) %>%
  rename(Answer=AOI.hit..P08.TOI.Q11.Act12.Snap...Q12.Answer.) %>%
  rename(Buttons=AOI.hit..P08.TOI.Q11.Act12.Snap...Q12.Buttons.) %>% 
  rename(CTC=AOI.hit..P08.TOI.Q11.Act12.Snap...Q12.CTC.) %>%  
  rename(FM=AOI.hit..P08.TOI.Q11.Act12.Snap...Q12.FMAOI.) %>%
  rename(Legend=AOI.hit..P08.TOI.Q11.Act12.Snap...Q12.Legend.) %>% 
  rename(Question=AOI.hit..P08.TOI.Q11.Act12.Snap...Q12.Question.) %>%
  rename(Window=AOI.hit..P08.TOI.Q11.Act12.Snap...Q12.Window.) %>%
  filter(Window=="1") %>%
  select(Index,Duration,Answer,Buttons,CTC,FM,Legend,Question,Window)

question12 <- curated08.Q12


# --------------------------------------------------------------------------------------------------
# Question 13

participant08.Q13 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP08/P08-TOI-Q21-Act13-Data.csv", header=TRUE)
attach (participant08.Q13)

curated08.Q13 <- participant08.Q13 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type.index, Gaze.event.duration..ms., starts_with("AOI.hit")) %>%
  distinct() %>%
  # renames the column names to be standard across the answers
  rename(Index=Eye.movement.type.index) %>%
  rename(Duration=Gaze.event.duration..ms.) %>%
  rename(Answer=AOI.hit..P08.TOI.Q21.Act13.Snap...Q13.Answer.) %>%
  rename(Buttons=AOI.hit..P08.TOI.Q21.Act13.Snap...Q13.Buttons.) %>% 
  # rename(CTC=AOI.hit..P08.TOI.Q21.Act13.Snap...Q08.CTC.) %>%  
  rename(FM=AOI.hit..P08.TOI.Q21.Act13.Snap...Q13.FMAOI.) %>%
  rename(Legend=AOI.hit..P08.TOI.Q21.Act13.Snap...Q13.Legend.) %>% 
  rename(Question=AOI.hit..P08.TOI.Q21.Act13.Snap...Q13.Question.) %>%
  rename(Window=AOI.hit..P08.TOI.Q21.Act13.Snap...Q13.Window.) %>%
  filter(Window=="1") %>%
  select(Index,Duration,Answer,Buttons,FM,Legend,Question,Window)


# If the question lacks CTCs, add the default empty CTC and put it after Buttons
curated08.Q13$CTC <- rep(0,nrow(curated08.Q13))
curated08.Q13 <- curated08.Q13 %>% relocate(CTC, .before = FM)

question13 <- curated08.Q13


# --------------------------------------------------------------------------------------------------
# Question 14

participant08.Q14 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP08/P08-TOI-Q03-Act14-Data.csv", header=TRUE)
attach (participant08.Q14)

curated08.Q14 <- participant08.Q14 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type.index, Gaze.event.duration..ms., starts_with("AOI.hit")) %>%
  distinct() %>%
  # renames the column names to be standard across the answers
  rename(Index=Eye.movement.type.index) %>%
  rename(Duration=Gaze.event.duration..ms.) %>%
  rename(Answer=AOI.hit..P08.TOI.Q03.Act14.Snap...Q14.Answer.) %>%
  rename(Buttons=AOI.hit..P08.TOI.Q03.Act14.Snap...Q14.Buttons.) %>% 
  # rename(CTC=AOI.hit..P08.TOI.Q03.Act14.Snap...Q08.CTC.) %>%  
  rename(FM=AOI.hit..P08.TOI.Q03.Act14.Snap...Q14.FMAOI.) %>%
  rename(Legend=AOI.hit..P08.TOI.Q03.Act14.Snap...Q14.Legend.) %>% 
  rename(Question=AOI.hit..P08.TOI.Q03.Act14.Snap...Q14.Question.) %>%
  rename(Window=AOI.hit..P08.TOI.Q03.Act14.Snap...Q14.Window.) %>%
  filter(Window=="1") %>%
  select(Index,Duration,Answer,Buttons,FM,Legend,Question,Window)


# If the question lacks CTCs, add the default empty CTC and put it after Buttons
curated08.Q14$CTC <- rep(0,nrow(curated08.Q14))
curated08.Q14 <- curated08.Q14 %>% relocate(CTC, .before = FM)

question14 <- curated08.Q14


# --------------------------------------------------------------------------------------------------
# Question 15

participant08.Q15 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP08/P08-TOI-Q19-Act15-Data.csv", header=TRUE)
attach (participant08.Q15)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated08.Q15 <- participant08.Q15 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type.index, Gaze.event.duration..ms., starts_with("AOI.hit")) %>%
  distinct() %>%
  # renames the column names to be standard across the answers
  rename(Index=Eye.movement.type.index) %>%
  rename(Duration=Gaze.event.duration..ms.) %>%
  rename(Answer=AOI.hit..P08.TOI.Q19.Act15.Snap...Q15.Answer.) %>%
  rename(Buttons=AOI.hit..P08.TOI.Q19.Act15.Snap...Q15.Buttons.) %>% 
  rename(CTC=AOI.hit..P08.TOI.Q19.Act15.Snap...Q15.CTC.) %>%  
  rename(FM=AOI.hit..P08.TOI.Q19.Act15.Snap...Q15.FMAOI.) %>%
  rename(Legend=AOI.hit..P08.TOI.Q19.Act15.Snap...Q15.Legend.) %>% 
  rename(Question=AOI.hit..P08.TOI.Q19.Act15.Snap...Q15.Question.) %>%
  rename(Window=AOI.hit..P08.TOI.Q19.Act15.Snap...Q15.Window.) %>%
  filter(Window=="1") %>%
  select(Index,Duration,Answer,Buttons,CTC,FM,Legend,Question,Window)

question15 <- curated08.Q15


# --------------------------------------------------------------------------------------------------
# Question 16

participant08.Q16 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP08/P08-TOI-Q14-Act16-Data.csv", header=TRUE)
attach (participant08.Q16)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated08.Q16 <- participant08.Q16 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type.index, Gaze.event.duration..ms., starts_with("AOI.hit")) %>%
  distinct() %>%
  # renames the column names to be standard across the answers
  rename(Index=Eye.movement.type.index) %>%
  rename(Duration=Gaze.event.duration..ms.) %>%
  rename(Answer=AOI.hit..P08.TOI.Q14.Act16.Snap...Q16.Answer.) %>%
  rename(Buttons=AOI.hit..P08.TOI.Q14.Act16.Snap...Q16.Buttons.) %>% 
  rename(CTC=AOI.hit..P08.TOI.Q14.Act16.Snap...Q16.CTC.) %>%  
  rename(FM=AOI.hit..P08.TOI.Q14.Act16.Snap...Q16.FMAOI.) %>%
  rename(Legend=AOI.hit..P08.TOI.Q14.Act16.Snap...Q16.Legend.) %>% 
  rename(Question=AOI.hit..P08.TOI.Q14.Act16.Snap...Q16.Question.) %>%
  rename(Window=AOI.hit..P08.TOI.Q14.Act16.Snap...Q16.Window.) %>%
  filter(Window=="1") %>%
  select(Index,Duration,Answer,Buttons,CTC,FM,Legend,Question,Window)

question16 <- curated08.Q16


# --------------------------------------------------------------------------------------------------
# Question 17

participant08.Q17 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP08/P08-TOI-Q09-Act17-Data.csv", header=TRUE)
attach (participant08.Q17)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated08.Q17 <- participant08.Q17 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type.index, Gaze.event.duration..ms., starts_with("AOI.hit")) %>%
  distinct() %>%
  # renames the column names to be standard across the answers
  rename(Index=Eye.movement.type.index) %>%
  rename(Duration=Gaze.event.duration..ms.) %>%
  rename(Answer=AOI.hit..P08.TOI.Q09.Act17.Snap...Q17.Answer.) %>%
  rename(Buttons=AOI.hit..P08.TOI.Q09.Act17.Snap...Q17.Buttons.) %>% 
  rename(CTC=AOI.hit..P08.TOI.Q09.Act17.Snap...Q17.CTC.) %>%  
  rename(FM=AOI.hit..P08.TOI.Q09.Act17.Snap...Q17.FMAOI.) %>%
  rename(Legend=AOI.hit..P08.TOI.Q09.Act17.Snap...Q17.Legend.) %>% 
  rename(Question=AOI.hit..P08.TOI.Q09.Act17.Snap...Q17.Question.) %>%
  rename(Window=AOI.hit..P08.TOI.Q09.Act17.Snap...Q17.Window.) %>%
  filter(Window=="1") %>%
  select(Index,Duration,Answer,Buttons,CTC,FM,Legend,Question,Window)

question17 <- curated08.Q17


# --------------------------------------------------------------------------------------------------
# Question 18

participant08.Q18 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP08/P08-TOI-Q10-Act18-Data.csv", header=TRUE)
attach (participant08.Q18)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated08.Q18 <- participant08.Q18 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type.index, Gaze.event.duration..ms., starts_with("AOI.hit")) %>%
  distinct() %>%
  # renames the column names to be standard across the answers
  rename(Index=Eye.movement.type.index) %>%
  rename(Duration=Gaze.event.duration..ms.) %>%
  rename(Answer=AOI.hit..P08.TOI.Q10.Act18.Snap...Q18.Answer.) %>%
  rename(Buttons=AOI.hit..P08.TOI.Q10.Act18.Snap...Q18.Buttons.) %>% 
  rename(CTC=AOI.hit..P08.TOI.Q10.Act18.Snap...Q18.CTC.) %>%  
  rename(FM=AOI.hit..P08.TOI.Q10.Act18.Snap...Q18.FMAOI.) %>%
  rename(Legend=AOI.hit..P08.TOI.Q10.Act18.Snap...Q18.Legend.) %>% 
  rename(Question=AOI.hit..P08.TOI.Q10.Act18.Snap...Q18.Question.) %>%
  rename(Window=AOI.hit..P08.TOI.Q10.Act18.Snap...Q18.Window.) %>%
  filter(Window=="1") %>%
  select(Index,Duration,Answer,Buttons,CTC,FM,Legend,Question,Window)

question18 <- curated08.Q18


# --------------------------------------------------------------------------------------------------
# Question 19

participant08.Q19 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP08/P08-TOI-Q18-Act19-Data.csv", header=TRUE)
attach (participant08.Q19)

curated08.Q19 <- participant08.Q19 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type.index, Gaze.event.duration..ms., starts_with("AOI.hit")) %>%
  distinct() %>%
  # renames the column names to be standard across the answers
  rename(Index=Eye.movement.type.index) %>%
  rename(Duration=Gaze.event.duration..ms.) %>%
  rename(Answer=AOI.hit..P08.TOI.Q18.Act19.Snap...Q19.Answer.) %>%
  rename(Buttons=AOI.hit..P08.TOI.Q18.Act19.Snap...Q19.Buttons.) %>% 
  # rename(CTC=AOI.hit..P08.TOI.Q18.Act19.Snap...Q08.CTC.) %>%  
  rename(FM=AOI.hit..P08.TOI.Q18.Act19.Snap...Q19.FMAOI.) %>%
  rename(Legend=AOI.hit..P08.TOI.Q18.Act19.Snap...Q19.Legend.) %>% 
  rename(Question=AOI.hit..P08.TOI.Q18.Act19.Snap...Q19.Question.) %>%
  rename(Window=AOI.hit..P08.TOI.Q18.Act19.Snap...Q19.Window.) %>%
  filter(Window=="1") %>%
  select(Index,Duration,Answer,Buttons,FM,Legend,Question,Window)


# If the question lacks CTCs, add the default empty CTC and put it after Buttons
curated08.Q19$CTC <- rep(0,nrow(curated08.Q19))
curated08.Q19 <- curated08.Q19 %>% relocate(CTC, .before = FM)

question19 <- curated08.Q19


# --------------------------------------------------------------------------------------------------
# Question 20

participant08.Q20 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP08/P08-TOI-Q17-Act20-Data.csv", header=TRUE)
attach (participant08.Q20)

curated08.Q20 <- participant08.Q20 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type.index, Gaze.event.duration..ms., starts_with("AOI.hit")) %>%
  distinct() %>%
  # renames the column names to be standard across the answers
  rename(Index=Eye.movement.type.index) %>%
  rename(Duration=Gaze.event.duration..ms.) %>%
  rename(Answer=AOI.hit..P08.TOI.Q17.Act20.Snap...Q20.Answer.) %>%
  rename(Buttons=AOI.hit..P08.TOI.Q17.Act20.Snap...Q20.Buttons.) %>% 
  # rename(CTC=AOI.hit..P08.TOI.Q17.Act20.Snap...Q08.CTC.) %>%  
  rename(FM=AOI.hit..P08.TOI.Q17.Act20.Snap...Q20.FMAOI.) %>%
  rename(Legend=AOI.hit..P08.TOI.Q17.Act20.Snap...Q20.Legend.) %>% 
  rename(Question=AOI.hit..P08.TOI.Q17.Act20.Snap...Q20.Question.) %>%
  rename(Window=AOI.hit..P08.TOI.Q17.Act20.Snap...Q20.Window.) %>%
  filter(Window=="1") %>%
  select(Index,Duration,Answer,Buttons,FM,Legend,Question,Window)


# If the question lacks CTCs, add the default empty CTC and put it after Buttons
curated08.Q20$CTC <- rep(0,nrow(curated08.Q20))
curated08.Q20 <- curated08.Q20 %>% relocate(CTC, .before = FM)

question20 <- curated08.Q20


# --------------------------------------------------------------------------------------------------
# Question 21

participant08.Q21 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP08/P08-TOI-Q01-Act21-Data.csv", header=TRUE)
attach (participant08.Q21)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated08.Q21 <- participant08.Q21 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type.index, Gaze.event.duration..ms., starts_with("AOI.hit")) %>%
  distinct() %>%
  # renames the column names to be standard across the answers
  rename(Index=Eye.movement.type.index) %>%
  rename(Duration=Gaze.event.duration..ms.) %>%
  rename(Answer=AOI.hit..P08.TOI.Q01.Act21.Snap...Q21.Answer.) %>%
  rename(Buttons=AOI.hit..P08.TOI.Q01.Act21.Snap...Q21.Buttons.) %>% 
  rename(CTC=AOI.hit..P08.TOI.Q01.Act21.Snap...Q21.CTC.) %>%  
  rename(FM=AOI.hit..P08.TOI.Q01.Act21.Snap...Q21.FMAOI.) %>%
  rename(Legend=AOI.hit..P08.TOI.Q01.Act21.Snap...Q21.Legend.) %>% 
  rename(Question=AOI.hit..P08.TOI.Q01.Act21.Snap...Q21.Question.) %>%
  rename(Window=AOI.hit..P08.TOI.Q01.Act21.Snap...Q21.Window.) %>%
  filter(Window=="1") %>%
  select(Index,Duration,Answer,Buttons,CTC,FM,Legend,Question,Window)

question21 <- curated08.Q21


# --------------------------------------------------------------------------------------------------
# Question 22

participant08.Q22 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP08/P08-TOI-Q23-Act22-Data.csv", header=TRUE)
attach (participant08.Q22)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated08.Q22 <- participant08.Q22 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type.index, Gaze.event.duration..ms., starts_with("AOI.hit")) %>%
  distinct() %>%
  # renames the column names to be standard across the answers
  rename(Index=Eye.movement.type.index) %>%
  rename(Duration=Gaze.event.duration..ms.) %>%
  rename(Answer=AOI.hit..P08.TOI.Q23.Act22.Snap...Q22.Answer.) %>%
  rename(Buttons=AOI.hit..P08.TOI.Q23.Act22.Snap...Q22.Buttons.) %>% 
  rename(CTC=AOI.hit..P08.TOI.Q23.Act22.Snap...Q22.CTC.) %>%  
  rename(FM=AOI.hit..P08.TOI.Q23.Act22.Snap...Q22.FMAOI.) %>%
  rename(Legend=AOI.hit..P08.TOI.Q23.Act22.Snap...Q22.Legend.) %>% 
  rename(Question=AOI.hit..P08.TOI.Q23.Act22.Snap...Q22.Question.) %>%
  rename(Window=AOI.hit..P08.TOI.Q23.Act22.Snap...Q22.Window.) %>%
  filter(Window=="1") %>%
  select(Index,Duration,Answer,Buttons,CTC,FM,Legend,Question,Window)

question22 <- curated08.Q22


# --------------------------------------------------------------------------------------------------
# Question 23

participant08.Q23 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP08/P08-TOI-Q15-Act23-Data.csv", header=TRUE)
attach (participant08.Q23)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated08.Q23 <- participant08.Q23 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type.index, Gaze.event.duration..ms., starts_with("AOI.hit")) %>%
  distinct() %>%
  # renames the column names to be standard across the answers
  rename(Index=Eye.movement.type.index) %>%
  rename(Duration=Gaze.event.duration..ms.) %>%
  rename(Answer=AOI.hit..P08.TOI.Q15.Act23.Snap...Q23.Answer.) %>%
  rename(Buttons=AOI.hit..P08.TOI.Q15.Act23.Snap...Q23.Buttons.) %>% 
  rename(CTC=AOI.hit..P08.TOI.Q15.Act23.Snap...Q23.CTC.) %>%  
  rename(FM=AOI.hit..P08.TOI.Q15.Act23.Snap...Q23.FMAOI.) %>%
  rename(Legend=AOI.hit..P08.TOI.Q15.Act23.Snap...Q23.Legend.) %>% 
  rename(Question=AOI.hit..P08.TOI.Q15.Act23.Snap...Q23.Question.) %>%
  rename(Window=AOI.hit..P08.TOI.Q15.Act23.Snap...Q23.Window.) %>%
  filter(Window=="1") %>%
  select(Index,Duration,Answer,Buttons,CTC,FM,Legend,Question,Window)

question23 <- curated08.Q23


# --------------------------------------------------------------------------------------------------
# Question 24

participant08.Q24 <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/PartP08/P08-TOI-Q06-Act24-Data.csv", header=TRUE)
attach (participant08.Q24)

# Filters the rows with all NAs, keeps only the fixations of Eye tracker events, and repeated elements
curated08.Q24 <- participant08.Q24 %>% filter(!across(everything(), is.na)) %>% 
  filter(Eye.movement.type=="Fixation" & Sensor=="Eye Tracker") %>% 
  select(Eye.movement.type.index, Gaze.event.duration..ms., starts_with("AOI.hit")) %>%
  distinct() %>%
  # renames the column names to be standard across the answers
  rename(Index=Eye.movement.type.index) %>%
  rename(Duration=Gaze.event.duration..ms.) %>%
  rename(Answer=AOI.hit..P08.TOI.Q06.Act24.Snap...Q24.Answer.) %>%
  rename(Buttons=AOI.hit..P08.TOI.Q06.Act24.Snap...Q24.Buttons.) %>% 
  rename(CTC=AOI.hit..P08.TOI.Q06.Act24.Snap...Q24.CTC.) %>%  
  rename(FM=AOI.hit..P08.TOI.Q06.Act24.Snap...Q24.FMAOI.) %>%
  rename(Legend=AOI.hit..P08.TOI.Q06.Act24.Snap...Q24.Legend.) %>% 
  rename(Question=AOI.hit..P08.TOI.Q06.Act24.Snap...Q24.Question.) %>%
  rename(Window=AOI.hit..P08.TOI.Q06.Act24.Snap...Q24.Window.) %>%
  filter(Window=="1") %>%
  select(Index,Duration,Answer,Buttons,CTC,FM,Legend,Question,Window)

question24 <- curated08.Q24


# TODO
# 1) To all the question frames add the participant number and the question number

participant.number <- 8

question01$Participant <- rep(participant.number,nrow(question01))
question01$QN <- rep(1,nrow(question01)) 
question01 <- question01 %>% relocate(Participant, .before = Index)
question01 <- question01 %>% relocate(QN, .before = Index)

question02$Participant <- rep(participant.number,nrow(question02))
question02$QN <- rep(2,nrow(question02)) 
question02 <- question02 %>% relocate(Participant, .before = Index)
question02 <- question02 %>% relocate(QN, .before = Index)

question03$Participant <- rep(participant.number,nrow(question03))
question03$QN <- rep(3,nrow(question03)) 
question03 <- question03 %>% relocate(Participant, .before = Index)
question03 <- question03 %>% relocate(QN, .before = Index)

question04$Participant <- rep(participant.number,nrow(question04))
question04$QN <- rep(4,nrow(question04)) 
question04 <- question04 %>% relocate(Participant, .before = Index)
question04 <- question04 %>% relocate(QN, .before = Index)

question05$Participant <- rep(participant.number,nrow(question05))
question05$QN <- rep(5,nrow(question05)) 
question05 <- question05 %>% relocate(Participant, .before = Index)
question05 <- question05 %>% relocate(QN, .before = Index)

question06$Participant <- rep(participant.number,nrow(question06))
question06$QN <- rep(6,nrow(question06)) 
question06 <- question06 %>% relocate(Participant, .before = Index)
question06 <- question06 %>% relocate(QN, .before = Index)

question07$Participant <- rep(participant.number,nrow(question07))
question07$QN <- rep(7,nrow(question07)) 
question07 <- question07 %>% relocate(Participant, .before = Index)
question07 <- question07 %>% relocate(QN, .before = Index)

question08$Participant <- rep(participant.number,nrow(question08))
question08$QN <- rep(8,nrow(question08)) 
question08 <- question08 %>% relocate(Participant, .before = Index)
question08 <- question08 %>% relocate(QN, .before = Index)

question09$Participant <- rep(participant.number,nrow(question09))
question09$QN <- rep(9,nrow(question09)) 
question09 <- question09 %>% relocate(Participant, .before = Index)
question09 <- question09 %>% relocate(QN, .before = Index)

question10$Participant <- rep(participant.number,nrow(question10))
question10$QN <- rep(10,nrow(question10)) 
question10 <- question10 %>% relocate(Participant, .before = Index)
question10 <- question10 %>% relocate(QN, .before = Index)



question11$Participant <- rep(participant.number,nrow(question11))
question11$QN <- rep(11,nrow(question11)) 
question11 <- question11 %>% relocate(Participant, .before = Index)
question11 <- question11 %>% relocate(QN, .before = Index)

question12$Participant <- rep(participant.number,nrow(question12))
question12$QN <- rep(12,nrow(question12)) 
question12 <- question12 %>% relocate(Participant, .before = Index)
question12 <- question12 %>% relocate(QN, .before = Index)

question13$Participant <- rep(participant.number,nrow(question13))
question13$QN <- rep(13,nrow(question13)) 
question13 <- question13 %>% relocate(Participant, .before = Index)
question13 <- question13 %>% relocate(QN, .before = Index)

question14$Participant <- rep(participant.number,nrow(question14))
question14$QN <- rep(14,nrow(question14)) 
question14 <- question14 %>% relocate(Participant, .before = Index)
question14 <- question14 %>% relocate(QN, .before = Index)

question15$Participant <- rep(participant.number,nrow(question15))
question15$QN <- rep(15,nrow(question15)) 
question15 <- question15 %>% relocate(Participant, .before = Index)
question15 <- question15 %>% relocate(QN, .before = Index)

question16$Participant <- rep(participant.number,nrow(question16))
question16$QN <- rep(16,nrow(question16)) 
question16 <- question16 %>% relocate(Participant, .before = Index)
question16 <- question16 %>% relocate(QN, .before = Index)

question17$Participant <- rep(participant.number,nrow(question17))
question17$QN <- rep(17,nrow(question17)) 
question17 <- question17 %>% relocate(Participant, .before = Index)
question17 <- question17 %>% relocate(QN, .before = Index)

question18$Participant <- rep(participant.number,nrow(question18))
question18$QN <- rep(18,nrow(question18)) 
question18 <- question18 %>% relocate(Participant, .before = Index)
question18 <- question18 %>% relocate(QN, .before = Index)

question19$Participant <- rep(participant.number,nrow(question19))
question19$QN <- rep(19,nrow(question19)) 
question19 <- question19 %>% relocate(Participant, .before = Index)
question19 <- question19 %>% relocate(QN, .before = Index)

question20$Participant <- rep(participant.number,nrow(question20))
question20$QN <- rep(20,nrow(question20)) 
question20 <- question20 %>% relocate(Participant, .before = Index)
question20 <- question20 %>% relocate(QN, .before = Index)


question21$Participant <- rep(participant.number,nrow(question21))
question21$QN <- rep(21,nrow(question21)) 
question21 <- question21 %>% relocate(Participant, .before = Index)
question21 <- question21 %>% relocate(QN, .before = Index)

question22$Participant <- rep(participant.number,nrow(question22))
question22$QN <- rep(22,nrow(question22)) 
question22 <- question22 %>% relocate(Participant, .before = Index)
question22 <- question22 %>% relocate(QN, .before = Index)

question23$Participant <- rep(participant.number,nrow(question23))
question23$QN <- rep(23,nrow(question23)) 
question23 <- question23 %>% relocate(Participant, .before = Index)
question23 <- question23 %>% relocate(QN, .before = Index)

question24$Participant <- rep(participant.number,nrow(question24))
question24$QN <- rep(24,nrow(question24)) 
question24 <- question24 %>% relocate(Participant, .before = Index)
question24 <- question24 %>% relocate(QN, .before = Index)


# 2) Merge all the questions into a single frame and save it

participant08.all.questions <- rbind(question01, question02, question03, question04, question05,
                                     question06, question07, question08, question09, question10,
                                     question11, question12, question13, question14, question15,
                                     question16, question17, question18, question19, question20,
                                     question21, question22, question23, question24)

write.csv(participant08.all.questions,
          file = "../../Experiment-Data/Eye-tracking-data-samples/PartP08/P08-Transitions-Data.csv", 
          row.names = FALSE)



