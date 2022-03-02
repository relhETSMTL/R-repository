# Processing data for answer eye transitions
# This file tests the code that is needed to produce the data for computing the eye transitions

library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(dplyr)


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
  select(Index,Duration,Answer,Buttons,FM,Legend,Question,Window)
  

question01 <- curated05.Q01

