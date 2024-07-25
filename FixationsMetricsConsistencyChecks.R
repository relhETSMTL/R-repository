# Consistency checks for the computation of the fixation time, count, and proportions 
library(tidyverse)
library(dplyr)

# Loads the complete experimet data for all 17 participants and their 24 questions
all.participants.data  <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/ExperimentCompleteDataSet.csv",
                                  header=TRUE,
                                  fileEncoding="latin1")
attach (all.participants.data)


# Check with the 

check.fixations.metrics <- all.participants.data %>%
  mutate(fixations.count.diff = totalFixations - fixations.Question - 
           fixations.Answer - fixations.Buttons - fixations.Legend - fixations.FM - fixations.CTC - fixations.Window) %>%
  mutate(fixations.time.diff = totalFixationTime - time.Question - 
           time.Answer - time.Buttons - time.Legend - time.FM - time.CTC - time.Window) %>%
  mutate(perc.fixations.count.diff = 1 - perc.fixations.Question  - 
           perc.fixations.Answer - perc.fixations.Buttons - perc.fixations.Legend - perc.fixations.FM - perc.fixations.CTC - perc.fixations.Window) %>%
  mutate(perc.fixations.time.diff = 1 - perc.time.Question  - 
           perc.time.Answer - perc.time.Buttons - perc.time.Legend - perc.time.FM - perc.time.CTC - perc.time.Window)

# Note: all the consistency checks of fixations pass correctly, the percentages add up to E+/-15 o E+/-16 which is good enogh for our purposes

###################################################################################

# Text the data generate for the transitions and how to change the Window

p02.transitions <- read.csv(file ="../../Experiment-Data/Eye-tracking-data-samples/PartP02/P02-Transitions-Data.csv", 
                            header=TRUE,
                            fileEncoding="latin1")
attach(p02.transitions)

p02.transitions.count <- p02.transitions %>%
  mutate(hits = Answer + Buttons + CTC + FM + Legend + Question + Window)

# 185 entries with only Window hit, 

# Recomputing the Window hits to those without any other AOI hit
p02.transitions.revised <- p02.transitions %>%
  mutate(Window = 1 - (Answer + Buttons + CTC + FM + Legend + Question))

p02.transitions.revised %>% filter(Window==1)


