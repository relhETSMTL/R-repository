# Objective: Basic data analysis of participants data
# Project: Eye-Tracking Analysis for Feature Models Comprehension
# Ecole de technologie superieure
# VERITAS team
# Authors: Elmira Sepasi, Kambiz Belouchi, Roberto E. Lopez-Herrejon
# Last update: 2021-12-05

library(ggplot2)
library(tidyverse)
library(hrbrthemes)

# File reads the experiment data 
curatedParticipantsData <- read.csv(file = "../../Experiment-Data/All-Participants-Curated-Data-Boolean.csv", header=TRUE)
attach (curatedParticipantsData)

# Bar chart with correct and incorrect responses for each question
question.correctness <- curatedParticipantsData %>% 
  ggplot(aes(x=QNumber, fill=Correct)) +
  # geom_bar(position ="dodge") +
  geom_bar() +
  theme_minimal() +
  scale_x_continuous(breaks=seq(1, 24, 1))  +
  scale_y_continuous(breaks=seq(1, 17, 1))  +
  labs(x="Question Number", y="Frequency")
question.correctness

# Bar chart with correct and incorrect responses for each participant
participation.correctness <- curatedParticipantsData %>% 
  ggplot(aes(x=ParticipantID, fill=Correct)) +
  # geom_bar(position ="dodge") +
  geom_bar() +
  #theme_minimal () +
  theme_minimal (axis.text.y=element_blank(),
         axis.ticks.y=element_blank()) +
  labs(x="Participants's Responses", y="Correct and Incorrect Questions") +
  coord_flip() +
  scale_y_continuous(breaks=seq(1, 24, 1)) 
participation.correctness

