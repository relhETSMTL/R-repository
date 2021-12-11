# Objective: Basic data analysis of participants data
# Project: Eye-Tracking Analysis for Feature Models Comprehension
# Ecole de technologie superieure
# VERITAS team
# Authors: Elmira Sepasi, Kambiz Belouchi, Roberto E. Lopez-Herrejon
# Last update: 2021-12-10

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
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
#  theme_minimal () +
#  theme_minimal (axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  labs(x="Participants's Responses", y="Correct and Incorrect Questions") +
  coord_flip() +
  scale_y_continuous(breaks=seq(1, 24, 1))

participation.correctness


# Boxplot of correct responses per participant, 284
boxplot.correct.answers <- curatedParticipantsData %>% filter(Correct=="True") %>% count(ParticipantID) %>%
  ggplot(aes(x = "", y=n)) +
  geom_boxplot() + 
  geom_jitter() +
  theme_minimal() +
  labs(y="Number of Correct Responses", x="") # +
#  theme(axis.title.y=element_blank(),
#        axis.text.y=element_blank(),
#        axis.ticks.y=element_blank())  +
#  coord_flip() # +
  # scale_y_continuous(limits=c(10,370), breaks=seq(10,370,40))
boxplot.correct.answers

# Summary of the correct answers
summary(curatedParticipantsData %>% filter(Correct=="True") %>% count(ParticipantID))

# Output
# ParticipantID       n        
# Min.   : 2    Min.   : 9.00  
# 1st Qu.: 6    1st Qu.:15.00  
# Median :10    Median :17.00  
# Mean   :10    Mean   :16.71  
# 3rd Qu.:14    3rd Qu.:20.00  
# Max.   :18    Max.   :21.00 

## Analysis of length of configurations in the question
configurationLength <- c(6, 7, 10, 6, 5, 6, 6, 6, 7, 6, 6, 6, 3, 7, 6, 6, 6, 7, 6, 6, 3, 6, 6, 6)

summary (as.factor (configurationLength))
# 3  5  6  7 10 
# 2  1 16  4  1
