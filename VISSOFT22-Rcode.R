# VISSOFT2023 
# Artifact Evaluation 
# R Script

library("tidyverse")
library(ggplot2)
library(hrbrthemes)

#################
# Loads original participants responses
participant.responses <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/CompleteParticipantsResponseData.csv", header=TRUE)
attach(participant.responses)

# Divides the responses according to t value
t2.data <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/2-ParticipantsResponses.csv", header=TRUE)
t3.data <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/3-ParticipantsResponses.csv", header=TRUE)

# Adds column with T values, 8 x 24 participants = 192 per each T value x 2 = 384
T <- c(rep(2,192),rep(3,192))
  
allt.data <- bind_rows(t2.data,t3.data)
allt.data$T = T

# Column names of t2.data and t3.data
# "Participant.ID"       "Question.Number"      "Accuracy"             "Elapsed.Time"         "Certainty.Assessment"
# "Difficulty.Level"     "Visualization.Method" "Number.Elements" 


#################
# Plot for the number of correct and incorrect responses by participant, for t=2 and t=3
participant.accuracy <- allt.data %>% 
  ggplot(aes(x=Participant.ID, fill=Accuracy)) + # , alpha=0.5
  # geom_bar(position ="dodge") +
  geom_bar() +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), panel.background = element_blank()) +
  #  theme_minimal () +
  #  theme_minimal (axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  labs(x="Participants' responses", y="Number of Correct and Incorrect responses") +
  coord_flip() +
  scale_y_continuous(breaks=seq(1, 24, 1)) +
  scale_fill_manual(values=c("red", "green"))
 # scale_fill_brewer(palette = "Set1")
participant.accuracy


# Plot per question how many participants correct and incorrect
# Bar chart with correct and incorrect responses for each question
question.accuracy <- allt.data %>% 
  ggplot(aes(x=Question.Number, fill=Accuracy)) +
  # geom_bar(position ="dodge") +
  geom_bar() +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) + # theme_minimal
  scale_x_continuous(breaks=seq(1, 16, 1))  +
  scale_y_continuous(breaks=seq(1, 24, 1))  +
  labs(x="Question Number", y="Frequency")  +
  #scale_fill_manual(values=c("#7463AC","gray80"))  
  #scale_colour_brewer(palette = "Set2")
  # scale_fill_brewer(palette = "Set1") # Accent, Set1, Pastel1
  scale_fill_manual(values=c("red", "green"))
question.accuracy



# geom_bar(aes(fill=Accuracy, alpha=0.5)) +
#  scale_fill_manual(values=c("green", "red")) +


#################


#################
