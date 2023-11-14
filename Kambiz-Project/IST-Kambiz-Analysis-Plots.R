# IST Journal Kambiz Paper
# File with all the statistical analysis and figures of the article

library("tidyverse")
library(ggplot2)
library(hrbrthemes)

# Loads the complete experiment data file
experiment.data <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/Curated-Data/Complete-Experiment-Data.csv", header=TRUE)


###############################################################
# General descriptive statistics

## Participant responses in general, total correct and total incorrect
accuracy.general <- experiment.data  %>%  
  group_by(Accuracy) %>%
  summarise(count=n()) %>% as.data.frame()
accuracy.general
# > accuracy.general
# Accuracy count
# 1    False    59
# 2     True   325

# Accuracy incorrect and correct responses for all the participants
general.accuracy.false <- accuracy.general$count[1]
general.accuracy.true <- accuracy.general$count[2]
general.accuracy.false
general.accuracy.true

# Percentage incorrect = 0.1536458
general.accuracy.false / (general.accuracy.false + general.accuracy.true)
# Percentage correct = 0.8463542
general.accuracy.true / (general.accuracy.false + general.accuracy.true)

# Plot for accuracy by participant responses 
participant.accuracy <- experiment.data %>% 
  ggplot(aes(x=Participant.ID, fill=Accuracy)) + # , alpha=0.5
  # geom_bar(position ="dodge") +
  geom_bar() +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), panel.background = element_blank()) +
  #  theme_minimal () +
  #  theme_minimal (axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  labs(x="Participants' responses", y="Number of Correct (True) and Incorrect (False) responses") +
  coord_flip() +
  scale_y_continuous(breaks=seq(1, 24, 1)) +
  scale_fill_manual(values=c("red", "green"))
# scale_fill_brewer(palette = "Set1")
participant.accuracy


# Plot per question how many participants correct and incorrect
# Bar chart with correct and incorrect responses for each question
question.accuracy <- experiment.data %>% 
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



################################################################################################
### Scratch code
# "../../../Eye-Tracking-Visualization/Experiment-Data/Curated-Data/Complete-Experiment-Data.csv"

## Loads the data files and completes the framework including T value
# Divides the responses according to t value
# e.data <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/2-ParticipantsResponses.csv", header=TRUE)
# t3.data <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/3-ParticipantsResponses.csv", header=TRUE)
# 
# allt.data <- bind_rows(t2.data,t3.data)

# Adds column with T values, 8 x 24 participants = 192 per each T value x 2 = 384
# T <- c(rep(2,192),rep(3,192))
# allt.data$T = T
