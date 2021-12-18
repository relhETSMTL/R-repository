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
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) + # theme_minimal
  scale_x_continuous(breaks=seq(1, 24, 1))  +
  scale_y_continuous(breaks=seq(1, 17, 1))  +
  labs(x="Question Number", y="Frequency")  +
  #scale_fill_manual(values=c("#7463AC","gray80"))  
  #scale_colour_brewer(palette = "Set2")
  scale_fill_brewer(palette = "Set1") # Accent, Set1, Pastel1
question.correctness

# Bar chart with correct and incorrect responses for each participant
participation.correctness <- curatedParticipantsData %>% 
  ggplot(aes(x=ParticipantID, fill=Correct)) +
  # geom_bar(position ="dodge") +
  geom_bar() +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), panel.background = element_blank()) +
#  theme_minimal () +
#  theme_minimal (axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  labs(x="Participants' responses", y="Number of correct and incorrect responses") +
  coord_flip() +
  scale_y_continuous(breaks=seq(1, 24, 1)) +
  scale_fill_brewer(palette = "Set1")
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




###################################################
# Testing example of plotting response time distribution per question
g <- ggplot(mpg, aes(class, cty))

g + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Box plot", 
       subtitle="City Mileage grouped by Class of vehicle",
       caption="Source: mpg",
       x="Class of Vehicle",
       y="City Mileage")


# Now the distribution of response time over each question, considering correct and incorrect answers
participantResponseTime <- curatedParticipantsData %>% ggplot(aes(x=QNumber, group=QNumber, ElapsedTime/1000)) +
  geom_boxplot(aes(fill=ElapsedTime/1000), varwidth=T, fill="plum") +
#  coord_flip() +
  labs(x="Question Number", y="All Answers - Time in Seconds") +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
#  theme_minimal() +
#  scale_x_continuous(breaks=seq(1, 24, 1))
  scale_x_discrete(limits=seq(1, 24, 1))  +
  ylim(0, 300) +
  scale_y_continuous(breaks=seq(0, 300, 60))
participantResponseTime


# Response time of correct answers
participantCorrectResponseTime <- curatedParticipantsData %>% filter(Correct=="True") %>% ggplot(aes(x=QNumber, group=QNumber, ElapsedTime/1000)) +
  geom_boxplot(aes(fill=ElapsedTime/1000), varwidth=T, fill="plum") +
  #  coord_flip() +
  labs(x="Question Number", y="Correct answers - Time secs") +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
  #  theme_minimal() +
  #  scale_x_continuous(breaks=seq(1, 24, 1))
  scale_x_discrete(limits=seq(1, 24, 1))  +
  ylim(0, 300) +
  scale_y_continuous(breaks=seq(0, 300, 60))
participantCorrectResponseTime

# Response time for incorrect answers
participantIncorrectResponseTime <- curatedParticipantsData %>% filter(Correct=="False") %>% ggplot(aes(x=QNumber, group=QNumber, ElapsedTime/1000)) +
  geom_boxplot(aes(fill=ElapsedTime/1000), varwidth=T, fill="plum") +
  #  coord_flip() +
  labs(x="Question Number", y="Incorrect answers - Time secs") +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
  #  theme_minimal() +
  #  scale_x_continuous(breaks=seq(1, 24, 1))
  scale_x_discrete(limits=seq(1, 24, 1))  +
  ylim(0, 300) +
  scale_y_continuous(breaks=seq(0, 300, 60))
participantIncorrectResponseTime


###############################
# Distribution of correct answers
correctAnswers <- curatedParticipantsData %>% filter(Correct=="True")
summary(correctAnswers$ElapsedTime)
sd(correctAnswers$ElapsedTime)

incorrectAnswers <- curatedParticipantsData %>% filter(Correct=="False")
summary(incorrectAnswers$ElapsedTime)
sd(correctAnswers$ElapsedTime)


# Distribution of correct answers per participant
correctAnswersParticipant <- curatedParticipantsData %>% filter(Correct=="True") %>% count(ParticipantID)
summary(correctAnswersParticipant$n)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 9.00   15.00   17.00   16.71   20.00   21.00 

# Distribution of incorrect answers per participant
incorrectAnswersParticipant <- curatedParticipantsData %>% filter(Correct=="False") %>% count(ParticipantID)
summary(incorrectAnswersParticipant$n)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.000   4.000   7.000   7.294   9.000  15.000 

# Distribution of correct answers per question
correctAnswersQN <- curatedParticipantsData %>% filter(Correct=="True") %>% count(QNumber)
summary(correctAnswersQN$n)
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6.00   10.00   12.50   11.83   14.00   17.00 
sd(correctAnswersQN$n)


# Distribution of incorrect answers per question
incorrectAnswersQN <- curatedParticipantsData %>% filter(Correct=="False") %>% count(QNumber)
summary(incorrectAnswersQN$n)
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6.00   10.00   12.50   11.83   14.00   17.00 
sd(incorrectAnswersQN$n)

###########################################

# File reads the experiment data 
experimentQuestionsData <- read.csv(file = "experiment-questions-analysis.csv", header=TRUE)
attach (experimentQuestionsData)

## Analysis of length of configurations in the question
#configurationLength <- c(6, 7, 10, 6, 5, 6, 6, 6, 7, 6, 6, 6, 3, 7, 6, 6, 6, 7, 6, 6, 3, 6, 6, 6)

configurationLength <- experimentQuestionsData$NumFeatures

summary (as.factor (configurationLength))
# 3  5  6  7  8 10 
# 2  1 14  4  2  1 

summary(experimentQuestionsData$Type)


#############################################################
# Computing the heatmap for the responses

# heatmap(as.matrix(mtcars), Colv = NA, Rowv = NA, scale="column")
participant2 <- read.csv(file = "../../Experiment-Data/Participant-2-Curated-Data.csv", header=TRUE)
attach (participant2)
participant3 <- read.csv(file = "../../Experiment-Data/Participant-3-Curated-Data.csv", header=TRUE)
attach (participant3)
participant4 <- read.csv(file = "../../Experiment-Data/Participant-4-Curated-Data.csv", header=TRUE)
attach (participant4)
participant5 <- read.csv(file = "../../Experiment-Data/Participant-5-Curated-Data.csv", header=TRUE)
attach (participant5)
participant6 <- read.csv(file = "../../Experiment-Data/Participant-6-Curated-Data.csv", header=TRUE)
attach (participant6)
participant7 <- read.csv(file = "../../Experiment-Data/Participant-7-Curated-Data.csv", header=TRUE)
attach (participant7)
participant8 <- read.csv(file = "../../Experiment-Data/Participant-8-Curated-Data.csv", header=TRUE)
attach (participant8)
participant9 <- read.csv(file = "../../Experiment-Data/Participant-9-Curated-Data.csv", header=TRUE)
attach (participant9)
participant10 <- read.csv(file = "../../Experiment-Data/Participant-10-Curated-Data.csv", header=TRUE)
attach (participant10)
participant11 <- read.csv(file = "../../Experiment-Data/Participant-11-Curated-Data.csv", header=TRUE)
attach (participant11)
participant12 <- read.csv(file = "../../Experiment-Data/Participant-12-Curated-Data.csv", header=TRUE)
attach (participant12)
participant13 <- read.csv(file = "../../Experiment-Data/Participant-13-Curated-Data.csv", header=TRUE)
attach (participant13)
participant14 <- read.csv(file = "../../Experiment-Data/Participant-14-Curated-Data.csv", header=TRUE)
attach (participant14)
participant15 <- read.csv(file = "../../Experiment-Data/Participant-15-Curated-Data.csv", header=TRUE)
attach (participant15)
participant16 <- read.csv(file = "../../Experiment-Data/Participant-16-Curated-Data.csv", header=TRUE)
attach (participant16)
participant17 <- read.csv(file = "../../Experiment-Data/Participant-17-Curated-Data.csv", header=TRUE)
attach (participant17)
participant18 <- read.csv(file = "../../Experiment-Data/Participant-18-Curated-Data.csv", header=TRUE)
attach (participant18)


###############################################################
# Creating the data frame with the list of responses
values.numquestion <- c(seq(1, 24, 1))

p1 <- participant2$Correct 
p2 <- participant3$Correct
p3 <- participant4$Correct
p4 <- participant5$Correct
p5 <- participant6$Correct
p6 <- participant7$Correct 
p7 <- participant8$Correct
p8 <- participant9$Correct
p9 <- participant10$Correct
p10 <- participant11$Correct
p11 <- participant12$Correct
p12 <- participant13$Correct
p13 <- participant14$Correct
p14 <- participant15$Correct
p15 <- participant16$Correct  # as.integer(participant16$Correct) not sure if it follows the same transformation for True and False
p16 <- participant17$Correct # as.integer(participant17$Correct)
p17 <- participant18$Correct # as.integer(participant18$Correct)

#responses.df <- data.frame(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17)

responses.df <- data.frame(matrix(ncol = 24, nrow = 17))
responses.df[1,] <- p1
responses.df[2,] <- p2
responses.df[3,] <- p3
responses.df[4,] <- p4
responses.df[5,] <- p5
responses.df[6,] <- p6
responses.df[7,] <- p7
responses.df[8,] <- p8
responses.df[9,] <- p9
responses.df[10,] <- p10
responses.df[11,] <- p11
responses.df[12,] <- p12
responses.df[13,] <- p13
responses.df[14,] <- p14
responses.df[15,] <- p15
responses.df[16,] <- p16
responses.df[17,] <- p17

# Changes the names of the columns of the data frame
columnNames <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15",
                 "Q16", "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24")

colnames(responses.df) <- columnNames

# Adds the participant column
participantNames <- c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10", "P11", "P12", "P13", "P14", "P15",
                      "P16", "P17")
responses.df$participantName <- participantNames

responses.df <- responses.df %>% relocate(participantName, .before = Q1)


## Function that converts booleans to integers
bool2Integer <- function(val) {
  retValue <- 0
  if (val == "True") {
    retValue <- 50
  } else {
    retValue <-100
  }
  retValue
}

# Did not work for transforming character "True" and "False" to numeric values for the heatmap
#test <- responses.df %>% 
#  select_if(is.logical) %>% 
#  mutate_all( ~ bool2Integer (.))
#
#responses.df %>% mutate(Q1_new = bool2Integer(Q1))
#responses.df %>% mutate_each(funs=(bool2Integer))


# Transforms boolean values into numeric values 
frameBool2Values <- function (df, num.rows, num.columns) {
  newdf <- data.frame(matrix(nrow = num.rows, ncol = num.columns))
  for (i in seq(1,num.rows,1)) {
    for (j in seq(1,num.columns,1)) {
      newdf[i,j] <- bool2Integer(df[i,j+1])
    } # j loop
  } # i loop
  newdf
} # function

# Calls the transformation function
responses.df.numeric <- frameBool2Values(responses.df, 17, 24)
columnNames <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15",
                 "Q16", "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24")
colnames(responses.df.numeric) <- columnNames
participantNames <- c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10", "P11", "P12", "P13", "P14", "P15",
                      "P16", "P17")
responses.df.numeric$participantName <- participantNames

responses.df.numeric <- responses.df.numeric %>% relocate(participantName, .before = Q1)


# TODO transform to numeric the values of the matrix as they are in character form now
heatmap(as.matrix(responses.df.numeric, scale="none"), Colv = NA, Rowv = NA, scale="column")

# heatmap(as.matrix(mtcars, scale="none"), Colv = NA, Rowv = NA, scale="column")