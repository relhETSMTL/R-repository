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
#####
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


#####
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


#####
# Plot grid of bars of Correct and Incorrect responses, y dimension t values, y dimensions 
accuracy.df <- data.frame(matrix(nrow = 4 * 2, ncol = 4))
columnNames <- c("T", "Visualization.Method", "Accuracy", "Number")
colnames(accuracy.df) <- columnNames



# Bar for t=2 and Scattered Plot
bars.t2.sp.data <- allt.data  %>% filter (T==2 & Visualization.Method=="2D-SP") %>% select(Accuracy) %>% 
  group_by(Accuracy) %>% summarise(n=n())

k <- 1
accuracy.df$T[k] <- 2
accuracy.df$Visualization.Method[k] <- "2D-SP"
accuracy.df$Accuracy[k] <- TRUE
accuracy.df$Number[k] <- as.numeric(bars.t2.sp.data[2,2]) # number of TRUE values

k <- k + 1
accuracy.df$T[k] <- 2
accuracy.df$Visualization.Method[k] <- "2D-SP"
accuracy.df$Accuracy[k] <- FALSE
accuracy.df$Number[k] <- as.numeric(bars.t2.sp.data[1,2]) # number of FALSE values


# Bar for t=2 and Parallel Dimensions
bars.t2.pd.data <- allt.data  %>% filter (T==2 & Visualization.Method=="2D-PD") %>% select(Accuracy) %>% 
  group_by(Accuracy) %>% summarise(n=n())

k <- k + 1
accuracy.df$T[k] <- 2
accuracy.df$Visualization.Method[k] <- "2D-PD"
accuracy.df$Accuracy[k] <- TRUE
accuracy.df$Number[k] <- as.numeric(bars.t2.pd.data[2,2]) # number of TRUE values

k <- k + 1
accuracy.df$T[k] <- 2
accuracy.df$Visualization.Method[k] <- "2D-PD"
accuracy.df$Accuracy[k] <- FALSE
accuracy.df$Number[k] <- as.numeric(bars.t2.pd.data[1,2]) # number of FALSE values


# Bar for t=3 and Scattered Plot
bars.t3.sp.data <- allt.data  %>% filter (T==3 & Visualization.Method=="3D-SP") %>% select(Accuracy) %>% 
  group_by(Accuracy) %>% summarise(n=n())

k <- k + 1
accuracy.df$T[k] <- 3
accuracy.df$Visualization.Method[k] <- "3D-SP"
accuracy.df$Accuracy[k] <- TRUE
accuracy.df$Number[k] <- as.numeric(bars.t3.sp.data[2,2]) # number of TRUE values

k <- k + 1
accuracy.df$T[k] <- 3
accuracy.df$Visualization.Method[k] <- "3D-SP"
accuracy.df$Accuracy[k] <- FALSE
accuracy.df$Number[k] <- as.numeric(bars.t3.sp.data[1,2]) # number of FALSE values


# Bar for t=3 and Parallel Dimensions
bars.t3.pd.data <- allt.data  %>% filter (T==3 & Visualization.Method=="3D-PD") %>% select(Accuracy) %>% 
  group_by(Accuracy) %>% summarise(n=n())

k <- k + 1
accuracy.df$T[k] <- 3
accuracy.df$Visualization.Method[k] <- "3D-PD"
accuracy.df$Accuracy[k] <- TRUE
accuracy.df$Number[k] <- as.numeric(bars.t3.pd.data[2,2]) # number of TRUE values

k <- k + 1
accuracy.df$T[k] <- 3
accuracy.df$Visualization.Method[k] <- "3D-PD"
accuracy.df$Accuracy[k] <- FALSE
accuracy.df$Number[k] <- as.numeric(bars.t3.pd.data[1,2]) # number of FALSE values


# Bar t=2 2D-SP
bars.t2.sp <- accuracy.df %>% filter (T==2 & Visualization.Method=="2D-SP") %>% 
  ggplot(aes(x=Accuracy, weight = Number)) + 
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_discrete(limits=seq(0, 100, 10)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  scale_fill_manual(values=c("red", "green")) +
  labs(x="Scatter Plot",y="T=2") +
  guides(fill = FALSE, alpha=FALSE) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 
bars.t2.sp


# Bar t=2 2D-PD
bars.t2.pd <- accuracy.df %>% filter (T==2 & Visualization.Method=="2D-PD") %>% 
  ggplot(aes(x=Accuracy, weight = Number)) + 
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_discrete(limits=seq(0, 100, 10)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  scale_fill_manual(values=c("red", "green")) +
  labs(x="Parallel Coordinates Plot",y="T=2") +
  guides(fill = FALSE, alpha=FALSE) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 
bars.t2.pd


# Bar t=3 2D-SP
bars.t3.sp <- accuracy.df %>% filter (T==3 & Visualization.Method=="3D-SP") %>% 
  ggplot(aes(x=Accuracy, weight = Number)) + 
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_discrete(limits=seq(0, 100, 10)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  scale_fill_manual(values=c("red", "green")) +
  labs(x="Scatter Plot",y="T=3") +
  guides(fill = FALSE, alpha=FALSE) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 
bars.t3.sp


# Bar t=3 2D-PD
bars.t3.pd <- accuracy.df %>% filter (T==3 & Visualization.Method=="3D-PD") %>% 
  ggplot(aes(x=Accuracy, weight = Number)) + 
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_discrete(limits=seq(0, 100, 10)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  scale_fill_manual(values=c("red", "green")) +
  labs(x="Parallel Coordinates Plot",y="T=3") +
  guides(fill = FALSE, alpha=FALSE) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 
bars.t3.pd


# Grid for the bar plots
# Creating the grid for the bars
library(gridExtra)
library(grid)

# Creates the grid for the stack bars
grid.data <- grid.arrange(bars.t3.sp,bars.t3.pd,
                          bars.t2.sp,bars.t2.pd,
                          ncol=2, nrow=2,
                          bottom = textGrob("Visualization Methods",gp=gpar(fontsize=15,font=3)),
                          left = textGrob("Covering Array Strength", rot=90, gp=gpar(fontsize=15,font=3)))


#####
# Tests of plotting difficulty level and certainty assessment
# Note: Distribution of True vs False in the Difficulty Level and Certainty Assessment
ggplot(allt.data, aes(x=Difficulty.Level, y=Certainty.Assessment)) + geom_point()

ggplot(allt.data, aes(x=Difficulty.Level, y=Certainty.Assessment, shape=Accuracy, color=Accuracy)) +  #  
  scale_color_manual(values=c("red", "green")) +
  #scale_color_manual(values=c('#999999','#E69F00', '#56B4E9'))+ 
  geom_point(size=5,alpha=0.5)


#####
#
# Boxplots of difficulty level per question
# Note: completely flatten, most of them with value of 1=very easy
difficulty.level <- allt.data %>% ggplot(aes(x=Question.Number, group=Question.Number, Difficulty.Level)) +
  geom_boxplot(aes(fill=Difficulty.Level), varwidth=T, fill="plum") +
  #  coord_flip() +
  labs(x="Question Number", y="Difficulty Level") +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
  #  theme_minimal() +
  #  scale_x_continuous(breaks=seq(1, 24, 1))
  scale_x_discrete(limits=seq(1, 16, 1))  +
  ylim(0, 20) +
  scale_y_continuous(breaks=seq(0, 20, 2))
difficulty.level


# Boxplots of difficulty level per question
# Note: Here the distributions are more meaningful
certainty.assessment <- allt.data %>% ggplot(aes(x=Question.Number, group=Question.Number, Certainty.Assessment)) +
  geom_boxplot(aes(fill=Certainty.Assessment), varwidth=T, fill="plum", alpha=0.5) +
  geom_jitter() +
  #  coord_flip() +
  labs(x="Question Number", y="Certainty Assessment") +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
  #  theme_minimal() +
  #  scale_x_continuous(breaks=seq(1, 24, 1))
  scale_x_discrete(limits=seq(1, 16, 1))  +
  ylim(0, 20) +
  scale_y_continuous(breaks=seq(0, 20, 2))
certainty.assessment



# Grid of Certainty Assessment
# t2 and parallel dimensions
bp.t2.pd <- allt.data %>% filter (T==2 & Visualization.Method=="2D-PD") %>%
  ggplot(aes(x=Question.Number, group=Question.Number, Certainty.Assessment)) +
  geom_boxplot(aes(fill=Certainty.Assessment), varwidth=T, fill="plum", alpha=0.5) +
  geom_jitter() +
  #  coord_flip() +
  labs(x="Parallel Coordinates Plot", y="T=2") +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50"), 
        axis.text.x=element_blank()) +
  #  theme_minimal() +
  #  scale_x_continuous(breaks=seq(1, 24, 1))
  # scale_x_discrete(limits=seq(1, 16, 1))  +
  ylim(0, 20)  +
  scale_y_continuous(breaks=seq(0, 20, 2))
bp.t2.pd


# t2 and scattered plot
bp.t2.sp <- allt.data %>% filter (T==2 & Visualization.Method=="2D-SP") %>%
  ggplot(aes(x=Question.Number, group=Question.Number, Certainty.Assessment)) +
  geom_boxplot(aes(fill=Certainty.Assessment), varwidth=T, fill="plum", alpha=0.5) +
  geom_jitter() +
  #  coord_flip() +
  labs(x="Scatter Plot", y="T=2") +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50"), 
        axis.text.x=element_blank()) +
  #  theme_minimal() +
  #  scale_x_continuous(breaks=seq(1, 24, 1))
  # scale_x_discrete(limits=seq(1, 16, 1))  +
  ylim(0, 20)  +
  scale_y_continuous(breaks=seq(0, 20, 2))
bp.t2.sp


# t3 and parallel dimensions
bp.t3.pd <- allt.data %>% filter (T==3 & Visualization.Method=="3D-PD") %>%
  ggplot(aes(x=Question.Number, group=Question.Number, Certainty.Assessment)) +
  geom_boxplot(aes(fill=Certainty.Assessment), varwidth=T, fill="plum", alpha=0.5) +
  geom_jitter() +
  #  coord_flip() +
  labs(x="Parallel Coordinates Plot", y="T=3") +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50"), 
        axis.text.x=element_blank()) +
  #  theme_minimal() +
  #  scale_x_continuous(breaks=seq(1, 24, 1))
  # scale_x_discrete(limits=seq(1, 16, 1))  +
  ylim(0, 20)  +
  scale_y_continuous(breaks=seq(0, 20, 2))
bp.t3.pd


# t3 and scattered plot
bp.t3.sp <- allt.data %>% filter (T==3 & Visualization.Method=="3D-SP") %>%
  ggplot(aes(x=Question.Number, group=Question.Number, Certainty.Assessment)) +
  geom_boxplot(aes(fill=Certainty.Assessment), varwidth=T, fill="plum", alpha=0.5) +
  geom_jitter() +
  #  coord_flip() +
  labs(x="Scatter Plot", y="T=3") +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50"), 
        axis.text.x=element_blank()) +
  #  theme_minimal() +
  #  scale_x_continuous(breaks=seq(1, 24, 1))
  # scale_x_discrete(limits=seq(1, 16, 1))  +
  ylim(0, 20)  +
  scale_y_continuous(breaks=seq(0, 20, 2))
bp.t3.sp


# Grid for the bar plots
# Creating the grid for the bars
library(gridExtra)
library(grid)

# Creates the grid for the stack bars
bp.grid.data <- grid.arrange(bp.t3.sp,bp.t3.pd,
                          bp.t2.sp,bp.t2.pd,
                          ncol=2, nrow=2,
                          bottom = textGrob("Visualization Methods",gp=gpar(fontsize=15,font=3)),
                          left = textGrob("Certainty Assessment", rot=90, gp=gpar(fontsize=15,font=3)))


###################################
# Eliminating some elements in the axis
# theme(axis.title.x=element_blank(),
#      axis.text.x=element_blank(),
#      axis.ticks.x=element_blank())


# geom_bar(aes(fill=Accuracy, alpha=0.5)) +
#  scale_fill_manual(values=c("green", "red")) +


#################

# GPL General statistics
library("tidyverse")

# Loads t=2 file data


#################
# Loads original participants responses
gpl.t.2 <- read.csv(file = "../../../Eye-Tracking-Visualization/Paper-Sources/OrgGPL/FM17.m.2Sets.csv", header=TRUE)
attach(gpl.t.2)

# Loads t=3 file data

gpl.t.3 <- read.csv(file = "../../../Eye-Tracking-Visualization/Paper-Sources/OrgGPL/FM17.m.3Sets.csv", header=TRUE)
attach(gpl.t.3)


#################
