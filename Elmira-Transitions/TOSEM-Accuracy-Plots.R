# TOSEM Paper
# Accuracy results figures
############################################
library(ggplot2)
library(tidyverse)
library(hrbrthemes) 

# Sets the dir to the current place
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

############################################
# Bar plots with distribution of accurate and inaccurate over NoF and NoC

####################################################################################################
# Reads the participants data
experiment.complete.data <- read.csv(file = "../../../Experiment-Data/Eye-tracking-data-samples/ExperimentCompleteDataSet.csv", 
                                     header=TRUE)
attach(experiment.complete.data)


##############################################################################################
# Descriptive Statistics

# Correct answers summary, Correct value = 2, data per participant
summary(experiment.complete.data %>% filter(Correct=="2") %>% count(ParticipantID))

# ParticipantID         n        
# Min.   : 2.00   Min.   : 9.00  
# 1st Qu.: 6.00   1st Qu.:15.00  
# Median :10.00   Median :17.00  
# Mean   :10.24   Mean   :16.71  
# 3rd Qu.:14.00   3rd Qu.:20.00  
# Max.   :19.00   Max.   :21.00  

# Incorrect answers summary, Correct value = 1, data per participant
summary(experiment.complete.data %>% filter(Correct=="1") %>% count(ParticipantID))

# ParticipantID         n         
# Min.   : 2.00   Min.   : 3.000  
# 1st Qu.: 6.00   1st Qu.: 4.000  
# Median :10.00   Median : 7.000  
# Mean   :10.24   Mean   : 7.294  
# 3rd Qu.:14.00   3rd Qu.: 9.000  
# Max.   :19.00   Max.   :15.000  

# Elapsed time for Correct answers for all participants, time in miliseconds
summary(experiment.complete.data %>% filter(Correct=="2") %>% select(ElapsedTime))

# ElapsedTime    
# Min.   :  6226  
# 1st Qu.: 24156  
# Median : 37440  
# Mean   : 42844  
# 3rd Qu.: 53448  
# Max.   :199063  

# Elapsed time for Incorrect answers for all participants, time in miliseconds
summary(experiment.complete.data %>% filter(Correct=="1") %>% select(ElapsedTime))

# ElapsedTime    
# Min.   :  6417  
# 1st Qu.: 28672  
# Median : 42209  
# Mean   : 52968  
# 3rd Qu.: 67066  
# Max.   :283369  

##############################################################################################
##############################################################################################

# Plot: Correct and Incorrect Responses per NoF and NoC

responses.data <- experiment.complete.data

# Creates the data frame with the information, 4 NoF, 3 NoC, correct, incorrect, Number
correct.incorrect.df <- data.frame(matrix(nrow = 4 * 3 * 2, ncol = 4))
columnNames <- c("NoF", "NoC", "Accuracy", "Number")
colnames(correct.incorrect.df) <- columnNames

k <-1
# Values for NoF 1..4
for (i in seq(1,4,1)) {
  # Values for NoC 1..3
  for (j in seq(1,3,1)) {
    
    print(c(i,j,k))
    
    # obtains the information for the table entry i, j
    data.point <- responses.data  %>% filter (NoF==i & NoC==j) %>% select(Correct) %>% 
      group_by(Correct) %>% summarise(n=n())
    
    # counts the number of correct and incorrect responses in entry i,j
    ncorrect <- as.numeric(data.point[2,2])
    nincorrect <- as.numeric(data.point[1,2])
    
    print(c(ncorrect, nincorrect))
    
    # Number of correct responses
    correct.incorrect.df$NoF[k]<- i
    correct.incorrect.df$NoC[k]<- j
    correct.incorrect.df$Accuracy[k] <- 'Correct'
    correct.incorrect.df$Number[k] <- ncorrect 
    k <- k + 1
    
    # Number of incorrect responses
    correct.incorrect.df$NoF[k]<- i
    correct.incorrect.df$NoC[k]<- j
    correct.incorrect.df$Accuracy[k] <- 'Incorrect'
    correct.incorrect.df$Number[k] <- nincorrect 
    k <- k + 1
    
  } # j loop
} # i loop


# Creating the bars
 
# Changing Correct for Accurate and Incorrect for Inaccurate
correct.incorrect.df <- correct.incorrect.df %>%
  mutate(Accuracy = case_when(Accuracy == "Correct" ~ "Accurate", Accuracy == "Incorrect" ~ "Inaccurate"))
  

########## -- From SPLC22-Artifact-Evaluation.R, changed colors and added number of tap
## NoC = 1
bars.1.1 <- correct.incorrect.df %>% filter (NoF==1 & NoC==1) %>%
  ggplot(aes(x=Accuracy, weight = Number)) + 
  coord_cartesian(ylim = c(0, 30)) +
  scale_y_discrete(limits=seq(0, 30, 5)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2, size = 4) + 
  scale_fill_manual(values=c("#009E73","#0072B2")) +
  labs(x="",y="") +
  guides(fill = FALSE, alpha=FALSE) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50"))


bars.2.1 <- correct.incorrect.df %>% filter (NoF==2 & NoC==1) %>%
  ggplot(aes(x=Accuracy, weight = Number)) +
  coord_cartesian(ylim = c(0, 30)) +
  scale_y_discrete(limits=seq(0, 30, 5)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2, size = 4) + 
  scale_fill_manual(values=c("#009E73","#0072B2")) +
  labs(x="",y="") +
  guides(fill = FALSE, alpha=FALSE) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 


bars.3.1 <- correct.incorrect.df %>% filter (NoF==3 & NoC==1) %>%
  ggplot(aes(x=Accuracy, weight = Number)) +
  coord_cartesian(ylim = c(0, 30)) +
  scale_y_discrete(limits=seq(0, 30, 5)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2, size = 4) + 
  scale_fill_manual(values=c("#009E73","#0072B2")) +
  labs(x="",y="") +
  guides(fill = FALSE, alpha=FALSE) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 


bars.4.1 <- correct.incorrect.df %>% filter (NoF==4 & NoC==1) %>%
  ggplot(aes(x=Accuracy, weight = Number)) + 
  coord_cartesian(ylim = c(0, 30)) +
  scale_y_discrete(limits=seq(0, 30, 5)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2, size = 4) + 
  scale_fill_manual(values=c("#009E73","#0072B2")) +
  labs(x="",y="") +
  guides(fill = FALSE, alpha=FALSE) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 


### NoC = 2
bars.1.2 <- correct.incorrect.df %>% filter (NoF==1 & NoC==2) %>%
  ggplot(aes(x=Accuracy, weight = Number)) + 
  coord_cartesian(ylim = c(0, 30)) +
  scale_y_discrete(limits=seq(0, 30, 5)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2, size = 4) + 
  scale_fill_manual(values=c("#009E73","#0072B2")) +
  labs(x="",y="") +
  guides(fill = FALSE, alpha=FALSE) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 

bars.2.2 <- correct.incorrect.df %>% filter (NoF==2 & NoC==2) %>%
  ggplot(aes(x=Accuracy, weight = Number)) +
  coord_cartesian(ylim = c(0, 30)) +
  scale_y_discrete(limits=seq(0, 30, 5)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.0, size = 4) + 
  scale_fill_manual(values=c("#009E73","#0072B2")) +
  labs(x="",y="") +
  guides(fill = FALSE, alpha=FALSE) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 


bars.3.2 <- correct.incorrect.df %>% filter (NoF==3 & NoC==2) %>%
  ggplot(aes(x=Accuracy, weight = Number)) +
  coord_cartesian(ylim = c(0, 30)) +
  scale_y_discrete(limits=seq(0, 30, 5)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.0, size = 4) + 
  scale_fill_manual(values=c("#009E73","#0072B2")) +
  labs(x="",y="") +
  guides(fill = FALSE, alpha=FALSE) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 


bars.4.2 <- correct.incorrect.df %>% filter (NoF==4 & NoC==2) %>%
  ggplot(aes(x=Accuracy, weight = Number)) + 
  coord_cartesian(ylim = c(0, 30)) +
  scale_y_discrete(limits=seq(0, 30, 5)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2, size = 4) + 
  scale_fill_manual(values=c("#009E73","#0072B2")) +
  labs(x="",y="") +
  guides(fill = FALSE, alpha=FALSE) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 


### NoC = 3
bars.1.3 <- correct.incorrect.df %>% filter (NoF==1 & NoC==3) %>%
  ggplot(aes(x=Accuracy, weight = Number)) + 
  coord_cartesian(ylim = c(0, 30)) +
  scale_y_discrete(limits=seq(0, 30, 5)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2, size = 4) + 
  scale_fill_manual(values=c("#009E73","#0072B2")) +
  labs(x="",y="") +
  guides(fill = FALSE, alpha=FALSE) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 

bars.2.3 <- correct.incorrect.df %>% filter (NoF==2 & NoC==3) %>%
  ggplot(aes(x=Accuracy, weight = Number)) +
  coord_cartesian(ylim = c(0, 30)) +
  scale_y_discrete(limits=seq(0, 30, 5)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2, size = 4) + 
  scale_fill_manual(values=c("#009E73","#0072B2")) +
  labs(x="",y="") +
  guides(fill = FALSE, alpha=FALSE) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 


bars.3.3 <- correct.incorrect.df %>% filter (NoF==3 & NoC==3) %>%
  ggplot(aes(x=Accuracy, weight = Number)) +
  coord_cartesian(ylim = c(0, 30)) +
  scale_y_discrete(limits=seq(0, 30, 5)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2, size = 4) + 
  scale_fill_manual(values=c("#009E73","#0072B2")) +
  labs(x="",y="") +
  guides(fill = FALSE, alpha=FALSE) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 


bars.4.3 <- correct.incorrect.df %>% filter (NoF==4 & NoC==3) %>%
  ggplot(aes(x=Accuracy, weight = Number)) + 
  coord_cartesian(ylim = c(0, 30)) +
  scale_y_discrete(limits=seq(0, 30, 5)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2, size = 4) + 
  scale_fill_manual(values=c("#009E73","#0072B2")) +
  labs(x="",y="") +
  guides(fill = FALSE, alpha=FALSE) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 


# Creating the grid for the bars

library(gridExtra)
library(grid)

# Creates the grid for the tables
grid.data <- grid.arrange(
  bars.1.3, bars.2.3, bars.3.3, bars.4.3,
  bars.1.2, bars.2.2, bars.3.2, bars.4.2,
  bars.1.1, bars.2.1, bars.3.1, bars.4.1,
  ncol=4, nrow=3,
  bottom = textGrob("NoF Ranges (1)..(4)",gp=gpar(fontsize=15,font=3)),
  left = textGrob("NoC Ranges (1)..(3)", rot=90, gp=gpar(fontsize=15,font=3)))


#################################################################################
# Plot for the distribution of accuracy by participant and by task
accuracy.data <- experiment.complete.data %>%
  mutate(Correct = case_when(Correct == 1 ~ "Inaccurate", Correct == 2 ~ "Accurate")) %>%
  rename(Accuracy="Correct")
accuracy.data$Accuracy = as.factor(accuracy.data$Accuracy)

participant.accuracy <- accuracy.data %>%   
  ggplot(aes(x=as.factor(ParticipantID), fill=Accuracy)) +  # using factor eliminates missing participants 1 and 15
  coord_cartesian(ylim = c(0, 24)) +
  scale_y_discrete(limits=seq(0, 24, 1)) +
  geom_bar(position="dodge") +
  theme(axis.text.x=element_blank(), panel.background = element_blank(), 
        panel.grid.major.y = element_line(colour = "grey50"), legend.position = "bottom") +
  labs(y="Frequency", x="Participant responses") +
  scale_fill_manual(values=c("#009E73","#0072B2"))
participant.accuracy


question.accuracy <- accuracy.data %>% 
  ggplot(aes(x=QNumber, fill=Accuracy)) +
  coord_cartesian(ylim = c(0, 17)) +
  scale_y_discrete(limits=seq(0, 17, 1)) +
  scale_x_discrete(limits=seq(1, 24, 1)) +
  geom_bar(position="dodge") +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50"), 
        legend.position = "bottom") + 
  labs(x="Task Number", y="Frequency")  +
  scale_fill_manual(values=c("#009E73","#0072B2"))  
question.accuracy

#################################################################################

# TODO
# 0. Mutate Correct=Accurate, Incorrect=Inaccurate
# 1. Make all the other 11 bar plots with the same patters

bars.t2.sp <- accuracy.df.revised %>% filter (T==2 & Visualization.Technique=="2D-SP") %>% 
  ggplot(aes(x=Accuracy, weight = Number)) + 
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_discrete(limits=seq(0, 100, 10)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, size = 5) + 
  # geom_text(aes(label = ..count..), stat = "count", vjust = -1.0, size = 5) + 
  scale_fill_manual(values=c("#009E73","#0072B2")) +
  labs(x="Scatter Plot",y="T=2") +
  guides(fill = FALSE, alpha=FALSE) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 
bars.t2.sp