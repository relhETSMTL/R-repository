# TOSEM Paper
# Accuracy results figures
############################################
library(ggplot2)
library(tidyverse)
library(hrbrthemes) 
library(gridExtra)
library(grid)

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
# Box plots for the response times, shown by range of NoF and Noc
response.time.data <- experiment.complete.data %>%
  mutate(Correct = case_when(Correct == 1 ~ "Inaccurate", Correct == 2 ~ "Accurate")) %>%
  rename(Accuracy="Correct")
response.time.data$Accuracy = as.factor(response.time.data$Accuracy)


##########
## NoC = 1
bp.response.time.1.1 <- response.time.data %>% 
  filter (NoC==1 & NoF==1) %>%
  ggplot(aes(x=Accuracy, group=Accuracy,ElapsedTime/1000)) +  
  geom_boxplot(aes(fill=ElapsedTime/1000), varwidth=T,  fill=c("#009E73","#0072B2"), alpha=0.5) +  
  geom_jitter() +
  labs(x="Range (1)", y="Range (1)") + 
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
  scale_y_continuous(breaks=seq(0, 300, 50),limits = c(0, 300))
bp.response.time.1.1


bp.response.time.1.2 <- response.time.data %>% 
  filter (NoC==1 & NoF==2) %>%
  ggplot(aes(x=Accuracy, group=Accuracy,ElapsedTime/1000)) +  
  geom_boxplot(aes(fill=ElapsedTime/1000), varwidth=T,  fill=c("#009E73","#0072B2"), alpha=0.5) +  
  geom_jitter() +
  labs(x="Range (2)", y="Range (1)") + 
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
  scale_y_continuous(breaks=seq(0, 300, 50),limits = c(0, 300))
bp.response.time.1.2


bp.response.time.1.3 <- response.time.data %>% 
  filter (NoC==1 & NoF==3) %>%
  ggplot(aes(x=Accuracy, group=Accuracy,ElapsedTime/1000)) +  
  geom_boxplot(aes(fill=ElapsedTime/1000), varwidth=T,  fill=c("#009E73","#0072B2"), alpha=0.5) +  
  geom_jitter() +
  labs(x="Range (3)", y="Range (1)") + 
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
  scale_y_continuous(breaks=seq(0, 300, 50),limits = c(0, 300))
bp.response.time.1.3


bp.response.time.1.4 <- response.time.data %>% 
  filter (NoC==1 & NoF==4) %>%
  ggplot(aes(x=Accuracy, group=Accuracy,ElapsedTime/1000)) +  
  geom_boxplot(aes(fill=ElapsedTime/1000), varwidth=T,  fill=c("#009E73","#0072B2"), alpha=0.5) +  
  geom_jitter() +
  labs(x="Range (4)", y="Range (1)") + 
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
  scale_y_continuous(breaks=seq(0, 300, 50),limits = c(0, 300))
bp.response.time.1.4


## NoC = 2
bp.response.time.2.1 <- response.time.data %>% 
  filter (NoC==2 & NoF==1) %>%
  ggplot(aes(x=Accuracy, group=Accuracy,ElapsedTime/1000)) +  
  geom_boxplot(aes(fill=ElapsedTime/1000), varwidth=T,  fill=c("#009E73","#0072B2"), alpha=0.5) +  
  geom_jitter() +
  labs(x="Range (1)", y="Range (2)") + 
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
  scale_y_continuous(breaks=seq(0, 300, 50),limits = c(0, 300))
bp.response.time.2.1


bp.response.time.2.2 <- response.time.data %>% 
  filter (NoC==2 & NoF==2) %>%
  ggplot(aes(x=Accuracy, group=Accuracy,ElapsedTime/1000)) +  
  geom_boxplot(aes(fill=ElapsedTime/1000), varwidth=T,  fill=c("#009E73","#0072B2"), alpha=0.5) +  
  geom_jitter() +
  labs(x="Range (2)", y="Range (2)") + 
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
  scale_y_continuous(breaks=seq(0, 300, 50),limits = c(0, 300))
bp.response.time.2.2


bp.response.time.2.3 <- response.time.data %>% 
  filter (NoC==2 & NoF==3) %>%
  ggplot(aes(x=Accuracy, group=Accuracy,ElapsedTime/1000)) +  
  geom_boxplot(aes(fill=ElapsedTime/1000), varwidth=T,  fill=c("#009E73","#0072B2"), alpha=0.5) +  
  geom_jitter() +
  labs(x="Range (3)", y="Range (2)") + 
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
  scale_y_continuous(breaks=seq(0, 300, 50),limits = c(0, 300))
bp.response.time.2.3


bp.response.time.2.4 <- response.time.data %>% 
  filter (NoC==2 & NoF==4) %>%
  ggplot(aes(x=Accuracy, group=Accuracy,ElapsedTime/1000)) +  
  geom_boxplot(aes(fill=ElapsedTime/1000), varwidth=T,  fill=c("#009E73","#0072B2"), alpha=0.5) +  
  geom_jitter() +
  labs(x="Range (4)", y="Range (2)") + 
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
  scale_y_continuous(breaks=seq(0, 300, 50),limits = c(0, 300))
bp.response.time.2.4


## NoC = 3
bp.response.time.3.1 <- response.time.data %>% 
  filter (NoC==3 & NoF==1) %>%
  ggplot(aes(x=Accuracy, group=Accuracy,ElapsedTime/1000)) +  
  geom_boxplot(aes(fill=ElapsedTime/1000), varwidth=T,  fill=c("#009E73","#0072B2"), alpha=0.5) +  
  geom_jitter() +
  labs(x="Range (1)", y="Range (3)") + 
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
  scale_y_continuous(breaks=seq(0, 300, 50),limits = c(0, 300))
bp.response.time.3.1


bp.response.time.3.2 <- response.time.data %>% 
  filter (NoC==3 & NoF==2) %>%
  ggplot(aes(x=Accuracy, group=Accuracy,ElapsedTime/1000)) +  
  geom_boxplot(aes(fill=ElapsedTime/1000), varwidth=T,  fill=c("#009E73","#0072B2"), alpha=0.5) +  
  geom_jitter() +
  labs(x="Range (2)", y="Range (3)") + 
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
  scale_y_continuous(breaks=seq(0, 300, 50),limits = c(0, 300))
bp.response.time.3.2


bp.response.time.3.3 <- response.time.data %>% 
  filter (NoC==3 & NoF==3) %>%
  ggplot(aes(x=Accuracy, group=Accuracy,ElapsedTime/1000)) +  
  geom_boxplot(aes(fill=ElapsedTime/1000), varwidth=T,  fill=c("#009E73","#0072B2"), alpha=0.5) +  
  geom_jitter() +
  labs(x="Range (3)", y="Range (3)") + 
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
  scale_y_continuous(breaks=seq(0, 300, 50),limits = c(0, 300))
bp.response.time.3.3


bp.response.time.3.4 <- response.time.data %>% 
  filter (NoC==3 & NoF==4) %>%
  ggplot(aes(x=Accuracy, group=Accuracy,ElapsedTime/1000)) + # , fill=Accuracy  
  geom_boxplot(aes(fill=ElapsedTime/1000), varwidth=T,  fill=c("#009E73","#0072B2"), alpha=0.5) +  
  geom_jitter() +
  labs(x="Range (4)", y="Range (3)") + 
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
  scale_y_continuous(breaks=seq(0, 300, 50),limits = c(0, 300))
bp.response.time.3.4



# Creates the grid for boxplots
# Works but: Reduce maximum range to 300 secs, Leave Range (N) for the axis of each boxplot, 
# Remove the Accurate and Inaccurate labels
grid.response.time <- 
    grid.arrange(bp.response.time.3.1,
                 bp.response.time.3.2,
                 bp.response.time.3.3,bp.response.time.3.4,
                 bp.response.time.2.1,bp.response.time.2.2,bp.response.time.2.3,bp.response.time.2.4,
                 bp.response.time.1.1,bp.response.time.1.2,bp.response.time.1.3,bp.response.time.1.4,
                 ncol=4, nrow=3,
                 bottom = textGrob("NoF Ranges",gp=gpar(fontsize=15,font=3)),
                 left = textGrob("NoC Ranges - time in seconds", rot=90, gp=gpar(fontsize=15,font=3)))




################################################################################
################################################################################
################################################################################


################################################################################
# Function that computes the means of the AOIs: 
# Question, Answer, Legend, Buttons, FM, Window, CTC
# Input: A dataframe from the complete experiment file for a combination of
#       NoC and NoF
# Return: the data frame with the means of each AOI for fixations and time
computeAOIAverageProportions <- function(combination.noc.nof) {
  
  result.df <- data.frame(matrix(nrow = 2 * 7, ncol = 3))
  columnNamesAOIs <- c("Measure", "AOI", "Proportion")
  colnames(result.df) <- columnNamesAOIs
  
  # proportions of count
  result.df[1,1] <- 'count'
  result.df[1,2] <- 'Question'
  result.df[1,3] <- mean (combination.noc.nof$perc.fixations.Question)
  
  result.df[2,1] <- 'count'
  result.df[2,2] <- 'Answer'
  result.df[2,3] <- mean(combination.noc.nof$perc.fixations.Answer)
  
  result.df[3,1] <- 'count'
  result.df[3,2] <- 'Legend'
  result.df[3,3] <- mean(combination.noc.nof$perc.fixations.Legend)
  
  result.df[4,1] <- 'count'
  result.df[4,2] <- 'Buttons'
  result.df[4,3] <- mean (combination.noc.nof$perc.fixations.Buttons)
  
  result.df[5,1] <- 'count'
  result.df[5,2] <- 'FM'
  result.df[5,3] <- mean(combination.noc.nof$perc.fixations.FM)
  
  result.df[6,1] <- 'count'
  result.df[6,2] <- 'Window'
  result.df[6,3] <- mean(combination.noc.nof$perc.fixations.Window)
  
  result.df[7,1] <- 'count'
  result.df[7,2] <- 'CTC'
  result.df[7,3] <- mean(combination.noc.nof$perc.fixations.CTC)
  
  # Proportions of time
  result.df[8,1] <- 'time'
  result.df[8,2] <- 'Question'
  result.df[8,3] <- mean (combination.noc.nof$perc.time.Question)
  
  result.df[9,1] <- 'time'
  result.df[9,2] <- 'Answer'
  result.df[9,3] <- mean(combination.noc.nof$perc.time.Answer)
  
  result.df[10,1] <- 'time'
  result.df[10,2] <- 'Legend'
  result.df[10,3] <- mean(combination.noc.nof$perc.time.Legend)
  
  result.df[11,1] <- 'time'
  result.df[11,2] <- 'Buttons'
  result.df[11,3] <- mean (combination.noc.nof$perc.time.Buttons)
  
  result.df[12,1] <- 'time'
  result.df[12,2] <- 'FM'
  result.df[12,3] <- mean(combination.noc.nof$perc.time.FM)
  
  result.df[13,1] <- 'time'
  result.df[13,2] <- 'Window'
  result.df[13,3] <- mean(combination.noc.nof$perc.time.Window)
  
  result.df[14,1] <- 'time'
  result.df[14,2] <- 'CTC'
  result.df[14,3] <- mean (combination.noc.nof$perc.time.CTC)
  
  # makes Measure factor values instead of characters
  result.df$Measure <- factor(result.df$Measure, levels = c('time', 'count'))
  result.df$AOI <- as.factor(result.df$AOI)
  
  # Reorders the factor names of the AOIS for a more meaningful color order
  result.df <- result.df %>% 
    mutate(AOI=fct_relevel(AOI,c("FM","Question","CTC","Answer","Window","Buttons","Legend")))
  
  # returns the produced data frame
  return (result.df)
  
} # computeAOIAverageProportions


################################################################################
# Function that computes a proportion plot for time or count for a combination
# of NoC and NoF data
# Input: 
#   proportions.df - data frame of proportions produced by computeAOIAverageProportions
#   measure.name - either "time" or "count" 
#   label.x - title to put on the x label (i.e. Range of NoF)
#   label.y - title to put on the y label (i.e. Range of NoC)
# Returns the plot for the given measure and combination of NoC and NoF proportions
computeAOIProportionsPlot <- function(proportions.df, measure.name, label.x, label.y) {
  
  # Color blind palette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999")
  
  proportions.plot <-  proportions.df %>% filter(Measure==measure.name) %>%
    ggplot(aes(x=reorder(AOI, -Proportion), y=Proportion, fill=AOI)) +
    geom_bar(stat="identity") +
    theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50"), 
          # axis.title.x = element_blank(), 
          axis.text.x=element_blank(),
          legend.position = "bottom") +
    labs(y=label.y, x=label.x) +
    # ggtitle("NoC=1 NoF=1") +
    scale_y_continuous(limits=c(0, 0.60), breaks=seq(0, 0.60, 0.05)) +
    scale_fill_manual(values=cbPalette)  
  
  # returns the plot
  return(proportions.plot)
  
} # of computeAOIProportionsPlot


################################################################################
################################################################################
################################################################################

# Data for all combinatios of NoC and NoF
# NoC=1
data.noc.1.nof.1.df <- computeAOIAverageProportions(experiment.complete.data %>% filter(NoC==1 & NoF==1))
data.noc.1.nof.2.df <- computeAOIAverageProportions(experiment.complete.data %>% filter(NoC==1 & NoF==2))
data.noc.1.nof.3.df <- computeAOIAverageProportions(experiment.complete.data %>% filter(NoC==1 & NoF==3))
data.noc.1.nof.4.df <- computeAOIAverageProportions(experiment.complete.data %>% filter(NoC==1 & NoF==4))

# NoC=2
data.noc.2.nof.1.df <- computeAOIAverageProportions(experiment.complete.data %>% filter(NoC==2 & NoF==1))
data.noc.2.nof.2.df <- computeAOIAverageProportions(experiment.complete.data %>% filter(NoC==2 & NoF==2))
data.noc.2.nof.3.df <- computeAOIAverageProportions(experiment.complete.data %>% filter(NoC==2 & NoF==3))
data.noc.2.nof.4.df <- computeAOIAverageProportions(experiment.complete.data %>% filter(NoC==2 & NoF==4))

# NoC=3
data.noc.3.nof.1.df <- computeAOIAverageProportions(experiment.complete.data %>% filter(NoC==3 & NoF==1))
data.noc.3.nof.2.df <- computeAOIAverageProportions(experiment.complete.data %>% filter(NoC==3 & NoF==2))
data.noc.3.nof.3.df <- computeAOIAverageProportions(experiment.complete.data %>% filter(NoC==3 & NoF==3))
data.noc.3.nof.4.df <- computeAOIAverageProportions(experiment.complete.data %>% filter(NoC==3 & NoF==4))


## Creation of the proportions box plots for combinations of NoC and NoF
# Plots for count measures
# Noc=1
prop.plot.count.noc.1.nof.1 <- computeAOIProportionsPlot (data.noc.1.nof.1.df, "count", "Range (1)", "Range (1)")
prop.plot.count.noc.1.nof.2 <- computeAOIProportionsPlot (data.noc.1.nof.2.df, "count", "Range (2)", "Range (1)")
prop.plot.count.noc.1.nof.3 <- computeAOIProportionsPlot (data.noc.1.nof.3.df, "count", "Range (3)", "Range (1)")
prop.plot.count.noc.1.nof.4 <- computeAOIProportionsPlot (data.noc.1.nof.4.df, "count", "Range (4)", "Range (1)")
# Noc=2
prop.plot.count.noc.2.nof.1 <- computeAOIProportionsPlot (data.noc.2.nof.1.df, "count", "Range (1)", "Range (2)")
prop.plot.count.noc.2.nof.2 <- computeAOIProportionsPlot (data.noc.2.nof.2.df, "count", "Range (2)", "Range (2)")
prop.plot.count.noc.2.nof.3 <- computeAOIProportionsPlot (data.noc.2.nof.3.df, "count", "Range (3)", "Range (2)")
prop.plot.count.noc.2.nof.4 <- computeAOIProportionsPlot (data.noc.2.nof.4.df, "count", "Range (4)", "Range (2)")
# Noc=3
prop.plot.count.noc.3.nof.1 <- computeAOIProportionsPlot (data.noc.3.nof.1.df, "count", "Range (1)", "Range (3)")
prop.plot.count.noc.3.nof.2 <- computeAOIProportionsPlot (data.noc.3.nof.2.df, "count", "Range (2)", "Range (3)")
prop.plot.count.noc.3.nof.3 <- computeAOIProportionsPlot (data.noc.3.nof.3.df, "count", "Range (3)", "Range (3)")
prop.plot.count.noc.3.nof.4 <- computeAOIProportionsPlot (data.noc.3.nof.4.df, "count", "Range (4)", "Range (3)")

# Plots for time measures
# Noc=1
prop.plot.time.noc.1.nof.1 <- computeAOIProportionsPlot (data.noc.1.nof.1.df, "time", "Range (1)", "Range (1)")
prop.plot.time.noc.1.nof.2 <- computeAOIProportionsPlot (data.noc.1.nof.2.df, "time", "Range (2)", "Range (1)")
prop.plot.time.noc.1.nof.3 <- computeAOIProportionsPlot (data.noc.1.nof.3.df, "time", "Range (3)", "Range (1)")
prop.plot.time.noc.1.nof.4 <- computeAOIProportionsPlot (data.noc.1.nof.4.df, "time", "Range (4)", "Range (1)")
# Noc=2
prop.plot.time.noc.2.nof.1 <- computeAOIProportionsPlot (data.noc.2.nof.1.df, "time", "Range (1)", "Range (2)")
prop.plot.time.noc.2.nof.2 <- computeAOIProportionsPlot (data.noc.2.nof.2.df, "time", "Range (2)", "Range (2)")
prop.plot.time.noc.2.nof.3 <- computeAOIProportionsPlot (data.noc.2.nof.3.df, "time", "Range (3)", "Range (2)")
prop.plot.time.noc.2.nof.4 <- computeAOIProportionsPlot (data.noc.2.nof.4.df, "time", "Range (4)", "Range (2)")
# Noc=3
prop.plot.time.noc.3.nof.1 <- computeAOIProportionsPlot (data.noc.3.nof.1.df, "time", "Range (1)", "Range (3)")
prop.plot.time.noc.3.nof.2 <- computeAOIProportionsPlot (data.noc.3.nof.2.df, "time", "Range (2)", "Range (3)")
prop.plot.time.noc.3.nof.3 <- computeAOIProportionsPlot (data.noc.3.nof.3.df, "time", "Range (3)", "Range (3)")
prop.plot.time.noc.3.nof.4 <- computeAOIProportionsPlot (data.noc.3.nof.4.df, "time", "Range (4)", "Range (3)")

## Creating the aggregate plots
# https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots
library(cowplot)

# Plot with the four combinations, pairs/triples & SP and PD plots
cowplot.proportions.count.grid <- 
  plot_grid(prop.plot.count.noc.3.nof.1 + theme(legend.position="none"), 
      prop.plot.count.noc.3.nof.2 + theme(legend.position="none"),
      prop.plot.count.noc.3.nof.3 + theme(legend.position="none"), 
      prop.plot.count.noc.3.nof.4 + theme(legend.position="none"),
      prop.plot.count.noc.2.nof.1 + theme(legend.position="none"),
      prop.plot.count.noc.2.nof.2 + theme(legend.position="none"), 
      prop.plot.count.noc.2.nof.3 + theme(legend.position="none"), 
      prop.plot.count.noc.2.nof.4 + theme(legend.position="none"),
      prop.plot.count.noc.1.nof.1 + theme(legend.position="none"), 
      prop.plot.count.noc.1.nof.2 + theme(legend.position="none"),
      prop.plot.count.noc.1.nof.3 + theme(legend.position="none"), 
      prop.plot.count.noc.1.nof.4 + theme(legend.position="none"),
      align = 'vh',
      hjust = -1,
      nrow = 3)

# computes the common legend
legend_b <- get_legend(prop.plot.count.noc.1.nof.4 + theme(legend.position="bottom"))

# References for adding the labels
# https://stackoverflow.com/questions/49577461/adding-x-and-y-laxis-label-to-ggplot-grid-build-with-cowplot

cowplot.proportions.count <- plot_grid( cowplot.proportions.count.grid, 
                                        legend_b, 
                                        ncol = 1, rel_heights = c(1, .1),
                                        scale = 0.95) + # rel_heights = c(1, .2)
                              draw_label("NoF Ranges", x=0.5, y=  0, vjust=-4.5, angle= 0) + 
                              draw_label("Noc Ranges", x=  0, y= 0.5, vjust= 1.5, angle=90) +
                              draw_label("Proportions of Fixations Count", x=0.5, y=0, vjust=-46, angle= 0)
cowplot.proportions.count




### Scract
# Overloaded captions
grid.proportions.count <- 
  grid.arrange(prop.plot.count.noc.3.nof.1 + theme(legend.position="none"), 
               prop.plot.count.noc.3.nof.2 + theme(legend.position="none"), 
               prop.plot.count.noc.3.nof.3 + theme(legend.position="none"), 
               prop.plot.count.noc.3.nof.4 + theme(legend.position="none"),
               prop.plot.count.noc.2.nof.1 + theme(legend.position="none"), 
               prop.plot.count.noc.2.nof.2 + theme(legend.position="none"),
               prop.plot.count.noc.2.nof.3 + theme(legend.position="none"),
               prop.plot.count.noc.2.nof.4 + theme(legend.position="none"),
               prop.plot.count.noc.1.nof.1 + theme(legend.position="none"), 
               prop.plot.count.noc.1.nof.2 + theme(legend.position="none"), 
               prop.plot.count.noc.1.nof.3 + theme(legend.position="none"), 
               prop.plot.count.noc.1.nof.4 + theme(legend.position="none"),
               #               legend_b,  # does not work correctly
               ncol=4, nrow=3,
               bottom = textGrob("NoF Ranges",gp=gpar(fontsize=15,font=3)),
               #               title = "Proportions of fixation count", # does not work
               left = textGrob("NoC Ranges", rot=90, gp=gpar(fontsize=15,font=3)))



################################################################################
################################################################################
################################################################################
################################################################################

# scratch 

################################################################################
# Test for the bar chart ordered by proportion of AOI
# Color blind palette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999")

proportion.time.noc.1.nof.1 <-  data.noc.3.nof.4.df %>% filter(Measure=="time") %>%
  ggplot(aes(x=reorder(AOI, -Proportion), y=Proportion, fill=AOI)) +
  geom_bar(stat="identity") +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50"), 
        # axis.title.x = element_blank(), 
        axis.text.x=element_blank(),
        legend.position = "bottom") +
  labs(y="Range (1)", x="Range (1)") +
  # ggtitle("NoC=1 NoF=1") +
  scale_y_continuous(limits=c(0, 0.60), breaks=seq(0, 0.60, 0.05)) +
  scale_fill_manual(values=cbPalette)  
proportion.time.noc.1.nof.1


# Data for t=2, Visualization Technique scatter plots
t2.sp.data <- experiment.data %>% filter(T==2 & Visualization.Technique=="2D-SP") 


t2.sp.df <- data.frame(matrix(nrow = 2 * 7, ncol = 3))
columnNamesAOIs <- c("Measure", "AOI", "Percentage")
colnames(t2.sp.df) <- columnNamesAOIs

t2.sp.df[1,1] <- 'count'
t2.sp.df[1,2] <- 'Axial'
t2.sp.df[1,3] <- mean (t2.sp.data$Axial.pfcount)

t2.sp.df[2,1] <- 'count'
t2.sp.df[2,2] <- 'Misc'
t2.sp.df[2,3] <- mean(t2.sp.data$Misc.pfcount)

t2.sp.df[3,1] <- 'count'
t2.sp.df[3,2] <- 'Navigation'
t2.sp.df[3,3] <- mean(t2.sp.data$Navigation.pfcount)

t2.sp.df[4,1] <- 'count'
t2.sp.df[4,2] <- 'Question'
t2.sp.df[4,3] <- mean (t2.sp.data$Question.pfcount)

t2.sp.df[5,1] <- 'count'
t2.sp.df[5,2] <- 'Response'
t2.sp.df[5,3] <- mean(t2.sp.data$Response.pfcount)

t2.sp.df[6,1] <- 'count'
t2.sp.df[6,2] <- 'Solution'
t2.sp.df[6,3] <- mean(t2.sp.data$Solution.pfcount)

t2.sp.df[7,1] <- 'count'
t2.sp.df[7,2] <- 'Target'
t2.sp.df[7,3] <- mean (t2.sp.data$Target.pfcount)

t2.sp.df[8,1] <- 'time'
t2.sp.df[8,2] <- 'Axial'
t2.sp.df[8,3] <- mean (t2.sp.data$Axial.pftime)

t2.sp.df[9,1] <- 'time'
t2.sp.df[9,2] <- 'Misc'
t2.sp.df[9,3] <- mean(t2.sp.data$Misc.pftime)

t2.sp.df[10,1] <- 'time'
t2.sp.df[10,2] <- 'Navigation'
t2.sp.df[10,3] <- mean(t2.sp.data$Navigation.pftime)

t2.sp.df[11,1] <- 'time'
t2.sp.df[11,2] <- 'Question'
t2.sp.df[11,3] <- mean (t2.sp.data$Question.pftime)

t2.sp.df[12,1] <- 'time'
t2.sp.df[12,2] <- 'Response'
t2.sp.df[12,3] <- mean(t2.sp.data$Response.pftime)

t2.sp.df[13,1] <- 'time'
t2.sp.df[13,2] <- 'Solution'
t2.sp.df[13,3] <- mean(t2.sp.data$Solution.pftime)

t2.sp.df[14,1] <- 'time'
t2.sp.df[14,2] <- 'Target'
t2.sp.df[14,3] <- mean (t2.sp.data$Target.pftime)

# makes Measure factor values instead of characters
t2.sp.df$Measure <- factor(t2.sp.df$Measure, levels = c('time', 'count'))
t2.sp.df$AOI <- as.factor(t2.sp.df$AOI)

# Mutates the names of the AOIs, solution and misc
t2.sp.df.renamed <- t2.sp.df %>%
  mutate(AOI = case_when(AOI == "Misc" ~ "Stimulus", AOI == "Response" ~ "Answer", TRUE ~ AOI)) %>%
  mutate(AOI=fct_relevel(AOI,c("Question","Answer","Axial","Target","Solution","Navigation","Stimulus")))

#orders the names of the AOI in terms of description order in the paper
ordered(t2.sp.df.renamed$AOI, levels=c("Question","Answer","Axial","Target","Solution","Navigation","Stimulus"))

# Color blind palette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999")

proportion.time.t2.sp <-  t2.sp.df.renamed %>% filter(Measure=="time") %>%
  #  ggplot(aes(x=AOI, y=Percentage, fill=AOI)) +
  ggplot(aes(x=reorder(AOI, -Percentage), y=Percentage, fill=AOI)) +
  geom_bar(stat="identity") +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50"), 
        axis.title.x = element_blank(), axis.text.x=element_blank(),
        legend.position = "bottom") +
  labs(y="Proportion of Fixation Time") +
  ggtitle("Scatter Plots for Pairs (2D-SP)") +
  scale_y_continuous(limits=c(0, 0.40), breaks=seq(0, 0.40, 0.05)) +
  scale_fill_manual(values=cbPalette)  
proportion.time.t2.sp



################################################################################
# Bar plots for the AOIs proportion of fixation time and fixation count across the ranges

# TODO
# 1. Function that computes a data frame with the average of proportions for each NoF, NoC range
# 2.

# Function that computes the bar plots in descending order by the  
# Ino
computeProportionFixationTime(aoi.data) <- function() {
  
  
}

sanity.checks.data <- experiment.complete.data %>% filter(ParticipantID==2) %>%
  mutate(sumFixationCounts=fixations.Question + fixations.Answer + fixations.Legend + fixations.CTC + 
           fixations.FM + fixations.Navigating + fixations.CTC) %>%
  mutate(correctFixationCounts= totalFixations-sumFixationCounts)
sanity.checks.data %>% select(sumFixationCounts, totalFixations, correctFixationCounts, fixations.Navigating)



sanity.checks.data <- experiment.complete.data %>% filter(ParticipantID==2) %>%
  mutate(sumFixationTimes=time.Question + time.Answer + time.Legend + time.CTC + 
           time.FM + time.Navigating + time.CTC) %>%
  mutate(correctFixationTimes= totalFixationTime-sumFixationTimes)
sanity.checks.data %>% select(sumFixationTimes, totalFixationTime, correctFixationTimes, time.Navigating)

test.accumulation <- experiment.complete.data %>% filter(NoC==1 & NoF==1 & ParticipantID==10 & QNumber==1)

test.accumulation$perc.fixations.Answer + test.accumulation$perc.fixations.Buttons + 
  test.accumulation$perc.fixations.CTC + test.accumulation$perc.fixations.FM +
  test.accumulation$perc.fixations.Legend + test.accumulation$perc.fixations.Question +
  test.accumulation$perc.fixations.Navigating


test.accumulation$perc.time.Answer + test.accumulation$perc.time.Buttons + 
  test.accumulation$perc.time.CTC + test.accumulation$perc.time.FM +
  test.accumulation$perc.time.Legend + test.accumulation$perc.time.Question +
  test.accumulation$perc.time.Navigating




test.accumulation$perc.time.Containing # must not should be included, study how it relates to navigating
# and window in the generation of the data for each participant with the r-code-PN.R scripts

################################################################################
################################################################################
################################################################################

# Test code for the boxplots of ranges

########## -- From SPLC22-Artifact-Evaluation.R, NoC boxplots for correct responses - From A
boxplot.1 <- experiment.complete.data %>% filter(Correct=="2" & NoC==1) %>% 
  ggplot(aes(x=NoF, group=NoF, ElapsedTime/1000)) +
  geom_boxplot(aes(fill=ElapsedTime/1000), varwidth=T, fill="lightblue", alpha=0.8) +
  labs(x="", y="") +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50"),
        axis.text.x=element_blank()) +
  coord_cartesian(ylim = c(0, 120)) +
  scale_y_continuous(breaks=seq(0, 120, 60))
boxplot.1


## -- From SPLC22-Artifact-Evaluation.R, changed colors and added number of tap
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


## Example from t2 and parallel dimensions - From jSS-New-Kambiz-Analysis-Plots.R
bp.t2.pd.res <- experiment.data %>% 
  mutate(Accuracy = case_when(Accuracy == 'True' ~ "Accurate", Accuracy == 'False' ~ "Inaccurate")) %>%
  filter (T==2 & Visualization.Technique=="2D-PD") %>%
  ggplot(aes(x=Accuracy, group=Accuracy,Elapsed.Time/1000)) +  
  geom_boxplot(aes(fill=Elapsed.Time/1000), varwidth=T,  fill=c("#009E73","#0072B2"), alpha=0.5) +  
  geom_jitter() +
  labs(x="Parallel Dimensions Plot", y="T=2") + 
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
  scale_y_continuous(breaks=seq(0, 350, 50),limits = c(0, 350))

bp.t2.pd.res


#################################################################################
#################################################################################
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