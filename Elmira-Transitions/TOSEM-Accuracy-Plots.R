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
response.time.data$Accuracy = as.factor(accuracy.data$Accuracy)


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