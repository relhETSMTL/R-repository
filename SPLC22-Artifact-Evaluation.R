# SPLC22 - Paper 32 
# Artifact Evaluation 
# R Script


library(ggplot2)
library(tidyverse)
library(hrbrthemes)

# install.packages("treemapify")
library(treemapify)




####################################################################################################
# Reads the participants data
experiment.complete.data <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/ExperimentCompleteDataSet.csv", 
                                     header=TRUE)
attach(experiment.complete.data)



##############################################################################################
##############################################################################################
##############################################################################################
# Figure 3
# Correct and Incorrect Responses per NoF and NoC

# responses.data <- read.csv(file = "../../Experiment-Data/All-Participants-Curated-Data-Boolean.csv", 
#                           header=TRUE)
# attach(responses.data)

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



############### Creating the bars

##########
## NoC = 1
bars.1.1 <- correct.incorrect.df %>% filter (NoF==1 & NoC==1) %>%
  ggplot(aes(x=Accuracy, weight = Number)) + 
  coord_cartesian(ylim = c(0, 30)) +
  scale_y_discrete(limits=seq(0, 30, 5)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  scale_fill_manual(values=c("green", "red")) + 
  labs(x="",y="") +
  guides(fill = FALSE, alpha=FALSE) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 


bars.2.1 <- correct.incorrect.df %>% filter (NoF==2 & NoC==1) %>%
  ggplot(aes(x=Accuracy, weight = Number)) +
  coord_cartesian(ylim = c(0, 30)) +
  scale_y_discrete(limits=seq(0, 30, 5)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  scale_fill_manual(values=c("green", "red")) +
  labs(x="",y="") +
  guides(fill = FALSE, alpha=FALSE) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 


bars.3.1 <- correct.incorrect.df %>% filter (NoF==3 & NoC==1) %>%
  ggplot(aes(x=Accuracy, weight = Number)) +
  coord_cartesian(ylim = c(0, 30)) +
  scale_y_discrete(limits=seq(0, 30, 5)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  scale_fill_manual(values=c("green", "red")) +
  labs(x="",y="") +
  guides(fill = FALSE, alpha=FALSE) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 


bars.4.1 <- correct.incorrect.df %>% filter (NoF==4 & NoC==1) %>%
  ggplot(aes(x=Accuracy, weight = Number)) + 
  coord_cartesian(ylim = c(0, 30)) +
  scale_y_discrete(limits=seq(0, 30, 5)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  scale_fill_manual(values=c("green", "red")) +
  labs(x="",y="") +
  guides(fill = FALSE, alpha=FALSE) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 

### NoC = 2
bars.1.2 <- correct.incorrect.df %>% filter (NoF==1 & NoC==2) %>%
  ggplot(aes(x=Accuracy, weight = Number)) + 
  coord_cartesian(ylim = c(0, 30)) +
  scale_y_discrete(limits=seq(0, 30, 5)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  scale_fill_manual(values=c("green", "red")) + 
  labs(x="",y="") +
  guides(fill = FALSE, alpha=FALSE) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 
#  theme_minimal()

bars.2.2 <- correct.incorrect.df %>% filter (NoF==2 & NoC==2) %>%
  ggplot(aes(x=Accuracy, weight = Number)) +
  coord_cartesian(ylim = c(0, 30)) +
  scale_y_discrete(limits=seq(0, 30, 5)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  scale_fill_manual(values=c("green", "red")) +
  labs(x="",y="") +
  guides(fill = FALSE, alpha=FALSE) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 


bars.3.2 <- correct.incorrect.df %>% filter (NoF==3 & NoC==2) %>%
  ggplot(aes(x=Accuracy, weight = Number)) +
  coord_cartesian(ylim = c(0, 30)) +
  scale_y_discrete(limits=seq(0, 30, 5)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  scale_fill_manual(values=c("green", "red")) +
  labs(x="",y="") +
  guides(fill = FALSE, alpha=FALSE) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 


bars.4.2 <- correct.incorrect.df %>% filter (NoF==4 & NoC==2) %>%
  ggplot(aes(x=Accuracy, weight = Number)) + 
  coord_cartesian(ylim = c(0, 30)) +
  scale_y_discrete(limits=seq(0, 30, 5)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  scale_fill_manual(values=c("green", "red")) +
  labs(x="",y="") +
  guides(fill = FALSE, alpha=FALSE) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 


### NoC = 3
bars.1.3 <- correct.incorrect.df %>% filter (NoF==1 & NoC==3) %>%
  ggplot(aes(x=Accuracy, weight = Number)) + 
  coord_cartesian(ylim = c(0, 30)) +
  scale_y_discrete(limits=seq(0, 30, 5)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  scale_fill_manual(values=c("green", "red")) + 
  labs(x="",y="") +
  guides(fill = FALSE, alpha=FALSE) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 

bars.2.3 <- correct.incorrect.df %>% filter (NoF==2 & NoC==3) %>%
  ggplot(aes(x=Accuracy, weight = Number)) +
  coord_cartesian(ylim = c(0, 30)) +
  scale_y_discrete(limits=seq(0, 30, 5)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  scale_fill_manual(values=c("green", "red")) +
  labs(x="",y="") +
  guides(fill = FALSE, alpha=FALSE) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 


bars.3.3 <- correct.incorrect.df %>% filter (NoF==3 & NoC==3) %>%
  ggplot(aes(x=Accuracy, weight = Number)) +
  coord_cartesian(ylim = c(0, 30)) +
  scale_y_discrete(limits=seq(0, 30, 5)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  scale_fill_manual(values=c("green", "red")) +
  labs(x="",y="") +
  guides(fill = FALSE, alpha=FALSE) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 


bars.4.3 <- correct.incorrect.df %>% filter (NoF==4 & NoC==3) %>%
  ggplot(aes(x=Accuracy, weight = Number)) + 
  coord_cartesian(ylim = c(0, 30)) +
  scale_y_discrete(limits=seq(0, 30, 5)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  scale_fill_manual(values=c("green", "red")) +
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



##############################################################################################
##############################################################################################
##############################################################################################
# Figure 4
# Response time in seconds for Correct Answers per NoF and NoC ranges


library(ggplot2)
library(tidyverse)
library(hrbrthemes)

# File reads the experiment data 
curatedParticipantsData <- experiment.complete.data



# NoC=1 
boxplot.1 <- curatedParticipantsData %>% filter(Correct=="2" & NoC==1) %>% 
  ggplot(aes(x=NoF, group=NoF, ElapsedTime/1000)) +
  geom_boxplot(aes(fill=ElapsedTime/1000), varwidth=T, fill="lightblue", alpha=0.8) +
  labs(x="", y="") +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50"),
        axis.text.x=element_blank()) +
  coord_cartesian(ylim = c(0, 120)) +
  scale_y_continuous(breaks=seq(0, 120, 60))
boxplot.1


# NoC=2 
boxplot.2 <- curatedParticipantsData %>% filter(Correct=="2" & NoC==2) %>% 
  ggplot(aes(x=NoF, group=NoF, ElapsedTime/1000)) +
  geom_boxplot(aes(fill=ElapsedTime/1000), varwidth=T, fill="lightblue", alpha=0.8) +
  labs(x="", y="") +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50"),
        axis.text.x=element_blank()) +
  coord_cartesian(ylim = c(0, 120)) +
  scale_y_continuous(breaks=seq(0, 120, 60))
boxplot.2


# NoC=3 
boxplot.3 <- curatedParticipantsData %>% filter(Correct=="2" & NoC==3) %>% 
  ggplot(aes(x=NoF, group=NoF, ElapsedTime/1000)) +
  geom_boxplot(aes(fill=ElapsedTime/1000), varwidth=T, fill="lightblue", alpha=0.8) +
  labs(x="", y="") +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50"),
        axis.text.x=element_blank()) +
  coord_cartesian(ylim = c(0, 120)) +
  scale_y_continuous(breaks=seq(0, 120, 60))
boxplot.3


grid.data <- grid.arrange(boxplot.3, boxplot.2, boxplot.1,
                          nrow=3,
                          bottom = textGrob("NoF Ranges (1)..(4)",gp=gpar(fontsize=15,font=3)),
                          left = textGrob("NoC Ranges (1)..(3)", rot=90, gp=gpar(fontsize=15,font=3)))



##############################################################################################
##############################################################################################
##############################################################################################
# Figure 5
# Average Proportions of Fixation Time and Count

data.CTC    <- experiment.complete.data %>% filter (fixations.CTC!=0.0)
data.noCTC  <- experiment.complete.data %>% filter (fixations.CTC==0.0)

ctc.avg.perc.fixations.Answer <- mean (data.CTC$perc.fixations.Answer)
ctc.avg.perc.fixations.Buttons <- mean (data.CTC$perc.fixations.Buttons)
ctc.avg.perc.fixations.CTC <- mean (data.CTC$perc.fixations.CTC)
ctc.avg.perc.fixations.FM <- mean (data.CTC$perc.fixations.FM)
ctc.avg.perc.fixations.Legend <- mean (data.CTC$perc.fixations.Legend)
ctc.avg.perc.fixations.Question <- mean (data.CTC$perc.fixations.Question)

noctc.avg.perc.fixations.Answer <- mean (data.noCTC$perc.fixations.Answer)
noctc.avg.perc.fixations.Buttons <- mean (data.noCTC$perc.fixations.Buttons)
noctc.avg.perc.fixations.CTC <- mean (data.noCTC$perc.fixations.CTC)
noctc.avg.perc.fixations.FM <- mean (data.noCTC$perc.fixations.FM)
noctc.avg.perc.fixations.Legend <- mean (data.noCTC$perc.fixations.Legend)
noctc.avg.perc.fixations.Question <- mean (data.noCTC$perc.fixations.Question)

ctc.avg.perc.time.Answer <- mean (data.CTC$perc.time.Answer)
ctc.avg.perc.time.Buttons <- mean (data.CTC$perc.time.Buttons)
ctc.avg.perc.time.CTC <- mean (data.CTC$perc.time.CTC)
ctc.avg.perc.time.FM <- mean (data.CTC$perc.time.FM)
ctc.avg.perc.time.Legend <- mean (data.CTC$perc.time.Legend)
ctc.avg.perc.time.Question <- mean (data.CTC$perc.time.Question)

noctc.avg.perc.time.Answer <- mean (data.noCTC$perc.time.Answer)
noctc.avg.perc.time.Buttons <- mean (data.noCTC$perc.time.Buttons)
noctc.avg.perc.time.CTC <- mean (data.noCTC$perc.time.CTC)
noctc.avg.perc.time.FM <- mean (data.noCTC$perc.time.FM)
noctc.avg.perc.time.Legend <- mean (data.noCTC$perc.time.Legend)
noctc.avg.perc.time.Question <- mean (data.noCTC$perc.time.Question)


# Creates the data frame with the information, 4 NoF, 3 NoC, correct, incorrect, Number
ctc.df <- data.frame(matrix(nrow = 2 * 6, ncol = 3))
columnNamesCTC <- c("Measure", "AOI", "Percentage")
colnames(ctc.df) <- columnNamesCTC

ctc.df[1,1] <- 'count'
ctc.df[1,2] <- 'Answer'
ctc.df[1,3] <- ctc.avg.perc.fixations.Answer

ctc.df[2,1] <- 'count'
ctc.df[2,2] <- 'Buttons'
ctc.df[2,3] <- ctc.avg.perc.fixations.Buttons

ctc.df[3,1] <- 'count'
ctc.df[3,2] <- 'CTC'
ctc.df[3,3] <- ctc.avg.perc.fixations.CTC


ctc.df[4,1] <- 'count'
ctc.df[4,2] <- 'FM'
ctc.df[4,3] <-  ctc.avg.perc.fixations.FM 

ctc.df[5,1] <- 'count'
ctc.df[5,2] <- 'Legend'
ctc.df[5,3] <-  ctc.avg.perc.fixations.Legend  


ctc.df[6,1] <- 'count'
ctc.df[6,2] <- 'Question'
ctc.df[6,3] <- ctc.avg.perc.fixations.Question  

ctc.df[7,1] <- 'time'
ctc.df[7,2] <- 'Answer'
ctc.df[7,3] <- ctc.avg.perc.time.Answer  

ctc.df[8,1] <- 'time'
ctc.df[8,2] <- 'Buttons'
ctc.df[8,3] <- ctc.avg.perc.time.Buttons

ctc.df[9,1] <- 'time'
ctc.df[9,2] <- 'CTC'
ctc.df[9,3] <- ctc.avg.perc.time.CTC


ctc.df[10,1] <- 'time'
ctc.df[10,2] <- 'FM'
ctc.df[10,3] <-  ctc.avg.perc.time.FM 

ctc.df[11,1] <- 'time'
ctc.df[11,2] <- 'Legend'
ctc.df[11,3] <-  ctc.avg.perc.time.Legend  


ctc.df[12,1] <- 'time'
ctc.df[12,2] <- 'Question'
ctc.df[12,3] <- ctc.avg.perc.time.Question  

# Changes the order to 
ctc.df$Measure <- factor(ctc.df$Measure, levels = c('time', 'count'))
ctc.plot <- ctc.df %>% ggplot(aes(fill=AOI, y=Percentage, x=Measure)) + 
  geom_bar(position="fill", stat="identity", alpha=0.7) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
  labs(x="With CTCs", y="") 

# Computing the data for without CTCs

# Creates the data frame with the information, 4 NoF, 3 NoC, correct, incorrect, Number
noctc.df <- data.frame(matrix(nrow = 2 * 6, ncol = 3))
columnNamesCTC <- c("Measure", "AOI", "Percentage")
colnames(noctc.df) <- columnNamesCTC

noctc.df[1,1] <- 'count'
noctc.df[1,2] <- 'Answer'
noctc.df[1,3] <- noctc.avg.perc.fixations.Answer

noctc.df[2,1] <- 'count'
noctc.df[2,2] <- 'Buttons'
noctc.df[2,3] <- noctc.avg.perc.fixations.Buttons

noctc.df[3,1] <- 'count'
noctc.df[3,2] <- 'CTC'
noctc.df[3,3] <- noctc.avg.perc.fixations.CTC


noctc.df[4,1] <- 'count'
noctc.df[4,2] <- 'FM'
noctc.df[4,3] <-  noctc.avg.perc.fixations.FM 

noctc.df[5,1] <- 'count'
noctc.df[5,2] <- 'Legend'
noctc.df[5,3] <-  noctc.avg.perc.fixations.Legend  


noctc.df[6,1] <- 'count'
noctc.df[6,2] <- 'Question'
noctc.df[6,3] <- noctc.avg.perc.fixations.Question  

noctc.df[7,1] <- 'time'
noctc.df[7,2] <- 'Answer'
noctc.df[7,3] <- noctc.avg.perc.time.Answer  

noctc.df[8,1] <- 'time'
noctc.df[8,2] <- 'Buttons'
noctc.df[8,3] <- noctc.avg.perc.time.Buttons

noctc.df[9,1] <- 'time'
noctc.df[9,2] <- 'CTC'
noctc.df[9,3] <- noctc.avg.perc.time.CTC


noctc.df[10,1] <- 'time'
noctc.df[10,2] <- 'FM'
noctc.df[10,3] <-  noctc.avg.perc.time.FM 

noctc.df[11,1] <- 'time'
noctc.df[11,2] <- 'Legend'
noctc.df[11,3] <-  noctc.avg.perc.time.Legend  


noctc.df[12,1] <- 'time'
noctc.df[12,2] <- 'Question'
noctc.df[12,3] <- noctc.avg.perc.time.Question  

noctc.df$Measure <- factor(ctc.df$Measure, levels = c('time', 'count'))

noctc.plot <- noctc.df %>% ggplot(aes(fill=AOI, y=Percentage, x=Measure)) + 
  geom_bar(position="fill", stat="identity", alpha=0.7) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
  labs(x="No CTCs", y="") 



# Test of merging the plots into a single figure
# https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots
library(cowplot)
prow <- plot_grid(ctc.plot + theme(legend.position="none"),
                  noctc.plot + theme(legend.position="none"),
                  align = 'vh',
                  # labels = c("CTC", "no CTC"),
                  hjust = -1,
                  nrow = 1)

legend_b <- get_legend(ctc.plot + theme(legend.position="bottom"))
fixation.plot <- plot_grid( prow, legend_b, ncol = 1, rel_heights = c(1, .2))
fixation.plot
