# SPLC22 Data processing code

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

# Note: False = 1, True = 2 in the values for correct response or not



####################################################################################################
### Computes a bubble plot with correct and incorrect answers for Correct and Incorrect responses
# for each NoF and NoC ranges

# Reads the file with the responses from the Java interface
responses.data <- read.csv(file = "../../Experiment-Data/All-Participants-Curated-Data-Boolean.csv", 
                                     header=TRUE)
attach(responses.data)

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

# Creates the bubble plot with the correct and incorrect bubbles colored by type
# Did not work very well
correct.incorrect.df  %>% ggplot(aes(x=NoF, y=NoC, size=Number, color=Accuracy)) +
  geom_point(alpha=0.5) +
  labs(x="NoF ranges", y="NoC ranges") +
  theme_minimal() +
 # scale_fill_brewer(palette = "Set1") +
  scale_x_discrete(limits=seq(1, 4, 1)) +
  scale_y_discrete(limits=seq(1, 3, 1)) +
  scale_fill_manual(values=c("red", "blue"))


############### Creating the bars
# Trying multiple grids with two bars 

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
#  theme_minimal()

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


# correct.incorrect.df %>% filter (NoF==4 & NoC==1) %>%
#   ggplot(aes(x=Accuracy, weight = Number)) +
#   coord_cartesian(ylim = c(0, 30)) +
#   scale_y_discrete(limits=seq(0, 30, 5)) +
#   geom_bar(aes(fill=Accuracy, alpha=0.5)) +
#   scale_fill_manual(values=c("green", "red")) +
#   labs(x="",y="") +
#   guides(fill = FALSE, alpha=FALSE) +
#   theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 
# 

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
#  theme_minimal()

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


##################

library(gridExtra)
library(grid)

# Creates the grid for the tables
grid.data <- grid.arrange(bars.1.3, bars.2.3, bars.3.3, bars.4.3,
              bars.1.1, bars.2.1, bars.3.1, bars.4.1,
             bars.1.2, bars.2.2, bars.3.1, bars.4.2,
             ncol=4, nrow=3,
             bottom = textGrob("NoF Ranges (1)..(4)",gp=gpar(fontsize=15,font=3)),
             left = textGrob("NoC Ranges (1)..(3)", rot=90, gp=gpar(fontsize=15,font=3)))



grid.data <- grid.arrange(
  bars.1.3, bars.2.3, bars.3.3, bars.4.3,
  bars.1.2, bars.2.2, bars.3.2, bars.4.2,
  bars.1.1, bars.2.1, bars.3.1, bars.4.1,
                        ncol=4, nrow=3,
                          bottom = textGrob("NoF Ranges (1)..(4)",gp=gpar(fontsize=15,font=3)),
                          left = textGrob("NoC Ranges (1)..(3)", rot=90, gp=gpar(fontsize=15,font=3)))


##################################################################################################


library(ggplot2)
library(tidyverse)
library(hrbrthemes)

# File reads the experiment data 
curatedParticipantsData <- read.csv(file = "../../Experiment-Data/All-Participants-Curated-Data-Boolean.csv", header=TRUE)
attach (curatedParticipantsData)

# Response time of correct answers, plot with all the numbers
participantCorrectResponseTime <- curatedParticipantsData %>% filter(Correct=="True") %>% 
  ggplot(aes(x=QNumber, group=QNumber, ElapsedTime/1000)) +
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



# NoC=1 
boxplot.1 <- curatedParticipantsData %>% filter(Correct=="True" & NoC==1) %>% 
  ggplot(aes(x=NoF, group=NoF, ElapsedTime/1000)) +
  geom_boxplot(aes(fill=ElapsedTime/1000), varwidth=T, fill="lightblue", alpha=0.8) +
  labs(x="", y="") +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50"),
        axis.text.x=element_blank()) +
  coord_cartesian(ylim = c(0, 120)) +
  scale_y_continuous(breaks=seq(0, 120, 60))
boxplot.1


# NoC=2 
boxplot.2 <- curatedParticipantsData %>% filter(Correct=="True" & NoC==2) %>% 
  ggplot(aes(x=NoF, group=NoF, ElapsedTime/1000)) +
  geom_boxplot(aes(fill=ElapsedTime/1000), varwidth=T, fill="lightblue", alpha=0.8) +
  labs(x="", y="") +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50"),
        axis.text.x=element_blank()) +
  coord_cartesian(ylim = c(0, 120)) +
  scale_y_continuous(breaks=seq(0, 120, 60))
boxplot.2


# NoC=3 
boxplot.3 <- curatedParticipantsData %>% filter(Correct=="True" & NoC==3) %>% 
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


####################################################################################################

# Questions with CTC

data.CTC    <- experiment.complete.data %>% filter (fixations.CTC!=0.0)
data.noCTC  <- experiment.complete.data %>% filter (fixations.CTC==0.0)

avg.perc.fixations.Answer <- mean (experiment.complete.data$perc.fixations.Answer)


####################################################################################################
####################################################################################################
# Tree map example
# https://r-charts.com/part-whole/treemapify/

# install.packages("ggplot2")
library(ggplot2)

group <- paste("Group", 1:9)
subgroup <- c("A", "C", "B", "A", "A",
              "C", "C", "B", "B")
value <- c(7, 25, 50, 5, 16,
           18, 30, 12, 41)

df <- data.frame(group, subgroup, value) 



ggplot(df, aes(area = value, fill = group, label = value)) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15)



####################################################################################################
####################################################################################################

# Tree map of average percentage of number of fixations over AOIs number of fixation

