# New plots for JSS special issue paper
# Colour blind palette
# https://stackoverflow.com/questions/66748997/how-to-

# JSS Journal Kambiz Paper
# File with all the statistical analysis and figures of the article

library("tidyverse")
library(ggplot2)
library(hrbrthemes)
library(gridExtra)
library(grid)


# Loads the complete experiment data file
experiment.data <- read.csv(file = "../../Eye-Tracking-Visualization/Experiment-Data/Curated-Data/Complete-Experiment-Data.csv", header=TRUE)

# Laptop@Office
# experiment.data <- read.csv(file = "C:/Research/Projects/R-Kambiz-Elmira/Kambiz-Data/Complete-Experiment-Data.csv", header=TRUE)

###############################################################
# General descriptive statistics

## Difficulty assessment 
his.difficulty <- ggplot(experiment.data, aes(x=Difficulty.Level)) + 
  geom_histogram(binwidth=1, stat="count", fill="blue") +
  stat_count(binwidth = 1, geom = 'text', color = 'black', aes(label = ..count..), vjust= -0.4,) +
  scale_x_continuous(breaks=seq(0, 20, 1)) +
  scale_y_continuous(breaks=seq(0, 380, 20)) + 
  xlab("Difficulty Assessment, Very easy..Very difficult") +
  theme_classic()

his.difficulty

experiment.data %>% select(Difficulty.Level) %>% summary()
# Difficulty.Level
# Min.   : 1.000  
# 1st Qu.: 1.000  
# Median : 1.000  
# Mean   : 1.237  
# 3rd Qu.: 1.000  
# Max.   :20.000  

## Certainty assessment
his.certainty <- ggplot(experiment.data, aes(x=Certainty.Assessment)) + 
  geom_histogram(binwidth=1, stat="count", fill="blue") +
  stat_count(binwidth = 1, geom = 'text', color = 'black', aes(label = ..count..), vjust= -0.4,) +
  scale_x_continuous(breaks=seq(0, 20, 1)) +
  scale_y_continuous(breaks=seq(0, 120, 10)) + 
  xlab("Certainty Assessment, Perfect..Failure") +
  theme_classic()

his.certainty

experiment.data %>% select(Certainty.Assessment) %>% summary()
# Certainty.Assessment
# Min.   : 1.00       
# 1st Qu.: 2.00       
# Median : 3.00       
# Mean   : 4.37       
# 3rd Qu.: 5.00       
# Max.   :20.00       

#######################
## Accuracy RQ1

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


# Note: palette.colors(), the default is a color-friendly palette

experiment.data %>% 
  mutate(Accuracy = case_when(Accuracy == "True" ~ "Accurate", Accuracy == "False" ~ "Inacurate"))

# Sanity check, the values are correctly transformed to Accurate and Inaccurate
accuracy.general.revised <- experiment.data  %>%  
  mutate(Accuracy = case_when(Accuracy == "True" ~ "Accurate", Accuracy == "False" ~ "Inacurate")) %>%
  group_by(Accuracy) %>%
  summarise(count=n()) %>% as.data.frame()
accuracy.general.revised

# Revised Figure 6a       
participant.accuracy <- experiment.data %>%   
  mutate(Accuracy = case_when(Accuracy == "True" ~ "Accurate", Accuracy == "False" ~ "Inacurate")) %>%
  ggplot(aes(x=Participant.ID, fill=Accuracy)) + # , alpha=0.5
  coord_cartesian(ylim = c(0, 16)) +
  scale_y_discrete(limits=seq(0, 16, 1)) +
  geom_bar(position="dodge") +
  theme(axis.text.x=element_blank(), panel.background = element_blank(), 
        panel.grid.major.y = element_line(colour = "grey50"), legend.position = "bottom") +
  labs(y="Frequency", x="Participant responses") +
  scale_fill_manual(values=c("#009E73","#0072B2"))
participant.accuracy

# Revised Figure 6b
# Plot per question how many participants correct and incorrect
# Bar chart with correct and incorrect responses for each question
question.accuracy <- experiment.data %>% 
  mutate(Accuracy = case_when(Accuracy == "True" ~ "Accurate", Accuracy == "False" ~ "Inacurate")) %>%
  ggplot(aes(x=Question.Number, fill=Accuracy)) +
  coord_cartesian(ylim = c(0, 24)) +
  scale_y_discrete(limits=seq(0, 24, 1)) +
  scale_x_discrete(limits=seq(1, 16, 1)) +
  geom_bar(position="dodge") +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50"), 
        legend.position = "bottom") + 
  labs(x="Question Number", y="Frequency")  +
  scale_fill_manual(values=c("#009E73","#0072B2"))  
  
question.accuracy

##############################################################################################################################
# Figure 7 revised. Change of colors and substitution of FALSE and TRUE

# Plot grid of bars of Correct and Incorrect responses, y dimension t values, y dimensions 
accuracy.df <- data.frame(matrix(nrow = 4 * 2, ncol = 4))
columnNames <- c("T", "Visualization.Technique", "Accuracy", "Number")
colnames(accuracy.df) <- columnNames

# Bar for t=2 and Scattered Plot
bars.t2.sp.data <- experiment.data  %>% filter (T==2 & Visualization.Technique=="2D-SP") %>% select(Accuracy) %>% 
  group_by(Accuracy) %>% summarise(n=n())

k <- 1
accuracy.df$T[k] <- 2
accuracy.df$Visualization.Technique[k] <- "2D-SP"
accuracy.df$Accuracy[k] <- TRUE
accuracy.df$Number[k] <- as.numeric(bars.t2.sp.data[2,2]) # number of TRUE values

k <- k + 1
accuracy.df$T[k] <- 2
accuracy.df$Visualization.Technique[k] <- "2D-SP"
accuracy.df$Accuracy[k] <- FALSE
accuracy.df$Number[k] <- as.numeric(bars.t2.sp.data[1,2]) # number of FALSE values

# Bar for t=2 and Parallel Dimensions
bars.t2.pd.data <- experiment.data  %>% filter (T==2 & Visualization.Technique=="2D-PD") %>% select(Accuracy) %>% 
  group_by(Accuracy) %>% summarise(n=n())

k <- k + 1
accuracy.df$T[k] <- 2
accuracy.df$Visualization.Technique[k] <- "2D-PD"
accuracy.df$Accuracy[k] <- TRUE
accuracy.df$Number[k] <- as.numeric(bars.t2.pd.data[2,2]) # number of TRUE values

k <- k + 1
accuracy.df$T[k] <- 2
accuracy.df$Visualization.Technique[k] <- "2D-PD"
accuracy.df$Accuracy[k] <- FALSE
accuracy.df$Number[k] <- as.numeric(bars.t2.pd.data[1,2]) # number of FALSE values

# Bar for t=3 and Scattered Plot
bars.t3.sp.data <- experiment.data  %>% filter (T==3 & Visualization.Technique=="3D-SP") %>% select(Accuracy) %>% 
  group_by(Accuracy) %>% summarise(n=n())

k <- k + 1
accuracy.df$T[k] <- 3
accuracy.df$Visualization.Technique[k] <- "3D-SP"
accuracy.df$Accuracy[k] <- TRUE
accuracy.df$Number[k] <- as.numeric(bars.t3.sp.data[2,2]) # number of TRUE values

k <- k + 1
accuracy.df$T[k] <- 3
accuracy.df$Visualization.Technique[k] <- "3D-SP"
accuracy.df$Accuracy[k] <- FALSE
accuracy.df$Number[k] <- as.numeric(bars.t3.sp.data[1,2]) # number of FALSE values

# Bar for t=3 and Parallel Dimensions
bars.t3.pd.data <- experiment.data  %>% filter (T==3 & Visualization.Technique=="3D-PD") %>% select(Accuracy) %>% 
  group_by(Accuracy) %>% summarise(n=n())

k <- k + 1
accuracy.df$T[k] <- 3
accuracy.df$Visualization.Technique[k] <- "3D-PD"
accuracy.df$Accuracy[k] <- TRUE
accuracy.df$Number[k] <- as.numeric(bars.t3.pd.data[2,2]) # number of TRUE values

k <- k + 1
accuracy.df$T[k] <- 3
accuracy.df$Visualization.Technique[k] <- "3D-PD"
accuracy.df$Accuracy[k] <- FALSE
accuracy.df$Number[k] <- as.numeric(bars.t3.pd.data[1,2]) # number of FALSE values


# Mutating the values of FALSE and TRUE for Inaccurate and Accurate
accuracy.df.revised <- accuracy.df %>% mutate(Accuracy = case_when(Accuracy == TRUE ~ "Accurate", Accuracy == FALSE ~ "Inacurate"))

# Bar t=2 2D-SP
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

# Bar t=2 2D-PD
bars.t2.pd <- accuracy.df.revised %>% filter (T==2 & Visualization.Technique=="2D-PD") %>% 
  ggplot(aes(x=Accuracy, weight = Number)) + 
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_discrete(limits=seq(0, 100, 10)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, size = 5) + 
  scale_fill_manual(values=c("#009E73","#0072B2")) +
  labs(x="Parallel Dimensions Plot",y="T=2") +
  guides(fill = FALSE, alpha=FALSE) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 
bars.t2.pd


# Bar t=3 3D-SP
bars.t3.sp <- accuracy.df.revised %>% filter (T==3 & Visualization.Technique=="3D-SP") %>% 
  ggplot(aes(x=Accuracy, weight = Number)) + 
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_discrete(limits=seq(0, 100, 10)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, size = 5) + 
  scale_fill_manual(values=c("#009E73","#0072B2")) +
  labs(x="Scatter Plot",y="T=3") +
  guides(fill = FALSE, alpha=FALSE) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 
bars.t3.sp


# Bar t=3 2D-PD
bars.t3.pd <- accuracy.df.revised %>% filter (T==3 & Visualization.Technique=="3D-PD") %>% 
  ggplot(aes(x=Accuracy, weight = Number)) + 
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_discrete(limits=seq(0, 100, 10)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, size = 5) + 
  scale_fill_manual(values=c("#009E73","#0072B2")) +
  labs(x="Parallel Dimensions Plot",y="T=3") +
  guides(fill = FALSE, alpha=FALSE) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 
bars.t3.pd

# Creates the grid for the stack bars of accuracy results
grid.data <- grid.arrange(bars.t3.sp,bars.t3.pd,
                          bars.t2.sp,bars.t2.pd,
                          ncol=2, nrow=2,
                          bottom = textGrob("Visualization Techniques",gp=gpar(fontsize=15,font=3)),
                          left = textGrob("Covering Array Strength", rot=90, gp=gpar(fontsize=15,font=3)))


################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################

## Scratch code


# # Previous model - with flip coords
# # Plot for accuracy by participant responses 
# # palette.colors()
# participant.accuracy <- experiment.data %>% 
#   ggplot(aes(x=Participant.ID, fill=Accuracy)) + # , alpha=0.5
#   geom_bar() +
#   theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), panel.background = element_blank()) +
#   labs(y="Frequency", x="Accurate and Inaccurate participant responses") +
# #  labs(x="Participants' responses", y="Number of Accurate (True) and Inaccurate (False) responses") +
# #  coord_flip() +
#   scale_y_continuous(breaks=seq(1, 24, 1))  +
#   scale_fill_manual(values=c("#0072B2", "#009E73"))
# # scale_fill_brewer(palette = "Set1")
# participant.accuracy
# 
# # Plot for accuracy by participant responses 
# participant.accuracy <- experiment.data %>% 
#   ggplot(aes(x=Participant.ID, fill=Accuracy)) + # , alpha=0.5
#   # geom_bar(position ="dodge") +
#   geom_bar() +
#   theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), panel.background = element_blank()) +
#   #  theme_minimal () +
#   #  theme_minimal (axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
#   labs(x="Participants' responses", y="Number of Accurate (True) and Inaccurate (False) responses") +
#   coord_flip() +
#   scale_y_continuous(breaks=seq(1, 24, 1)) +
#   scale_fill_manual(values=c("red", "green"))
# # scale_fill_brewer(palette = "Set1")
# participant.accuracy


  # axis.text.x=element_blank(), 
  #scale_x_continuous(breaks=seq(1, 16, 1))  +
  
  # axis.text.x=element_blank(), 
  #scale_x_continuous(breaks=seq(1, 16, 1))  +
  
  # geom_bar(position ="dodge") +
  # geom_bar() +
  # theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) + # theme_minimal
  # scale_y_continuous(breaks=seq(1, 24, 1))  +
  #scale_fill_manual(values=c("#7463AC","gray80"))  
  #scale_colour_brewer(palette = "Set2")
  # scale_fill_brewer(palette = "Set1") # Accent, Set1, Pastel1
  # scale_fill_manual(values=c("red", "green"))
  # question.accuracy



