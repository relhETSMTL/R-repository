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


#######
## Figures for the paper

# Accuracy, number of papers and visualization methods
t2.acc.vm.pairs <- allt.data %>% filter (T==2 & Accuracy=="True") 

library(ggplot2)

ggplot(t2.acc.vm.pairs, aes(x = Number.Elements, y = Visualization.Method )) +  # , size = size
  # geom_point()
  geom_count()


# Shows the number of pars as factors for the ones that responded correctly Accuracy=="True"
ggplot(t2.acc.vm.pairs) + 
  geom_count(mapping = aes(x = as.factor(Number.Elements), y = Visualization.Method))


# TO DO, show the numbers in the bubbles and change color
ggplot(t2.acc.vm.pairs) + 
 geom_count(mapping = aes(x = as.factor(Number.Elements), y = Visualization.Method)) + 
  scale_size_area()



a <- c(1,1,3,4,5,5,1,1,2,3,4,1,3,2,1,1,5,1,4,3,2,3,1,0,2)
b <- c(1,2,3,5,5,5,2,1,1,3,4,3,3,4,1,1,4,1,4,2,2,3,0,0,1)

#I count the occurence of each couple of values. Eg : number of time a=1 and b=1, number of time a=1 and b=2 etc...
AA <- xyTable(a,b)

#Now I can plot this ! I represent the dots as big as the couple occurs often
coeff_bigger <- 2
plot(AA$x , AA$y , cex=AA$number*coeff_bigger  , pch=16 , col=rgb(0,0,1,0.5) , xlab= "value of a" , ylab="value of b" , xlim=c(0,6) , ylim=c(0,6) )
text(AA$x , AA$y , AA$number )


# Another example of graph

data <- data.frame(x = c(2, 2, 2, 2, 3, 3, 3, 3, 3, 4),
                   y = c(1, 2, 2, 2, 2, 2, 3, 3, 3, 3),
                   id = c("a", "b", "b", "b", "c", 
                          "c", "d", "d", "d", "e"))
data2 <- data %>% 
  group_by(id) %>%
  summarise(x = mean(x), y = mean(y), count = n())


# Testing grouping by visualization method, number of elements, and accuracy with summarizing by counting
t2.vm.ne.acc <- allt.data %>% filter (T==2) %>% select(Visualization.Method,Number.Elements,Accuracy) %>%
  group_by(Visualization.Method,Number.Elements,Accuracy) %>%
  summarise(count=n()) %>% as.data.frame() %>% 
  # Completing the table with the values that are zero
  add_row(Visualization.Method="2D-PD", Number.Elements=136, Accuracy="False", count=0) %>%
  add_row(Visualization.Method="2D-SP", Number.Elements=209, Accuracy="True", count=0) %>%
  add_row(Visualization.Method="2D-SP", Number.Elements=440, Accuracy="False", count=0)
  


ggplot(t2.vm.ne.acc, aes(fill=Accuracy, y=count, x=as.factor(Number.Elements))) + 
         geom_bar(position="dodge", stat="identity")


x.values <- as.factor(t2.vm.ne.acc$Number.Elements)
t2.vm.ne.acc %>% filter (Visualization.Method=="2D-SP") %>%
  ggplot(aes(fill=Accuracy, alpha=0.5, y=count, x=as.factor(Number.Elements))) + 
  #theme(panel.background = element_blank()) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) + # theme_minimal
  scale_fill_manual(values=c("red", "green")) +
  guides(alpha=FALSE) +
  scale_y_continuous(breaks=seq(0, 24, 1)) +
  labs(x="Number of Pairs",y="2D-Scatter Plot") +
  geom_bar(position="dodge", stat="identity")



t2.vm.ne.acc %>% filter (Visualization.Method=="2D-PD") %>%
  ggplot(aes(fill=Accuracy, alpha=0.5, y=count, x=as.factor(Number.Elements))) + 
  #theme(panel.background = element_blank()) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) + # theme_minimal
  scale_fill_manual(values=c("red", "green")) +
  guides(alpha=FALSE) +
  scale_y_continuous(breaks=seq(0, 24, 1)) +
  labs(x="Number of Pairs",y="2D-Parallel Dimensions") +
  geom_bar(position="dodge", stat="identity")


#######################################################################
#######################################################################
#######################################################################
# Using facets, horizontal by visualization method for t=2

ggplot(t2.vm.ne.acc, aes(fill = Accuracy, alpha=0.5, y=count, x=as.factor(Number.Elements))) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_text(aes(label = count),  hjust= -0.4, position = position_dodge(1), size = 3.5) +   
  # hjust = -0.3, vjust= -0.5,
  # vjust = -0.2 , hjust = -0.1
  # scale_fill_viridis(discrete = T, option = "E") +
  # scale_x_discrete(labels=setNames(data$condition, data$cond2)) +
  ggtitle("Accuracy results by Visualization Method and Number of Pairs") +
  facet_grid(Visualization.Method ~ . , scales = "free", space = "free") +  # vertical facets
  theme(legend.position="none") +
  scale_fill_manual(values=c("red", "green")) +
  xlab("Number of Pairs") +
  ylab("Count") + 
  guides(alpha=FALSE) +
  coord_flip() +
  theme(strip.text.x = element_text(angle = 0)) +
  scale_y_continuous(breaks=seq(0, 24, 1)) +
  theme_classic() -> plot.vm.t2.accuracy


# Using facets, horizontal by visualization method for t=3
t3.vm.ne.acc <- allt.data %>% filter (T==3) %>% select(Visualization.Method,Number.Elements,Accuracy) %>%
  group_by(Visualization.Method,Number.Elements,Accuracy) %>%
  summarise(count=n()) %>% as.data.frame() %>% 
  # Completing the table with the values that are zero
  add_row(Visualization.Method="3D-SP", Number.Elements=2491, Accuracy="False", count=0)


ggplot(t3.vm.ne.acc, aes(fill = Accuracy, alpha=0.5, y=count, x=as.factor(Number.Elements))) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_text(aes(label = count),  hjust= -0.4, position = position_dodge(1), size = 3.5) +   
  ggtitle("Accuracy results by Visualization Method and Number of Triplets") +
  facet_grid(Visualization.Method ~ . , scales = "free", space = "free") +  # vertical facets
  theme(legend.position="none") +
  scale_fill_manual(values=c("red", "green")) +
  xlab("Number of Triplets") +
  ylab("Count") + 
  guides(alpha=FALSE) +
  coord_flip() +
  theme(strip.text.x = element_text(angle = 0)) +
  scale_y_continuous(breaks=seq(0, 24, 1)) +
  theme_classic() -> plot.vm.t3.accuracy


#######################################################################
#######################################################################
#######################################################################


#######################################################################
#######################################################################
#######################################################################


# Response time for t=2 and number of pairs and methods
t2.vm.ne.et <- allt.data %>% filter (T==2 & Accuracy="True") %>% 
  select(Visualization.Method,Number.Elements,Elapsed.Time)


## All correct except the label of the legend
ggplot(t2.vm.ne.et, aes(x = as.factor(Number.Elements), y = Elapsed.Time/1000, 
       shape = Visualization.Method, colour = Visualization.Method)) +  # , size=0.01, 
  geom_point(size=2) + 
  # guides(fill = guide_legend(title = "Visn Method", title.position = "left")) +
  theme(panel.background = element_blank(),panel.grid.major.y = element_line(colour = "grey50")) +
  scale_colour_brewer(palette = "Set1") +
  scale_shape_manual(values = c(1,2)) +
  xlab("Number of Pairs") +
  ylab("Elapsed Time in Seconds") + 
  # ylim(0, 220) +
  scale_y_continuous(breaks=seq(0, 220, 20)) + 
  # geom_point(shape = 21) +
  # scale_color_manual(name='Visualization Method') + 
  geom_jitter(width = 0.25)


# Plot with test for labeling legend
ggplot(t2.vm.ne.et, aes(x = as.factor(Number.Elements), y = Elapsed.Time/1000, 
                        shape = Visualization.Method, colour = Visualization.Method)) +  # , size=0.01, 
  geom_point(size=2) + 
  theme(panel.background = element_blank(),panel.grid.major.y = element_line(colour = "grey50"),
        legend.position = "bottom") +
  scale_colour_brewer(palette = "Set1") +
  scale_shape_manual(values = c(1,2)) +
  xlab("Number of Pairs") +
  ylab("Elapsed Time in Seconds") + 
  labs(colour = "Visualization Method", shape="Visualization Method") +
  scale_y_continuous(breaks=seq(0, 220, 20)) + 
  # guides(colour = FALSE) + # Does not work
  geom_jitter(width = 0.25)


# guides(fill = guide_legend(title = "Visualization Method")) + # , title.position = "left"
# guides(fill = guide_legend(title = "LEFT", title.position = "left"))  


# # Response time of correct answers
# participantCorrectResponseTime <- curatedParticipantsData %>% filter(Correct=="True") %>% ggplot(aes(x=QNumber, group=QNumber, ElapsedTime/1000)) +
#   geom_boxplot(aes(fill=ElapsedTime/1000), varwidth=T, fill="plum") +
#   #  coord_flip() +
#   labs(x="Question Number", y="Correct answers - Time secs") +
#   theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
#   #  theme_minimal() +
#   #  scale_x_continuous(breaks=seq(1, 24, 1))
#   scale_x_discrete(limits=seq(1, 24, 1))  +
#   ylim(0, 300) +
#   scale_y_continuous(breaks=seq(0, 300, 60))
# participantCorrectResponseTime


#######################################################################
#######################################################################
#######################################################################


### Example with facets

# https://stackoverflow.com/questions/67775607/allowing-duplicate-x-axis-categorical-groups-in-ggplot2geom-bar

library(ggplot2)
library(viridis)
specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value, stringsAsFactors = FALSE)
data$condition[c(11, 12)] <- "other"
data$cond2 <- data$condition
data$cond2[c(11, 12)] <- make.unique(data$condition[c(11, 12)]) 

# Graph
ggplot(data, aes(fill = condition, y=value, x=cond2)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  scale_x_discrete(labels=setNames(data$condition, data$cond2)) +
  ggtitle("Studying 4 species..") +
  facet_grid(specie~., scales = "free", space = "free") +
  theme(legend.position="none") +
  xlab("") +
  coord_flip() +
  theme(strip.text.x = element_text(angle = 0)) +
  theme_classic()

### End example with facets

# Another example of facets
# https://sscc.wisc.edu/sscc/pubs/dvr/two-variables.html
# https://r-graphics.org/




# axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
  #  theme_minimal () +
  #  theme_minimal (axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  # labs(x="Participants' responses", y="Number of Correct and Incorrect responses") +
  # coord_flip() +




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


# Completing the table with the values that are zero
# 

# ## Example with geom count
# ggplot(mpg, aes(cty, hwy)) +
#   geom_point()
# 
# 
# ggplot(mpg, aes(cty, hwy)) +
#   geom_count()
# 
# 
# # Best used in conjunction with scale_size_area which ensures that
# # counts of zero would be given size 0. Doesn't make much different
# # here because the smallest count is already close to 0.
# ggplot(mpg, aes(cty, hwy)) +
#   geom_count() +
#   scale_size_area()
# 
# 
# # Display proportions instead of counts -------------------------------------
# # By default, all categorical variables in the plot form the groups.
# # Specifying geom_count without a group identifier leads to a plot which is
# # not useful:
# d <- ggplot(diamonds, aes(x = cut, y = clarity))
# d + geom_count(aes(size = after_stat(prop)))
# 
# # To correct this problem and achieve a more desirable plot, we need
# # to specify which group the proportion is to be calculated over.
# d + geom_count(aes(size = after_stat(prop), group = 1)) +
#   scale_size_area(max_size = 10)
# 
# 
# # Or group by x/y variables to have rows/columns sum to 1.
# d + geom_count(aes(size = after_stat(prop), group = cut)) +
#   scale_size_area(max_size = 10)
# 
# d + geom_count(aes(size = after_stat(prop), group = clarity)) +
#   scale_size_area(max_size = 10)





