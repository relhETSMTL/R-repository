# IST Journal Kambiz Paper
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

# Plot for accuracy by participant responses 
participant.accuracy <- experiment.data %>% 
  ggplot(aes(x=Participant.ID, fill=Accuracy)) + # , alpha=0.5
  # geom_bar(position ="dodge") +
  geom_bar() +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), panel.background = element_blank()) +
  #  theme_minimal () +
  #  theme_minimal (axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  labs(x="Participants' responses", y="Number of Accurate (True) and Inaccurate (False) responses") +
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


## Correct responses per participant
correct.participant <- experiment.data %>% filter (Accuracy=="True") %>% 
  group_by(Participant.ID) %>%
  summarise(count=n()) %>% as.data.frame()

summary(correct.participant$count)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 8.00   13.00   14.00   13.54   15.25   16.00 


# Correct responses per question
correct.question <- experiment.data %>% filter (Accuracy=="True") %>% 
  group_by(Question.Number) %>%
  summarise(count=n()) %>% as.data.frame()

summary(correct.question$count)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 10.00   17.75   23.00   20.31   23.25   24.00 


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

# Bar t=2 2D-SP
bars.t2.sp <- accuracy.df %>% filter (T==2 & Visualization.Technique=="2D-SP") %>% 
  ggplot(aes(x=Accuracy, weight = Number)) + 
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_discrete(limits=seq(0, 100, 10)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, size = 5) + 
  # geom_text(aes(label = ..count..), stat = "count", vjust = -1.0, size = 5) + 
  scale_fill_manual(values=c("red", "green")) +
  labs(x="Scatter Plot",y="T=2") +
  guides(fill = FALSE, alpha=FALSE) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 
bars.t2.sp

# Bar t=2 2D-PD
bars.t2.pd <- accuracy.df %>% filter (T==2 & Visualization.Technique=="2D-PD") %>% 
  ggplot(aes(x=Accuracy, weight = Number)) + 
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_discrete(limits=seq(0, 100, 10)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, size = 5) + 
  scale_fill_manual(values=c("red", "green")) +
  labs(x="Parallel Dimensions Plot",y="T=2") +
  guides(fill = FALSE, alpha=FALSE) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 
bars.t2.pd


# Bar t=3 3D-SP
bars.t3.sp <- accuracy.df %>% filter (T==3 & Visualization.Technique=="3D-SP") %>% 
  ggplot(aes(x=Accuracy, weight = Number)) + 
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_discrete(limits=seq(0, 100, 10)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, size = 5) + 
  scale_fill_manual(values=c("red", "green")) +
  labs(x="Scatter Plot",y="T=3") +
  guides(fill = FALSE, alpha=FALSE) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 
bars.t3.sp


# Bar t=3 2D-PD
bars.t3.pd <- accuracy.df %>% filter (T==3 & Visualization.Technique=="3D-PD") %>% 
  ggplot(aes(x=Accuracy, weight = Number)) + 
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_discrete(limits=seq(0, 100, 10)) +
  geom_bar(aes(fill=Accuracy, alpha=0.5)) +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.1, size = 5) + 
  scale_fill_manual(values=c("red", "green")) +
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

# Facet plots for accuracy distributed by t and number of pairs

# t=2 value,  grouping by visualization technique, number of pairs, and accuracy with summarizing by counting
t2.vm.ne.acc <- experiment.data %>% filter (T==2) %>% select(Visualization.Technique,Number.Elements,Accuracy) %>%
  group_by(Visualization.Technique,Number.Elements,Accuracy) %>%
  summarise(count=n()) %>% as.data.frame() %>% 
  # Completing the table with the values that are zero
  add_row(Visualization.Technique="2D-PD", Number.Elements=136, Accuracy="False", count=0) %>%
  add_row(Visualization.Technique="2D-SP", Number.Elements=209, Accuracy="False", count=0) %>%
  add_row(Visualization.Technique="2D-SP", Number.Elements=440, Accuracy="False", count=0)

ggplot(t2.vm.ne.acc, aes(fill = Accuracy, alpha=0.5, y=count, x=as.factor(Number.Elements))) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_text(aes(label = count),  hjust= -0.4, position = position_dodge(1), size = 3.5) +   
  # hjust = -0.3, vjust= -0.5,
  # vjust = -0.2 , hjust = -0.1
  # scale_fill_viridis(discrete = T, option = "E") +
  # scale_x_discrete(labels=setNames(data$condition, data$cond2)) +
  # ggtitle("Accuracy results by Visualization Technique and Number of Pairs") +
  ggtitle("     Scatter Plot (2D-SP)       Parallel Dimensions (2D-PD)") +
  facet_grid(Visualization.Technique ~ . , scales = "free", space = "free") +  # vertical facets
  theme(legend.position="none") +
  scale_fill_manual(values=c("red", "green")) +
  xlab("Number of Pairs") +
  ylab("Count") + 
  guides(alpha=FALSE) +
  coord_flip() +
  theme(strip.text.x = element_text(angle = 0)) +
  scale_y_continuous(breaks=seq(0, 24, 1)) +
  theme_classic() -> accuracy.vm.ne.t2

  accuracy.vm.ne.t2


  # t=3 value,  grouping by visualization technique, number of pairs, and accuracy with summarizing by counting
  t3.vm.ne.acc <- experiment.data %>% filter (T==3) %>% select(Visualization.Technique,Number.Elements,Accuracy) %>%
    group_by(Visualization.Technique,Number.Elements,Accuracy) %>%
    summarise(count=n()) %>% as.data.frame() %>% 
    # Completing the table with the values that are zero
    add_row(Visualization.Technique="3D-SP", Number.Elements=2491, Accuracy="False", count=0)
  
  
  ggplot(t3.vm.ne.acc, aes(fill = Accuracy, alpha=0.5, y=count, x=as.factor(Number.Elements))) + 
    geom_bar(position="dodge", stat="identity") + 
    geom_text(aes(label = count),  hjust= -0.4, position = position_dodge(1), size = 3.5) +   
    #  ggtitle("Accuracy results by Visualization Technique and Number of Triplets") +
    ggtitle("     Scatter Plot (3D-SP)     Parallel Dimensions (3D-PD)") +
    facet_grid(Visualization.Technique ~ . , scales = "free", space = "free") +  # vertical facets
    theme(legend.position="none") +
    scale_fill_manual(values=c("red", "green")) +
    xlab("Number of Triplets") +
    ylab("Count") + 
    guides(alpha=FALSE) +
    coord_flip() +
    theme(strip.text.x = element_text(angle = 0)) +
    scale_y_continuous(breaks=seq(0, 24, 1)) +
    theme_classic() -> accuracy.vm.ne.t3 
  
  accuracy.vm.ne.t3
  
  ## Correct responses per participant
  correct.participant <- experiment.data %>% filter (Accuracy=="True") %>% 
    group_by(Participant.ID) %>%
    summarise(count=n()) %>% as.data.frame()
  
  summary(correct.participant$count)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 8.00   13.00   14.00   13.54   15.25   16.00 

  ## Correct responses by t value
  correct.participant.t <- experiment.data %>% filter (Accuracy=="True") %>% 
    select(Participant.ID,T) %>%
    group_by(Participant.ID,T) %>%
    summarise(count=n()) %>% as.data.frame()
  
  # t=2
  correct.participant.t %>% filter (T==2) %>% summary()
  # Participant.ID       T         count      
  # P03    : 1     Min.   :2   Min.   :4.000  
  # P04    : 1     1st Qu.:2   1st Qu.:7.000  
  # P05    : 1     Median :2   Median :8.000  
  # P06    : 1     Mean   :2   Mean   :7.292  
  # P07    : 1     3rd Qu.:2   3rd Qu.:8.000  
  # P08    : 1     Max.   :2   Max.   :8.000    
  
  # t=3
  correct.participant.t %>% filter (T==3) %>% summary()
  # Participant.ID       T         count     
  # P03    : 1     Min.   :3   Min.   :3.00  
  # P04    : 1     1st Qu.:3   1st Qu.:6.00  
  # P05    : 1     Median :3   Median :6.00  
  # P06    : 1     Mean   :3   Mean   :6.25  
  # P07    : 1     3rd Qu.:3   3rd Qu.:8.00  
  # P08    : 1     Max.   :3   Max.   :8.00  
  

  ## Correct responses by visualization method
  correct.participant.vm  <- experiment.data %>% filter (Accuracy=="True") %>% 
    select(Participant.ID,Visualization.Technique) %>%
    group_by(Participant.ID,Visualization.Technique) %>%
    summarise(count=n()) %>% as.data.frame()
  
  # Scattered Plot 2D-SP or 3D-SP
  correct.participant.vm.sp <- correct.participant.vm %>% 
    filter (Visualization.Technique=="2D-SP" | Visualization.Technique=="3D-SP") %>% 
    select(Participant.ID,count) %>%
    group_by(Participant.ID) %>% summarise(total=sum(count)) %>% as.data.frame()
  
  summary(correct.participant.vm.sp$total)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 5.000   6.000   7.000   6.833   8.000   8.000   
  
  # Parallel dimensions Plot 2D-PD or 3D-PD
  correct.participant.vm.pd <- correct.participant.vm %>% 
    filter (Visualization.Technique=="2D-PD" | Visualization.Technique=="3D-PD") %>% 
    select(Participant.ID,count) %>%
    group_by(Participant.ID) %>% summarise(total=sum(count)) %>% as.data.frame()
  
  summary(correct.participant.vm.pd$total)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 3.000   5.750   7.000   6.708   8.000   8.000

  
  #################################################
  ### RQ2 Time on task
  
  # Grid of Time on task per t value and visualization method
  
  # t2 and parallel dimensions
  bp.t2.pd.res <- experiment.data %>% 
    mutate(Accuracy = case_when(Accuracy == 'True' ~ TRUE, Accuracy == 'False' ~ FALSE)) %>%
    filter (T==2 & Visualization.Technique=="2D-PD") %>%
    ggplot(aes(x=Accuracy, group=Accuracy,Elapsed.Time/1000)) +  
    geom_boxplot(aes(fill=Elapsed.Time/1000), varwidth=T,  fill=c("red", "green"), alpha=0.5) +  
    geom_jitter() +
    labs(x="Parallel Dimensions Plot", y="T=2") + 
    theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
    scale_y_continuous(breaks=seq(0, 350, 50),limits = c(0, 350))
  
  bp.t2.pd.res
  
  
  # t2 and scatter plot
  bp.t2.sp.res <- experiment.data %>% 
    mutate(Accuracy = case_when(Accuracy == 'True' ~ TRUE, Accuracy == 'False' ~ FALSE)) %>%
    filter (T==2 & Visualization.Technique=="2D-SP") %>%
    ggplot(aes(x=Accuracy, group=Accuracy,Elapsed.Time/1000)) + 
    geom_boxplot(aes(fill=Elapsed.Time/1000), varwidth=T,  fill=c("red", "green"), alpha=0.5) + 
    geom_jitter() +
    labs(x="Scatter Plot", y="T=2") + 
    theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
    scale_y_continuous(breaks=seq(0, 350, 50),limits = c(0, 350))
  
  bp.t2.sp.res
  
  
  # t3 and parallel dimensions
  bp.t3.pd.res <- experiment.data %>% 
    mutate(Accuracy = case_when(Accuracy == 'True' ~ TRUE, Accuracy == 'False' ~ FALSE)) %>%
    filter (T==3 & Visualization.Technique=="3D-PD") %>%
    ggplot(aes(x=Accuracy, group=Accuracy,Elapsed.Time/1000)) +  
    scale_y_continuous(breaks=seq(0, 350, 50), limits = c(0, 350)) +
    geom_boxplot(aes(fill=Elapsed.Time/1000), varwidth=T,  fill=c("red", "green"), alpha=0.5) +  
    geom_jitter() +
    labs(x="Parallel Dimensions Plot", y="T=3") + 
    theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 
 
  bp.t3.pd.res
  
  
  # t3 and scatter plot
  bp.t3.sp.res <- experiment.data %>% 
    mutate(Accuracy = case_when(Accuracy == 'True' ~ TRUE, Accuracy == 'False' ~ FALSE)) %>%
    filter (T==3 & Visualization.Technique=="3D-SP") %>%
    ggplot(aes(x=Accuracy, group=Accuracy,Elapsed.Time/1000)) +  
    geom_boxplot(aes(fill=Elapsed.Time/1000), varwidth=T,  fill=c("red", "green"), alpha=0.5) +  
    geom_jitter() +
    labs(x="Scatter Plot", y="T=3") + 
    theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
    ylim(0, 550)  +
    scale_y_continuous(breaks=seq(0, 350, 50),limits = c(0, 350))
  
  bp.t3.sp.res
  
  # Creates the grid for the stack bars
  grid.res <- grid.arrange(bp.t3.sp.res,bp.t3.pd.res,
                           bp.t2.sp.res,bp.t2.pd.res,
                           ncol=2, nrow=2,
                           bottom = textGrob("Visualization Techniques",gp=gpar(fontsize=15,font=3)),
                           left = textGrob("Covering Array Strength - Time in secs", rot=90, gp=gpar(fontsize=15,font=3)))
  
  
  
  ### Note: Elapsed.Time is the time taken by the web interface, this is the one used for reporting the results
  # For research question Q2 time-on-task
  
  ## Total Elapsed time by participants (incorrect and correct answers)
  elapsed.time.participant <- experiment.data %>%
    select(Participant.ID, Elapsed.Time) %>%
    group_by(Participant.ID) %>%
    summarise(total=sum(Elapsed.Time)/1000/60)  # Total time in minutes per participant
  

  summary(elapsed.time.participant$total)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 6.125  13.598  16.836  17.457  22.103  24.960 
  
  sd(elapsed.time.participant$total)
  # [1] 5.073546
  
  # Total Elapsed time for correct answers, in general --> not by participant
  elapsed.time.participant.correct <- experiment.data %>% filter (Accuracy=="True")  %>% 
    select(Elapsed.Time) # %>%
  # group_by(Participant.ID) %>%
  # summarise(total=sum(Elapsed.Time)/1000/60)  # Total time in minutes
  
  
  summary(elapsed.time.participant.correct$Elapsed.Time/1000)
  # In seconds
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 4.256  34.208  47.712  64.303  78.943 465.551 
  

  sd(elapsed.time.participant.correct$Elapsed.Time/1000)
  # 51.03093
  
  
  # Total Elapsed time for incorrect answers, in general --> not by participant
  elapsed.time.participant.incorrect <- experiment.data %>% filter (Accuracy=="False")  %>% 
    select(Elapsed.Time) 
  
  
  summary(elapsed.time.participant.incorrect$Elapsed.Time/1000)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 16.21   43.92   70.27   71.86   90.38  161.98 
  
  sd(elapsed.time.participant.incorrect$Elapsed.Time/1000)
  # 37.72342
  
  
  # Time on task based on the value of T, pairs vs triplets
  
  # t=2
  t2.et <- experiment.data %>% filter (T==2)  %>% 
    select(Elapsed.Time)
  summary(t2.et)
  # Elapsed.Time   
  # Min.   :  7985  
  # 1st Qu.: 28907  
  # Median : 41128  
  # Mean   : 49383  
  # 3rd Qu.: 58108  
  # Max.   :208219  
  
  
  sd(t2.et$Elapsed.Time)
  # [1] 31466.61
  
  # t=3
  t3.et <- experiment.data %>% filter (T==3)  %>% 
    select(Elapsed.Time)
  summary(t3.et)
  # Min.   :  4256  
  # 1st Qu.: 44360  
  # Median : 71400  
  # Mean   : 81544  
  # 3rd Qu.: 99999  
  # Max.   :465551   
  
  sd(t3.et$Elapsed.Time)
  # 57918.63 
  
  
  # Time on task based on the visualization techniques
  
  ## Response time over visualization methods
  # vm = scatter plot
  sp.res <- experiment.data %>% filter (Visualization.Technique=="2D-SP" | Visualization.Technique=="3D-SP")  %>% 
    select(Elapsed.Time)
  summary(sp.res)
  # Elapsed.Time   
  # Min.   :  4484  
  # 1st Qu.: 31458  
  # Median : 49425  
  # Mean   : 69844  
  # 3rd Qu.: 90270  
  # Max.   :465551  
  
  sd(sp.res$Elapsed.Time)
  # [1] 59741.97
  
  
  # vm = parallel dimensions plot
  pd.res <- experiment.data %>% filter (Visualization.Technique=="2D-PD" | Visualization.Technique=="3D-PD")  %>% 
    select(Elapsed.Time)
  summary(pd.res)
  # Elapsed.Time   
  # Min.   :  4256  
  # 1st Qu.: 36179  
  # Median : 49968  
  # Mean   : 61084  
  # 3rd Qu.: 75968  
  # Max.   :199136  
  
  sd(pd.res$Elapsed.Time)
  # [1] 35452.81
  
  
  
  ## Cross factors results

    # scatter plot, t=2,incorrect
  experiment.data %>% filter (Visualization.Technique=="2D-SP" & Accuracy=="False") %>% 
    select(Visualization.Technique, Accuracy, Elapsed.Time) %>% summary()
  # Visualization.Technique  Accuracy  Elapsed.Time  
  # 2D-PD:0                 False:2   Min.   :81855  
  # 2D-SP:2                 True :0   1st Qu.:83239  
  # 3D-PD:0                           Median :84624  
  # 3D-SP:0                           Mean   :84624  
  # 3rd Qu.:86008  
  # Max.   :87392  
  
  # scatter plot, t=2,correct
  experiment.data %>% filter (Visualization.Technique=="2D-SP" & Accuracy=="True") %>% 
    select(Visualization.Technique, Accuracy, Elapsed.Time) %>% summary()
  # Visualization.Technique  Accuracy   Elapsed.Time   
  # 2D-PD: 0                False: 0   Min.   :  7985  
  # 2D-SP:94                True :94   1st Qu.: 24300  
  # 3D-PD: 0                           Median : 37016  
  # 3D-SP: 0                           Mean   : 41830  
  # 3rd Qu.: 50696  
  # Max.   :208219   
  
  
  # parallel coordinates plot, t=2,incorrect
  experiment.data %>% filter (Visualization.Technique=="2D-PD" & Accuracy=="False") %>% 
    select(Visualization.Technique, Accuracy, Elapsed.Time) %>% summary()
  # Visualization.Technique  Accuracy   Elapsed.Time   
  # 2D-PD:15                False:15   Min.   : 23247  
  # 2D-SP: 0                True : 0   1st Qu.: 47327  
  # 3D-PD: 0                           Median : 54143  
  # 3D-SP: 0                           Mean   : 65858  
  # 3rd Qu.: 76224  
  # Max.   :156703  
  
  
  
  # parallel coordinates plot, t=2,correct
  experiment.data %>% filter (Visualization.Technique=="2D-PD" & Accuracy=="True") %>% 
    select(Visualization.Technique, Accuracy, Elapsed.Time) %>% summary()
  # Visualization.Technique  Accuracy   Elapsed.Time   
  # 2D-PD:81                False: 0   Min.   : 15776  
  # 2D-SP: 0                True :81   1st Qu.: 33791  
  # 3D-PD: 0                           Median : 46223  
  # 3D-SP: 0                           Mean   : 54228  
  # 3rd Qu.: 64956  
  # Max.   :156559 
  
   
  # scatter plot, t=3,correct
  experiment.data %>% filter (Visualization.Technique=="3D-SP" & Accuracy=="True") %>% 
    select(Visualization.Technique, Accuracy, Elapsed.Time) %>% summary()
  # Visualization.Technique  Accuracy   Elapsed.Time   
  # 2D-PD: 0             False: 0   Min.   :  4484  
  # 2D-SP: 0             True :70   1st Qu.: 44999  
  # 3D-PD: 0                        Median : 82146  
  # 3D-SP:70                        Mean   :100418  
  # 3rd Qu.:129488  
  # Max.   :465551  
  
  # scatter plot, t=3, incorrect
  experiment.data %>% filter (Visualization.Technique=="3D-SP" & Accuracy=="False") %>% 
    select(Visualization.Technique, Accuracy, Elapsed.Time) %>% summary()
  # Visualization.Technique  Accuracy   Elapsed.Time   
  # 2D-PD: 0             False:26   Min.   : 24863  
  # 2D-SP: 0             True : 0   1st Qu.: 71948  
  # 3D-PD: 0                        Median : 80393  
  # 3D-SP:26                        Mean   : 87670  
  # 3rd Qu.:111337  
  # Max.   :161983 
  
  
  # parallel coordinates plot, t=3,correct
  experiment.data %>% filter (Visualization.Technique=="3D-PD" & Accuracy=="True") %>% 
    select(Visualization.Technique, Accuracy, Elapsed.Time) %>% summary()
  # Visualization.Technique  Accuracy   Elapsed.Time   
  # 2D-PD: 0             False: 0   Min.   :  4256  
  # 2D-SP: 0             True :80   1st Qu.: 44976  
  # 3D-PD:80                        Median : 60800  
  # 3D-SP: 0                        Mean   : 69309  
  # 3rd Qu.: 84808  
  # Max.   :199136  
  
  
  # parallel coordinates plot, t=3,incorrect
  experiment.data %>% filter (Visualization.Technique=="3D-PD" & Accuracy=="False") %>% 
    select(Visualization.Technique, Accuracy, Elapsed.Time) %>% summary()
  # Visualization.Technique  Accuracy   Elapsed.Time   
  # 2D-PD: 0             False:16   Min.   : 16210  
  # 2D-SP: 0             True : 0   1st Qu.: 31740  
  # 3D-PD:16                        Median : 41414  
  # 3D-SP: 0                        Mean   : 50191  
  # 3rd Qu.: 53058  
  # Max.   :133983  
  

  ### Summary of time-on-task based on values of t and visualization techniques

  ## For t=2
  t2.vt.ne.tot <- experiment.data %>% filter (T==2) %>% 
    select(Visualization.Technique,Number.Elements,Elapsed.Time,Accuracy)
  
  ggplot(t2.vt.ne.tot, aes(alpha=0.5, y=Elapsed.Time/1000, x=as.factor(Number.Elements), colour=Accuracy)) +
    geom_point(size=5, alpha=0.5, shape=21, stroke=1) +
    facet_grid(Visualization.Technique ~ . , scales = "free", space = "free") +  # vertical facets
    theme(legend.position="none") +
    scale_color_manual(values = c("red","green")) +
    xlab("Number of Pairs") +
    ylab("Time-on-task seconds") +  
    guides(alpha=FALSE) +
    coord_flip() +
    theme(strip.text.x = element_text(angle = 0)) +
    theme_classic()
  

  # For t=3 
  t3.vt.ne.tot <- experiment.data %>% filter (T==3) %>% 
    select(Visualization.Technique,Number.Elements,Elapsed.Time,Accuracy)
  
  ggplot(t3.vt.ne.tot, aes(alpha=0.5, y=Elapsed.Time/1000, x=as.factor(Number.Elements), colour=Accuracy)) +
    geom_point(size=5, alpha=0.5, shape=21, stroke=1) +
    facet_grid(Visualization.Technique ~ . , scales = "free", space = "free") +  # vertical facets
    theme(legend.position="none") +
    scale_color_manual(values = c("red","green")) +
    xlab("Number of Triplets") +
    ylab("Time-on-task seconds") +  
    guides(alpha=FALSE) +
    coord_flip() +
    theme(strip.text.x = element_text(angle = 0)) +
    theme_classic()
  
  
#####################
## Descriptive statistcs for RQ3  

  # [1] "Participant.ID"           "Question.Number"          "T"                        "Visualization.Technique" 
  # [5] "Accuracy"                 "Elapsed.Time.Eye.Tracker" "Fixation.Time"            "Fixation.Count"          
  # [9] "Question.pftime"          "Question.pfcount"         "Response.pftime"          "Response.pfcount"        
  # [13] "Misc.pftime"              "Misc.pfcount"             "Navigation.pftime"        "Navigation.pfcount"      
  # [17] "Axial.pftime"             "Axial.pfcount"            "Solution.pftime"          "Solution.pfcount"        
  # [21] "Target.pftime"            "Target.pfcount"           "Elapsed.Time"             "Certainty.Assessment"    
  # [25] "Difficulty.Level"         "Number.Elements"         
  
  # AOIs to consider in the summary:
  # Question.pftime, Question.pfcount
  # Response.pftime, Response.pfcount 
  # Misc.pftime, Misc.pfcount
  # Navigation.pftime, Navigation.pfcount
  # Axial.pftime, Axial.pfcount
  # Solution.pftime, Solution.pfcount        
  # Target.pftime, Target.pfcount
  
  # Sequence of data to be stored
  # count, time
  # Axial, Misc, Navigation, Question, Response, Solution, Target  
  
  #####################################################################################
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
  
  t2.sp.aois.plot <- t2.sp.df %>% ggplot(aes(fill=AOI, y=Percentage, x=Measure)) + 
    geom_bar(position="fill", stat="identity", alpha=0.7) +
    theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
    labs(x="t=2 and scatter plot", y="") 
  t2.sp.aois.plot
  
  
  ### Data for t=2, Visualization Technique parallel dimensions plots
  t2.pd.data <- experiment.data %>% filter(T==2 & Visualization.Technique=="2D-PD") 
  
  
  t2.pd.df <- data.frame(matrix(nrow = 2 * 7, ncol = 3))
  columnNamesAOIs <- c("Measure", "AOI", "Percentage")
  colnames(t2.pd.df) <- columnNamesAOIs
  
  t2.pd.df[1,1] <- 'count'
  t2.pd.df[1,2] <- 'Axial'
  t2.pd.df[1,3] <- mean (t2.pd.data$Axial.pfcount)
  
  t2.pd.df[2,1] <- 'count'
  t2.pd.df[2,2] <- 'Misc'
  t2.pd.df[2,3] <- mean(t2.pd.data$Misc.pfcount)
  
  t2.pd.df[3,1] <- 'count'
  t2.pd.df[3,2] <- 'Navigation'
  t2.pd.df[3,3] <- mean(t2.pd.data$Navigation.pfcount)
  
  t2.pd.df[4,1] <- 'count'
  t2.pd.df[4,2] <- 'Question'
  t2.pd.df[4,3] <- mean (t2.pd.data$Question.pfcount)
  
  t2.pd.df[5,1] <- 'count'
  t2.pd.df[5,2] <- 'Response'
  t2.pd.df[5,3] <- mean(t2.pd.data$Response.pfcount)
  
  t2.pd.df[6,1] <- 'count'
  t2.pd.df[6,2] <- 'Solution'
  t2.pd.df[6,3] <- mean(t2.pd.data$Solution.pfcount)
  
  t2.pd.df[7,1] <- 'count'
  t2.pd.df[7,2] <- 'Target'
  t2.pd.df[7,3] <- mean (t2.pd.data$Target.pfcount)
  
  t2.pd.df[8,1] <- 'time'
  t2.pd.df[8,2] <- 'Axial'
  t2.pd.df[8,3] <- mean (t2.pd.data$Axial.pftime)
  
  t2.pd.df[9,1] <- 'time'
  t2.pd.df[9,2] <- 'Misc'
  t2.pd.df[9,3] <- mean(t2.pd.data$Misc.pftime)
  
  t2.pd.df[10,1] <- 'time'
  t2.pd.df[10,2] <- 'Navigation'
  t2.pd.df[10,3] <- mean(t2.pd.data$Navigation.pftime)
  
  t2.pd.df[11,1] <- 'time'
  t2.pd.df[11,2] <- 'Question'
  t2.pd.df[11,3] <- mean (t2.pd.data$Question.pftime)
  
  t2.pd.df[12,1] <- 'time'
  t2.pd.df[12,2] <- 'Response'
  t2.pd.df[12,3] <- mean(t2.pd.data$Response.pftime)
  
  t2.pd.df[13,1] <- 'time'
  t2.pd.df[13,2] <- 'Solution'
  t2.pd.df[13,3] <- mean(t2.pd.data$Solution.pftime)
  
  t2.pd.df[14,1] <- 'time'
  t2.pd.df[14,2] <- 'Target'
  t2.pd.df[14,3] <- mean (t2.pd.data$Target.pftime)
  
  # makes Measure factor values instead of characters
  t2.pd.df$Measure <- factor(t2.pd.df$Measure, levels = c('time', 'count'))
  
  t2.pd.aois.plot <- t2.pd.df %>% ggplot(aes(fill=AOI, y=Percentage, x=Measure)) + 
    geom_bar(position="fill", stat="identity", alpha=0.7) +
    theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
    labs(x="t=2 and parallel dimensions plot", y="") 
  t2.pd.aois.plot
  
  
  #####################################################################################
  # Data for t=3, Visualization Technique scatter plots
  t3.sp.data <- experiment.data %>% filter(T==3 & Visualization.Technique=="3D-SP") 
  
  
  t3.sp.df <- data.frame(matrix(nrow = 2 * 7, ncol = 3))
  columnNamesAOIs <- c("Measure", "AOI", "Percentage")
  colnames(t3.sp.df) <- columnNamesAOIs
  
  t3.sp.df[1,1] <- 'count'
  t3.sp.df[1,2] <- 'Axial'
  t3.sp.df[1,3] <- mean (t3.sp.data$Axial.pfcount)
  
  t3.sp.df[2,1] <- 'count'
  t3.sp.df[2,2] <- 'Misc'
  t3.sp.df[2,3] <- mean(t3.sp.data$Misc.pfcount)
  
  t3.sp.df[3,1] <- 'count'
  t3.sp.df[3,2] <- 'Navigation'
  t3.sp.df[3,3] <- mean(t3.sp.data$Navigation.pfcount)
  
  t3.sp.df[4,1] <- 'count'
  t3.sp.df[4,2] <- 'Question'
  t3.sp.df[4,3] <- mean (t3.sp.data$Question.pfcount)
  
  t3.sp.df[5,1] <- 'count'
  t3.sp.df[5,2] <- 'Response'
  t3.sp.df[5,3] <- mean(t3.sp.data$Response.pfcount)
  
  t3.sp.df[6,1] <- 'count'
  t3.sp.df[6,2] <- 'Solution'
  t3.sp.df[6,3] <- mean(t3.sp.data$Solution.pfcount)
  
  t3.sp.df[7,1] <- 'count'
  t3.sp.df[7,2] <- 'Target'
  t3.sp.df[7,3] <- mean (t3.sp.data$Target.pfcount)
  
  t3.sp.df[8,1] <- 'time'
  t3.sp.df[8,2] <- 'Axial'
  t3.sp.df[8,3] <- mean (t3.sp.data$Axial.pftime)
  
  t3.sp.df[9,1] <- 'time'
  t3.sp.df[9,2] <- 'Misc'
  t3.sp.df[9,3] <- mean(t3.sp.data$Misc.pftime)
  
  t3.sp.df[10,1] <- 'time'
  t3.sp.df[10,2] <- 'Navigation'
  t3.sp.df[10,3] <- mean(t3.sp.data$Navigation.pftime)
  
  t3.sp.df[11,1] <- 'time'
  t3.sp.df[11,2] <- 'Question'
  t3.sp.df[11,3] <- mean (t3.sp.data$Question.pftime)
  
  t3.sp.df[12,1] <- 'time'
  t3.sp.df[12,2] <- 'Response'
  t3.sp.df[12,3] <- mean(t3.sp.data$Response.pftime)
  
  t3.sp.df[13,1] <- 'time'
  t3.sp.df[13,2] <- 'Solution'
  t3.sp.df[13,3] <- mean(t3.sp.data$Solution.pftime)
  
  t3.sp.df[14,1] <- 'time'
  t3.sp.df[14,2] <- 'Target'
  t3.sp.df[14,3] <- mean (t3.sp.data$Target.pftime)
  
  # makes Measure factor values instead of characters
  t3.sp.df$Measure <- factor(t3.sp.df$Measure, levels = c('time', 'count'))
  
  t3.sp.aois.plot <- t3.sp.df %>% ggplot(aes(fill=AOI, y=Percentage, x=Measure)) + 
    geom_bar(position="fill", stat="identity", alpha=0.7) +
    theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
    labs(x="t=3 and scatter plot", y="") 
  t3.sp.aois.plot
  
  
  ### Data for t=3, Visualization Technique parallel dimensions plots
  t3.pd.data <- experiment.data %>% filter(T==3 & Visualization.Technique=="3D-PD") 
  
  
  t3.pd.df <- data.frame(matrix(nrow = 2 * 7, ncol = 3))
  columnNamesAOIs <- c("Measure", "AOI", "Percentage")
  colnames(t3.pd.df) <- columnNamesAOIs
  
  t3.pd.df[1,1] <- 'count'
  t3.pd.df[1,2] <- 'Axial'
  t3.pd.df[1,3] <- mean (t3.pd.data$Axial.pfcount)
  
  t3.pd.df[2,1] <- 'count'
  t3.pd.df[2,2] <- 'Misc'
  t3.pd.df[2,3] <- mean(t3.pd.data$Misc.pfcount)
  
  t3.pd.df[3,1] <- 'count'
  t3.pd.df[3,2] <- 'Navigation'
  t3.pd.df[3,3] <- mean(t3.pd.data$Navigation.pfcount)
  
  t3.pd.df[4,1] <- 'count'
  t3.pd.df[4,2] <- 'Question'
  t3.pd.df[4,3] <- mean (t3.pd.data$Question.pfcount)
  
  t3.pd.df[5,1] <- 'count'
  t3.pd.df[5,2] <- 'Response'
  t3.pd.df[5,3] <- mean(t3.pd.data$Response.pfcount)
  
  t3.pd.df[6,1] <- 'count'
  t3.pd.df[6,2] <- 'Solution'
  t3.pd.df[6,3] <- mean(t3.pd.data$Solution.pfcount)
  
  t3.pd.df[7,1] <- 'count'
  t3.pd.df[7,2] <- 'Target'
  t3.pd.df[7,3] <- mean (t3.pd.data$Target.pfcount)
  
  t3.pd.df[8,1] <- 'time'
  t3.pd.df[8,2] <- 'Axial'
  t3.pd.df[8,3] <- mean (t3.pd.data$Axial.pftime)
  
  t3.pd.df[9,1] <- 'time'
  t3.pd.df[9,2] <- 'Misc'
  t3.pd.df[9,3] <- mean(t3.pd.data$Misc.pftime)
  
  t3.pd.df[10,1] <- 'time'
  t3.pd.df[10,2] <- 'Navigation'
  t3.pd.df[10,3] <- mean(t3.pd.data$Navigation.pftime)
  
  t3.pd.df[11,1] <- 'time'
  t3.pd.df[11,2] <- 'Question'
  t3.pd.df[11,3] <- mean (t3.pd.data$Question.pftime)
  
  t3.pd.df[12,1] <- 'time'
  t3.pd.df[12,2] <- 'Response'
  t3.pd.df[12,3] <- mean(t3.pd.data$Response.pftime)
  
  t3.pd.df[13,1] <- 'time'
  t3.pd.df[13,2] <- 'Solution'
  t3.pd.df[13,3] <- mean(t3.pd.data$Solution.pftime)
  
  t3.pd.df[14,1] <- 'time'
  t3.pd.df[14,2] <- 'Target'
  t3.pd.df[14,3] <- mean (t3.pd.data$Target.pftime)
  
  # makes Measure factor values instead of characters
  t3.pd.df$Measure <- factor(t3.pd.df$Measure, levels = c('time', 'count'))
  
  t3.pd.aois.plot <- t3.pd.df %>% ggplot(aes(fill=AOI, y=Percentage, x=Measure)) + 
    geom_bar(position="fill", stat="identity", alpha=0.7) +
    theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
    labs(x="t=3 and parallel dimensions plot", y="") 
  t3.pd.aois.plot
  

## Computing the average percentages per question  
  
  # Test of merging the plots into a single figure
  # https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots
  library(cowplot)
  prow <- plot_grid(t3.sp.aois.plot + theme(legend.position="none"),
                    t3.pd.aois.plot + theme(legend.position="none"),
                    align = 'vh',
                    # labels = c("CTC", "no CTC"),
                    hjust = -1,
                    nrow = 1)
  
  legend_b <- get_legend(t3.pd.aois.plot + theme(legend.position="bottom"))
  aois.plot <- plot_grid( prow, legend_b, ncol = 1, rel_heights = c(1, .2))
  aois.plot


  # Plot with the four combinations, mean percentage of fixation time and fixation count
  prow4 <- plot_grid(t3.sp.aois.plot + theme(legend.position="none"),
                     t3.pd.aois.plot + theme(legend.position="none"),
                     t2.sp.aois.plot + theme(legend.position="none"),
                     t2.pd.aois.plot + theme(legend.position="none"),
                     align = 'vh',
                    hjust = -1,
                    nrow = 2)
  legend_b4 <- get_legend(t3.pd.aois.plot + theme(legend.position="bottom"))
  aois.plot4 <- plot_grid( prow4, legend_b4, ncol = 1, rel_heights = c(1, .2))
  aois.plot4

  
  #####################################################################################
  # Reordering the AOI data by time and count in descending order
  
  t2.sp.df %>% filter(Measure=='time') %>% arrange(desc(Percentage))
  # Measure        AOI Percentage
  # 1    time Navigation 0.24318438
  # 2    time     Target 0.22318437
  # 3    time      Axial 0.21334583
  # 4    time   Question 0.18165312
  # 5    time   Response 0.07565938
  # 6    time   Solution 0.04076042
  # 7    time       Misc 0.02221250
    
  t2.sp.df %>% filter(Measure=='count') %>% arrange(desc(Percentage))
  # Measure        AOI Percentage
  # 1   count Navigation 0.26278333
  # 2   count      Axial 0.24077187
  # 3   count     Target 0.20298229
  # 4   count   Question 0.19607604
  # 5   count   Response 0.04378125
  # 6   count   Solution 0.02973542
  # 7   count       Misc 0.02387292
    
  t2.pd.df %>% filter(Measure=='time') %>% arrange(desc(Percentage))
  # Measure        AOI Percentage
  # 1    time      Axial 0.36291563
  # 2    time Navigation 0.19619792
  # 3    time   Question 0.15041771
  # 4    time     Target 0.13832812
  # 5    time   Solution 0.06678021
  # 6    time   Response 0.05933021
  # 7    time       Misc 0.02602917
    
  t2.pd.df %>% filter(Measure=='count') %>% arrange(desc(Percentage))
  # Measure        AOI Percentage
  # 1   count      Axial 0.35606875
  # 2   count Navigation 0.24196146
  # 3   count   Question 0.18446250
  # 4   count     Target 0.08887813
  # 5   count   Solution 0.05118229
  # 6   count   Response 0.04323542
  # 7   count       Misc 0.03420521  
  
  
  t3.sp.df %>% filter(Measure=='time') %>% arrange(desc(Percentage))
  # 1    time Navigation 0.29747396
  # 2    time     Target 0.21359896
  # 3    time      Axial 0.17944896
  # 4    time   Question 0.14352917
  # 5    time       Misc 0.12094062
  # 6    time   Response 0.03138125
  # 7    time   Solution 0.01410729
    
  t3.sp.df %>% filter(Measure=='count') %>% arrange(desc(Percentage))
  # Measure        AOI Percentage
  # 1   count Navigation 0.28285729
  # 2   count     Target 0.20028229
  # 3   count      Axial 0.19175833
  # 4   count   Question 0.17665938
  # 5   count       Misc 0.11778646
  # 6   count   Response 0.02099583
  # 7   count   Solution 0.01011250
  
  
  t3.pd.df %>% filter(Measure=='time') %>% arrange(desc(Percentage))
  # Measure        AOI Percentage
  # 1    time      Axial 0.35480417
  # 2    time     Target 0.16628125
  # 3    time Navigation 0.16516771
  # 4    time   Question 0.15792917
  # 5    time   Response 0.06853125
  # 6    time   Solution 0.06415729
  # 7    time       Misc 0.02311458  

  t3.pd.df %>% filter(Measure=='count') %>% arrange(desc(Percentage))
  # Measure        AOI Percentage
  # 1   count      Axial 0.35863646
  # 2   count Navigation 0.21635104
  # 3   count   Question 0.19246979
  # 4   count     Target 0.11715417
  # 5   count   Response 0.04775833
  # 6   count   Solution 0.03842188
  # 7   count       Misc 0.02920521  
  

  
  #####################################################################################
  # Box plots of time and count per combination
  
 summary(experiment.data$Fixation.Time)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 3651   28204   40637   53161   66064  408149 

  # Box plots of fixation time per visualizaton technique  
  bp.fixation.time <- experiment.data %>% 
    ggplot(aes(x=Visualization.Technique, group=Visualization.Technique,Fixation.Time/1000)) +  
    geom_boxplot(aes(fill=Fixation.Time/1000), varwidth=T, fill=c("turquoise", "magenta","blue","yellow"), alpha=0.5) +  # fill=c("red", "green"),
    geom_jitter() +
    labs(x="Visualization Technique", y="Fixation Time (secs)") + 
    theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50"))  +
    scale_y_continuous(breaks=seq(0, 450, 50),limits = c(0, 450)) + 
    xlim("2D-SP","2D-PD","3D-SP","3D-PD") # reorders the values
  
  bp.fixation.time 
    
  summary(experiment.data$Fixation.Count)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 9.00   97.75  136.00  178.73  219.00  987.00  
      
  # Box plots of fixation count per visualizaton technique  
  bp.fixation.count <- experiment.data %>% 
    ggplot(aes(x=Visualization.Technique, group=Visualization.Technique,Fixation.Count)) +  
    geom_boxplot(aes(fill=Fixation.Count), varwidth=T, fill=c("turquoise", "magenta","blue","yellow"), alpha=0.5) +  # fill=c("red", "green"),
    geom_jitter() +
    labs(x="Visualization Technique", y="Fixation Count") + 
    theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50"))  +
    scale_y_continuous(breaks=seq(0, 1000, 100),limits = c(0, 1000)) + 
    xlim("2D-SP","2D-PD","3D-SP","3D-PD") # reorders the values 
  
  bp.fixation.count 
  
  
  ### Fixation time and fixation count summaries

  experiment.data %>% select(Fixation.Count, Visualization.Technique) %>% group_by(Visualization.Technique) %>%
    summarise(median = median (Fixation.Count), avg = mean(Fixation.Count), std = sd(Fixation.Count), 
              min = min(Fixation.Count), max = max(Fixation.Count)) %>% as.data.frame()
  
  # Visualization.Technique median      avg       std min max
  # 1                   2D-PD  122.0 142.8021  76.43485  48 459
  # 2                   2D-SP  113.0 131.6250  85.58003  24 596
  # 3                   3D-PD  146.5 174.1354  97.68032   9 535
  # 4                   3D-SP  232.5 266.3438 172.83353  31 987

    

  experiment.data %>% select(Fixation.Time, Visualization.Technique) %>% group_by(Visualization.Technique) %>%
    summarise(median = median (Fixation.Time), avg = mean(Fixation.Time/1000), std = sd(Fixation.Time/1000), 
              min = min(Fixation.Time/1000), max = max(Fixation.Time/1000)) %>% as.data.frame()
  
  # Visualization.Technique median      avg      std    min     max
  # 1                   2D-PD  36736 45.09638 25.93038 13.022 133.775
  # 2                   2D-SP  28921 34.15934 23.11749  8.376 171.443
  # 3                   3D-PD  43140 53.50670 30.60615  3.651 169.558
  # 4                   3D-SP  66875 79.88126 61.83897  8.926 408.149

################################################################################################
### Scratch code

### Complete working facet figure for time on task
  # ## Revised plot, including accuracy to distinguish points in the grid by False or True
  # t2.vt.ne.tot <- experiment.data %>% filter (T==2) %>% 
  #   select(Visualization.Technique,Number.Elements,Elapsed.Time,Accuracy)
  # 
  # ggplot(t2.vt.ne.tot, aes(alpha=0.5, y=Elapsed.Time/1000, x=as.factor(Number.Elements), colour=Accuracy
  # )) +   # alpha=0.5, shape=Accuracy colour=Accuracy , colour=Accuracy
  #   # geom_bar(position="dodge", stat="identity") + 
  #   geom_point(size=5, alpha=0.5, shape=21, stroke=1) +
  #   #scale_shape(solid = FALSE) +
  #   # geom_jitter(width = 0.25) + 
  #   # geom_text(aes(label = count),  hjust= -0.4, position = position_dodge(1), size = 3.5) +   
  #   facet_grid(Visualization.Technique ~ . , scales = "free", space = "free") +  # vertical facets
  #   theme(legend.position="none") +
  #   #    scale_color_manual(values = ifelse(t2.vt.ne.tot$Accuracy=="FALSE","red","green")) +
  #   scale_color_manual(values = c("red","green")) +
  #   # scale_fill_manual(values=c("red", "green")) +
  #   xlab("Number of Pairs") +
  #   ylab("Time-on-task seconds") +  
  #   guides(alpha=FALSE) +
  #   coord_flip() +
  #   theme(strip.text.x = element_text(angle = 0)) +
  #   #    scale_y_continuous(breaks=seq(0, 24, 1)) +
  #   theme_classic()
  
  
### Old examples of graphs for time on task (elapsed time)  

  # # Response time for t=2 and number of pairs and visualization technique
  # t2.vt.ne.tot <- experiment.data %>% filter (T==2) %>% #  & Accuracy=="True"
  #   select(Visualization.Technique,Number.Elements,Elapsed.Time)
  # 
  # ggplot(t2.vt.ne.tot, aes(x = as.factor(Number.Elements), y = Elapsed.Time/1000, 
  #                          shape = Visualization.Technique, colour = Visualization.Technique)) +  # , size=0.01, 
  #   geom_point(size=2) + 
  #   theme(panel.background = element_blank(),panel.grid.major.y = element_line(colour = "grey50"),
  #         legend.position = "top",
  #         legend.title = element_text(size = 12)) +  
  #   # face = "italic", family = "Times", colour = "red", 
  #   scale_colour_brewer(palette = "Set1") +
  #   scale_shape_manual(values = c(1,2)) +
  #   xlab("Number of Pairs") +
  #   ylab("Elapsed Time in seconds") + 
  #   #  labs(colour = "Visualization Method", shape="Visualization Method") +
  #   labs(colour = "Visualization Technique", shape="Visualization Technique") +
  #   scale_y_continuous(breaks=seq(0, 220, 20)) + 
  #   # guides(colour = FALSE) + # Does not work
  #   geom_jitter(width = 0.25)    
  
  
  # ggplot(t2.vt.ne.tot, aes(x = as.factor(Number.Elements), y = Elapsed.Time/1000, 
  #                          shape = Accuracy, colour = Accuracy)) +  # , size=0.01, 
  #   geom_point(size=2) + 
  #   theme(panel.background = element_blank(),panel.grid.major.y = element_line(colour = "grey50"),
  #         legend.position = "top",
  #         legend.title = element_text(size = 12)) +  
  #   # face = "italic", family = "Times", colour = "red", 
  #   scale_colour_brewer(palette = "Set1") +
  #   scale_shape_manual(values = c(1,2)) +
  #   xlab("Number of Pairs") +
  #   ylab("Elapsed Time in seconds") + 
  #   #  labs(colour = "Visualization Method", shape="Visualization Method") +
  #   labs(colour = "Accuracy", shape="Accuracy") +
  #   scale_y_continuous(breaks=seq(0, 220, 20)) + 
  #   # guides(colour = FALSE) + # Does not work
  #   geom_jitter(width = 0.25)  

  # ggplot(t3.vt.ne.tot, aes(x = as.factor(Number.Elements), y = Elapsed.Time/1000, 
  #                          shape = Visualization.Technique, colour = Visualization.Technique)) +  # , size=0.01, 
  #   geom_point(size=2) + 
  #   theme(panel.background = element_blank(),panel.grid.major.y = element_line(colour = "grey50"),
  #         legend.position = "top",
  #         legend.title = element_text(size = 12)) +  
  #   scale_colour_brewer(palette = "Set1") +
  #   scale_shape_manual(values = c(1,2)) +
  #   xlab("Number of Triplets") +
  #   ylab("Elapsed Time in seconds") + 
  #   #  labs(colour = "Visualization Method", shape="Visualization Method") +
  #   labs(colour = "Visualization Technique", shape="Visualization Technique") +
  #   scale_y_continuous(breaks=seq(0, 480, 40)) + 
  #   geom_jitter(width = 0.25)
  
  
  
    
  
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
