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

# Percentage inaccurate
accuracy.general[1,2]/nrow(experiment.data)
# Percentage accurate
accuracy.general[2,2]/nrow(experiment.data)

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
# experiment.data %>% 
#   mutate(Accuracy = case_when(Accuracy == "True" ~ "Accurate", Accuracy == "False" ~ "Inacurate"))
# 
# # Data for the Accuracy table
# 
# experiment.data %>% 
#   mutate(Accuracy = case_when(Accuracy == "True" ~ "Accurate", Accuracy == "False" ~ "Inacurate"))


## Correct responses per participant
correct.participant <- experiment.data %>% filter (Accuracy=="True") %>% 
  group_by(Participant.ID) %>%
  summarise(count=n()) %>% as.data.frame()

summary(correct.participant$count)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 8.00   13.00   14.00   13.54   15.25   16.00 

sd(correct.participant$count)
# 2.283764

# Correct responses per question
correct.question <- experiment.data %>% filter (Accuracy=="True") %>% 
  group_by(Question.Number) %>%
  summarise(count=n()) %>% as.data.frame()

summary(correct.question$count)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 10.00   17.75   23.00   20.31   23.25   24.00 

sd(correct.question$count)
# 4.316152


## Correct responses by t value
correct.participant.t <- experiment.data %>% filter (Accuracy=="True") %>% 
  select(Participant.ID,T) %>%
  group_by(Participant.ID,T) %>%
  summarise(count=n()) %>% as.data.frame()

accurate.t2 <- correct.participant.t %>% filter (T==2)
accurate.t2
sd(accurate.t2$count)

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


accurate.t3 <- correct.participant.t %>% filter (T==3)
accurate.t3
sd(accurate.t3$count)



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

sd(correct.participant.vm.sp$total)

# Parallel dimensions Plot 2D-PD or 3D-PD
correct.participant.vm.pd <- correct.participant.vm %>% 
  filter (Visualization.Technique=="2D-PD" | Visualization.Technique=="3D-PD") %>% 
  select(Participant.ID,count) %>%
  group_by(Participant.ID) %>% summarise(total=sum(count)) %>% as.data.frame()

summary(correct.participant.vm.pd$total)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.000   5.750   7.000   6.708   8.000   8.000

sd(correct.participant.vm.pd$total)



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
  labs(x="Task Number", y="Frequency")  +
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



##############################################################################################################################
# Figure 8 revised. Facet plots for accuracy distributed by t and number of pairs
# Change of colors and substitution of FALSE and TRUE
# Flip coordinates, add guides

# Pairs 
# t=2 value,  grouping by visualization technique, number of pairs, and accuracy with summarizing by counting
t2.vm.ne.acc <- experiment.data %>% filter (T==2) %>% select(Visualization.Technique,Number.Elements,Accuracy) %>%
  group_by(Visualization.Technique,Number.Elements,Accuracy) %>%
  summarise(Count=n()) %>% as.data.frame() %>% 
  # Completing the table with the values that are zero
  add_row(Visualization.Technique="2D-PD", Number.Elements=136, Accuracy="False", Count=0) %>%
  add_row(Visualization.Technique="2D-SP", Number.Elements=209, Accuracy="False", Count=0) %>%
  add_row(Visualization.Technique="2D-SP", Number.Elements=440, Accuracy="False", Count=0)


accuracy.t2.sp.npairs <- t2.vm.ne.acc %>% filter(Visualization.Technique=="2D-SP") %>%   
  mutate(Accuracy = case_when(Accuracy == "True" ~ "Accurate", Accuracy == "False" ~ "Inacurate")) %>%
  ggplot(aes(x=as.factor(Number.Elements), y=Count, fill=Accuracy)) + # , alpha=0.5
  coord_cartesian(ylim = c(0, 24)) +
  scale_y_discrete(limits=seq(0, 24, 1)) +
  geom_bar(position="dodge",stat="identity") + # 
  # geom_text(aes(label = ..count..), vjust = -0.1, size = 5) + # aes(label = ..count..), stat = "count", 
  # geom_text(aes(label = count),  hjust= 1, vjust = -0.1, position = position_dodge(1), size = 5) +  
  theme( panel.background = element_blank(), 
        panel.grid.major.y = element_line(colour = "grey50")) + # , legend.position = "none"
  ggtitle("Scatter Plots for Pairs (2D-SP)") +
  labs(y="Count", x="Number of Pairs") +
  scale_fill_manual(values=c("#009E73","#0072B2"))
accuracy.t2.sp.npairs


accuracy.t2.pd.npairs <- t2.vm.ne.acc %>% filter(Visualization.Technique=="2D-PD") %>%   
  mutate(Accuracy = case_when(Accuracy == "True" ~ "Accurate", Accuracy == "False" ~ "Inacurate")) %>%
  ggplot(aes(x=as.factor(Number.Elements), y=Count, fill=Accuracy)) + # , alpha=0.5
  coord_cartesian(ylim = c(0, 24)) +
  scale_y_discrete(limits=seq(0, 24, 1)) +
  geom_bar(position="dodge",stat="identity") + # 
  # geom_text(aes(label = ..count..), vjust = -0.1, size = 5) + # aes(label = ..count..), stat = "count", 
  # geom_text(aes(label = count),  hjust= 1, vjust = -0.1, position = position_dodge(1), size = 5) +  
  theme( panel.background = element_blank(), 
         panel.grid.major.y = element_line(colour = "grey50"), legend.position = "none") +
  ggtitle("Parallel Dimensions Plots for Pairs (2D-PD)") +
  labs(y="Count", x="Number of Pairs") +
  scale_fill_manual(values=c("#009E73","#0072B2"))
accuracy.t2.pd.npairs


# t=3 value,  grouping by visualization technique, number of pairs, and accuracy with summarizing by counting
t3.vm.ne.acc <- experiment.data %>% filter (T==3) %>% select(Visualization.Technique,Number.Elements,Accuracy) %>%
  group_by(Visualization.Technique,Number.Elements,Accuracy) %>%
  summarise(Count=n()) %>% as.data.frame() %>% 
  # Completing the table with the values that are zero
  add_row(Visualization.Technique="3D-SP", Number.Elements=2491, Accuracy="False", Count=0)


accuracy.t3.sp.ntriplets <- t3.vm.ne.acc %>% filter(Visualization.Technique=="3D-SP") %>%   
  mutate(Accuracy = case_when(Accuracy == "True" ~ "Accurate", Accuracy == "False" ~ "Inacurate")) %>%
  ggplot(aes(x=as.factor(Number.Elements), y=Count, fill=Accuracy)) + # , alpha=0.5
  coord_cartesian(ylim = c(0, 24)) +
  scale_y_discrete(limits=seq(0, 24, 1)) +
  geom_bar(position="dodge",stat="identity") + # 
  # geom_text(aes(label = ..count..), vjust = -0.1, size = 5) + # aes(label = ..count..), stat = "count", 
  # geom_text(aes(label = count),  hjust= 1, vjust = -0.1, position = position_dodge(1), size = 5) +  
  theme( panel.background = element_blank(), 
         panel.grid.major.y = element_line(colour = "grey50"), legend.position = "none") +
  ggtitle("Scatter Plots for Triplets (3D-SP)") +
  labs(y="Count", x="Number of Triplets") +
  scale_fill_manual(values=c("#009E73","#0072B2"))
accuracy.t3.sp.ntriplets


accuracy.t3.pd.ntriplets <- t3.vm.ne.acc %>% filter(Visualization.Technique=="3D-PD") %>%   
  mutate(Accuracy = case_when(Accuracy == "True" ~ "Accurate", Accuracy == "False" ~ "Inacurate")) %>%
  ggplot(aes(x=as.factor(Number.Elements), y=Count, fill=Accuracy)) + # , alpha=0.5
  coord_cartesian(ylim = c(0, 24)) +
  scale_y_discrete(limits=seq(0, 24, 1)) +
  geom_bar(position="dodge",stat="identity") + # 
  # geom_text(aes(label = ..count..), vjust = -0.1, size = 5) + # aes(label = ..count..), stat = "count", 
  # geom_text(aes(label = count),  hjust= 1, vjust = -0.1, position = position_dodge(1), size = 5) +  
  theme( panel.background = element_blank(), 
         panel.grid.major.y = element_line(colour = "grey50"), legend.position = "none") +
  ggtitle("Parallel Dimensions Plots for Triplets (3D-PD)") +
  labs(y="Count", x="Number of Triplets") +
  scale_fill_manual(values=c("#009E73","#0072B2"))
accuracy.t3.pd.ntriplets


# Test of merging the plots into a single figure
# https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots
library(cowplot)


# Plot with the four combinations, pairs/triples & SP and PD plots
prow4 <- plot_grid(accuracy.t3.sp.ntriplets + theme(legend.position="none"), 
                   accuracy.t3.pd.ntriplets + theme(legend.position="none"),
                   accuracy.t2.sp.npairs + theme(legend.position="none"),
                   accuracy.t2.pd.npairs + theme(legend.position="none"),
                   align = 'vh',
                   hjust = -1,
                   nrow = 2)


# prow <- plot_grid(t3.sp.aois.plot + theme(legend.position="none"),
#                   t3.pd.aois.plot + theme(legend.position="none"),
#                   align = 'vh',
#                   # labels = c("CTC", "no CTC"),
#                   hjust = -1,
#                   nrow = 1)

legend_b <- get_legend(accuracy.t3.sp.ntriplets + theme(legend.position="bottom"))


accuracy.breakdown.plot <- plot_grid( prow4, legend_b, ncol = 1, rel_heights = c(1, .1)) # rel_heights = c(1, .2)
accuracy.breakdown.plot



####################################################################################
# Figure 9 revised
# Change the values FALSE TRUE to Inaccurate Accurate and colors of palette

### RQ2 Time on task

# Grid of Time on task per t value and visualization method

# t2 and parallel dimensions
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


# t2 and scatter plot
bp.t2.sp.res <- experiment.data %>% 
  mutate(Accuracy = case_when(Accuracy == 'True' ~ "Accurate", Accuracy == 'False' ~ "Inaccurate")) %>%
  filter (T==2 & Visualization.Technique=="2D-SP") %>%
  ggplot(aes(x=Accuracy, group=Accuracy,Elapsed.Time/1000)) + 
  geom_boxplot(aes(fill=Elapsed.Time/1000), varwidth=T,  fill=c("#009E73","#0072B2"), alpha=0.5) + 
  geom_jitter() +
  labs(x="Scatter Plot", y="T=2") + 
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
  scale_y_continuous(breaks=seq(0, 350, 50),limits = c(0, 350))

bp.t2.sp.res


# t3 and parallel dimensions
bp.t3.pd.res <- experiment.data %>% 
  mutate(Accuracy = case_when(Accuracy == 'True' ~ "Accurate", Accuracy == 'False' ~ "Inaccurate")) %>%
  filter (T==3 & Visualization.Technique=="3D-PD") %>%
  ggplot(aes(x=Accuracy, group=Accuracy,Elapsed.Time/1000)) +  
  scale_y_continuous(breaks=seq(0, 350, 50), limits = c(0, 350)) +
  geom_boxplot(aes(fill=Elapsed.Time/1000), varwidth=T,  fill=c("#009E73","#0072B2"), alpha=0.5) +  
  geom_jitter() +
  labs(x="Parallel Dimensions Plot", y="T=3") + 
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) 

bp.t3.pd.res


# t3 and scatter plot
bp.t3.sp.res <- experiment.data %>% 
  mutate(Accuracy = case_when(Accuracy == 'True' ~ "Accurate", Accuracy == 'False' ~ "Inaccurate")) %>%
  filter (T==3 & Visualization.Technique=="3D-SP") %>%
  ggplot(aes(x=Accuracy, group=Accuracy,Elapsed.Time/1000)) +  
  geom_boxplot(aes(fill=Elapsed.Time/1000), varwidth=T,  fill=c("#009E73","#0072B2"), alpha=0.5) +  
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


############# Time-on-task descriptive statistics

### Note: Elapsed.Time is the time taken by the web interface, this is the one used for reporting the results
# For research question Q2 time-on-task

## Total Elapsed time by participants (incorrect and correct answers)
elapsed.time.participant <- experiment.data %>%
  select(Participant.ID, Elapsed.Time) %>%
  group_by(Participant.ID) %>%
  summarise(total=sum(Elapsed.Time)/1000)  # Total time in minutes per participant


summary(elapsed.time.participant$total)
# Values in seconds
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 367.5   815.9  1010.2  1047.4  1326.2  1497.6 

# Values in minutes
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6.125  13.598  16.836  17.457  22.103  24.960 

sd(elapsed.time.participant$total)

# Seconds
# 304.4128

# Minutes
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






###################################################################################
# Figure 10 revised
# Change the values FALSE TRUE to Inaccurate Accurate and colors of palette

### Summary of time-on-task based on values of t and visualization techniques

## For t=2
t2.vt.ne.tot <- experiment.data %>% filter (T==2) %>% 
  select(Visualization.Technique,Number.Elements,Elapsed.Time,Accuracy)

# Scatter plot task on time summary accurate/inaccurate for number of pairs 
tot.t2.sp.breakdown <- t2.vt.ne.tot %>% filter(Visualization.Technique=="2D-SP") %>%
  mutate(Accuracy = case_when(Accuracy == 'True' ~ "Accurate", Accuracy == 'False' ~ "Inaccurate")) %>%
  ggplot(aes(x=as.factor(Number.Elements), y=Elapsed.Time/1000, fill=Accuracy)) + 
  geom_boxplot(varwidth=T) +
  geom_jitter(width = 0.1) +
  facet_wrap(~Accuracy) + 
  # theme(legend.position="bottom") +
  theme(panel.background = element_blank(), legend.position="none", panel.grid.major.y = element_line(colour = "grey50")) +
  ggtitle("Scatter Plots for Pairs (2D-SP)") +
  labs(x="Number of Pairs", y="Time-on-task seconds") + 
  scale_y_continuous(breaks=seq(0, 350, 50),limits = c(0, 350)) +
  scale_fill_manual(values=c("#009E73","#0072B2"))
tot.t2.sp.breakdown

# Parallel dimensions plot task on time summary accurate/inaccurate for number of pairs 
tot.t2.pd.breakdown <- t2.vt.ne.tot %>% filter(Visualization.Technique=="2D-PD") %>%
  mutate(Accuracy = case_when(Accuracy == 'True' ~ "Accurate", Accuracy == 'False' ~ "Inaccurate")) %>%
  ggplot(aes(x=as.factor(Number.Elements), y=Elapsed.Time/1000, fill=Accuracy)) + 
  geom_boxplot(varwidth=T) +
  geom_jitter(width = 0.1) +
  facet_wrap(~Accuracy) + 
  # theme(legend.position="bottom") +
  theme(panel.background = element_blank(), legend.position="none", panel.grid.major.y = element_line(colour = "grey50")) +
  ggtitle("Parallel Dimensions Plots for Pairs (2D-PD)") +
  labs(x="Number of Pairs", y="Time-on-task seconds") + 
  scale_y_continuous(breaks=seq(0, 350, 50),limits = c(0, 350)) +
  scale_fill_manual(values=c("#009E73","#0072B2"))
tot.t2.pd.breakdown


# For t=3 
t3.vt.ne.tot <- experiment.data %>% filter (T==3) %>% 
 select(Visualization.Technique,Number.Elements,Elapsed.Time,Accuracy)

# Scatter plot task on time summary accurate/inaccurate for number of triples 
tot.t3.sp.breakdown <- t3.vt.ne.tot %>% filter(Visualization.Technique=="3D-SP") %>%
  mutate(Accuracy = case_when(Accuracy == 'True' ~ "Accurate", Accuracy == 'False' ~ "Inaccurate")) %>%
  ggplot(aes(x=as.factor(Number.Elements), y=Elapsed.Time/1000, fill=Accuracy)) + 
  geom_boxplot(varwidth=T) +
  geom_jitter(width = 0.1) +
  facet_wrap(~Accuracy) + 
  # theme(legend.position="bottom") +
  theme(panel.background = element_blank(), legend.position="none", panel.grid.major.y = element_line(colour = "grey50")) +
  ggtitle("Scatter Plots for Triplets (3D-SP)") +
  labs(x="Number of Triplets", y="Time-on-task seconds") + 
  scale_y_continuous(breaks=seq(0, 350, 50),limits = c(0, 350)) +
  scale_fill_manual(values=c("#009E73","#0072B2"))
tot.t3.sp.breakdown

# Parallel dimensions plot task on time summary accurate/inaccurate for number of triplets 
tot.t3.pd.breakdown <- t3.vt.ne.tot %>% filter(Visualization.Technique=="3D-PD") %>%
  mutate(Accuracy = case_when(Accuracy == 'True' ~ "Accurate", Accuracy == 'False' ~ "Inaccurate")) %>%
  ggplot(aes(x=as.factor(Number.Elements), y=Elapsed.Time/1000, fill=Accuracy)) + 
  geom_boxplot(varwidth=T) +
  geom_jitter(width = 0.1) +
  facet_wrap(~Accuracy) + 
  # theme(legend.position="bottom") +
  theme(panel.background = element_blank(), legend.position="none", panel.grid.major.y = element_line(colour = "grey50")) +
  ggtitle("Parallel Dimensions Plots for Triplets (3D-PD)") +
  labs(x="Number of Triplets", y="Time-on-task seconds") + 
  scale_y_continuous(breaks=seq(0, 350, 50),limits = c(0, 350)) +
  scale_fill_manual(values=c("#009E73","#0072B2"))
tot.t3.pd.breakdown
 
# Time on task breakdown by accurate and inaccurate, triplets and pairs
prow4.tot.breakdown <- plot_grid(tot.t3.sp.breakdown + theme(legend.position="none"), 
                   tot.t3.pd.breakdown  + theme(legend.position="none"),
                   tot.t2.sp.breakdown  + theme(legend.position="none"),
                   tot.t2.pd.breakdown + theme(legend.position="none"),
                   align = 'vh',
                   hjust = -1,
                   nrow = 2)
prow4.tot.breakdown



##################################################################################
## RQ3

## Difficulty assessment 
his.difficulty <- ggplot(experiment.data, aes(x=Difficulty.Level)) + 
  geom_histogram(binwidth=1, stat="count", fill="blue") +
  stat_count(binwidth = 1, geom = 'text', color = 'black', aes(label = ..count..), vjust= -0.4,) +
  scale_x_continuous(breaks=seq(0, 20, 1)) +
  scale_y_continuous(breaks=seq(0, 380, 20)) + 
  xlab("Difficulty Assessment, Very easy=0 ... Very difficult=20") +
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
  xlab("Certainty Assessment, Perfect=0 (completely sure) ... Failure=20 (completely unsure)") +
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


###################
########### RQ4

# Figure 12 and Descriptive statistics

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


###################################################################################
# Figure 13 revised
# Change the plots to percentage bars with labels at the bottom

# TODO adjust plot to be bars, change to percentage, change palette to make it colour friendly

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
  ggplot(aes(x=AOI, y=Percentage, fill=AOI)) +
  geom_bar(stat="identity") +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50"), 
        axis.title.x = element_blank(), axis.text.x=element_blank(),
        legend.position = "bottom") +
labs(y="Proportion of Fixation Time") +
ggtitle("Scatter Plots for Pairs (2D-SP)") +
scale_y_continuous(limits=c(0, 0.40), breaks=seq(0, 0.40, 0.05)) +
scale_fill_manual(values=cbPalette)  
proportion.time.t2.sp

proportion.count.t2.sp <-  t2.sp.df.renamed %>% filter(Measure=="count") %>%
  ggplot(aes(x=AOI, y=Percentage, fill=AOI)) +
  geom_bar(stat="identity") +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50"), 
        axis.title.x = element_blank(), axis.text.x=element_blank(),
        legend.position = "bottom") +
  labs(y="Proportion of Fixation Count") +
  ggtitle("Scatter Plots for Pairs (2D-SP)") +
  scale_y_continuous(limits=c(0, 0.40), breaks=seq(0, 0.40, 0.05)) +
  scale_fill_manual(values=cbPalette)  
proportion.count.t2.sp

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
t2.pd.df$AOI <- as.factor(t2.pd.df$AOI)


# # makes Measure factor values instead of characters
# t2.sp.df$Measure <- factor(t2.sp.df$Measure, levels = c('time', 'count'))
# t2.sp.df$AOI <- as.factor(t2.sp.df$AOI)

# Mutates the names of the AOIs, solution and misc
t2.pd.df.renamed <- t2.pd.df %>%
  mutate(AOI = case_when(AOI == "Misc" ~ "Stimulus", AOI == "Response" ~ "Answer", TRUE ~ AOI)) %>%
  mutate(AOI=fct_relevel(AOI,c("Question","Answer","Axial","Target","Solution","Navigation","Stimulus")))


# Color blind palette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
# cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999")

proportion.time.t2.pd <-  t2.pd.df.renamed %>% filter(Measure=="time") %>%
  ggplot(aes(x=AOI, y=Percentage, fill=AOI)) +
  geom_bar(stat="identity") +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50"), 
        axis.title.x = element_blank(),axis.text.x=element_blank(),
        legend.position = "bottom") +
  labs(y="Proportion of Fixation Time") +
  ggtitle("Parallel Dimensions Plots for Pairs (2D-PD)") +
  scale_y_continuous(limits=c(0, 0.40), breaks=seq(0, 0.40, 0.05)) +
  scale_fill_manual(values=cbPalette)  
proportion.time.t2.pd

proportion.count.t2.pd <-  t2.pd.df.renamed %>% filter(Measure=="count") %>%
  ggplot(aes(x=AOI, y=Percentage, fill=AOI)) +
  geom_bar(stat="identity") +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50"), 
        axis.title.x = element_blank(),axis.text.x=element_blank(),
        legend.position = "bottom") +
  labs(y="Proportion of Fixation Count") +
  ggtitle("Parallel Dimensions Plots for Pairs (2D-PD)") +
  scale_y_continuous(limits=c(0, 0.40), breaks=seq(0, 0.40, 0.05)) +
  scale_fill_manual(values=cbPalette)  
proportion.count.t2.pd


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
t3.sp.df$AOI <- as.factor(t3.sp.df$AOI)

# Mutates the names of the AOIs, solution and misc
t3.sp.df.renamed <- t3.sp.df %>%
  mutate(AOI = case_when(AOI == "Misc" ~ "Stimulus", AOI == "Response" ~ "Answer", TRUE ~ AOI)) %>%
  mutate(AOI=fct_relevel(AOI,c("Question","Answer","Axial","Target","Solution","Navigation","Stimulus")))

# # Color blind palette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
# cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999")

proportion.time.t3.sp <-  t3.sp.df.renamed %>% filter(Measure=="time") %>%
  ggplot(aes(x=AOI, y=Percentage, fill=AOI)) +
  geom_bar(stat="identity") +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50"), 
        axis.title.x = element_blank(), axis.text.x=element_blank(),
        legend.position = "bottom") +
  labs(y="Proportion of Fixation Time") +
  ggtitle("Scatter Plots for Triplets (3D-SP)") +
  scale_y_continuous(limits=c(0, 0.40), breaks=seq(0, 0.40, 0.05)) +
  scale_fill_manual(values=cbPalette)  
proportion.time.t3.sp

proportion.count.t3.sp <-  t3.sp.df.renamed %>% filter(Measure=="count") %>%
  ggplot(aes(x=AOI, y=Percentage, fill=AOI)) +
  geom_bar(stat="identity") +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50"), 
        axis.title.x = element_blank(), axis.text.x=element_blank(),
        legend.position = "bottom") +
  labs(y="Proportion of Fixation Count") +
  ggtitle("Scatter Plots for Triplets (3D-SP)") +
  scale_y_continuous(limits=c(0, 0.40), breaks=seq(0, 0.40, 0.05)) +
  scale_fill_manual(values=cbPalette)  
proportion.count.t3.sp


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
t3.pd.df$AOI <- as.factor(t3.pd.df$AOI)

# Mutates the names of the AOIs, solution and misc
t3.pd.df.renamed <- t3.pd.df %>%
  mutate(AOI = case_when(AOI == "Misc" ~ "Stimulus", AOI == "Response" ~ "Answer", TRUE ~ AOI)) %>%
  mutate(AOI=fct_relevel(AOI,c("Question","Answer","Axial","Target","Solution","Navigation","Stimulus")))


proportion.time.t3.pd <-  t3.pd.df.renamed %>% filter(Measure=="time") %>%
  ggplot(aes(x=AOI, y=Percentage, fill=AOI)) +
  geom_bar(stat="identity") +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50"), 
        axis.title.x = element_blank(), axis.text.x=element_blank(),
        legend.position = "bottom") +
  labs(y="Proportion of Fixation Time") +
  ggtitle("Parallel Dimensions for Triplets (3D-PD)") +
  scale_y_continuous(limits=c(0, 0.40), breaks=seq(0, 0.40, 0.05)) +
  scale_fill_manual(values=cbPalette)  
proportion.time.t3.pd

proportion.count.t3.pd <-  t3.pd.df.renamed %>% filter(Measure=="count") %>%
  ggplot(aes(x=AOI, y=Percentage, fill=AOI)) +
  geom_bar(stat="identity") +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50"), 
        axis.title.x = element_blank(), axis.text.x=element_blank(),
        legend.position = "bottom") +
  labs(y="Proportion of Fixation Count") +
  ggtitle("Parallel Dimensions for Triplets (3D-PD)") +
  scale_y_continuous(limits=c(0, 0.40), breaks=seq(0, 0.40, 0.05)) +
  scale_fill_manual(values=cbPalette)  
proportion.count.t3.pd

###################################################################################
### Constructing the two figures for Proportions of Fixation Time and Proportions of Fixation Count 
library(cowplot)

# Summary of proportion of count
prow4.aoi.prop.count <- plot_grid(proportion.count.t3.sp + theme(legend.position="none"), 
                                 proportion.count.t3.pd  + theme(legend.position="none"),
                                 proportion.count.t2.sp + theme(legend.position="none"),
                                 proportion.count.t2.pd + theme(legend.position="none"),
                                 align = 'vh',
                                 hjust = -1,
                                 nrow = 2)
prow4.aoi.prop.count

prop.count.legend <- get_legend(proportion.count.t3.sp + theme(legend.position="bottom"))
aoi.prop.count.plot <- plot_grid( prow4.aoi.prop.count, prop.count.legend, ncol = 1, rel_heights = c(1, .1))
aoi.prop.count.plot


# Summary of proportion of time
prow4.aoi.prop.time <- plot_grid(proportion.time.t3.sp + theme(legend.position="none"), 
                                  proportion.time.t3.pd  + theme(legend.position="none"),
                                  proportion.time.t2.sp + theme(legend.position="none"),
                                  proportion.time.t2.pd + theme(legend.position="none"),
                                  align = 'vh',
                                  hjust = -1,
                                  nrow = 2)
prow4.aoi.prop.time

prop.time.legend <- get_legend(proportion.time.t3.sp + theme(legend.position="bottom"))
aoi.prop.time.plot <- plot_grid( prow4.aoi.prop.time, prop.time.legend, ncol = 1, rel_heights = c(1, .1))
aoi.prop.time.plot



##################################################################################
## Figure 14 revised

# Selects the columns for T=2, for proportion of time and proportion of count for the 7 AOIs and the aggregated total and Accuracy
rq4.data.ptime.t2 <- experiment.data %>% filter(T==2) %>% 
  select(Question.pftime,Response.pftime,Axial.pftime,Target.pftime,Solution.pftime,Navigation.pftime,Misc.pftime)

rq4.data.pcount.t2 <- experiment.data %>% filter(T==2) %>% 
  select(Question.pfcount, Response.pfcount, Axial.pfcount, Target.pfcount, Solution.pfcount, Navigation.pfcount, Misc.pfcount)


# Checking normality distribution of all the variables
shapiro.test(rq4.data.ptime.t2$Question.pftime) # W = 0.93728, p-value = 2.176e-07
shapiro.test(rq4.data.ptime.t2$Response.pftime) # W = 0.74697, p-value < 2.2e-16
shapiro.test(rq4.data.ptime.t2$Misc.pftime) # W = 0.85194, p-value = 1.071e-12
shapiro.test(rq4.data.ptime.t2$Navigation.pftime) # W = 0.94018, p-value = 3.825e-07
shapiro.test(rq4.data.ptime.t2$Axial.pftime) # W = 0.953, p-value = 5.662e-06
shapiro.test(rq4.data.ptime.t2$Solution.pftime) # W = 0.92047, p-value = 1.084e-08
shapiro.test(rq4.data.ptime.t2$Target.pftime) # W = 0.97868, p-value = 0.005001

# Note: All p-values were below < 0.05 --> 
# p-values < alpha value --> Reject H0 that values are normally distributed
# Conclusions: None of the variables are normally distributed

shapiro.test(rq4.data.pcount.t2$Question.pfcount) # W = 0.95353, p-value = 6.376e-06
shapiro.test(rq4.data.pcount.t2$Response.pfcount) # W = 0.90723, p-value = 1.328e-09
shapiro.test(rq4.data.pcount.t2$Misc.pfcount) # W = 0.85103, p-value = 9.674e-13
shapiro.test(rq4.data.pcount.t2$Navigation.pfcount) # W = 0.96328, p-value = 6.574e-05
shapiro.test(rq4.data.pcount.t2$Axial.pfcount) # W = 0.98186, p-value = 0.01377
shapiro.test(rq4.data.pcount.t2$Solution.pfcount) # W = 0.92026, p-value = 1.046e-08
shapiro.test(rq4.data.pcount.t2$Target.pfcount) # W = 0.97127, p-value = 0.000557

# Note: All p-values were below < 0.05 --> 
# p-values < alpha value --> Reject H0 that values are normally distributed
# Conclusions: None of the variables are normally distributed

# Library for showing the correlation matrix visualization
library(corrplot)
library("Hmisc")

#### Proportion of fixation times
# changing the names of the columns
colnames(rq4.data.ptime.t2) <- c("Question", "Answer","Axial","Target", "Solution", "Navigation", "Stimulus") 
correlation.ptime.t2 <- cor(rq4.data.ptime.t2, method="spearman")
# corrplot(correlation.ptime.t2, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, addCoef.col = "black")

corrplot(correlation.ptime.t2, type = "upper", order = "original", tl.col = "black", tl.srt = 45, addCoef.col = "black")


t2.ptime <- rcorr(as.matrix(rq4.data.ptime.t2), type="spearman")
t2.ptime

# Question Response  Misc Navigation Axial Solution Target
# Question       1.00     0.25  0.22      -0.43 -0.19     0.00  -0.10
# Response       0.25     1.00  0.18      -0.19 -0.26    -0.03  -0.04
# Misc           0.22     0.18  1.00      -0.11  0.00    -0.04  -0.14
# Navigation    -0.43    -0.19 -0.11       1.00 -0.32    -0.37  -0.04
# Axial         -0.19    -0.26  0.00      -0.32  1.00     0.04  -0.50
# Solution       0.00    -0.03 -0.04      -0.37  0.04     1.00  -0.01
# Target        -0.10    -0.04 -0.14      -0.04 -0.50    -0.01   1.00
# 
# n= 192 
# 
# 
# P
# Question Response Misc   Navigation Axial  Solution Target
# Question            0.0006   0.0021 0.0000     0.0082 0.9962   0.1729
# Response   0.0006            0.0109 0.0100     0.0002 0.6457   0.6216
# Misc       0.0021   0.0109          0.1139     0.9753 0.5681   0.0602
# Navigation 0.0000   0.0100   0.1139            0.0000 0.0000   0.6082
# Axial      0.0082   0.0002   0.9753 0.0000            0.5639   0.0000
# Solution   0.9962   0.6457   0.5681 0.0000     0.5639          0.8735
# Target     0.1729   0.6216   0.0602 0.6082     0.0000 0.8735         


#### Proportion of fixation count
colnames(rq4.data.pcount.t2) <- c("Question", "Answer","Axial","Target", "Solution", "Navigation", "Stimulus") 
correlation.pcount.t2 <- cor(rq4.data.pcount.t2, method="spearman")
corrplot(correlation.pcount.t2, type = "upper", order = "original", tl.col = "black", tl.srt = 45, addCoef.col = "black")

t2.pcount <- rcorr(as.matrix(rq4.data.pcount.t2), type="spearman")
t2.pcount

# Question Response  Misc Navigation Axial Solution Target
# Question       1.00     0.16  0.23      -0.58 -0.14     0.07  -0.09
# Response       0.16     1.00  0.24      -0.09 -0.20    -0.03  -0.12
# Misc           0.23     0.24  1.00      -0.19  0.10    -0.01  -0.27
# Navigation    -0.58    -0.09 -0.19       1.00 -0.31    -0.35  -0.03
# Axial         -0.14    -0.20  0.10      -0.31  1.00     0.08  -0.57
# Solution       0.07    -0.03 -0.01      -0.35  0.08     1.00  -0.07
# Target        -0.09    -0.12 -0.27      -0.03 -0.57    -0.07   1.00
# 
# n= 192 
# 
# 
# P
# Question Response Misc   Navigation Axial   Solution Target
# Question            0.0251   0.0011 0.0000     0.0532 0.3169   0.2280
# Response   0.0251            0.0008 0.2074     0.0059 0.6795   0.1044
# Misc       0.0011   0.0008          0.0072     0.1761 0.8724   0.0002
# Navigation 0.0000   0.2074   0.0072            0.0000 0.0000   0.6720
# Axial      0.0532   0.0059   0.1761 0.0000            0.2841   0.0000
# Solution   0.3169   0.6795   0.8724 0.0000     0.2841          0.3665
# Target     0.2280   0.1044   0.0002 0.6720     0.0000 0.3665         

##########################

# Selects the columns for T=3, for proportion of time and proportion of count for the 7 AOIs and the aggregated total and Accuracy
rq4.data.ptime.t3 <- experiment.data %>% filter(T==3) %>% 
  select(Question.pftime,Response.pftime,Axial.pftime,Target.pftime,Solution.pftime,Navigation.pftime,Misc.pftime)

rq4.data.pcount.t3 <- experiment.data %>% filter(T==3) %>% 
  select(Question.pfcount, Response.pfcount, Axial.pfcount, Target.pfcount, Solution.pfcount, Navigation.pfcount, Misc.pfcount)

# Checking normality distribution of all the variables
shapiro.test(rq4.data.ptime.t3$Question.pftime) 
shapiro.test(rq4.data.ptime.t3$Response.pftime) 
shapiro.test(rq4.data.ptime.t3$Misc.pftime) 
shapiro.test(rq4.data.ptime.t3$Navigation.pftime) 
shapiro.test(rq4.data.ptime.t3$Axial.pftime) 
shapiro.test(rq4.data.ptime.t3$Solution.pftime) 
shapiro.test(rq4.data.ptime.t3$Target.pftime) 

# Note: All p-values were below < 0.05 --> 
# p-values < alpha value --> Reject H0 that values are normally distributed
# Conclusions: None of the variables are normally distributed

shapiro.test(rq4.data.pcount.t3$Question.pfcount) 
shapiro.test(rq4.data.pcount.t3$Response.pfcount) 
shapiro.test(rq4.data.pcount.t3$Misc.pfcount) 
shapiro.test(rq4.data.pcount.t3$Navigation.pfcount) 
shapiro.test(rq4.data.pcount.t3$Axial.pfcount) 
shapiro.test(rq4.data.pcount.t3$Solution.pfcount) 
shapiro.test(rq4.data.pcount.t3$Target.pfcount) 

# Note: All p-values were below < 0.05 --> 
# p-values < alpha value --> Reject H0 that values are normally distributed
# Conclusions: None of the variables are normally distributed

# Library for showing the correlation matrix visualization
library(corrplot)
library("Hmisc")

#### Proportion of fixation times
# changing the names of the columns
colnames(rq4.data.ptime.t3) <- c("Question", "Answer","Axial","Target", "Solution", "Navigation", "Stimulus") 
correlation.ptime.t3 <- cor(rq4.data.ptime.t3, method="spearman")
corrplot(correlation.ptime.t3, type = "upper", order = "original", tl.col = "black", tl.srt = 45, addCoef.col = "black")

t3.ptime <- rcorr(as.matrix(rq4.data.ptime.t3), type="spearman")
t3.ptime

#             Question  Response  Misc Navigation Axial Solution Target
# Question       1.00     0.37  0.07      -0.36  0.15     0.19  -0.29
# Response       0.37     1.00 -0.20      -0.52  0.21     0.34  -0.21
# Misc           0.07    -0.20  1.00       0.13 -0.37    -0.43  -0.09
# Navigation    -0.36    -0.52  0.13       1.00 -0.50    -0.40   0.08
# Axial          0.15     0.21 -0.37      -0.50  1.00     0.36  -0.48
# Solution       0.19     0.34 -0.43      -0.40  0.36     1.00  -0.20
# Target        -0.29    -0.21 -0.09       0.08 -0.48    -0.20   1.00
# 
# n= 192 
# 
# 
# P
# Question Response Misc   Navigation Axial  Solution Target
# Question            0.0000   0.3595 0.0000     0.0322 0.0097   0.0000
# Response   0.0000            0.0065 0.0000     0.0039 0.0000   0.0035
# Misc       0.3595   0.0065          0.0681     0.0000 0.0000   0.2149
# Navigation 0.0000   0.0000   0.0681            0.0000 0.0000   0.2983
# Axial      0.0322   0.0039   0.0000 0.0000            0.0000   0.0000
# Solution   0.0097   0.0000   0.0000 0.0000     0.0000          0.0064
# Target     0.0000   0.0035   0.2149 0.2983     0.0000 0.0064         




#### Proportion of fixation count
colnames(rq4.data.pcount.t3) <- c("Question", "Answer","Axial","Target", "Solution", "Navigation", "Stimulus") 
correlation.pcount.t3 <- cor(rq4.data.pcount.t3, method="spearman")
corrplot(correlation.pcount.t3, type = "upper", order = "original", tl.col = "black", tl.srt = 45, addCoef.col = "black")

t3.pcount <- rcorr(as.matrix(rq4.data.pcount.t3), type="spearman")
t3.pcount


#             Question Response  Misc Navigation Axial Solution Target
# Question       1.00     0.33  0.06      -0.46  0.08     0.20  -0.30
# Response       0.33     1.00 -0.15      -0.41  0.23     0.34  -0.28
# Misc           0.06    -0.15  1.00      -0.05 -0.47    -0.45   0.09
# Navigation    -0.46    -0.41 -0.05       1.00 -0.45    -0.27   0.09
# Axial          0.08     0.23 -0.47      -0.45  1.00     0.36  -0.53
# Solution       0.20     0.34 -0.45      -0.27  0.36     1.00  -0.23
# Target        -0.30    -0.28  0.09       0.09 -0.53    -0.23   1.00
# 
# n= 192 
# 
# 
# P
# Question Response Misc   Navigation Axial  Solution Target
# Question            0.0000   0.3805 0.0000     0.2588 0.0061   0.0000
# Response   0.0000            0.0382 0.0000     0.0011 0.0000   0.0000
# Misc       0.3805   0.0382          0.4649     0.0000 0.0000   0.2165
# Navigation 0.0000   0.0000   0.4649            0.0000 0.0002   0.1957
# Axial      0.2588   0.0011   0.0000 0.0000            0.0000   0.0000
# Solution   0.0061   0.0000   0.0000 0.0002     0.0000          0.0014
# Target     0.0000   0.0000   0.2165 0.1957     0.0000 0.0014   


################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################

## Scratch code

# ## Computing the average percentages per question  
# 
# # Test of merging the plots into a single figure
# # https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots
# library(cowplot)
# prow <- plot_grid(t3.sp.aois.plot + theme(legend.position="none"),
#                   t3.pd.aois.plot + theme(legend.position="none"),
#                   align = 'vh',
#                   # labels = c("CTC", "no CTC"),
#                   hjust = -1,
#                   nrow = 1)
# 
# legend_b <- get_legend(t3.pd.aois.plot + theme(legend.position="bottom"))
# aois.plot <- plot_grid( prow, legend_b, ncol = 1, rel_heights = c(1, .2))
# aois.plot
# 
# 
# # Plot with the four combinations, mean percentage of fixation time and fixation count
# prow4 <- plot_grid(t3.sp.aois.plot + theme(legend.position="none"),
#                    t3.pd.aois.plot + theme(legend.position="none"),
#                    t2.sp.aois.plot + theme(legend.position="none"),
#                    t2.pd.aois.plot + theme(legend.position="none"),
#                    align = 'vh',
#                    hjust = -1,
#                    nrow = 2)
# legend_b4 <- get_legend(t3.pd.aois.plot + theme(legend.position="bottom"))
# aois.plot4 <- plot_grid( prow4, legend_b4, ncol = 1, rel_heights = c(1, .2))
# aois.plot4

# # Deprecated Figure 13
# t3.pd.aois.plot <- t3.pd.df %>% ggplot(aes(fill=AOI, y=Percentage, x=Measure)) + 
#   geom_bar(position="fill", stat="identity", alpha=0.7) +
#   theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
#   labs(x="t=3 and parallel dimensions plot", y="") 
# t3.pd.aois.plot

# Old figure 13
# t3.sp.aois.plot <- t3.sp.df %>% ggplot(aes(fill=AOI, y=Percentage, x=Measure)) + 
#   geom_bar(position="fill", stat="identity", alpha=0.7) +
#   theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
#   labs(x="t=3 and scatter plot", y="") 
# t3.sp.aois.plot

# Deprecated
# t2.pd.aois.plot <- t2.pd.df %>% ggplot(aes(fill=AOI, y=Percentage, x=Measure)) + 
#   geom_bar(position="fill", stat="identity", alpha=0.7) +
#   theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
#   labs(x="t=2 and parallel dimensions plot", y="") 
# t2.pd.aois.plot

# # Copied example of barplots for accuracy results
# question.accuracy <- experiment.data %>% 
#   mutate(Accuracy = case_when(Accuracy == "True" ~ "Accurate", Accuracy == "False" ~ "Inacurate")) %>%
#   ggplot(aes(x=Question.Number, fill=Accuracy)) +
#   coord_cartesian(ylim = c(0, 24)) +
#   scale_y_discrete(limits=seq(0, 24, 1)) +
#   scale_x_discrete(limits=seq(1, 16, 1)) +
#   geom_bar(position="dodge") +
#   theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50"), 
#         legend.position = "bottom") + 
#   labs(x="Question Number", y="Frequency")  +
#   scale_fill_manual(values=c("#009E73","#0072B2"))  
# question.accuracy

# deprecated all rainbow graph Figure 13
# t2.sp.aois.plot <- t2.sp.df %>% ggplot(aes(fill=AOI, y=Percentage, x=Measure)) + 
#   geom_bar(position="fill", stat="identity", alpha=0.7) +
#   theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
#   labs(x="t=2 and scatter plot", y="") 
# t2.sp.aois.plot

# ggplot(t3.vt.ne.tot, aes(alpha=0.5, y=Elapsed.Time/1000, x=as.factor(Number.Elements), colour=Accuracy)) +
#   geom_point(size=5, alpha=0.5, shape=21, stroke=1) +
#   facet_grid(Visualization.Technique ~ . , scales = "free", space = "free") +  # vertical facets
#   theme(legend.position="none") +
#   scale_color_manual(values = c("red","green")) +
#   xlab("Number of Triplets") +
#   ylab("Time-on-task seconds") +  
#   guides(alpha=FALSE) +
#   coord_flip() +
#   theme(strip.text.x = element_text(angle = 0)) +
#   theme_classic()


# t2.vt.ne.tot %>% filter(Visualization.Technique=="2D-PD") %>%
#   ggplot(aes(fill=Accuracy, y=Elapsed.Time/1000, x=as.factor(Number.Elements))) + 
#   geom_violin(alpha=0.5, outlier.colour="transparent") + # position="dodge", 
#   geom_jitter(width = 0.1) +
#   theme(legend.position="bottom")
#   
# # # Code example
# data %>%
#   mutate(day = fct_reorder(day, tip)) %>%
#   mutate(day = factor(day, levels=c("Thur", "Fri", "Sat", "Sun"))) %>%
#   ggplot(aes(fill=sex, y=tip, x=day)) + 
#   geom_violin(position="dodge", alpha=0.5, outlier.colour="transparent") +
#   scale_fill_viridis(discrete=T, name="") +
#   theme_ipsum()  +
#   xlab("") +
#   ylab("Tip (%)") +
#   ylim(0,40)
# 
# 
# 
# 
# 
# ggplot(t2.vt.ne.tot, aes(alpha=0.5, y=Elapsed.Time/1000, x=as.factor(Number.Elements), colour=Accuracy)) +
#   geom_point(size=5, alpha=0.5, shape=21, stroke=1) +
#   facet_grid(Visualization.Technique ~ . , scales = "free", space = "free") +  # vertical facets
#   theme(legend.position="none") +
#   scale_color_manual(values = c("red","green")) +
#   xlab("Number of Pairs") +
#   ylab("Time-on-task seconds") +  
#   guides(alpha=FALSE) +
#   coord_flip() +
#   theme(strip.text.x = element_text(angle = 0)) +
#   theme_classic()
# 
# 

# tot.t2.sp.breakdown <- t2.vt.ne.tot %>% filter(Visualization.Technique=="2D-SP") %>%
#   ggplot(aes(x=as.factor(Number.Elements), y=Elapsed.Time/1000, fill=Accuracy)) + 
#   geom_boxplot(varwidth=T) +
#   geom_jitter(width = 0.1) +
#   theme(legend.position="bottom")
# tot.t2.sp.breakdown
# 
# bp.t3.sp.res <- experiment.data %>% 
#   mutate(Accuracy = case_when(Accuracy == 'True' ~ "Accurate", Accuracy == 'False' ~ "Inaccurate")) %>%
#   filter (T==3 & Visualization.Technique=="3D-SP") %>%
#   ggplot(aes(x=Accuracy, group=Accuracy,Elapsed.Time/1000)) +  
#   geom_boxplot(aes(fill=Elapsed.Time/1000), varwidth=T,  fill=c("#009E73","#0072B2"), alpha=0.5) +  
#   geom_jitter() +
#   labs(x="Scatter Plot", y="T=3") + 
#   theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
#   ylim(0, 550)  +
#   scale_y_continuous(breaks=seq(0, 350, 50),limits = c(0, 350))




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


# ggplot(t3.vm.ne.acc, aes(fill = Accuracy, alpha=0.5, y=count, x=as.factor(Number.Elements))) + 
#   geom_bar(position="dodge", stat="identity") + 
#   geom_text(aes(label = count),  hjust= -0.4, position = position_dodge(1), size = 3.5) +   
#   #  ggtitle("Accuracy results by Visualization Technique and Number of Triplets") +
#   ggtitle("     Scatter Plot (3D-SP)     Parallel Dimensions (3D-PD)") +
#   facet_grid(Visualization.Technique ~ . , scales = "free", space = "free") +  # vertical facets
#   theme(legend.position="none") +
#   scale_fill_manual(values=c("red", "green")) +
#   xlab("Number of Triplets") +
#   ylab("Count") + 
#   guides(alpha=FALSE) +
#   coord_flip() +
#   theme(strip.text.x = element_text(angle = 0)) +
#   scale_y_continuous(breaks=seq(0, 24, 1)) +
#   theme_classic() -> accuracy.vm.ne.t3 
# 
# accuracy.vm.ne.t3

# # Revised Figure 6a       
# participant.accuracy <- experiment.data %>%   
#   mutate(Accuracy = case_when(Accuracy == "True" ~ "Accurate", Accuracy == "False" ~ "Inacurate")) %>%
#   ggplot(aes(x=Participant.ID, fill=Accuracy)) + # , alpha=0.5
#   coord_cartesian(ylim = c(0, 16)) +
#   scale_y_discrete(limits=seq(0, 16, 1)) +
#   geom_bar(position="dodge") +
#   theme(axis.text.x=element_blank(), panel.background = element_blank(), 
#         panel.grid.major.y = element_line(colour = "grey50"), legend.position = "bottom") +
#   labs(y="Frequency", x="Participant responses") +
#   scale_fill_manual(values=c("#009E73","#0072B2"))
# participant.accuracy

# # ---- Original version
# ggplot(t2.vm.ne.acc, aes(fill = Accuracy, alpha=0.5, y=count, x=as.factor(Number.Elements))) + 
#   geom_bar(position="dodge", stat="identity") + 
#   geom_text(aes(label = count),  hjust= -0.4, position = position_dodge(1), size = 3.5) +   
#   # hjust = -0.3, vjust= -0.5,
#   # vjust = -0.2 , hjust = -0.1
#   # scale_fill_viridis(discrete = T, option = "E") +
#   # scale_x_discrete(labels=setNames(data$condition, data$cond2)) +
#   # ggtitle("Accuracy results by Visualization Technique and Number of Pairs") +
#   ggtitle("     Scatter Plot (2D-SP)       Parallel Dimensions (2D-PD)") +
#   facet_grid(Visualization.Technique ~ . , scales = "free", space = "free") +  # vertical facets
#   theme(legend.position="none") +
#   scale_fill_manual(values=c("red", "green")) +
#   xlab("Number of Pairs") +
#   ylab("Count") + 
#   guides(alpha=FALSE) +
#   coord_flip() +
#   theme(strip.text.x = element_text(angle = 0)) +
#   scale_y_continuous(breaks=seq(0, 24, 1)) +
#   theme_classic() -> accuracy.vm.ne.t2
# accuracy.vm.ne.t2
# # ---- end of original version


# # Revised version with changes made
# ggplot(t2.vm.ne.acc, aes(fill = Accuracy, alpha=0.5, y=count, x=as.factor(Number.Elements))) + 
#   geom_bar(position="dodge", stat="identity") + 
#   geom_text(aes(label = count),  hjust= 1, vjust = -0.1, position = position_dodge(1), size = 5) +   
#   # hjust = -0.3, vjust= -0.5,
#   # vjust = -0.2 , hjust = -0.1
#   # scale_fill_viridis(discrete = T, option = "E") +
#   # scale_x_discrete(labels=setNames(data$condition, data$cond2)) +
#   # ggtitle("Accuracy results by Visualization Technique and Number of Pairs") +
#   ggtitle("     Scatter Plot (2D-SP)                   Parallel Dimensions (2D-PD)") +
#   facet_grid(Visualization.Technique ~ . , scales = "free", space = "free") +  # vertical facets
#   theme(legend.position="bottom") +
#   scale_fill_manual(values=c("red", "green")) +
#   xlab("Number of Pairs") +
#   ylab("Count") + 
#   # guides(alpha=FALSE) +
#   guides(fill = FALSE, alpha=FALSE) + # panel.grid.major.y = element_line(colour = "grey50")
#   theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "black")) -> accuracy.vm.ne.t2  # , panel.grid.major.y = element_line(colour = "grey50")
#   # coord_flip() +
#   # theme(strip.text.x = element_text(angle = 0)) +
#   #  scale_x_continuous(breaks=seq(0, 24, 1)) +
#   # theme_classic()  -> accuracy.vm.ne.t2
# 
# 
# accuracy.vm.ne.t2

