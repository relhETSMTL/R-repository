# VISSOFT 2023
# Descriptive Statistics Results

library("tidyverse")

## Loads the data files and completes the framework including T value
# Divides the responses according to t value
t2.data <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/2-ParticipantsResponses.csv", header=TRUE)
t3.data <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/3-ParticipantsResponses.csv", header=TRUE)

allt.data <- bind_rows(t2.data,t3.data)

# Adds column with T values, 8 x 24 participants = 192 per each T value x 2 = 384
T <- c(rep(2,192),rep(3,192))
allt.data$T = T


#######
# ## Responses by t value and visualization methods
# 
# 
# ggplot(t3.vm.ne.acc, aes(fill = Accuracy, alpha=0.5, y=count, x=as.factor(Number.Elements))) + 
#   geom_bar(position="dodge", stat="identity") + 
#   geom_text(aes(label = count),  hjust= -0.4, position = position_dodge(1), size = 3.5) +   
#   ggtitle("Accuracy results by Visualization Method and Number of Triplets") +
#   facet_grid(Visualization.Method ~ . , scales = "free", space = "free") +  # vertical facets
#   theme(legend.position="none") +
#   scale_fill_manual(values=c("red", "green")) +
#   xlab("Number of Triplets") +
#   ylab("Count") + 
#   guides(alpha=FALSE) +
#   coord_flip() +
#   theme(strip.text.x = element_text(angle = 0)) +
#   scale_y_continuous(breaks=seq(0, 24, 1)) +
#   theme_classic() -> plot.vm.t3.accuracy


########
## Participant responses in general, total correct and total incorrect
accuracy.general <- allt.data %>%  
  group_by(Accuracy) %>%
  summarise(count=n()) %>% as.data.frame()
# Accuracy count
# 1    False   105
# 2     True   279
# > 24 * 16
# [1] 384
# > 105 + 279
# [1] 384
# > 105/384
# [1] 0.2734375
# > 279/384
# [1] 0.7265625
# > 70+80+48+81   # Total correct responses
# [1] 279
# > 26+16+48+15   # Total incorrect responses
# [1] 105

#######
## Correct responses per participant

correct.participant <- allt.data %>% filter (Accuracy=="True") %>% 
  group_by(Participant.ID) %>%
  summarise(count=n()) %>% as.data.frame()

summary(correct.participant$count)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6.00   11.00   12.00   11.62   13.25   14.00 

## Correct responses by t value
correct.participant.t <- allt.data %>% filter (Accuracy=="True") %>% 
  select(Participant.ID,T) %>%
  group_by(Participant.ID,T) %>%
  summarise(count=n()) %>% as.data.frame()

# t=2
correct.participant.t %>% filter (T==2) %>% summary()
# Participant.ID       T         count      
# P03    : 1     Min.   :2   Min.   :2.000  
# P04    : 1     1st Qu.:2   1st Qu.:5.000  
# P05    : 1     Median :2   Median :6.000  
# P06    : 1     Mean   :2   Mean   :5.375  
# P07    : 1     3rd Qu.:2   3rd Qu.:6.000  
# P08    : 1     Max.   :2   Max.   :6.000  

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
correct.participant.vm  <- allt.data %>% filter (Accuracy=="True") %>% 
  select(Participant.ID,Visualization.Method) %>%
  group_by(Participant.ID,Visualization.Method) %>%
  summarise(count=n()) %>% as.data.frame()

# Scattered Plot 2D-SP or 3D-SP
correct.participant.vm.sp <- correct.participant.vm %>% 
  filter (Visualization.Method=="2D-SP" | Visualization.Method=="3D-SP") %>% 
  select(Participant.ID,count) %>%
  group_by(Participant.ID) %>% summarise(total=sum(count)) %>% as.data.frame()

summary(correct.participant.vm.sp$total)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.000   4.000   5.000   4.917   6.000   6.000 

# Parallel dimensions Plot 2D-PD or 3D-PD
correct.participant.vm.pd <- correct.participant.vm %>% 
  filter (Visualization.Method=="2D-PD" | Visualization.Method=="3D-PD") %>% 
  select(Participant.ID,count) %>%
  group_by(Participant.ID) %>% summarise(total=sum(count)) %>% as.data.frame()

summary(correct.participant.vm.pd$total)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.000   5.750   7.000   6.708   8.000   8.000 


################################################################################

############
# Grid of Response time per t value and visualization method

# t2 and parallel dimensions
bp.t2.pd.res <- allt.data %>% 
  mutate(Accuracy = case_when(Accuracy == 'True' ~ TRUE, Accuracy == 'False' ~ FALSE)) %>%
  filter (T==2 & Visualization.Method=="2D-PD") %>%
  ggplot(aes(x=Accuracy, group=Accuracy,Elapsed.Time/1000)) + #  Question.Number, Certainty.Assessment
  geom_boxplot(aes(fill=Elapsed.Time/1000), varwidth=T,  fill=c("red", "green"), alpha=0.5) + #  fill="plum",
  geom_jitter() +
  #  coord_flip() +
  labs(x="Parallel Coordinates Plot", y="T=2") + 
  # scale_fill_manual(values=c("red", "green")) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
#  ylim(0, 550)  +
  scale_y_continuous(breaks=seq(0, 350, 50),limits = c(0, 350))

bp.t2.pd.res


# t2 and scatter plot
bp.t2.sp.res <- allt.data %>% 
  mutate(Accuracy = case_when(Accuracy == 'True' ~ TRUE, Accuracy == 'False' ~ FALSE)) %>%
  filter (T==2 & Visualization.Method=="2D-SP") %>%
  ggplot(aes(x=Accuracy, group=Accuracy,Elapsed.Time/1000)) + #  Question.Number, Certainty.Assessment
  geom_boxplot(aes(fill=Elapsed.Time/1000), varwidth=T,  fill=c("red", "green"), alpha=0.5) + #  fill="plum",
  geom_jitter() +
  #  coord_flip() +
  labs(x="Scatter Plot", y="T=2") + 
  # scale_fill_manual(values=c("red", "green")) +
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
#  ylim(0, 550)  +
  scale_y_continuous(breaks=seq(0, 350, 50),limits = c(0, 350))

bp.t2.sp.res


# t3 and parallel dimensions
bp.t3.pd.res <- allt.data %>% 
  mutate(Accuracy = case_when(Accuracy == 'True' ~ TRUE, Accuracy == 'False' ~ FALSE)) %>%
  filter (T==3 & Visualization.Method=="3D-PD") %>%
  ggplot(aes(x=Accuracy, group=Accuracy,Elapsed.Time/1000)) +  
  # ylim(0, 550)  +
  scale_y_continuous(breaks=seq(0, 350, 50), limits = c(0, 350)) +
  geom_boxplot(aes(fill=Elapsed.Time/1000), varwidth=T,  fill=c("red", "green"), alpha=0.5) +  
  geom_jitter() +
  labs(x="Parallel Coordinates Plot", y="T=3") + 
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) # +
  # ylim(0, 550)  +
  # scale_y_continuous(breaks=seq(0, 550, 50))
bp.t3.pd.res


# t3 and scatter plot
bp.t3.sp.res <- allt.data %>% 
  mutate(Accuracy = case_when(Accuracy == 'True' ~ TRUE, Accuracy == 'False' ~ FALSE)) %>%
  filter (T==3 & Visualization.Method=="3D-SP") %>%
  ggplot(aes(x=Accuracy, group=Accuracy,Elapsed.Time/1000)) +  
  geom_boxplot(aes(fill=Elapsed.Time/1000), varwidth=T,  fill=c("red", "green"), alpha=0.5) +  
  geom_jitter() +
  labs(x="Scatter Plot", y="T=3") + 
  theme(panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey50")) +
  ylim(0, 550)  +
  scale_y_continuous(breaks=seq(0, 350, 50),limits = c(0, 350))
  
bp.t3.sp.res

# Grid for the bar plots
# Creating the grid for the bars
library(gridExtra)
library(grid)

# Creates the grid for the stack bars
grid.res <- grid.arrange(bp.t3.sp.res,bp.t3.pd.res,
                          bp.t2.sp.res,bp.t2.pd.res,
                          ncol=2, nrow=2,
                          bottom = textGrob("Visualization Methods",gp=gpar(fontsize=15,font=3)),
                          left = textGrob("Covering Array Strength - Time in secs", rot=90, gp=gpar(fontsize=15,font=3)))



# allt.data %>% mutate(Accuracy = case_when(Accuracy == 'True' ~ TRUE, Accuracy == 'False' ~ FALSE))


## Total Elapsed time by participants (incorrect and correct answers)
elapsed.time.participant <- allt.data %>%
  select(Participant.ID, Elapsed.Time) %>%
  group_by(Participant.ID) %>%
  summarise(total=sum(Elapsed.Time)/1000/60)  # Total time in minutes


# Sanity check
allt.data %>% select(Participant.ID, Elapsed.Time) %>% 
  filter(Participant.ID=="P03") %>% summarize(total=sum(Elapsed.Time)/1000/60)

summary(elapsed.time.participant$total)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6.125  13.598  16.836  17.457  22.103  24.960 

sd(elapsed.time.participant$total)
# [1] 5.073546

# Total Elapsed time for correct answers, in general --> not by participant
elapsed.time.participant.correct <- allt.data %>% filter (Accuracy=="True")  %>% 
  select(Elapsed.Time) # %>%
  # group_by(Participant.ID) %>%
  # summarise(total=sum(Elapsed.Time)/1000/60)  # Total time in minutes


summary(elapsed.time.participant.correct$Elapsed.Time/1000)
# In seconds
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4.256  35.798  50.225  68.194  82.270 465.551 

# In minutes
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.07093 0.59664 0.83708 1.13656 1.37118 7.75918 

sd(elapsed.time.participant.correct$Elapsed.Time/1000)
# 53.14025


# Total Elapsed time for incorrect answers, in general --> not by participant
elapsed.time.participant.incorrect <- allt.data %>% filter (Accuracy=="False")  %>% 
  select(Elapsed.Time) 


summary(elapsed.time.participant.incorrect$Elapsed.Time/1000)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 7.985  29.919  47.167  58.209  76.494 161.983 

sd(elapsed.time.participant.incorrect$Elapsed.Time/1000)
# [1] 36.22685


## Response time over the t values
# t=2
t2.et <- allt.data %>% filter (T==2)  %>% 
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
t3.et <- allt.data %>% filter (T==3)  %>% 
  select(Elapsed.Time)
summary(t3.et)
# Min.   :  4256  
# 1st Qu.: 44360  
# Median : 71400  
# Mean   : 81544  
# 3rd Qu.: 99999  
# Max.   :465551   

sd(t3.et$Elapsed.Time)
# [1] 57918.63 


## Response time over visualization methods
# vm = scatter plot
sp.res <- allt.data %>% filter (Visualization.Method=="2D-SP" | Visualization.Method=="3D-SP")  %>% 
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
pd.res <- allt.data %>% filter (Visualization.Method=="2D-PD" | Visualization.Method=="3D-PD")  %>% 
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

# scatter plot, t=3,correct
allt.data %>% filter (Visualization.Method=="3D-SP" & Accuracy=="True") %>% 
  select(Visualization.Method, Accuracy, Elapsed.Time) %>% summary()
# Visualization.Method  Accuracy   Elapsed.Time   
# 2D-PD: 0             False: 0   Min.   :  4484  
# 2D-SP: 0             True :70   1st Qu.: 44999  
# 3D-PD: 0                        Median : 82146  
# 3D-SP:70                        Mean   :100418  
# 3rd Qu.:129488  
# Max.   :465551  

# scatter plot, t=3, incorrect
allt.data %>% filter (Visualization.Method=="3D-SP" & Accuracy=="False") %>% 
  select(Visualization.Method, Accuracy, Elapsed.Time) %>% summary()
# Visualization.Method  Accuracy   Elapsed.Time   
# 2D-PD: 0             False:26   Min.   : 24863  
# 2D-SP: 0             True : 0   1st Qu.: 71948  
# 3D-PD: 0                        Median : 80393  
# 3D-SP:26                        Mean   : 87670  
# 3rd Qu.:111337  
# Max.   :161983 


# parallel coordinates plot, t=3,correct
allt.data %>% filter (Visualization.Method=="3D-PD" & Accuracy=="True") %>% 
  select(Visualization.Method, Accuracy, Elapsed.Time) %>% summary()
# Visualization.Method  Accuracy   Elapsed.Time   
# 2D-PD: 0             False: 0   Min.   :  4256  
# 2D-SP: 0             True :80   1st Qu.: 44976  
# 3D-PD:80                        Median : 60800  
# 3D-SP: 0                        Mean   : 69309  
# 3rd Qu.: 84808  
# Max.   :199136  


# parallel coordinates plot, t=3,incorrect
allt.data %>% filter (Visualization.Method=="3D-PD" & Accuracy=="False") %>% 
  select(Visualization.Method, Accuracy, Elapsed.Time) %>% summary()
# Visualization.Method  Accuracy   Elapsed.Time   
# 2D-PD: 0             False:16   Min.   : 16210  
# 2D-SP: 0             True : 0   1st Qu.: 31740  
# 3D-PD:16                        Median : 41414  
# 3D-SP: 0                        Mean   : 50191  
# 3rd Qu.: 53058  
# Max.   :133983  

# parallel coordinates plot, t=2,incorrect
allt.data %>% filter (Visualization.Method=="2D-PD" & Accuracy=="False") %>% 
  select(Visualization.Method, Accuracy, Elapsed.Time) %>% summary()
# Visualization.Method  Accuracy   Elapsed.Time   
# 2D-PD:15             False:15   Min.   : 23247  
# 2D-SP: 0             True : 0   1st Qu.: 47327  
# 3D-PD: 0                        Median : 54143  
# 3D-SP: 0                        Mean   : 65858  
# 3rd Qu.: 76224  
# Max.   :156703 

# parallel coordinates plot, t=2,correct
allt.data %>% filter (Visualization.Method=="2D-PD" & Accuracy=="True") %>% 
  select(Visualization.Method, Accuracy, Elapsed.Time) %>% summary()
# Visualization.Method  Accuracy   Elapsed.Time   
# 2D-PD:81             False: 0   Min.   : 15776  
# 2D-SP: 0             True :81   1st Qu.: 33791  
# 3D-PD: 0                        Median : 46223  
# 3D-SP: 0                        Mean   : 54228  
# 3rd Qu.: 64956  
# Max.   :156559 

# scatter plot, t=2,incorrect
allt.data %>% filter (Visualization.Method=="2D-SP" & Accuracy=="False") %>% 
  select(Visualization.Method, Accuracy, Elapsed.Time) %>% summary()
# Visualization.Method  Accuracy   Elapsed.Time   
# 2D-PD: 0             False:48   Min.   :  7985  
# 2D-SP:48             True : 0   1st Qu.: 23668  
# 3D-PD: 0                        Median : 37807  
# 3D-SP: 0                        Mean   : 42534  
# 3rd Qu.: 52063  
# Max.   :126866 

# scatter plot, t=2,correct
allt.data %>% filter (Visualization.Method=="2D-SP" & Accuracy=="True") %>% 
  select(Visualization.Method, Accuracy, Elapsed.Time) %>% summary()
# Visualization.Method  Accuracy   Elapsed.Time   
# 2D-PD: 0             False: 0   Min.   : 10544  
# 2D-SP:48             True :48   1st Qu.: 25972  
# 3D-PD: 0                        Median : 35976  
# 3D-SP: 0                        Mean   : 42910  
# 3rd Qu.: 56575  
# Max.   :208219  

#####
## Difficulty level data
## General histogram

# library
library(ggplot2)

# basic histogram
his.difficulty <- ggplot(allt.data, aes(x=Difficulty.Level)) + 
  geom_histogram(binwidth=1, stat="count") +
  stat_count(binwidth = 1, geom = 'text', color = 'black', aes(label = ..count..), vjust= -0.4,) +
  #           position = position_stack(vjust = -0.5)) +
#  geom_text(aes(label = count),  hjust= -0.4, position = position_dodge(1), size = 3.5) +   
  scale_x_continuous(breaks=seq(1, 20, 1)) 

his.difficulty

# 1 = 368, 2=1, 3=4, 4=2, 5=2, 6=2,9=1,10=1,11=1,13=1,20=1
# 368 + 1 + 4 + 2 + 2 + 2 + 1 + 1 + 1 +1 + 1 = 384

# TODO analysis of the 16 entries that are not 1

# ggplot(data = d, 
#        aes(x = discrete_var)) + 
#   geom_histogram(stat = "count") +
#   stat_count(binwidth = 1, 
#              geom = 'text', 
#              color = 'white', 
#              aes(label = ..count..),
#              position = position_stack(vjust = 0.5))

# Certainty assessment
# Simple histogram
his.certainty <- ggplot(allt.data, aes(x=Certainty.Assessment)) + 
  geom_histogram(binwidth=1, stat="count") +
  stat_count(binwidth = 1, geom = 'text', color = 'black', aes(label = ..count..), vjust= -0.4,) +
  #           position = position_stack(vjust = -0.5)) +
  #  geom_text(aes(label = count),  hjust= -0.4, position = position_dodge(1), size = 3.5) +   
  scale_x_continuous(breaks=seq(1, 20, 1)) 

his.certainty


