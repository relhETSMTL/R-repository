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
## Responses by t value and visualization methods


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



