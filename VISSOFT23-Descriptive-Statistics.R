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

