# IST Journal Kambiz Work
# Debugging data
# Roberto E. Lopez-Herrejon
# Last update: 2023-10-18


library("tidyverse")

## Loads the data files and completes the framework including T value
# Divides the responses according to t value
all.gaze.data <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/Analyzed-2023-10-17.csv", 
                          header=TRUE)


# Removes the last 4 columns
# colnames(all.gaze.data) --> "NA.", "NA..1","NA..2","NA..3" 
gaze.data <- all.gaze.data %>% 
          select(ParticipantID, QuestionNumber, T, Visualization,Accuracy,
                 ElapsedTime, Question.pftime, Question.pfcount, Response.pftime, Response.pfcount,
                 Misc.pftime, Misc.pfcount, Navigation.pftime, Navigation.pfcount, Axial.pftime,
                 Axial.pfcount, Solution.pftime, Solution.pfcount, Target.pftime, Target.pfcount)

# How are NAs distributed, per each of the columms
# > colSums(is.na(gaze.data))
# ParticipantID     QuestionNumber                  T      Visualization           Accuracy        ElapsedTime 
# 0                  0                  0                  0                  0                  0 
# Question.pftime   Question.pfcount    Response.pftime   Response.pfcount        Misc.pftime       Misc.pfcount 
# 0                  0                  0                  0                  0                  0 
# Navigation.pftime Navigation.pfcount       Axial.pftime      Axial.pfcount    Solution.pftime   Solution.pfcount 
# 0                  0                  0                  0                 48                 48 
# Target.pftime     Target.pfcount 
# 0                  0 
# NOTE: Only in Solution.pfcont and Solution.pftime, in 24 x 2 = 48 responses

with_NA <- gaze.data %>% filter(is.na(Solution.pfcount))

# In this filtered frame, we can see that all questions 13 and 15 for all the participants 
# do not have neither fixation count or time --> red flag check

## Replace the NAs with 0
gaze.data <- gaze.data %>% replace_na(list(Solution.pftime=0, Solution.pfcount=0))

## Add a column that adds up all the fixation times percentages
gaze.data.totals <- gaze.data %>% mutate(TotalPercTime = Question.pftime + Response.pftime + Misc.pftime + Navigation.pftime +
                                    Axial.pftime + Solution.pftime + Target.pftime,
                                  TotalPectFixations = Question.pfcount + Response.pfcount + Misc.pfcount + Navigation.pfcount +
                                    Axial.pfcount + Solution.pfcount + Target.pfcount )

# Checking the rows with percentages > 1, 287 time, 279 fixations,  269 both,  percentage 70% just too many ...
gaze.data.totals %>% filter( TotalPercTime > 1 ) %>% count()

gaze.data.totals %>% filter( TotalPectFixations > 1 ) %>% count()

gaze.data.totals %>% filter( TotalPercTime > 1 & TotalPectFixations > 1 ) %>% count()

###################################################################################################################
###################################################################################################################
###################################################################################################################

### A concrete participant question P25
p25.act11.data <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/P25-TOI-Q06-Act11-Curated.csv", 
                          header=TRUE)

# > colnames(p25.act11.data)
# [1] "Eye.movement.type"               "Eye.movement.type.index"         "Gaze.event.duration..ms."       
# [4] "Sensor"                          "AOI.P25.Q06.Act11.Answer"        "AOI.P25.Q06.Act11.Stimulus"     
# [7] "AOI.P25.Q06.Act11.Axis1"         "AOI.P25.Q06.Act11.Axis1Feature1" "AOI.P25.Q06.Act11.Axis1Feature2"
# [10] "AOI.P25.Q06.Act11.Axis1Feature3" "AOI.P25.Q06.Act11.Axis2"         "AOI.P25.Q06.Act11.Axis2Feature1"
# [13] "AOI.P25.Q06.Act11.Axis2Feature2" "AOI.P25.Q06.Act11.Axis2Feature3" "AOI.P25.Q06.Act11.Axis3"        
# [16] "AOI.P25.Q06.Act11.Axis3Feature1" "AOI.P25.Q06.Act11.Axis3Feature2" "AOI.P25.Q06.Act11.Axis3Feature3"
# [19] "AOI.P25.Q06.Act11.Axis4"         "AOI.P25.Q06.Act11.Axis4Solution" "AOI.P25.Q06.Act11.Diagram"      
# [22] "AOI.P25.Q06.Act11.Question"   

# There are 4 rows without Stimulus hit --> Remove them, total 219 fixations
p25.act11.data.cleaned <- p25.act11.data %>% filter (AOI.P25.Q06.Act11.Stimulus==1)
  
# Elapsed time
Elapsed.Time <- sum(p25.act11.data.cleaned$Gaze.event.duration..ms.)
# [1] 78319
# [1] 77844 <-- revised

# Number of fixations
Number.Fixations <- nrow (p25.act11.data.cleaned)
# [1] 223
# [1] 219


# 0 Axis 1
p25.act11.data.cleaned %>% filter(AOI.P25.Q06.Act11.Axis1==1) %>% count()

# 0 Axis 2
p25.act11.data.cleaned %>% filter(AOI.P25.Q06.Act11.Axis2==1) %>% count()

# 72 Axis 3
p25.act11.data.cleaned %>% filter(AOI.P25.Q06.Act11.Axis3==1) %>% count()

# In Axis 3, Feature1 = 5, Feature2 = 1, Feature3 = 6
p25.act11.data.cleaned %>% filter(AOI.P25.Q06.Act11.Axis3==1 & AOI.P25.Q06.Act11.Axis3Feature1==1) %>% count()
p25.act11.data.cleaned %>% filter(AOI.P25.Q06.Act11.Axis3==1 & AOI.P25.Q06.Act11.Axis3Feature2==1) %>% count()
p25.act11.data.cleaned %>% filter(AOI.P25.Q06.Act11.Axis3==1 & AOI.P25.Q06.Act11.Axis3Feature3==1) %>% count()

# Axis 4 =  60, Axis 4 Solution = 35
p25.act11.data.cleaned %>% filter(AOI.P25.Q06.Act11.Axis4==1) %>% count()
p25.act11.data.cleaned %>% filter(AOI.P25.Q06.Act11.Axis4==1 & AOI.P25.Q06.Act11.Axis4Solution) %>% count()


# Note: bind_rows(...)  --> Axial.data <- bind_rows(Axis1.data, Axis2.data, Axis3.data)
# Axial data Axis1 NULL, Axis2 NULL
# Axial3 data has 60 rows
Axis3.data <- p25.act11.data.cleaned %>% 
  # In Axis 3 excluding those that have the target data
  filter(AOI.P25.Q06.Act11.Axis3==1 & 
           AOI.P25.Q06.Act11.Axis3Feature1==0 & AOI.P25.Q06.Act11.Axis3Feature2==0 & 
           AOI.P25.Q06.Act11.Axis3Feature3==0) 

# Axial4, data has 25 rows
Axis4.data <- p25.act11.data.cleaned %>% 
  # In Axis 4 excluding those that have the target data
  filter(AOI.P25.Q06.Act11.Axis4==1 & AOI.P25.Q06.Act11.Axis4Solution==0) 

# 85 rows for axial data time
Axial.data <- bind_rows(Axis3.data, Axis4.data)

# Note: Consistency checks, the Axis_i values are disjoint, there can be only one selected at most

# Solution data, only those with solution selected = 35 rows. With Axis4 then is 25 + 35 = 60
Solution.data <- p25.act11.data.cleaned %>% 
  # In Axis 4 with the solution selected 
  filter(AOI.P25.Q06.Act11.Axis4==1 & AOI.P25.Q06.Act11.Axis4Solution==1) 


