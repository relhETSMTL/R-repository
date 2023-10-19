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


##########################################################################################################
##########################################################################################################
##########################################################################################################
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


# Response data, 15 fixations
Response.data <- p25.act11.data.cleaned %>% 
  # The Answer is selected 
  filter(AOI.P25.Q06.Act11.Answer==1) 


# Target data, 12 fixations
Target.data <- p25.act11.data.cleaned %>% 
  # Any of the Axis_i_Feature_j_ are selected 
  filter(AOI.P25.Q06.Act11.Axis1Feature1==1 | AOI.P25.Q06.Act11.Axis1Feature2==1 | AOI.P25.Q06.Act11.Axis1Feature3==1 |
         AOI.P25.Q06.Act11.Axis2Feature1==1 | AOI.P25.Q06.Act11.Axis2Feature2==1 | AOI.P25.Q06.Act11.Axis2Feature3==1 |
         AOI.P25.Q06.Act11.Axis3Feature1==1 | AOI.P25.Q06.Act11.Axis3Feature2==1 | AOI.P25.Q06.Act11.Axis3Feature3==1) 

# Navigation data, Diagram AOI - SUM all Axis , 45 fixations
Navigation.data <- p25.act11.data.cleaned %>%
  # Diagram selected but the rest of Axis 1..4 unselected
  filter(AOI.P25.Q06.Act11.Diagram==1 & AOI.P25.Q06.Act11.Axis1==0 & AOI.P25.Q06.Act11.Axis2==0 & 
         AOI.P25.Q06.Act11.Axis3==0 & AOI.P25.Q06.Act11.Axis4==0)
  
# Question data, 32 fixations
Question.data <- p25.act11.data.cleaned %>%
  # Question selected
  filter(AOI.P25.Q06.Act11.Question==1)
  
# Miscellaneous, Stimulus - Question - Response - Diagram
Misc.data <- p25.act11.data.cleaned %>%
  # Stimulus selected but Question, Answer and Diagram unselected
  filter(AOI.P25.Q06.Act11.Stimulus==1 & AOI.P25.Q06.Act11.Question==0 & AOI.P25.Q06.Act11.Answer==0 &
         AOI.P25.Q06.Act11.Diagram==0)


# Computing the percentages of fixations and percentage of time
# Elapsed.Time and Number.Fixations

# (ParticipantID, QuestionNumber, T, Visualization,Accuracy,
#   ElapsedTime, Question.pftime, Question.pfcount, Response.pftime, Response.pfcount,
#   Misc.pftime, Misc.pfcount, Navigation.pftime, Navigation.pfcount, Axial.pftime,
#   Axial.pfcount, Solution.pftime, Solution.pfcount, Target.pftime, Target.pfcount)

pElapsedTime <- Elapsed.Time
pQuestion.pftime <- sum(Question.data$Gaze.event.duration..ms.)/Elapsed.Time
pResponse.pftime <- sum(Response.data$Gaze.event.duration..ms.)/Elapsed.Time
pMisc.pftime <- sum(Misc.data$Gaze.event.duration..ms.)/Elapsed.Time
pNavigation.pftime <- sum(Navigation.data$Gaze.event.duration..ms.)/Elapsed.Time
pAxial.pftime <- sum(Axial.data$Gaze.event.duration..ms.)/Elapsed.Time
pSolution.pftime <- sum(Solution.data$Gaze.event.duration..ms.)/Elapsed.Time
pTarget.pftime <- sum(Target.data$Gaze.event.duration..ms.)/Elapsed.Time

# 1.023971, still over 1
p25.totalpftime <- pQuestion.pftime + pResponse.pftime + pMisc.pftime + pNavigation.pftime + 
  pAxial.pftime + pSolution.pftime + pTarget.pftime

# 228 rows
nrow(Question.data) + nrow(Response.data) + nrow(Misc.data) + nrow(Navigation.data) + nrow(Axial.data) +
  nrow(Solution.data) + nrow(Target.data)

# 219 rows
nrow(p25.act11.data.cleaned)

# Problem, I am counting 9 rows duplicated
p25.act11.data.duplicated <- bind_rows(Question.data, Response.data, Misc.data, Navigation.data, Axial.data, 
                                       Solution.data, Target.data)


# Finding the duplicated rows
duplicates <- p25.act11.data.duplicated %>% group_by_all() %>% filter(n() > 1) %>% ungroup()

# Which filters capture the duplicated rows?

# Question 12 hits 
duplicates %>% filter(AOI.P25.Q06.Act11.Question==1) %>% count()   

# Response  6 hits
duplicates %>% filter(AOI.P25.Q06.Act11.Answer==1) %>% count()

# Misc 0 hits
duplicates %>% filter(AOI.P25.Q06.Act11.Stimulus==1 & AOI.P25.Q06.Act11.Question==0 & AOI.P25.Q06.Act11.Answer==0 &
                        AOI.P25.Q06.Act11.Diagram==0) %>% count()

# Navigation 18 hits -- all the duplicates
duplicates %>% filter(AOI.P25.Q06.Act11.Diagram==1 & AOI.P25.Q06.Act11.Axis1==0 & AOI.P25.Q06.Act11.Axis2==0 & 
                          AOI.P25.Q06.Act11.Axis3==0 & AOI.P25.Q06.Act11.Axis4==0) %>% count()

# Axial - Axis1 and Axis2 NULL, Axis3 & Axis 4, 0 hits
duplicates %>%  filter((AOI.P25.Q06.Act11.Axis3==1 & 
                         AOI.P25.Q06.Act11.Axis3Feature1==0 & AOI.P25.Q06.Act11.Axis3Feature2==0 & 
                         AOI.P25.Q06.Act11.Axis3Feature3==0) |
                        (AOI.P25.Q06.Act11.Axis4==1 & AOI.P25.Q06.Act11.Axis4Solution==0)) %>% count() 

# Solution, 0 hits
duplicates %>% filter(AOI.P25.Q06.Act11.Axis4==1 & AOI.P25.Q06.Act11.Axis4Solution==1) %>% count()


# Target, 0 hits
duplicates %>%   
  filter(AOI.P25.Q06.Act11.Axis1Feature1==1 | AOI.P25.Q06.Act11.Axis1Feature2==1 | AOI.P25.Q06.Act11.Axis1Feature3==1 |
         AOI.P25.Q06.Act11.Axis2Feature1==1 | AOI.P25.Q06.Act11.Axis2Feature2==1 | AOI.P25.Q06.Act11.Axis2Feature3==1 |
         AOI.P25.Q06.Act11.Axis3Feature1==1 | AOI.P25.Q06.Act11.Axis3Feature2==1 | AOI.P25.Q06.Act11.Axis3Feature3==1) %>% count()


#######
## Fixing the duplication

# Original definition of Navigation data, Diagram AOI - SUM all Axis , 45 fixations
Navigation.data <- p25.act11.data.cleaned %>%
  # Diagram selected but the rest of Axis 1..4 unselected
  filter(AOI.P25.Q06.Act11.Diagram==1 & AOI.P25.Q06.Act11.Axis1==0 & AOI.P25.Q06.Act11.Axis2==0 & 
           AOI.P25.Q06.Act11.Axis3==0 & AOI.P25.Q06.Act11.Axis4==0)


# Extended definition of Navigation --> 36 rows, the 9 that result in the duplicated are removed
Navigation2.data <- p25.act11.data.cleaned %>%
  # Diagram selected but the rest of Axis 1..4 unselected
  filter(AOI.P25.Q06.Act11.Diagram==1 & AOI.P25.Q06.Act11.Axis1==0 & AOI.P25.Q06.Act11.Axis2==0 & 
           AOI.P25.Q06.Act11.Axis3==0 & AOI.P25.Q06.Act11.Axis4==0 &
           AOI.P25.Q06.Act11.Question==0 & AOI.P25.Q06.Act11.Answer==0) # Added

# Reconstructing, obtains the same number from the cleaned version
p25.act11.data.duplicated2 <- bind_rows(Question.data, Response.data, Misc.data, Navigation2.data, Axial.data, 
                                       Solution.data, Target.data)
# > count(p25.act11.data.duplicated2)
# n
# 1 219

# Finding the duplicated rows --> no duplicates
no.duplicates <- p25.act11.data.duplicated2 %>% group_by_all() %>% filter(n() > 1) %>% ungroup()


# Rechecking the time
pElapsedTime <- Elapsed.Time
pQuestion.pftime <- sum(Question.data$Gaze.event.duration..ms.)/Elapsed.Time
pResponse.pftime <- sum(Response.data$Gaze.event.duration..ms.)/Elapsed.Time
pMisc.pftime <- sum(Misc.data$Gaze.event.duration..ms.)/Elapsed.Time
pNavigation.pftime <- sum(Navigation2.data$Gaze.event.duration..ms.)/Elapsed.Time
pAxial.pftime <- sum(Axial.data$Gaze.event.duration..ms.)/Elapsed.Time
pSolution.pftime <- sum(Solution.data$Gaze.event.duration..ms.)/Elapsed.Time
pTarget.pftime <- sum(Target.data$Gaze.event.duration..ms.)/Elapsed.Time

# 1.023971, still over 1, p25.totalpftime2 exactly equal to 1
p25.totalpftime2 <- pQuestion.pftime + pResponse.pftime + pMisc.pftime + pNavigation.pftime + 
  pAxial.pftime + pSolution.pftime + pTarget.pftime

# 219 rows
nrow(Question.data) + nrow(Response.data) + nrow(Misc.data) + nrow(Navigation2.data) + nrow(Axial.data) +
  nrow(Solution.data) + nrow(Target.data)
  
### Now computing the data for the fixation counts
pQuestion.pfcount <- nrow(Question.data)/Number.Fixations
pResponse.pfcount <- nrow(Response.data)/Number.Fixations
pMisc.pfcount <- nrow(Misc.data)/Number.Fixations
pNavigation.pfcount <- nrow(Navigation2.data)/Number.Fixations
pAxial.pfcount <- nrow(Axial.data)/Number.Fixations
pSolution.pfcount <- nrow(Solution.data)/Number.Fixations
pTarget.pfcount <- nrow(Target.data)/Number.Fixations

# Exactly the value of 1
p25.totalpfcount <- pQuestion.pfcount + pResponse.pfcount + pMisc.pfcount + pNavigation.pfcount + 
  pAxial.pfcount + pSolution.pfcount + pTarget.pfcount


# Displaying the values for comparison with Kambiz's results

pQuestion.pftime + pResponse.pftime + pMisc.pftime + pNavigation.pftime + 
  pAxial.pftime + pSolution.pftime + pTarget.pftime

pQuestion.pfcount + pResponse.pfcount + pMisc.pfcount + pNavigation.pfcount + 
  pAxial.pfcount + pSolution.pfcount + pTarget.pfcount

# Data from Excel file
# P25	11	3	PD	TRUE	94447	1.35	1.138	0.105	0.143	0.053	0.067	0.014	0.018	0.086	0.161	0.36	0.408	0.355	0.157	0.377	0.184	NA	NA	NA	NA
# Elapsed Time 94447, Elapsed.Time 77844 --> OK less than the recorded time. Was it from the web interface? Remove rows with Stimulus==0
# pfTime 1.35, 1 --> you have more repeated rows than the 9 rows I had
# pfCount 1.138, 1 
# Question.pftime 0.105, 0.1052104 --> seems OK
# Question.pfcount 0.143, 0.1461187 --> it is lower
# Response.pftime 0.053, 	0.05374852 --> seems OK
# Response.pfcount 0.067, 0.06849315 --> it is lower
# Misc.pftime 0.014, 0.01360413 --> seems OK rounded up
# Misc.pfcount 0.018, 0.01826484 --> seems OK when truncated
# Navigation.pftime 0.086, 0.08681465 --> seems OK	  
# Navigation.pfcount 0.161, 0.1643836 --> it is lower
# Axial.pftime 0.36, 0.3396794 --> it is higher	!!!
# Axial.pfcount 0.408, 0.3881279 --> it is higher !!!
# Solution.pftime 0.355, 0.3568162 --> it is lower
# Solution.pfcount 0.157, 0.1598174 --> it is lower
# Target.pftime 0.377, 0.04412671 --> way bigger!!!
# Target.pfcount 0.184, 0.05479452 --> way bigger

# Checking values of pftimes for Kambiz data
# pftime = 1.35
0.105 + 0.053 + 0.014 + 0.086 + 0.36 + 0.355 + 0.377
# pfcount = 1.138
0.143 + 0.067 + 0.018 + 0.161 + 0.408 + 0.157 + 0.184


