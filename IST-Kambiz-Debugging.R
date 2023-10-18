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

