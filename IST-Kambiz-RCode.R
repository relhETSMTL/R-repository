# IST Journal 
# R Code
# Contains all the code for the graphs and the statistical analysis of the journal paper

library("tidyverse")

###########################################################################
### Eye-tracker data tidy up

## Loads Eye-tracker data for all the 24 participants x 16 questions
eye.tracker.data <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/Curated-Data/Eye-Tracker-Data.csv", 
                          header=TRUE)

# Eliminating the following columns, originally 29 columns
elim.cols <- c(HTMLvsCOMPUTER,RecordingElapsedTime,ComputerElapsedTime,COMPUTERvsFIXATION,
AccumulatedFixationTime,AccumulatedFixationCount,NA)

# Removing 7 consistency columns
eye.tracker.data.cleaned <- eye.tracker.data %>% 
  select(-c(HTMLvsCOMPUTER, RecordingElapsedTime, ComputerElapsedTime, COMPUTERvsFIXATION,
            AccumulatedFixationTime, AccumulatedFixationCount, NA.)) 

# Renaming some column names for consistency with web interface data
# "ParticipantID", "QuestionNumber", "Visualization", "ElapsedTime"
eye.tracker.data.cleaned <- eye.tracker.data.cleaned %>%
  rename(Participant.ID = ParticipantID,
         Question.Number = QuestionNumber,
         Visualization.Technique = Visualization,
         Elapsed.Time.Eye.Tracker = ElapsedTime)





# Renaming the columns