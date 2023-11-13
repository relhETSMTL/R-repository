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

# Column names are now
# "Participant.ID"           "Question.Number"          "T"                        "Visualization.Technique" 
# "Accuracy"                 "Elapsed.Time.Eye.Tracker" "Fixation.Time"            "Fixation.Count"          
# "Question.pftime"          "Question.pfcount"         "Response.pftime"          "Response.pfcount"        
# "Misc.pftime"              "Misc.pfcount"             "Navigation.pftime"        "Navigation.pfcount"      
# "Axial.pftime"             "Axial.pfcount"            "Solution.pftime"          "Solution.pfcount"        
# "Target.pftime"            "Target.pfcount"          

# Writes out the cleaned data in a file
write.csv(eye.tracker.data.cleaned,
          file = "../../../Eye-Tracking-Visualization/Experiment-Data/Curated-Data/Eye-Tracker-Data-Curated.csv", 
          row.names=FALSE)


###########################################################################
### Web interface data merging
t2.web.data <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/2-ParticipantsResponses.csv", header=TRUE)
t3.web.data <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/3-ParticipantsResponses.csv", header=TRUE)

complete.web.data <- bind_rows(t2.web.data,t3.web.data)

# Adds column with T values, 8 x 24 participants = 192 per each T value x 2 = 384
T <- c(rep(2,192),rep(3,192))
complete.web.data$T = T


# Column names
# "Participant.ID"       "Question.Number"      "Accuracy"             "Elapsed.Time"         "Certainty.Assessment"
# "Difficulty.Level"     "Visualization.Method" "Number.Elements"      "T"     


# Renaming the column Visualization.Method to Visualization.Technique
complete.web.data.cleaned <- complete.web.data %>% rename(Visualization.Technique = Visualization.Method)


# Writes out the cleaned data in the file
write.csv(complete.web.data.cleaned,
          file = "../../../Eye-Tracking-Visualization/Experiment-Data/Curated-Data/Web-Interface-Data-Curated.csv", 
          row.names=FALSE)
