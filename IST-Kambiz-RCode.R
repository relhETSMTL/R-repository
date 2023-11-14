# IST Journal 
# Contains all the code for cleaning and merging the experiment data from web interface and eye-tracker
#
# First step: Clean unnedded columns and modify column names from row eye tracker data
# Input file for eye-tracker data: Eye-Tracker-Data.csv  # Raw export from Kambiz code
# Output file for curated eye-tracker data: Eye-Tracker-Data-Curated.csv
# 
# Second step: Merges the participants responses into a single one
# Input files: 2-ParticipantsResponses.csv, 3-ParticipantsResponses.csv --> curated web interfaces responses data
# Output file: Web-Interface-Data-Curated.csv --> merged and completed data from these curated participants files
#
# Third step: Joins the web data with the eye tracker data into a single file
# Input: two data frames with the computed data
# Output file: Complete-Experiment-Data.csv
#
# Last update: 2023-11-14

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

# Changing the values of visualization technique
eye.tracker.data.cleaned <- eye.tracker.data.cleaned %>%
  mutate(Visualization.Technique = case_when ((Visualization.Technique == "PD" & T==2) ~ "2D-PD",
                                              (Visualization.Technique == "PD" & T==3) ~ "3D-PD",
                                              (Visualization.Technique == "SP" & T==2) ~ "2D-SP",
                                              (Visualization.Technique == "SP" & T==3) ~ "3D-SP"))


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



###########################################################################
# Joining the two web and eye-tracker data
complete.experiment.data <- full_join(eye.tracker.data.cleaned, complete.web.data.cleaned)

# Writes out the cleaned data in the file
write.csv(complete.experiment.data,
          file = "../../../Eye-Tracking-Visualization/Experiment-Data/Curated-Data/Complete-Experiment-Data.csv", 
          row.names=FALSE)



###########################################################################
###########################################################################
###########################################################################
#### Scratch code
# eye.tracker.data.cleaned %>%
#   mutate(Visualization.Technique = case_when (Visualization.Technique == "PD" ~ "2D-PD",
#                                               TRUE ~ Visualization.Technique))
# 
# et <- eye.tracker.data.cleaned %>%
#   mutate(Visualization.Technique = if_else (Visualization.Technique == "PD", "2D-PD"))
# 
# # Works for t=2 and PD
# eye.tracker.data.cleaned %>%
#   mutate(Visualization.Technique = case_when ((Visualization.Technique == "PD" & T==2) ~ "2D-PD"))
