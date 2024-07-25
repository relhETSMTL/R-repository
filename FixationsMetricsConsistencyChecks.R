# Consistency checks for the computation of the fixation time, count, and proportions 
library(tidyverse)
library(dplyr)


################################################################################
################################################################################
### Multiple tests of to check consistency of transitions

# Loads the complete experimet data for all 17 participants and their 24 questions
all.participants.data  <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/ExperimentCompleteDataSet.csv",
                                  header=TRUE,
                                  fileEncoding="latin1")
attach (all.participants.data)


# Check with the 

check.fixations.metrics <- all.participants.data %>%
  mutate(fixations.count.diff = totalFixations - fixations.Question - 
           fixations.Answer - fixations.Buttons - fixations.Legend - fixations.FM - fixations.CTC - fixations.Window) %>%
  mutate(fixations.time.diff = totalFixationTime - time.Question - 
           time.Answer - time.Buttons - time.Legend - time.FM - time.CTC - time.Window) %>%
  mutate(perc.fixations.count.diff = 1 - perc.fixations.Question  - 
           perc.fixations.Answer - perc.fixations.Buttons - perc.fixations.Legend - perc.fixations.FM - perc.fixations.CTC - perc.fixations.Window) %>%
  mutate(perc.fixations.time.diff = 1 - perc.time.Question  - 
           perc.time.Answer - perc.time.Buttons - perc.time.Legend - perc.time.FM - perc.time.CTC - perc.time.Window)

# Note: all the consistency checks of fixations pass correctly, the percentages add up to E+/-15 o E+/-16 which is good enogh for our purposes
# > summary(check.fixations.metrics$perc.fixations.count.diff)
# Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -1.305e-15 -3.070e-16  0.000e+00  3.282e-18  3.216e-16  1.311e-15 
# > summary(check.fixations.metrics$perc.fixations.time.diff)
# Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -1.214e-15 -3.622e-16 -3.469e-17 -9.423e-18  3.740e-16  1.360e-15

###################################################################################

# Text the data generate for the transitions and how to change the Window

p02.transitions <- read.csv(file ="../../Experiment-Data/Eye-tracking-data-samples/PartP02/P02-Transitions-Data.csv", 
                            header=TRUE,
                            fileEncoding="latin1")
attach(p02.transitions)

p02.transitions.count <- p02.transitions %>%
  mutate(hits = Answer + Buttons + CTC + FM + Legend + Question + Window)

# 185 entries with only Window hit, 

# Recomputing the Window hits to those without any other AOI hit
p02.transitions.revised <- p02.transitions %>%
  mutate(Window = 1 - (Answer + Buttons + CTC + FM + Legend + Question))

p02.transitions.revised %>% filter(Window==1)

# For Participant 02 there are 185 Window fixations
# Test to see if the Transitions-Curated-Data file generated with the adjustments on the Window count 
# Produce the same output

write.csv(p02.transitions.revised, file = "../../Experiment-Data/Eye-tracking-data-samples/PartP02/Revised-P02-Transitions-Data.csv", row.names = FALSE)


## Calls the transitions processing on all the participants files
revised.participant.p02 <- computeTransitionsParticipant("../../Experiment-Data/Eye-tracking-data-samples/PartP02/Revised-P02-Transitions-Data.csv",
                                                 "../../Experiment-Data/Eye-tracking-data-samples/PartP02/Revised-P02-Transitions-Curated-Data.csv")



################################################################################
################################################################################
################################################################################

# Function: computeWindowAOIFixations
# Changes the values of Windows AOI to 1 only when there are no other hits in any of the other AOIs
computeWindowAOIFixations <- function(inputFile, outputFile) {
  
  # load the input file with the original Transitions-Data for a participant  
  original.transitions <- read.csv(file =inputFile, header=TRUE, fileEncoding="latin1")
  attach(original.transitions)

  # recomputing the Window hits to those without any other AOI hit
  # if there is a hit the value is 1
  revised.transitions <- original.transitions %>%
    mutate(Window = 1 - (Answer + Buttons + CTC + FM + Legend + Question))
  
  # saves the revised file in the original participant directories
  write.csv(revised.transitions, file = outputFile, row.names = FALSE)
  
  # returns the revised transitions
  return (revised.transitions)
  
} # end of computeWindowAOIFixations


################################################################################
## Transforms the Window AOIs hit for all the participants

revised.P02 <- computeWindowAOIFixations ("../../Experiment-Data/Eye-tracking-data-samples/Deprecated-Transitions-Data/P02-Transitions-Data.csv",
                                          "../../Experiment-Data/Eye-tracking-data-samples/PartP02/P02-Transitions-Data.csv")
revised.P03 <- computeWindowAOIFixations ("../../Experiment-Data/Eye-tracking-data-samples/Deprecated-Transitions-Data/P03-Transitions-Data.csv",
                                          "../../Experiment-Data/Eye-tracking-data-samples/PartP03/P03-Transitions-Data.csv")
revised.P04 <- computeWindowAOIFixations ("../../Experiment-Data/Eye-tracking-data-samples/Deprecated-Transitions-Data/P04-Transitions-Data.csv",
                                          "../../Experiment-Data/Eye-tracking-data-samples/PartP04/P04-Transitions-Data.csv")
revised.P05 <- computeWindowAOIFixations ("../../Experiment-Data/Eye-tracking-data-samples/Deprecated-Transitions-Data/P05-Transitions-Data.csv",
                                          "../../Experiment-Data/Eye-tracking-data-samples/PartP05/P05-Transitions-Data.csv")
revised.P06 <- computeWindowAOIFixations ("../../Experiment-Data/Eye-tracking-data-samples/Deprecated-Transitions-Data/P06-Transitions-Data.csv",
                                          "../../Experiment-Data/Eye-tracking-data-samples/PartP06/P06-Transitions-Data.csv")


revised.P07 <- computeWindowAOIFixations ("../../Experiment-Data/Eye-tracking-data-samples/Deprecated-Transitions-Data/P07-Transitions-Data.csv",
                                          "../../Experiment-Data/Eye-tracking-data-samples/PartP07/P07-Transitions-Data.csv")
revised.P08 <- computeWindowAOIFixations ("../../Experiment-Data/Eye-tracking-data-samples/Deprecated-Transitions-Data/P08-Transitions-Data.csv",
                                          "../../Experiment-Data/Eye-tracking-data-samples/PartP08/P08-Transitions-Data.csv")
revised.P09 <- computeWindowAOIFixations ("../../Experiment-Data/Eye-tracking-data-samples/Deprecated-Transitions-Data/P09-Transitions-Data.csv",
                                          "../../Experiment-Data/Eye-tracking-data-samples/PartP09/P09-Transitions-Data.csv")
revised.P10 <- computeWindowAOIFixations ("../../Experiment-Data/Eye-tracking-data-samples/Deprecated-Transitions-Data/P10-Transitions-Data.csv",
                                          "../../Experiment-Data/Eye-tracking-data-samples/PartP10/P10-Transitions-Data.csv")
revised.P11 <- computeWindowAOIFixations ("../../Experiment-Data/Eye-tracking-data-samples/Deprecated-Transitions-Data/P11-Transitions-Data.csv",
                                          "../../Experiment-Data/Eye-tracking-data-samples/PartP11/P11-Transitions-Data.csv")


revised.P12 <- computeWindowAOIFixations ("../../Experiment-Data/Eye-tracking-data-samples/Deprecated-Transitions-Data/P12-Transitions-Data.csv",
                                          "../../Experiment-Data/Eye-tracking-data-samples/PartP12/P12-Transitions-Data.csv")
revised.P13 <- computeWindowAOIFixations ("../../Experiment-Data/Eye-tracking-data-samples/Deprecated-Transitions-Data/P13-Transitions-Data.csv",
                                          "../../Experiment-Data/Eye-tracking-data-samples/PartP13/P13-Transitions-Data.csv")
revised.P14 <- computeWindowAOIFixations ("../../Experiment-Data/Eye-tracking-data-samples/Deprecated-Transitions-Data/P14-Transitions-Data.csv",
                                          "../../Experiment-Data/Eye-tracking-data-samples/PartP14/P14-Transitions-Data.csv")
revised.P16 <- computeWindowAOIFixations ("../../Experiment-Data/Eye-tracking-data-samples/Deprecated-Transitions-Data/P16-Transitions-Data.csv",
                                          "../../Experiment-Data/Eye-tracking-data-samples/PartP16/P16-Transitions-Data.csv")
revised.P17 <- computeWindowAOIFixations ("../../Experiment-Data/Eye-tracking-data-samples/Deprecated-Transitions-Data/P17-Transitions-Data.csv",
                                          "../../Experiment-Data/Eye-tracking-data-samples/PartP17/P17-Transitions-Data.csv")


revised.P18 <- computeWindowAOIFixations ("../../Experiment-Data/Eye-tracking-data-samples/Deprecated-Transitions-Data/P18-Transitions-Data.csv",
                                          "../../Experiment-Data/Eye-tracking-data-samples/PartP18/P18-Transitions-Data.csv")
revised.P19 <- computeWindowAOIFixations ("../../Experiment-Data/Eye-tracking-data-samples/Deprecated-Transitions-Data/P19-Transitions-Data.csv",
                                          "../../Experiment-Data/Eye-tracking-data-samples/PartP19/P19-Transitions-Data.csv")


# Done
# 1. Create a function that receives an original PN-Transitions-Data.csv file and computes the value s of AOI Window
# 2. Call the function for all the 17 participants to generate the PN-Transitions-Curated-Data.csv files
# 3. Manually test any differences of the newly generated Transitions Curated files with the previous versions
#   All of them should be equal, in that case, there is no need to recompute the files that aggregates them
