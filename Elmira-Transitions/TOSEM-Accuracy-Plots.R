# TOSEM Paper
# Accuracy results figures
############################################
library(ggplot2)
library(tidyverse)
library(hrbrthemes) 




############################################
# Bar plots with distribution of accurate and inaccurate over NoF and NoC

####################################################################################################
# Reads the participants data
experiment.complete.data <- read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/ExperimentCompleteDataSet.csv", 
                                     header=TRUE)
attach(experiment.complete.data)


##############################################################################################
##############################################################################################
##############################################################################################
# Descriptive Statistics

# Correct answers summary, Correct value = 2, data per participant
summary(experiment.complete.data %>% filter(Correct=="2") %>% count(ParticipantID))

# ParticipantID         n        
# Min.   : 2.00   Min.   : 9.00  
# 1st Qu.: 6.00   1st Qu.:15.00  
# Median :10.00   Median :17.00  
# Mean   :10.24   Mean   :16.71  
# 3rd Qu.:14.00   3rd Qu.:20.00  
# Max.   :19.00   Max.   :21.00  

# Incorrect answers summary, Correct value = 1, data per participant
summary(experiment.complete.data %>% filter(Correct=="1") %>% count(ParticipantID))

# ParticipantID         n         
# Min.   : 2.00   Min.   : 3.000  
# 1st Qu.: 6.00   1st Qu.: 4.000  
# Median :10.00   Median : 7.000  
# Mean   :10.24   Mean   : 7.294  
# 3rd Qu.:14.00   3rd Qu.: 9.000  
# Max.   :19.00   Max.   :15.000  

# Elapsed time for Correct answers for all participants, time in miliseconds
summary(experiment.complete.data %>% filter(Correct=="2") %>% select(ElapsedTime))

# ElapsedTime    
# Min.   :  6226  
# 1st Qu.: 24156  
# Median : 37440  
# Mean   : 42844  
# 3rd Qu.: 53448  
# Max.   :199063  

# Elapsed time for Incorrect answers for all participants, time in miliseconds
summary(experiment.complete.data %>% filter(Correct=="1") %>% select(ElapsedTime))

# ElapsedTime    
# Min.   :  6417  
# 1st Qu.: 28672  
# Median : 42209  
# Mean   : 52968  
# 3rd Qu.: 67066  
# Max.   :283369  