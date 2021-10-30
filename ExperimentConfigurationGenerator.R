# Experiment Configuration Generator
# Objective: Generates the configuration files for N participants where the questions are randomly chosen from a question file
# Project: Eye-Tracking Analysis of Feature Models Comprehension
# Ecole de technologie superieure
# VERITAS team
# Authors: Elmira Sepasi, Kambiz Belouchi, Roberto E. Lopez-Herrejon
# Last update: 2021-10-30


library(tidyverse)

# Loads the questions file. This file has the following information:
# QN (Question number),
# QFigureFile (Full path of the feature model figure) [0]
# QHeader (Question Header) [1]
# QText (Question Text),  [2]
# QRecordStart (Records start time of question, true or false) [3]
# QRecordEnd (Records end time of question, true or false) [4]
# QRecordElapsed (Records elapsed time of question, true or false) [5]
# QRecordAnswer (Records answer of question, true or false) [6]
# QType (Type of question=radio,check,text,bool), [7]
# QAnswers [8]

# File with experiment question descriptions 
questionsData <- read.csv(file = "experiment-questions.csv", header=TRUE)
attach (questionsData)



number.configurations <- 30
number.questions <- 24 # computed after reading the configurations file

# For loop for the generation of the configurations
set.seed(10) # seeds the random number generator,
for(i in 1:number.configurations) {
  
  # Obtains a random sample of all the questions
  samplei <- sample(1:number.questions, number.questions)
  
  # Creates a new dataframe with the same structure but with a different order of the questions
  newConfigurationFrame <- questionsData[0,]
  # newFrame %>% add_row()
  # newFrame <- newFrame %>% add_row(questionsData[5,])
  
  # Adding each question in the sample to the generated configuration file
  for (j in samplei) {
    newConfigurationFrame <- newConfigurationFrame %>% add_row(questionsData[j,])
  }
  
  print(samplei) # prints the order of the questions
  print(newConfigurationFrame) # prints the new configuration frame to be created
  
  
  # Creates a new file with the desired configuration
  write.csv(newConfigurationFrame,paste("configuration-",i,".csv", sep=""), row.names = FALSE, col.names = TRUE)
  
  
} # for all the configurations


# TODO
# Adding " " to the strings in the configuraton file. Is it a problem when reading it in the FigureManagement tool?
# Concatenating the string values. - with paste



