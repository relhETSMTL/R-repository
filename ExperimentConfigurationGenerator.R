# Experiment Configuration Generator
# Objective: Generates the configuration files for N participants where the questions are randomly chosen from a question file
# Project: Eye-Tracking Analysis of Feature Models Comprehension
# Ecole de technologie superieure
# VERITAS team
# Authors: Elmira Sepasi, Kambiz Belouchi, Roberto E. Lopez-Herrejon
# Last update: 2021-11-08


# Format generated for the configuraton file. The first six rows indicate:
# 1) File descriptor, 2) Legend for the welcome message, 3) With and height of the question windown
# 4) X,  Y position of the question windown, 5) Number of questions, 
# 6) "randomized" or the sequence of questions.
# 
# Example
# Experiment: First eye-tracker experiment   // File descriptor
# Eye Tracker Experiment - Comprehension of Feature Models // Legend for welcome message
# 1100, 800 // width, height question window
# 0,0   // x,y position of the question window. Zeroes if relative to main framework
# 4 // number questions
# randomized // either randomized or the sequence of questions like 1,4,5,2,3
# 
# Note: After the header, the details of the questions are added as from the questions file, except omitting the
# QN column.

# This is the format of the question. Loaded from the questions file.
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


library(tidyverse)

# File with experiment question descriptions 
questionsData <- read.csv(file = "experiment-questions.csv", header=TRUE)
attach (questionsData)

# File with the warmp up questions
warmupQuestionsData <- read.csv(file = "warmup-questions.csv", header=TRUE)
attach (warmupQuestionsData)

# Makes the QN negative to identify them from regular questions
for (k in (1:nrow(warmupQuestionsData))) {
  warmupQuestionsData[k,1] <- (warmupQuestionsData[k,1]*-1)
}

# Number of random configuration files to generate.
number.configurations <- 30

# Header construction. Note: Adapt for each new experiment.
configuration.file.descriptor <- "Experiment: First eye-tracker experiment   // File descriptor\n"
configuration.welcome.message <- "Eye Tracker Experiment - Comprehension of Feature Models // Legend for welcome message\n"
configuration.width.height <- "1100, 800 // width, height question window\n"
configuration.x.y <- "0,0   // x,y position of the question window. Zeroes if relative to main framework\n"
configuration.number.questions <- nrow(questionsData) + nrow(warmupQuestionsData)  # computed from the questions file + warmp up questions
configuration.randomized <- "randomized // either randomized or the sequence of questions like 1,4,5,2,3\n"

# For loop for the generation of the configurations
set.seed(10) # seeds the random number generator,
for(i in 1:number.configurations) {
  
  # Obtains a random sample of all the questions, without considering the warm up questions
  # which always go in the same order 
  samplei <- sample(1:nrow(questionsData), nrow(questionsData))
  
  # Creates a new dataframe with the same structure but with a different order of the questions
  newConfigurationFrame <- questionsData[0,]
  
  # Add to newConfigurationFrame the warm up questions
  for (k in (1:nrow(warmupQuestionsData))) {
    # Adds the row to the frame
    newConfigurationFrame <- newConfigurationFrame %>% add_row(warmupQuestionsData[k,])
  }
  
  # Adding each question in the sample to the generated configuration file
  for (j in samplei) {
    newConfigurationFrame <- newConfigurationFrame %>% add_row(questionsData[j,])
  }
  
  # Note: We kept the question number to facilitate tracking the answers
  # newConfigurationFrame <- newConfigurationFrame %>% select(-QN) # @Deprecated
  
  # Prints the order of the questions and the configuration frame
  print(samplei) # prints the order of the questions
  print(newConfigurationFrame) # prints the new configuration frame to be created
  
  # Saves the header of the configuration file
  configuration.filename <- paste("configuration-",i,".config", sep="")
  cat(configuration.file.descriptor, file=configuration.filename)
  cat(configuration.welcome.message, file=configuration.filename, append=TRUE)
  cat(configuration.width.height, file=configuration.filename, append=TRUE)
  cat(configuration.x.y, file=configuration.filename, append=TRUE)
  cat(paste(configuration.number.questions, "   // number of questions\n", sep=""), 
      file=configuration.filename, append=TRUE)
  cat(configuration.randomized, file=configuration.filename, append=TRUE)
  
  # Creates a new file with the desired configuration
  write.table(newConfigurationFrame, paste("configuration-",i,".config", sep=""), row.names=F, col.names=F, 
              sep=",", append = TRUE, quote=FALSE)
  
} # for all the configurations
