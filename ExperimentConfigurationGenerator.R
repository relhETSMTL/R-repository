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

# TODO
# Write a function that generates M combinations of N questions each,Generate a 

number.configurations <- 30
number.questions <- 24 # computed after reading the configurations file

# For loop for the generation of the configurations
for(i in 1:number.configurations) {
  print(i)
}

