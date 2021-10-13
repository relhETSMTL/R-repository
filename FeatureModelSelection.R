# Feature Model Selection
# Objective: Randomnly selects FMs 
# Project: Eye-Tracking Analysis of Feature Models Comprehension
# Ecole de technologie superieure
# VERITAS team
# Authors: Elmira Sepasi, Kambiz Belouchi, Roberto E. Lopez-Herrejon


# This code is written to select FM comprehension's experiment samples.

fmData <- read.csv(file = "fmstats.csv", header=TRUE)
attach (fmData)
fmData