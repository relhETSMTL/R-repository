# Kambiz Experiment Design Setup
# Experiment Design
# 1. Random selection and assignment of FMs to the 
# Author: Roberto Erick Lopez Herrejon

# library(ggplot2)
library(tidyverse)
# library(hrbrthemes)

# Seeds the random number generator for replicability
set.seed(37)



# Loads the names of feature models of quartiles 1 and 2
# Q1
q1Data <- read.csv(file = "./FM-Visualization/Q1.csv", header=TRUE)
attach (q1Data)
# Q2
q2Data <- read.csv(file = "./FM-Visualization/Q2.csv", header=TRUE)
attach (q2Data)


# Loads the 16 alternatives for Covered Yes/No answers
coverageAssignmentsData <- read.csv(file = "./FM-Visualization/ResponseAssignmentOptions.csv", header=TRUE)
attach (coverageAssignmentsData)


t2 <- rep(2,4)      # sequence of 2,2,2,2
v1 <- rep(1,4)      # sequence of 1,1,1,1
t3 <- rep(3,4)      # sequence of 3,3,3,3
v2 <- rep(2,4)      # sequence of 2,2,2,2


# Format of the generated file
# QN	T	Vis	FM	Y-N
# 1	2	1	FM1	Yes
# 2	2	1	FM7	No
# 3	2	1	FM5	No
# 4	2	1	FM29	Yes

#########################
### T=2, Visualization 1


# Selection of four FM
q1FM <- q1Data %>% sample_n(2)
q2FM <- q2Data %>% sample_n(2)


qn.1.4 <- seq(1,4)  # question numbers 1 to 4




# Selection of two FM from Q2

v <- c(as.vector(q1Data$Q1[1]), as.vector(q2Data$Q2[2]))
v[1]




