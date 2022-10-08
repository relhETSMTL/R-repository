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



# CReating data frame to hold the experiment data matrix
# QN - question number, T  strength 2 or 3, Vis visualization 1 or 2
# FM feature model name
# Y-N pair/triplet the expected correct answer is covered (True) or not (False) 
column.names <- c("QN", "T", "Vis", "FM", "Y-N")

experimentDesign.data.frame <- 
  setNames(data.frame(matrix(ncol = 5, nrow = 0)), column.names)


##################################
### T=2, Vis=1

# Selection of four FM
q1FM <- q1Data %>% sample_n(2)
q2FM <- q2Data %>% sample_n(2)

# Selection of coverage yes/no assignments
coverage <- coverageAssignmentsData %>% sample_n(1)

# Question 1
experimentDesign.data.frame[1,1] ="1"
experimentDesign.data.frame[1,2] ="2"
experimentDesign.data.frame[1,3] ="1"
experimentDesign.data.frame[1,4] =as.vector(q1FM$Q1[1])
experimentDesign.data.frame[1,5] =coverage$V1[1]

# Question 2
experimentDesign.data.frame[2,1] ="2"
experimentDesign.data.frame[2,2] ="2"
experimentDesign.data.frame[2,3] ="1"
experimentDesign.data.frame[2,4] =as.vector(q1FM$Q1[2])
experimentDesign.data.frame[2,5] =coverage$V2[1]


# Question 3
experimentDesign.data.frame[3,1] ="3"
experimentDesign.data.frame[3,2] ="2"
experimentDesign.data.frame[3,3] ="1"
experimentDesign.data.frame[3,4] = as.vector(q2FM$Q2[1])
experimentDesign.data.frame[3,5] =coverage$V3[1]


# Question 4
experimentDesign.data.frame[4,1] ="4"
experimentDesign.data.frame[4,2] ="2"
experimentDesign.data.frame[4,3] ="1"
experimentDesign.data.frame[4,4] =as.vector(q2FM$Q2[2])
experimentDesign.data.frame[4,5] =coverage$V4[1]


##################################
### T=2, Vis=2

# Selection of four FM
q1FM <- q1Data %>% sample_n(2)
q2FM <- q2Data %>% sample_n(2)

# Selection of coverage yes/no assignments
coverage <- coverageAssignmentsData %>% sample_n(1)


# Question 5
experimentDesign.data.frame[5,1] ="5"
experimentDesign.data.frame[5,2] ="2"
experimentDesign.data.frame[5,3] ="2"
experimentDesign.data.frame[5,4] =as.vector(q1FM$Q1[1])
experimentDesign.data.frame[5,5] =coverage$V1[1]

# Question 6
experimentDesign.data.frame[6,1] ="6"
experimentDesign.data.frame[6,2] ="2"
experimentDesign.data.frame[6,3] ="2"
experimentDesign.data.frame[6,4] =as.vector(q1FM$Q1[2])
experimentDesign.data.frame[6,5] =coverage$V2[1]


# Question 7
experimentDesign.data.frame[7,1] ="7"
experimentDesign.data.frame[7,2] ="2"
experimentDesign.data.frame[7,3] ="2"
experimentDesign.data.frame[7,4] =as.vector(q2FM$Q2[1])
experimentDesign.data.frame[7,5] =coverage$V3[1]


# Question 8
experimentDesign.data.frame[8,1] ="8"
experimentDesign.data.frame[8,2] ="2"
experimentDesign.data.frame[8,3] ="2"
experimentDesign.data.frame[8,4] =as.vector(q2FM$Q2[2])
experimentDesign.data.frame[8,5] =coverage$V4[1]


##################################
### T=3, Vis=1

# Selection of four FM
q1FM <- q1Data %>% sample_n(2)
q2FM <- q2Data %>% sample_n(2)

# Selection of coverage yes/no assignments
coverage <- coverageAssignmentsData %>% sample_n(1)

# Question 9
experimentDesign.data.frame[9,1] ="9"
experimentDesign.data.frame[9,2] ="3"
experimentDesign.data.frame[9,3] ="1"
experimentDesign.data.frame[9,4] =as.vector(q1FM$Q1[1])
experimentDesign.data.frame[9,5] =coverage$V1[1]

# Question 10
experimentDesign.data.frame[10,1] ="10"
experimentDesign.data.frame[10,2] ="3"
experimentDesign.data.frame[10,3] ="1"
experimentDesign.data.frame[10,4] =as.vector(q1FM$Q1[2])
experimentDesign.data.frame[10,5] =coverage$V2[1]


# Question 11
experimentDesign.data.frame[11,1] ="11"
experimentDesign.data.frame[11,2] ="3"
experimentDesign.data.frame[11,3] ="1"
experimentDesign.data.frame[11,4] =as.vector(q2FM$Q2[1])
experimentDesign.data.frame[11,5] =coverage$V3[1]


# Question 12
experimentDesign.data.frame[12,1] ="12"
experimentDesign.data.frame[12,2] ="3"
experimentDesign.data.frame[12,3] ="1"
experimentDesign.data.frame[12,4] =as.vector(q2FM$Q2[2])
experimentDesign.data.frame[12,5] =coverage$V4[1]


##################################
### T=3, Vis=2

# Selection of four FM
q1FM <- q1Data %>% sample_n(2)
q2FM <- q2Data %>% sample_n(2)

# Selection of coverage yes/no assignments
coverage <- coverageAssignmentsData %>% sample_n(1)

# Question 13
experimentDesign.data.frame[13,1] ="13"
experimentDesign.data.frame[13,2] ="3"
experimentDesign.data.frame[13,3] ="2"
experimentDesign.data.frame[13,4] =as.vector(q1FM$Q1[1])
experimentDesign.data.frame[13,5] =coverage$V1[1]

# Question 14
experimentDesign.data.frame[14,1] ="14"
experimentDesign.data.frame[14,2] ="3"
experimentDesign.data.frame[14,3] ="2"
experimentDesign.data.frame[14,4] =as.vector(q1FM$Q1[2])
experimentDesign.data.frame[14,5] =coverage$V2[1]


# Question 15
experimentDesign.data.frame[15,1] ="15"
experimentDesign.data.frame[15,2] ="3"
experimentDesign.data.frame[15,3] ="2"
experimentDesign.data.frame[15,4] =as.vector(q2FM$Q2[1])
experimentDesign.data.frame[15,5] =coverage$V3[1]


# Question 16
experimentDesign.data.frame[16,1] ="16"
experimentDesign.data.frame[16,2] ="3"
experimentDesign.data.frame[16,3] ="2"
experimentDesign.data.frame[16,4] =as.vector(q2FM$Q2[2])
experimentDesign.data.frame[16,5] =coverage$V4[1]


#####################

# Writes out the experiment design file
write.csv(experimentDesign.data.frame, "./FM-Visualization/ExperimentDesign.csv", row.names = FALSE, quote=FALSE)



# Format of the generated file
# QN	T	Vis	FM	Y-N
# 1	2	1	FM1	Yes
# 2	2	1	FM7	No
# 3	2	1	FM5	No
# 4	2	1	FM29	Yes



