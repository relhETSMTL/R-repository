# Kambiz Experiment Design Setup
# Experiment Design
# 1. Random selection and assignment of FMs to the 
# Author: Roberto Erick Lopez Herrejon

# library(ggplot2)
library(tidyverse)
# library(hrbrthemes)

#install.packages('gtools')  # install if necesssary
library(gtools)

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
# coverageAssignmentsData <- read.csv(file = "./FM-Visualization/ResponseAssignmentOptions.csv", header=TRUE)
# attach (coverageAssignmentsData)


## Computes the 24 combinations of two True and two False for each of the matrix cell 2 x 2
## 4 x 3 X 2 X 1 = 24 combinations
choices <- c(1:4) # 1,2,3,4  different value assignments
coverageAssignmentsOptions <- permutations(n = 4, r = 4, v = choices, repeats.allowed = FALSE)


# Creating data frame to hold the experiment data matrix
# QN - question number, T  strength 2 or 3, Vis visualization 1 or 2
# FM feature model name
# T-F pair/triplet the expected correct answer is covered (True) or not (False) 
column.names <- c("QN", "T", "Vis", "FM", "T-F")

experimentDesign.data.frame <- 
  setNames(data.frame(matrix(ncol = 5, nrow = 0)), column.names)


# Function that maps the coverage permutations to boolean values
# Mapping 1=true, 2=false, 3=true, 4=false

mapCoverageToBoolean = function (sampledCoverage) {

  # if equal to 1
  if (sampledCoverage == 1) result <- TRUE
  
  
  # if equal to 2
  if (sampledCoverage == 2) result <- FALSE 
  
  # if equal to 3
  if (sampledCoverage == 3) result <- TRUE

  
  # if equal to 4 
  if (sampledCoverage == 4)  result <- FALSE

  return(result)
  
}  # end of function mapCoverageToBoolean



##################################
### T=2, Vis=1

# Selection of four FM
q1FM <- q1Data %>% sample_n(2)
q2FM <- q2Data %>% sample_n(2)

# Samples one combination of coverage assignments
sampledNumber <- sample(24,1,replace=FALSE)
sampledCoverage <- coverageAssignmentsOptions[sampledNumber,]
sc1 <- sampledCoverage

# Question 1
experimentDesign.data.frame[1,1] ="1"
experimentDesign.data.frame[1,2] ="2"
experimentDesign.data.frame[1,3] ="1"
experimentDesign.data.frame[1,4] =as.vector(q1FM$Q1[1])
experimentDesign.data.frame[1,5] =mapCoverageToBoolean(sampledCoverage[1])

# Question 2
experimentDesign.data.frame[2,1] ="2"
experimentDesign.data.frame[2,2] ="2"
experimentDesign.data.frame[2,3] ="1"
experimentDesign.data.frame[2,4] =as.vector(q1FM$Q1[2])
experimentDesign.data.frame[2,5] =mapCoverageToBoolean(sampledCoverage[2])


# Question 3
experimentDesign.data.frame[3,1] ="3"
experimentDesign.data.frame[3,2] ="2"
experimentDesign.data.frame[3,3] ="1"
experimentDesign.data.frame[3,4] =as.vector(q2FM$Q2[1])
experimentDesign.data.frame[3,5] =mapCoverageToBoolean(sampledCoverage[3])


# Question 4
experimentDesign.data.frame[4,1] ="4"
experimentDesign.data.frame[4,2] ="2"
experimentDesign.data.frame[4,3] ="1"
experimentDesign.data.frame[4,4] =as.vector(q2FM$Q2[2])
experimentDesign.data.frame[4,5] =mapCoverageToBoolean(sampledCoverage[4])


##################################
### T=2, Vis=2

# Selection of four FM
q1FM <- q1Data %>% sample_n(2)
q2FM <- q2Data %>% sample_n(2)

# Samples one combination of coverage assignments
sampledNumber <- sample(24,1,replace=FALSE)
sampledCoverage <- coverageAssignmentsOptions[sampledNumber,]
sc2 <- sampledCoverage

# Question 5
experimentDesign.data.frame[5,1] ="5"
experimentDesign.data.frame[5,2] ="2"
experimentDesign.data.frame[5,3] ="2"
experimentDesign.data.frame[5,4] =as.vector(q1FM$Q1[1])
experimentDesign.data.frame[5,5] =mapCoverageToBoolean(sampledCoverage[1])

# Question 6
experimentDesign.data.frame[6,1] ="6"
experimentDesign.data.frame[6,2] ="2"
experimentDesign.data.frame[6,3] ="2"
experimentDesign.data.frame[6,4] =as.vector(q1FM$Q1[2])
experimentDesign.data.frame[6,5] =mapCoverageToBoolean(sampledCoverage[2])


# Question 7
experimentDesign.data.frame[7,1] ="7"
experimentDesign.data.frame[7,2] ="2"
experimentDesign.data.frame[7,3] ="2"
experimentDesign.data.frame[7,4] =as.vector(q2FM$Q2[1])
experimentDesign.data.frame[7,5] =mapCoverageToBoolean(sampledCoverage[3])


# Question 8
experimentDesign.data.frame[8,1] ="8"
experimentDesign.data.frame[8,2] ="2"
experimentDesign.data.frame[8,3] ="2"
experimentDesign.data.frame[8,4] =as.vector(q2FM$Q2[2])
experimentDesign.data.frame[8,5] =mapCoverageToBoolean(sampledCoverage[4])


##################################
### T=3, Vis=1

# Selection of four FM
q1FM <- q1Data %>% sample_n(2)
q2FM <- q2Data %>% sample_n(2)

# Samples one combination of coverage assignments
sampledNumber <- sample(24,1,replace=FALSE)
sampledCoverage <- coverageAssignmentsOptions[sampledNumber,]
sc3 <- sampledCoverage

# Question 9
experimentDesign.data.frame[9,1] ="9"
experimentDesign.data.frame[9,2] ="3"
experimentDesign.data.frame[9,3] ="1"
experimentDesign.data.frame[9,4] =as.vector(q1FM$Q1[1])
experimentDesign.data.frame[9,5] =mapCoverageToBoolean(sampledCoverage[1])

# Question 10
experimentDesign.data.frame[10,1] ="10"
experimentDesign.data.frame[10,2] ="3"
experimentDesign.data.frame[10,3] ="1"
experimentDesign.data.frame[10,4] =as.vector(q1FM$Q1[2])
experimentDesign.data.frame[10,5] =mapCoverageToBoolean(sampledCoverage[2])


# Question 11
experimentDesign.data.frame[11,1] ="11"
experimentDesign.data.frame[11,2] ="3"
experimentDesign.data.frame[11,3] ="1"
experimentDesign.data.frame[11,4] =as.vector(q2FM$Q2[1])
experimentDesign.data.frame[11,5] =mapCoverageToBoolean(sampledCoverage[3])


# Question 12
experimentDesign.data.frame[12,1] ="12"
experimentDesign.data.frame[12,2] ="3"
experimentDesign.data.frame[12,3] ="1"
experimentDesign.data.frame[12,4] =as.vector(q2FM$Q2[2])
experimentDesign.data.frame[12,5] =mapCoverageToBoolean(sampledCoverage[4])


##################################
### T=3, Vis=2

# Selection of four FM
q1FM <- q1Data %>% sample_n(2)
q2FM <- q2Data %>% sample_n(2)

# Samples one combination of coverage assignments
sampledNumber <- sample(24,1,replace=FALSE)
sampledCoverage <- coverageAssignmentsOptions[sampledNumber,]
sc4 <- sampledCoverage

# Question 13
experimentDesign.data.frame[13,1] ="13"
experimentDesign.data.frame[13,2] ="3"
experimentDesign.data.frame[13,3] ="2"
experimentDesign.data.frame[13,4] =as.vector(q1FM$Q1[1])
experimentDesign.data.frame[13,5] =mapCoverageToBoolean(sampledCoverage[1])

# Question 14
experimentDesign.data.frame[14,1] ="14"
experimentDesign.data.frame[14,2] ="3"
experimentDesign.data.frame[14,3] ="2"
experimentDesign.data.frame[14,4] =as.vector(q1FM$Q1[2])
experimentDesign.data.frame[14,5] =mapCoverageToBoolean(sampledCoverage[2])


# Question 15
experimentDesign.data.frame[15,1] ="15"
experimentDesign.data.frame[15,2] ="3"
experimentDesign.data.frame[15,3] ="2"
experimentDesign.data.frame[15,4] =as.vector(q2FM$Q2[1])
experimentDesign.data.frame[15,5] =mapCoverageToBoolean(sampledCoverage[3])


# Question 16
experimentDesign.data.frame[16,1] ="16"
experimentDesign.data.frame[16,2] ="3"
experimentDesign.data.frame[16,3] ="2"
experimentDesign.data.frame[16,4] =as.vector(q2FM$Q2[2])
experimentDesign.data.frame[16,5] =mapCoverageToBoolean(sampledCoverage[4])


#####################

# Writes out the experiment design file
write.csv(experimentDesign.data.frame, "./FM-Visualization/ExperimentDesign.csv", row.names = FALSE, quote=FALSE)


#####################

# Creation of the random assignment of sequence of questions

nQuestions <- 16
nCombinations <- 50

# Creates the data frame to save the combinations
question.col.names <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16")

questionSequences.data.frame <- 
  setNames(data.frame(matrix(ncol = 16, nrow = 0)), question.col.names)

# Loop that iterates over the number of combinations by adding a row to the frame
for(i in 1:nCombinations) {
  # Random order of the questions
  samplei <- sample(1:nQuestions, nQuestions)

  questionSequences.data.frame[i,] = sample(1:nQuestions, nQuestions)
}


# Writes out the random assignment file
write.csv(questionSequences.data.frame, "./FM-Visualization/ParticipantsQuestionsAssignments.csv", row.names = FALSE, quote=FALSE)





# Format of the generated file
# QN	T	Vis	FM	Y-N
# 1	2	1	FM1	Yes
# 2	2	1	FM7	No
# 3	2	1	FM5	No
# 4	2	1	FM29	Yes


# ## Testing the computation permutations of Yes, No
# 
# # R starts with 1, so we need to create a list of 0 to 10
# choices = c(0:9)
# 
# # n is the number of options, r is the number of groups, v is the value of our choices
# res = permutations(n = 10, r = 3, v = choices, repeats.allowed = FALSE)
# 
# 
# ## For our purposes
# ## 4 x 3 X 2 X 1 = 24 combinations
# choices = c(1:4)
# res = permutations(n = 4, r = 4, v = choices, repeats.allowed = FALSE)

# Examples of obtaining the permutation data
# > res[1,]
# [1] 1 2 3 4
# > res[1,][1]
# [1] 1
# > res[1,][2]
# [1] 2
# > res[21,][3]

# if (sampledCoverage[1] == 3) {
#   TRUE
# }
# 
# if (sampledCoverage[1] == 4) {
#   TRUE
# }

