# Research Question 1 Analysis
# RQ1: Is there any effect of number of elements and visualization technique on the accuracy of responses?
# RQ1a: For pairs, t=2
# RQ1b: For triplets, t=3

# Loads the necessary libraries
library("tidyverse")
library(ggplot2)
# library(hrbrthemes)
# library(gridExtra)
# library(grid)


# Loads the complete experiment data file
experiment.data <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/Curated-Data/Complete-Experiment-Data.csv", 
                            header=TRUE)

# Selects the columns for RQ1 for t=2
rq1.data.t2 <- experiment.data %>% filter(T==2) %>% 
  select(Visualization.Technique,Number.Elements,Accuracy)

# Selects the columns for RQ1 for t=3
rq1.data.t3 <- experiment.data %>% filter(T==3) %>% 
  select(Visualization.Technique,Number.Elements,Accuracy)

###############
### Analysis for T=2, pairs

# Binary Logistic Regression
# Outcome variable: 
#   Accuracy --> Categorical, two factor values "False" and "True"
# Predictor variables:
#   Number.Elements --> Continuous
#   Visualization.Technique --> Categorical, two values "2D-SP" (scatter plots), "2D-PD" (parallel dimensions plots)


# Outcome Variable Levels: Reference group is the first value = False means WITHOUT the outcome, 
#  Second value = True means WITH the outcome
# Conclusion: No need to relevel the factors prior to applying the general linear mixed regression function
# > levels(rq1.data.t2$Accuracy)
# [1] "False" "True" 



