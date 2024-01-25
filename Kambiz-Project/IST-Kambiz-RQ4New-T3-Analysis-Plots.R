# New Research Question 4 Analysis
# T=2, correlation studies

library("tidyverse")
library(ggplot2)
library(package = "tableone")     # descriptive statistics table
library(package = "odds.n.ends")  # odds ratio analysis
library(car)

# library(hrbrthemes)
# library(gridExtra)
# library(grid)


# Loads the complete experiment data file
experiment.data <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/Curated-Data/Complete-Experiment-Data.csv", 
                            header=TRUE)

# Selects the columns for T=3, for proportion of time and proportion of count for the 7 AOIs and the aggregated total and Accuracy
rq4.data.ptime.t3 <- experiment.data %>% filter(T==3) %>% 
  select(Accuracy,Fixation.Time, Question.pftime, Response.pftime, Misc.pftime, Navigation.pftime, Axial.pftime, Solution.pftime, Target.pftime)

rq4.data.pcount.t3 <- experiment.data %>% filter(T==3) %>% 
  select(Accuracy,Fixation.Count,Question.pfcount, Response.pfcount, Misc.pfcount, Navigation.pfcount, Axial.pfcount, Solution.pfcount, Target.pfcount)


# Checking normality distribution of all the variables
shapiro.test(rq4.data.ptime.t3$Question.pftime) # W = 0.96867, p-value = 0.0002709
shapiro.test(rq4.data.ptime.t3$Response.pftime) # W = 0.51869, p-value < 2.2e-16
shapiro.test(rq4.data.ptime.t3$Misc.pftime) # W = 0.6673, p-value < 2.2e-16
shapiro.test(rq4.data.ptime.t3$Navigation.pftime) # W = 0.94248, p-value = 6.048e-07
shapiro.test(rq4.data.ptime.t3$Axial.pftime) # W = 0.94822, p-value = 1.987e-06
shapiro.test(rq4.data.ptime.t3$Solution.pftime) # W = 0.73326, p-value < 2.2e-16
shapiro.test(rq4.data.ptime.t3$Target.pftime) # W = 0.95068, p-value = 3.38e-06

# Note: All p-values were below < 0.05 --> 
# p-values < alpha value --> Reject H0 that values are normally distributed
# Conclusions: None of the variables are normally distributed


shapiro.test(rq4.data.pcount.t3$Question.pfcount) #  W = 0.98633, p-value = 0.06006
shapiro.test(rq4.data.pcount.t3$Response.pfcount) # W = 0.54203, p-value < 2.2e-16
shapiro.test(rq4.data.pcount.t3$Misc.pfcount) # W = 0.71452, p-value < 2.2e-16
shapiro.test(rq4.data.pcount.t3$Navigation.pfcount) # W = 0.96488, p-value = 9.898e-05
shapiro.test(rq4.data.pcount.t3$Axial.pfcount) # W = 0.96416, p-value = 8.239e-05
shapiro.test(rq4.data.pcount.t3$Solution.pfcount) # W = 0.79163, p-value = 2.868e-15
shapiro.test(rq4.data.pcount.t3$Target.pfcount) # W = 0.96705, p-value = 0.0001752




