# New Research Question 4 Analysis
# T=3, correlation studies

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

# Selects the columns for T=2, for proportion of time and proportion of count for the 7 AOIs and the aggregated total and Accuracy
rq4.data.ptime.t3 <- experiment.data %>% filter(T==3) %>% 
  select(Question.pftime, Response.pftime, Misc.pftime, Navigation.pftime, Axial.pftime, Solution.pftime, Target.pftime)

rq4.data.pcount.t3 <- experiment.data %>% filter(T==3) %>% 
  select(Question.pfcount, Response.pfcount, Misc.pfcount, Navigation.pfcount, Axial.pfcount, Solution.pfcount, Target.pfcount)


# Checking normality distribution of all the variables
shapiro.test(rq4.data.ptime.t3$Question.pftime) 
shapiro.test(rq4.data.ptime.t3$Response.pftime) 
shapiro.test(rq4.data.ptime.t3$Misc.pftime) 
shapiro.test(rq4.data.ptime.t3$Navigation.pftime) 
shapiro.test(rq4.data.ptime.t3$Axial.pftime) 
shapiro.test(rq4.data.ptime.t3$Solution.pftime) 
shapiro.test(rq4.data.ptime.t3$Target.pftime) 

# Note: All p-values were below < 0.05 --> 
# p-values < alpha value --> Reject H0 that values are normally distributed
# Conclusions: None of the variables are normally distributed

shapiro.test(rq4.data.pcount.t3$Question.pfcount) 
shapiro.test(rq4.data.pcount.t3$Response.pfcount) 
shapiro.test(rq4.data.pcount.t3$Misc.pfcount) 
shapiro.test(rq4.data.pcount.t3$Navigation.pfcount) 
shapiro.test(rq4.data.pcount.t3$Axial.pfcount) 
shapiro.test(rq4.data.pcount.t3$Solution.pfcount) 
shapiro.test(rq4.data.pcount.t3$Target.pfcount) 

# Note: All p-values were below < 0.05 --> 
# p-values < alpha value --> Reject H0 that values are normally distributed
# Conclusions: None of the variables are normally distributed

# Library for showing the correlation matrix visualization
library(corrplot)
library("Hmisc")

#### Proportion of fixation times
# changing the names of the columns
colnames(rq4.data.ptime.t3) <- c("Question", "Response","Misc","Navigation","Axial","Solution","Target") 
correlation.ptime.t3 <- cor(rq4.data.ptime.t3, method="spearman")
corrplot(correlation.ptime.t3, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

t3.ptime <- rcorr(as.matrix(rq4.data.ptime.t3), type="spearman")
t3.ptime

# Question Response  Misc Navigation Axial Solution Target
# Question       1.00     0.37  0.07      -0.36  0.15     0.19  -0.29
# Response       0.37     1.00 -0.20      -0.52  0.21     0.34  -0.21
# Misc           0.07    -0.20  1.00       0.13 -0.37    -0.43  -0.09
# Navigation    -0.36    -0.52  0.13       1.00 -0.50    -0.40   0.08
# Axial          0.15     0.21 -0.37      -0.50  1.00     0.36  -0.48
# Solution       0.19     0.34 -0.43      -0.40  0.36     1.00  -0.20
# Target        -0.29    -0.21 -0.09       0.08 -0.48    -0.20   1.00
# 
# n= 192 
# 
# 
# P
# Question Response Misc   Navigation Axial  Solution Target
# Question            0.0000   0.3595 0.0000     0.0322 0.0097   0.0000
# Response   0.0000            0.0065 0.0000     0.0039 0.0000   0.0035
# Misc       0.3595   0.0065          0.0681     0.0000 0.0000   0.2149
# Navigation 0.0000   0.0000   0.0681            0.0000 0.0000   0.2983
# Axial      0.0322   0.0039   0.0000 0.0000            0.0000   0.0000
# Solution   0.0097   0.0000   0.0000 0.0000     0.0000          0.0064
# Target     0.0000   0.0035   0.2149 0.2983     0.0000 0.0064         

#### Proportion of fixation count
colnames(rq4.data.pcount.t3) <- c("Question", "Response","Misc","Navigation","Axial","Solution","Target") 
correlation.pcount.t3 <- cor(rq4.data.pcount.t3, method="spearman")
corrplot(correlation.pcount.t3, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

t3.pcount <- rcorr(as.matrix(rq4.data.pcount.t3), type="spearman")
t3.pcount


# Question Response  Misc Navigation Axial Solution Target
# Question       1.00     0.33  0.06      -0.46  0.08     0.20  -0.30
# Response       0.33     1.00 -0.15      -0.41  0.23     0.34  -0.28
# Misc           0.06    -0.15  1.00      -0.05 -0.47    -0.45   0.09
# Navigation    -0.46    -0.41 -0.05       1.00 -0.45    -0.27   0.09
# Axial          0.08     0.23 -0.47      -0.45  1.00     0.36  -0.53
# Solution       0.20     0.34 -0.45      -0.27  0.36     1.00  -0.23
# Target        -0.30    -0.28  0.09       0.09 -0.53    -0.23   1.00
# 
# n= 192 
# 
# 
# P
# Question Response Misc   Navigation Axial  Solution Target
# Question            0.0000   0.3805 0.0000     0.2588 0.0061   0.0000
# Response   0.0000            0.0382 0.0000     0.0011 0.0000   0.0000
# Misc       0.3805   0.0382          0.4649     0.0000 0.0000   0.2165
# Navigation 0.0000   0.0000   0.4649            0.0000 0.0002   0.1957
# Axial      0.2588   0.0011   0.0000 0.0000            0.0000   0.0000
# Solution   0.0061   0.0000   0.0000 0.0002     0.0000          0.0014
# Target     0.0000   0.0000   0.2165 0.1957     0.0000 0.0014         

