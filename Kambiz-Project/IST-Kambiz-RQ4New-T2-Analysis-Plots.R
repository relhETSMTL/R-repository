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

# Selects the columns for T=2, for proportion of time and proportion of count for the 7 AOIs and the aggregated total and Accuracy
rq4.data.ptime.t2 <- experiment.data %>% filter(T==2) %>% 
  select(Question.pftime, Response.pftime, Misc.pftime, Navigation.pftime, Axial.pftime, Solution.pftime, Target.pftime)

rq4.data.pcount.t2 <- experiment.data %>% filter(T==2) %>% 
  select(Question.pfcount, Response.pfcount, Misc.pfcount, Navigation.pfcount, Axial.pfcount, Solution.pfcount, Target.pfcount)


# Checking normality distribution of all the variables
shapiro.test(rq4.data.ptime.t2$Question.pftime) # W = 0.93728, p-value = 2.176e-07
shapiro.test(rq4.data.ptime.t2$Response.pftime) # W = 0.74697, p-value < 2.2e-16
shapiro.test(rq4.data.ptime.t2$Misc.pftime) # W = 0.85194, p-value = 1.071e-12
shapiro.test(rq4.data.ptime.t2$Navigation.pftime) # W = 0.94018, p-value = 3.825e-07
shapiro.test(rq4.data.ptime.t2$Axial.pftime) # W = 0.953, p-value = 5.662e-06
shapiro.test(rq4.data.ptime.t2$Solution.pftime) # W = 0.92047, p-value = 1.084e-08
shapiro.test(rq4.data.ptime.t2$Target.pftime) # W = 0.97868, p-value = 0.005001

# Note: All p-values were below < 0.05 --> 
# p-values < alpha value --> Reject H0 that values are normally distributed
# Conclusions: None of the variables are normally distributed

shapiro.test(rq4.data.pcount.t2$Question.pfcount) # W = 0.95353, p-value = 6.376e-06
shapiro.test(rq4.data.pcount.t2$Response.pfcount) # W = 0.90723, p-value = 1.328e-09
shapiro.test(rq4.data.pcount.t2$Misc.pfcount) # W = 0.85103, p-value = 9.674e-13
shapiro.test(rq4.data.pcount.t2$Navigation.pfcount) # W = 0.96328, p-value = 6.574e-05
shapiro.test(rq4.data.pcount.t2$Axial.pfcount) # W = 0.98186, p-value = 0.01377
shapiro.test(rq4.data.pcount.t2$Solution.pfcount) # W = 0.92026, p-value = 1.046e-08
shapiro.test(rq4.data.pcount.t2$Target.pfcount) # W = 0.97127, p-value = 0.000557

# Note: All p-values were below < 0.05 --> 
# p-values < alpha value --> Reject H0 that values are normally distributed
# Conclusions: None of the variables are normally distributed

# Library for showing the correlation matrix visualization
library(corrplot)
library("Hmisc")

#### Proportion of fixation times
# changing the names of the columns
colnames(rq4.data.ptime.t2) <- c("Question", "Response","Misc","Navigation","Axial","Solution","Target") 
correlation.ptime.t2 <- cor(rq4.data.ptime.t2, method="spearman")
corrplot(correlation.ptime.t2, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

t2.ptime <- rcorr(as.matrix(rq4.data.ptime.t2), type="spearman")
t2.ptime

# Question Response  Misc Navigation Axial Solution Target
# Question       1.00     0.25  0.22      -0.43 -0.19     0.00  -0.10
# Response       0.25     1.00  0.18      -0.19 -0.26    -0.03  -0.04
# Misc           0.22     0.18  1.00      -0.11  0.00    -0.04  -0.14
# Navigation    -0.43    -0.19 -0.11       1.00 -0.32    -0.37  -0.04
# Axial         -0.19    -0.26  0.00      -0.32  1.00     0.04  -0.50
# Solution       0.00    -0.03 -0.04      -0.37  0.04     1.00  -0.01
# Target        -0.10    -0.04 -0.14      -0.04 -0.50    -0.01   1.00
# 
# n= 192 
# 
# 
# P
# Question Response Misc   Navigation Axial  Solution Target
# Question            0.0006   0.0021 0.0000     0.0082 0.9962   0.1729
# Response   0.0006            0.0109 0.0100     0.0002 0.6457   0.6216
# Misc       0.0021   0.0109          0.1139     0.9753 0.5681   0.0602
# Navigation 0.0000   0.0100   0.1139            0.0000 0.0000   0.6082
# Axial      0.0082   0.0002   0.9753 0.0000            0.5639   0.0000
# Solution   0.9962   0.6457   0.5681 0.0000     0.5639          0.8735
# Target     0.1729   0.6216   0.0602 0.6082     0.0000 0.8735         


#### Proportion of fixation count
colnames(rq4.data.pcount.t2) <- c("Question", "Response","Misc","Navigation","Axial","Solution","Target") 
correlation.pcount.t2 <- cor(rq4.data.pcount.t2, method="spearman")
corrplot(correlation.pcount.t2, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

t2.pcount <- rcorr(as.matrix(rq4.data.pcount.t2), type="spearman")
t2.pcount

# Question Response  Misc Navigation Axial Solution Target
# Question       1.00     0.16  0.23      -0.58 -0.14     0.07  -0.09
# Response       0.16     1.00  0.24      -0.09 -0.20    -0.03  -0.12
# Misc           0.23     0.24  1.00      -0.19  0.10    -0.01  -0.27
# Navigation    -0.58    -0.09 -0.19       1.00 -0.31    -0.35  -0.03
# Axial         -0.14    -0.20  0.10      -0.31  1.00     0.08  -0.57
# Solution       0.07    -0.03 -0.01      -0.35  0.08     1.00  -0.07
# Target        -0.09    -0.12 -0.27      -0.03 -0.57    -0.07   1.00
# 
# n= 192 
# 
# 
# P
# Question Response Misc   Navigation Axial  Solution Target
# Question            0.0251   0.0011 0.0000     0.0532 0.3169   0.2280
# Response   0.0251            0.0008 0.2074     0.0059 0.6795   0.1044
# Misc       0.0011   0.0008          0.0072     0.1761 0.8724   0.0002
# Navigation 0.0000   0.2074   0.0072            0.0000 0.0000   0.6720
# Axial      0.0532   0.0059   0.1761 0.0000            0.2841   0.0000
# Solution   0.3169   0.6795   0.8724 0.0000     0.2841          0.3665
# Target     0.2280   0.1044   0.0002 0.6720     0.0000 0.3665         



