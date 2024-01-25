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



correlation.ptime.t2 <- cor(rq4.data.ptime.t2, method="spearman")


library(corrplot)
corrplot(correlation.ptime.t2, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


# install.packages("Hmisc")
library("Hmisc")
t2.ptime <- rcorr(as.matrix(rq4.data.ptime.t2), type="spearman")
t2.ptime

