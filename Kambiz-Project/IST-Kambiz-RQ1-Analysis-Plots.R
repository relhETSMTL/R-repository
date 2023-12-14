# Research Question 1 Analysis
# RQ1: Is there any effect of number of elements and visualization technique on the accuracy of responses?
# RQ1a: For pairs, t=2
# RQ1b: For triplets, t=3

# Loads the necessary libraries
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

# Selects the columns for RQ1 for t=2
rq1.data.t2 <- experiment.data %>% filter(T==2) %>% 
  select(Visualization.Technique,Number.Elements,Accuracy) %>%
  droplevels()

# Selects the columns for RQ1 for t=3
rq1.data.t3 <- experiment.data %>% filter(T==3) %>% 
  select(Visualization.Technique,Number.Elements,Accuracy) %>%
  droplevels()

############################################################################################################################
############################################################################################################################
############################################################################################################################
### Analysis for T=2, pairs

# Binary Logistic Regression
# Outcome variable: 
#   Accuracy --> Categorical, two factor values "False" and "True"
# Predictor variables:
#   Number.Elements --> Continuous
#   Visualization.Technique --> Categorical, two values "2D-SP" (scatter plots), "2D-PD" (parallel dimensions plots)
#   Interaction between Number.Elements and Visualization.Technique

# Outcome Variable Levels: Reference group is the first value = False means WITHOUT the outcome, 
#  Second value = True means WITH the outcome
# Conclusion: No need to relevel the factors prior to applying the general linear mixed regression function
# > levels(rq1.data.t2$Accuracy)
# [1] "False" "True" 

# Problem: levels of Visualization.Technique, it shows all four levels even though only the ones for T=2 were filtered
# Solution: use the droplevels() function to eliminate unused levels in all the columns of the data frame
# > levels(x=rq1.data.t2$Visualization.Technique)
# [1] "2D-PD" "2D-SP"


# Exploring normal distribution on Number.Elements
rq1.data.t2 %>%
  ggplot(aes(x = Number.Elements)) +
  geom_density(fill = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(y = "Probability density", x = "Number of Elements t=2")

# Conclusion: Number.Elements do not follow a normal distribution
# Get a table of descriptive statistics
table.desc.t2 <- CreateTableOne(data = rq1.data.t2,
                                strata="Accuracy")
print(table.desc.t2,
      nonnormal = 'Number.Elements',
      showAllLevels = TRUE)

#                                 level     Overall                
# n                                         192                 
# Visualization.Technique (%)     2D-PD     96 (50.0)      
#                                 2D-SP     96 (50.0)          
# Number.Elements (median [IQR])            131.50 [125.00, 266.75]
# Accuracy (%)                    False     17 ( 8.9)          
#                                 True     175 (91.1)  


# Binary Logistic Regression Function

t2.model <- glm(formula = Accuracy ~ Number.Elements + Visualization.Technique + Number.Elements*Visualization.Technique,
                       data = rq1.data.t2,
                       family = binomial("logit"))
summary(object = t2.model)

# get model fit, model significance, odds ratios
odds.n.ends(mod = t2.model)


# Analysis of Odd rations
#                                               OR          2.5 %       97.5 %
#   (Intercept)                                 7.2407362   2.73662762  21.453463
# Number.Elements                               0.9986322   0.99491703  1.002713
# Visualization.Technique2D-SP                  0.9694571   0.02165941  23.666822
# Number.Elements:Visualization.Technique2D-SP  1.0156904   0.99759791  1.06

# Those that DO NOT include the value of 1 in the range:
# (Intercept)  7.2407362   2.73662762  21.453463 --> Unclear the interpretation 
# Visualization.Technique2D-SP 1.0156904   0.99759791  1.06 --> 
#   Interpretation: 2D-SP has slightly higher odds (1.0156904) of accurate responses over 2D-PD
# No other significant odds ratio


## Assumption 1: Linearity
## Notes: For logistic regression, the linearity is tested for each continuous predictor but not the relationship. Instead, 
# the log-odds of the predicted probability against the continuous predictors of the model.
# Continuous predictor: Number.Elements

#make a variable of the log-odds of the predicted values
logit.t2 <- log(x = t2.model$fitted.values/(1-t2.model$fitted.values))

# make a small data frame with the log-odds variable and the age predictor
linearity.t2.data <- data.frame(logit.t2, Number.Elements = t2.model$model$Number.Elements)

# create a plot (Figure 10.9)
linearity.t2.data %>%
  ggplot(aes(x = Number.Elements, y = logit.t2))+
  geom_point(aes(size = "Observation"), color = "gray60", alpha = .6) +
  geom_smooth(se = FALSE, aes(color = "Loess curve")) +
  geom_smooth(method = lm, se = FALSE, aes(color = "linear")) +
  theme_minimal() +
  labs(x = "Number of Elements", y = "Log-odds of accuracy predicted probability") +
  scale_color_manual(name="Type of fit line", values=c("dodgerblue2",
                                                       "deeppink")) +
  scale_size_manual(values = 1.5, name = "")


# It is clearly non-linear from the figure.The loess curve is very far from the linear model line.
# Assumption: Not met.
# Question: We have only 5 values as number of elements. Is it really continuous or better model it was ordinal? If ordinal, then
# the assumption is met by vacuity because there are no continuous predictors.


## Assumption 2: No perfect multicollinearity
# compute GVIF
vif.t2 <- car::vif(mod = t2.model)
vif.t2
# Number.Elements         Visualization.Technique     Number.Elements:Visualization.Technique 
# 1.118315                      4.749727                                4.510710 
# The Df are 1 for all the factors, thus the thresholds for GVIF are GVIF^(1/(2*Df)), since all Df are one then GVIF^(1/2)
# The values are:
#   Number.Elements GVIF =  1.057504
#   Visualization.Technique GVIF =  2.179387
#   Number.Elements:Visualization.Technique = 2.123843 
# Threshold values < 2.5 --> meets the non multicollinearity values ( > 2.5 fails the assumption)
  