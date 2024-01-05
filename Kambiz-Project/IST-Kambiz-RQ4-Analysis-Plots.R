# Research Question 4 Analysis
# RQ4: Is the response accuracy related to the proportion of the amount of visual attention given to the different
#      components of the visual stimuli of the test coverage questions?
# RQ4a: For pairs, t=2 accuracy and AOIs proportion time
# RQ4b: For pairs, t=2 accuracy and AOIs proportion count
# RQ4c: For triplets, t=3 accuracy and AOIs proportion time
# RQ4d: For triplets, t=3 accuracy and AOIs proportion count
# Note: Here, we only consider the accurate answers, Accuracy==True values

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

# Selects the columns for T=2, for proportion of time and proportion of count for the 7 AOIs and the aggregated total and Accuracy
rq4a.data.ptime.t2 <- experiment.data %>% filter(T==2) %>% 
  select(Accuracy,Fixation.Time, Question.pftime, Response.pftime, Misc.pftime, Navigation.pftime, Axial.pftime, Solution.pftime, Target.pftime)

rq4b.data.pcount.t2 <- experiment.data %>% filter(T==2) %>% 
  select(Accuracy,Fixation.Count,Question.pfcount, Response.pfcount, Misc.pfcount, Navigation.pfcount, Axial.pfcount, Solution.pfcount, Target.pfcount)



############################################################################################################################
############################################################################################################################
############################################################################################################################
### Research Question 4a -  accuracy and AOIs proportion time 
### Analysis for T=2, pairs

# Binary Logistic Regression
# Outcome variable: 
#   Accuracy --> Categorical, two factor values "False" and "True"
# Predictor variables: All of them numerical values, percentage of fixation time to answer each question
#   Question.pftime, Response.pftime, Misc.pftime, Navigation.pftime, Axial.pftime, Solution.pftime, Target.pftime


# Outcome Variable Levels: Reference group is the first value = False means WITHOUT the outcome, 
#  Second value = True means WITH the outcome
# Conclusion: No need to relevel the factors prior to applying the general linear mixed regression function
# > levels(rq4a.data.ptime.t2$Accuracy)
# [1] "False" "True" 



# Exploring normal distribution on Question.pftime
rq4a.data.ptime.t2 %>%
  ggplot(aes(x = Question.pftime)) +
  geom_density(fill = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(y = "Probability density", x = "Question.pftime t=2")


rq4a.data.ptime.t2 %>%
  ggplot(aes(x = Response.pftime)) +
  geom_density(fill = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(y = "Probability density", x = "Response.pftime t=2")

rq4a.data.ptime.t2 %>%
  ggplot(aes(x = Misc.pftime)) +
  geom_density(fill = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(y = "Probability density", x = "Misc.pftime t=2")

rq4a.data.ptime.t2 %>%
  ggplot(aes(x = Navigation.pftime)) +
  geom_density(fill = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(y = "Probability density", x = "Navigation.pftime t=2")

rq4a.data.ptime.t2 %>%
  ggplot(aes(x = Axial.pftime)) +
  geom_density(fill = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(y = "Probability density", x = "Axial.pftime t=2")

rq4a.data.ptime.t2 %>%
  ggplot(aes(x = Solution.pftime)) +
  geom_density(fill = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(y = "Probability density", x = "Solution.pftime t=2")

rq4a.data.ptime.t2 %>%
  ggplot(aes(x = Target.pftime)) +
  geom_density(fill = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(y = "Probability density", x = "Target.pftime t=2")


shapiro.test(rq4a.data.ptime.t2$Fixation.Time)
# W = 0.81817, p-value = 3.269e-14

shapiro.test(rq4a.data.ptime.t2$Question.pftime)
# W = 0.93728, p-value = 2.176e-07

shapiro.test(rq4a.data.ptime.t2$Response.pftime)
# W = 0.74697, p-value < 2.2e-16

shapiro.test(rq4a.data.ptime.t2$Misc.pftime)
# W = 0.85194, p-value = 1.071e-12

shapiro.test(rq4a.data.ptime.t2$Navigation.pftime)
# W = 0.94018, p-value = 3.825e-07

shapiro.test(rq4a.data.ptime.t2$Axial.pftime)
# W = 0.953, p-value = 5.662e-06

shapiro.test(rq4a.data.ptime.t2$Solution.pftime)
# W = 0.92047, p-value = 1.084e-08

shapiro.test(rq4a.data.ptime.t2$Target.pftime)
# W = 0.97868, p-value = 0.005001

# Note: All p-values were below < 0.05 --> 
# p-values < alpha value --> Reject H0 that values are normally distributed
# Conclusions: None of the variables are normally distributed

# Get a table of descriptive statistics
table.desc.rq4a.time <- CreateTableOne(data = rq4a.data.ptime.t2,
                                strata="Accuracy")
print(table.desc.rq4a.time,
      nonnormal = c('Fixation.Time','Question.pftime','Response.pftime','Misc.pftime','Navigation.pftime',
                    'Axial.pftime','Solution.pftime','Target.pftime'),
      showAllLevels = TRUE)

#                                       level False                         True                          p      test   
# n                                            17                           175                                    
# Accuracy (%)                     False       17 (100.0)                     0 (  0.0)              <0.001        
#                                 True         0 (  0.0)                   175 (100.0)                            
# Fixation.Time (median [IQR])           47425.00 [35962.00, 61995.00] 31464.00 [23158.00, 46796.50]  0.009 nonnorm
# Question.pftime (median [IQR])             0.17 [0.09, 0.21]             0.15 [0.10, 0.21]          0.895 nonnorm
# Response.pftime (median [IQR])             0.06 [0.03, 0.10]             0.05 [0.03, 0.09]          0.810 nonnorm
# Misc.pftime (median [IQR])                 0.02 [0.01, 0.02]             0.02 [0.01, 0.03]          0.353 nonnorm
# Navigation.pftime (median [IQR])           0.19 [0.15, 0.22]             0.21 [0.12, 0.29]          0.375 nonnorm
# Axial.pftime (median [IQR])                0.35 [0.29, 0.49]             0.24 [0.18, 0.35]          0.001 nonnorm
# Solution.pftime (median [IQR])             0.05 [0.01, 0.09]             0.05 [0.02, 0.08]          0.600 nonnorm
# Target.pftime (median [IQR])               0.06 [0.00, 0.16]             0.18 [0.12, 0.27]          0.001 nonnorm



# Binary Logistic Regression Function
t2.ptime.model <- glm(formula = Accuracy ~ Question.pftime + Response.pftime + Misc.pftime + Navigation.pftime + Axial.pftime + Solution.pftime + Target.pftime,
                data = rq4a.data.ptime.t2,
                family = binomial("logit"))
summary(object = t2.ptime.model)

# get model fit, model significance, odds ratios
odds.n.ends(mod = t2.ptime.model)


# Chi-squared        d.f.           p 
# 17.606       7.000       0.014 
# 
# $`Contingency tables (model fit): frequency predicted`
# Number observed
# Number predicted   1   0 Sum
# 1   175  17 192
# 0     0   0   0
# Sum 175  17 192
# 
# $`Count R-squared (model fit): percent correctly predicted`
# [1] 91.14583
# 
# $`Model sensitivity`
# [1] 1
# 
# $`Model specificity`
# [1] 0
# 
# $`Predictor odds ratios and 95% CI`
# OR 2.5 % 97.5 %
#   (Intercept)         0     0    Inf
# Question.pftime   Inf     0    Inf
# Response.pftime   Inf     0    Inf
# Misc.pftime       Inf     0    Inf
# Navigation.pftime Inf     0    Inf
# Axial.pftime      Inf     0    Inf
# Solution.pftime   Inf     0    Inf
# Target.pftime     Inf     0    Inf


# Those that DO NOT include the value of 1 in the range:
# All of them include it in the range, thus unclear interpretation.


## Assumption 1: Linearity
## Notes: For logistic regression, the linearity is tested for each continuous predictor but not the relationship. Instead, 
# the log-odds of the predicted probability against the continuous predictors of the model.
# Continuous predictor: Question.pftime,Response.pftime, Misc.pftime, Navigation.pftime, Axial.pftime, Solution.pftime, Target.pftime 


# Question AOI
# Closer but may be not completely linear
#make a variable of the log-odds of the predicted values
logit.t2.ptime <- log(x = t2.ptime.model$fitted.values/(1-t2.ptime.model$fitted.values))

# make a small data frame with the log-odds variable and the age predictor
linearity.t2.ptime.question.data <- data.frame(logit.t2.ptime, Question.pftime = t2.ptime.model$model$Question.pftime)

# create a plot (Figure 10.9)
linearity.t2.ptime.question.data %>%
  ggplot(aes(x = Question.pftime, y = logit.t2.ptime))+
  geom_point(aes(size = "Observation"), color = "gray60", alpha = .6) +
  geom_smooth(se = FALSE, aes(color = "Loess curve")) +
  geom_smooth(method = lm, se = FALSE, aes(color = "linear")) +
  theme_minimal() +
  labs(x = "Proportion Fixation Time on Question AOI", y = "Log-odds of accuracy predicted probability") +
  scale_color_manual(name="Type of fit line", values=c("dodgerblue2",
                                                       "deeppink")) +
  scale_size_manual(values = 1.5, name = "")

# Response AOI
# Closer below 0.2 values where most of the observations are. May be not completely linear.

# make a small data frame with the log-odds variable and the age predictor
linearity.t2.ptime.response.data <- data.frame(logit.t2.ptime, Response.pftime = t2.ptime.model$model$Response.pftime)

# create a plot (Figure 10.9)
linearity.t2.ptime.response.data %>%
  ggplot(aes(x = Response.pftime, y = logit.t2.ptime))+
  geom_point(aes(size = "Observation"), color = "gray60", alpha = .6) +
  geom_smooth(se = FALSE, aes(color = "Loess curve")) +
  geom_smooth(method = lm, se = FALSE, aes(color = "linear")) +
  theme_minimal() +
  labs(x = "Proportion Fixation Time on Response AOI", y = "Log-odds of accuracy predicted probability") +
  scale_color_manual(name="Type of fit line", values=c("dodgerblue2",
                                                       "deeppink")) +
  scale_size_manual(values = 1.5, name = "")


# Misc AOI
# Very close to linear with a a couple of bumps near 0.02 amd 0.04

# make a small data frame with the log-odds variable and the age predictor
linearity.t2.ptime.misc.data <- data.frame(logit.t2.ptime, Misc.pftime = t2.ptime.model$model$Misc.pftime)

# create a plot (Figure 10.9)
linearity.t2.ptime.misc.data %>%
  ggplot(aes(x = Misc.pftime, y = logit.t2.ptime))+
  geom_point(aes(size = "Observation"), color = "gray60", alpha = .6) +
  geom_smooth(se = FALSE, aes(color = "Loess curve")) +
  geom_smooth(method = lm, se = FALSE, aes(color = "linear")) +
  theme_minimal() +
  labs(x = "Proportion Fixation Time on Misc AOI", y = "Log-odds of accuracy predicted probability") +
  scale_color_manual(name="Type of fit line", values=c("dodgerblue2",
                                                       "deeppink")) +
  scale_size_manual(values = 1.5, name = "")



# Navigation AOI
# Close at the middle, bigger on the sides, above 0.5 and below 0.1 

# make a small data frame with the log-odds variable and the age predictor
linearity.t2.ptime.navigation.data <- data.frame(logit.t2.ptime, Navigation.pftime = t2.ptime.model$model$Navigation.pftime)

# create a plot (Figure 10.9)
linearity.t2.ptime.navigation.data %>%
  ggplot(aes(x = Navigation.pftime, y = logit.t2.ptime))+
  geom_point(aes(size = "Observation"), color = "gray60", alpha = .6) +
  geom_smooth(se = FALSE, aes(color = "Loess curve")) +
  geom_smooth(method = lm, se = FALSE, aes(color = "linear")) +
  theme_minimal() +
  labs(x = "Proportion Fixation Time on Navigation AOI", y = "Log-odds of accuracy predicted probability") +
  scale_color_manual(name="Type of fit line", values=c("dodgerblue2",
                                                       "deeppink")) +
  scale_size_manual(values = 1.5, name = "")


# Axial AOI
# Very close to line after 0.2  

# make a small data frame with the log-odds variable and the age predictor
linearity.t2.ptime.axial.data <- data.frame(logit.t2.ptime, Axial.pftime = t2.ptime.model$model$Axial.pftime)

# create a plot (Figure 10.9)
linearity.t2.ptime.axial.data %>%
  ggplot(aes(x = Axial.pftime, y = logit.t2.ptime))+
  geom_point(aes(size = "Observation"), color = "gray60", alpha = .6) +
  geom_smooth(se = FALSE, aes(color = "Loess curve")) +
  geom_smooth(method = lm, se = FALSE, aes(color = "linear")) +
  theme_minimal() +
  labs(x = "Proportion Fixation Time on Axial AOI", y = "Log-odds of accuracy predicted probability") +
  scale_color_manual(name="Type of fit line", values=c("dodgerblue2",
                                                       "deeppink")) +
  scale_size_manual(values = 1.5, name = "")


# Solution AOI
# Very close, only separates after 0.15 for a few data  

# make a small data frame with the log-odds variable and the age predictor
linearity.t2.ptime.solution.data <- data.frame(logit.t2.ptime, Solution.pftime = t2.ptime.model$model$Solution.pftime)

# create a plot (Figure 10.9)
linearity.t2.ptime.solution.data %>%
  ggplot(aes(x = Solution.pftime, y = logit.t2.ptime))+
  geom_point(aes(size = "Observation"), color = "gray60", alpha = .6) +
  geom_smooth(se = FALSE, aes(color = "Loess curve")) +
  geom_smooth(method = lm, se = FALSE, aes(color = "linear")) +
  theme_minimal() +
  labs(x = "Proportion Fixation Time on Solution AOI", y = "Log-odds of accuracy predicted probability") +
  scale_color_manual(name="Type of fit line", values=c("dodgerblue2",
                                                       "deeppink")) +
  scale_size_manual(values = 1.5, name = "")

# Target AOI
# Very close throughout the whole range. Most likely linear.  

# make a small data frame with the log-odds variable and the age predictor
linearity.t2.ptime.target.data <- data.frame(logit.t2.ptime, Target.pftime = t2.ptime.model$model$Target.pftime)

# create a plot (Figure 10.9)
linearity.t2.ptime.target.data %>%
  ggplot(aes(x = Target.pftime, y = logit.t2.ptime))+
  geom_point(aes(size = "Observation"), color = "gray60", alpha = .6) +
  geom_smooth(se = FALSE, aes(color = "Loess curve")) +
  geom_smooth(method = lm, se = FALSE, aes(color = "linear")) +
  theme_minimal() +
  labs(x = "Proportion Fixation Time on Target AOI", y = "Log-odds of accuracy predicted probability") +
  scale_color_manual(name="Type of fit line", values=c("dodgerblue2",
                                                       "deeppink")) +
  scale_size_manual(values = 1.5, name = "")


# Summary of linearity findings: Misc, Navigation, Axial, Solution, Target appear to be linear. 
# The problems are in Question and Response. 

# Assumption: Not clear if it is met or not.


## Assumption 2: No perfect multicollinearity
# compute GVIF
vif.ptime.t2 <- car::vif(mod = t2.ptime.model)
vif.ptime.t2

# Question.pftime   Response.pftime       Misc.pftime Navigation.pftime      Axial.pftime   Solution.pftime     Target.pftime 
# 1426130.63        1058400.64          60762.87        2665642.08        4562653.57         352851.31        1368079.84 

# Checking values of GVIF
vif.ptime.t2[1]^(1/(2*t2.ptime.model$df.null))
vif.ptime.t2[2]^(1/(2*t2.ptime.model$df.null))
vif.ptime.t2[3]^(1/(2*t2.ptime.model$df.null))
vif.ptime.t2[4]^(1/(2*t2.ptime.model$df.null))
vif.ptime.t2[5]^(1/(2*t2.ptime.model$df.null))
vif.ptime.t2[6]^(1/(2*t2.ptime.model$df.null))
vif.ptime.t2[7]^(1/(2*t2.ptime.model$df.null))

# The Df are 191 for all the factors, thus the thresholds for GVIF are GVIF^(1/(2*Df)), since all Df are one then GVIF^(1/2)
# Question.pftime  1.037792 
# Response.pftime  1.036982 
# Misc.pftime  1.029254 
# Navigation.pftime 1.039493 
# Axial.pftime 1.040956 
# Solution.pftime 1.034005 
# Target.pftime  1.037679 
# Threshold values < 2.5 --> meets the non multicollinearity values ( > 2.5 fails the assumption)

##### Model diagnostics

## Finding outliers with residuals
rq4a.data.ptime.t2.cleaned <- rq4a.data.ptime.t2 %>%
  mutate(standarized = rstandard(model = t2.ptime.model))

# check the residuals for large values > 2 or <-2
rq4a.data.ptime.t2.cleaned %>%
  #  drop_na(standarized) %>%
  filter(standarized >2 | standarized < -2)

# There are eight outliers.
# Accuracy Fixation.Time Question.pftime Response.pftime Misc.pftime Navigation.pftime Axial.pftime Solution.pftime Target.pftime
# 1    False         30062          0.2603          0.0579      0.0519            0.1852       0.2923          0.0942        0.0582
# 2    False         50516          0.0882          0.0225      0.0071            0.3509       0.2017          0.0096        0.3200
# 3    False         40208          0.1701          0.0800      0.0860            0.2927       0.3486          0.0139        0.0087
# 4    False         47544          0.0936          0.0487      0.0131            0.2192       0.3859          0.0100        0.2294
# 5    False         44632          0.2588          0.0228      0.0104            0.0866       0.4197          0.0443        0.1574
# 6    False         74966          0.2491          0.1253      0.0242            0.1753       0.3100          0.0000        0.1161
# 7    False         35962          0.1624          0.0072      0.0172            0.2197       0.2693          0.1092        0.2151
# 8    False        101820          0.0660          0.0318      0.0153            0.3623       0.2908          0.0019        0.2319
# standarized
# 1   -2.120111
# 2   -2.941963
# 3   -2.233403
# 4   -2.376341
# 5   -2.098842
# 6   -2.068485
# 7   -2.702816
# 8   -2.663736

## Using df-betas for identifying influential values

# Computing influences
influence.ptime.t2.mod <- influence.measures(model = t2.ptime.model)
# summarize data frame with dfbetas, cooks, leverage
summary(object = influence.t2.mod$infmat)
# Conclusion: No df_beta values have a Maximum > 2, hence no influential observations

# save the data frame
influence.ptime.t2 <- data.frame(influence.ptime.t2.mod$infmat)

# Filtering by Cook Distance, > 4/n where n is the number of observations
n.ptime.t2 <- nrow(rq4a.data.ptime.t2.cleaned)
influence.ptime.t2 %>% filter(cook.d > 4/n.ptime.t2) %>% nrow()
# Conclusion: There are 16 observations above the threshold for Cook distance


# Leverage 2 * p / n,  p=number of parameters including intercept (=4, columns if dfb beta), n=number of observations
# Since we are considering the 7 AOIs and Intercept
# https://online.stat.psu.edu/stat501/lesson/11/11.2
# Threshold value = 2 * 8 / 192
p.ptime.t2 <- 8
influence.ptime.t2 %>% filter(hat > 0.08333333) %>% nrow() # ((2*8)/192))
# Conclusion: Based on this metric, 26 influential values were found

# Finding influential values combining cook.d and hat metrics
influence.ptime.t2 %>% filter(hat > 0.08333333 & cook.d > 4/n.ptime.t2)
# There are 6 such influential values


### Forest plots

#Box 10.2
# get odds ratio table from lib.model
odds.ptime.t2.mod <- data.frame(odds.n.ends(mod = t2.ptime.model)[6])

# make row names a variable
odds.ptime.t2.mod$var <- row.names(x = odds.ptime.t2.mod)

# change variable names for easier use
names(x = odds.ptime.t2.mod) <- c("OR", "lower", "upper", "variable")

# forest plot of odds ratios from lib.model (Figure 10.15)
odds.ptime.t2.mod %>%
  ggplot(aes(x = variable, y = OR, ymin = lower, ymax = upper)) +
  geom_pointrange(color = "#7463AC") +
  geom_hline(yintercept = 1, lty = 2, color = "deeppink",
             size = 1) +
  coord_flip() +
  labs(x = "Variable from accuracy model", y = "Odds ratio (95% CI)") +
  theme_minimal()


# clean variable names for graph
odds.ptime.t2.mod.cleaned <- odds.ptime.t2.mod %>%
  mutate(variable = dplyr::recode(.x = variable,   # Function name class with function from car package
                                  "(Intercept)" = "Intercept",
                                  "Axial.pftime" = "Axial AOI %FTime",
                                  "Misc.pftime" = "Misc AOI %FTime",
                                  "Navigation.pftime" = "Navigation AOI %FTime",
                                  "Question.pftime" = "Question AOI %FTime",
                                  "Response.pftime" = "Response AOI %FTime",
                                  "Solution.pftime" = "Solution AOI %FTime",
                                  "Target.pftime" = "Target AOI %FTime"))


# change scale of y-axis (flipped) to log scale for visualization
odds.ptime.t2.mod.cleaned %>%
  ggplot(aes(x = variable, y = OR, ymin = lower, ymax = upper)) +
  geom_pointrange(color = "#7463AC") +
  geom_hline(yintercept = 1, lty = 2, color = "deeppink", size = 1) +
  scale_y_log10(breaks = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10),
                minor_breaks = NULL)+
  coord_flip() +
  labs(x = "Variable from accuracy model", y = "Odds ratio (95% CI)") +
  theme_minimal()



############################################################################################################################
############################################################################################################################
############################################################################################################################

## RQ4b, proportions of fixation counts on accuracy
t2.pcount.model <- glm(formula = Accuracy ~ Question.pfcount + Response.pfcount + Misc.pfcount + Navigation.pfcount + Axial.pfcount + Solution.pfcount + Target.pfcount,
                       data = rq4b.data.pcount.t2,
                       family = binomial("logit"))
summary(object = t2.pcount.model)

# get model fit, model significance, odds ratios
odds.n.ends(mod = t2.pcount.model)








### Analysis for T=3, triplets


# Binary Logistic Regression
# Outcome variable: 
#   Accuracy --> Categorical, two factor values "False" and "True"
# Predictor variables:
#   Number.Elements --> Continuous
#   Visualization.Technique --> Categorical, two values "3D-SP" (scatter plots), "3D-PD" (parallel dimensions plots)
#   Interaction between Number.Elements and Visualization.Technique

# Outcome Variable Levels: Reference group is the first value = False means WITHOUT the outcome, 
#  Second value = True means WITH the outcome
# Conclusion: No need to relevel the factors prior to applying the general linear mixed regression function
# > levels(rq1.data.t3$Accuracy)
# [1] "False" "True" 

# Problem: levels of Visualization.Technique, it shows all four levels even though only the ones for T=2 were filtered
# Solution: use the droplevels() function to eliminate unused levels in all the columns of the data frame
# > levels(x=rq1.data.t2$Visualization.Technique)
# [1] "3D-PD" "3D-SP"


# Exploring normal distribution on Number.Elements
rq1.data.t3 %>%
  ggplot(aes(x = Number.Elements)) +
  geom_density(fill = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(y = "Probability density", x = "Number of Elements t=3")

# Conclusion: Number.Elements do not follow a normal distribution
# Get a table of descriptive statistics
table.desc.t3 <- CreateTableOne(data = rq1.data.t3,
                                strata="Accuracy")
print(table.desc.t3,
      nonnormal = 'Number.Elements',
      showAllLevels = TRUE)

# Stratified by Accuracy
#                                   level   False                   True                     p      test   
# n                                        42                       150                                 
# Visualization.Technique (%)     3D-PD     16 ( 38.1)              80 ( 53.3)            0.116        
#                                 3D-SP     26 ( 61.9)              70 ( 46.7)                         
# Number.Elements (median [IQR])  463.00 [463.00, 565.00] 565.00    [144.00, 2491.00]  0.042 nonnorm
# Accuracy (%)                    False     42 (100.0)               0 (  0.0)           <0.001        
#                                 True       0 (  0.0)             150 (100.0)             


# Binary Logistic Regression Function

t3.model <- glm(formula = Accuracy ~ Number.Elements + Visualization.Technique + Number.Elements*Visualization.Technique,
                data = rq1.data.t3,
                family = binomial("logit"))
summary(object = t3.model)

# get model fit, model significance, odds ratios
odds.n.ends(mod = t3.model)

# $`Logistic regression model significance`
# Chi-squared        d.f.           p 
# 15.081       3.000       0.002 
# 
# $`Contingency tables (model fit): frequency predicted`
#                   Number observed
# Number predicted   1   0  Sum
#               1   150  42 192
#               0     0   0   0
#               Sum 150  42 192
# 
# $`Count R-squared (model fit): percent correctly predicted`
# [1] 78.125
# $`Count R-squared (model fit): percent correctly predicted`
# [1] 78.125
# 
# $`Model sensitivity`
# [1] 1
# 
# $`Model specificity`
# [1] 0

# Analysis of Odd rations
#                                               OR        2.5 %         97.5 %
#   (Intercept)                                 2.9354765 1.3813932     6.637225
# Number.Elements                               1.0006038 0.9999281     1.001463
# Visualization.Technique3D-SP                  0.4556689 0.1629032      1.228265
# Number.Elements:Visualization.Technique3D-SP  1.0003614 0.9992713      1.001523

# Those that DO NOT include the value of 1 in the range:
# (Intercept)  2.9354765 1.3813932     6.637225         --> Unclear the interpretation 
# Visualization.Technique3D-SP 0.4556689 0.1629032      1.228265 --> 
#   Interpretation: 2D-SP has slightly higher odds (1.228265) of accurate responses over 3D-PD
# No other significant odds ratio


## Assumption 1: Linearity
## Notes: For logistic regression, the linearity is tested for each continuous predictor but not the relationship. Instead, 
# the log-odds of the predicted probability against the continuous predictors of the model.
# Continuous predictor: Number.Elements

#make a variable of the log-odds of the predicted values
logit.t3 <- log(x = t3.model$fitted.values/(1-t3.model$fitted.values))

# make a small data frame with the log-odds variable and the age predictor
linearity.t3.data <- data.frame(logit.t3, Number.Elements = t3.model$model$Number.Elements)

# create a plot (Figure 10.9)
linearity.t3.data %>%
  ggplot(aes(x = Number.Elements, y = logit.t3))+
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
# Question: We have only 6 values as number of elements. Is it really continuous or better model it was ordinal? If ordinal, then
# the assumption is met by vacuity because there are no continuous predictors.


## Assumption 2: No perfect multicollinearity
# compute GVIF
vif.t3 <- car::vif(mod = t3.model)
vif.t3
# Number.Elements                 Visualization.Technique Number.Elements:Visualization.Technique 
# 1.959208                                1.953720                                2.608900 

# Number.Elements         Visualization.Technique     Number.Elements:Visualization.Technique 
# 1.118315                      4.749727                                4.510710 

# The Df are 1 for all the factors, thus the thresholds for GVIF are GVIF^(1/(2*Df)), since all Df are one then GVIF^(1/2)
# The values are:
#   Number.Elements GVIF =  1.399717 
#   Visualization.Technique GVIF =  0.9768601
#   Number.Elements:Visualization.Technique =  1.30445 
# Threshold values < 2.5 --> meets the non multicollinearity values ( > 2.5 fails the assumption)

##### Model diagnostics

## Finding outliers with residuals
rq1.data.t3.cleaned <- rq1.data.t3 %>%
  mutate(standarized = rstandard(model = t3.model))

# check the residuals for large values > 2 or <-2
rq1.data.t3.cleaned %>%
  #  drop_na(standarized) %>%
  filter(standarized >2 | standarized < -2)

# There is one outlier.
# Visualization.Technique Number.Elements Accuracy standarized
# 1                   3D-PD            2491    False   -2.343966

# Looking at the first outlier  
rq1.data.t3.cleaned %>% filter (Number.Elements==2491)
# Out of the 28 observations for this number of elements (2 questions x 24 participants) this was the only FALSE accuracy response
# There was one question with parallel dimensions plots and one question for scatter plots

# Conclusions: Yes, the outlier but they do not seem to be erroneous.

## Using df-betas for identifying influential values

# Computing influences
influence.t3.mod <- influence.measures(model = t3.model)
# summarize data frame with dfbetas, cooks, leverage
summary(object = influence.t3.mod$infmat)
# Conclusion: No df_beta values have a Maximum > 2, hence no influential observations

# save the data frame
influence.t3 <- data.frame(influence.t3.mod$infmat)

# Filtering by Cook Distance, > 4/n where n is the number of observations
n.t3 <- nrow(rq1.data.t3.cleaned)
influence.t3 %>% filter(cook.d > 4/n.t3) %>% nrow()
# Conclusion: There is one observation above the threshold for Cook distance


# Leverage 2 * p / n,  p=number of parameters including intercept (=4, columns if dfb beta), n=number of observations
# Since we are considering the Number Elements, Visualization Technique, Interaction, and Intercept
# https://online.stat.psu.edu/stat501/lesson/11/11.2
# Threshold value = 2 * 4 / 192
p.t3 <- 4
influence.t3 %>% filter(hat > 0.04166667) %>% nrow() 
# Conclusion: Based on this metric, no influential values were found

### Forest plots

#Box 10.2
# get odds ratio table from lib.model
odds.t3.mod <- data.frame(odds.n.ends(mod = t3.model)[6])

# make row names a variable
odds.t3.mod$var <- row.names(x = odds.t3.mod)

# change variable names for easier use
names(x = odds.t3.mod) <- c("OR", "lower", "upper", "variable")

# forest plot of odds ratios from lib.model (Figure 10.15)
odds.t3.mod %>%
  ggplot(aes(x = variable, y = OR, ymin = lower, ymax = upper)) +
  geom_pointrange(color = "#7463AC") +
  geom_hline(yintercept = 1, lty = 2, color = "deeppink",
             size = 1) +
  coord_flip() +
  labs(x = "Variable from accuracy model", y = "Odds ratio (95% CI)") +
  theme_minimal()


# clean variable names for graph
odds.t3.mod.cleaned <- odds.t3.mod %>%
  mutate(variable = dplyr::recode(.x = variable,   # Function name class with function from car package
                                  "(Intercept)" = "Intercept",
                                  "Number.Elements" = "Number Triplets",
                                  "Visualization.Technique3D-SP" = "Scatter Plots",
                                  "Number.Elements:Visualization.Technique3D-SP" = "Interaction"))


# change scale of y-axis (flipped) to log scale for visualization
odds.t3.mod.cleaned %>%
  ggplot(aes(x = variable, y = OR, ymin = lower, ymax = upper)) +
  geom_pointrange(color = "#7463AC") +
  geom_hline(yintercept = 1, lty = 2, color = "deeppink", size = 1) +
  scale_y_log10(breaks = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10),
                minor_breaks = NULL)+
  coord_flip() +
  labs(x = "Variable from accuracy model", y = "Odds ratio (95% CI)") +
  theme_minimal()
