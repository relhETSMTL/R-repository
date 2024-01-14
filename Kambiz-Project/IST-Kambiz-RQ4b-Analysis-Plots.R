# Research Question 4b Analysis
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

# # Selects the columns for T=2, for proportion of time and proportion of count for the 7 AOIs and the aggregated total and Accuracy
# rq4a.data.ptime.t2 <- experiment.data %>% filter(T==2) %>% 
#   select(Accuracy,Fixation.Time, Question.pftime, Response.pftime, Misc.pftime, Navigation.pftime, Axial.pftime, Solution.pftime, Target.pftime)

rq4b.data.pcount.t2 <- experiment.data %>% filter(T==2) %>% 
  select(Accuracy,Fixation.Count,Question.pfcount, Response.pfcount, Misc.pfcount, Navigation.pfcount, Axial.pfcount, Solution.pfcount, Target.pfcount)



############################################################################################################################
############################################################################################################################
############################################################################################################################
### Research Question 4a -  accuracy and AOIs proportion count 
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



# Exploring normal distribution on Question.pfcount
rq4b.data.pcount.t2 %>%
  ggplot(aes(x = Question.pfcount)) +
  geom_density(fill = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(y = "Probability density", x = "Question.pfcount t=2")


rq4b.data.pcount.t2 %>%
  ggplot(aes(x = Response.pfcount)) +
  geom_density(fill = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(y = "Probability density", x = "Response.pfcount t=2")

rq4b.data.pcount.t2 %>%
  ggplot(aes(x = Misc.pfcount)) +
  geom_density(fill = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(y = "Probability density", x = "Misc.pfcount t=2")

rq4b.data.pcount.t2 %>%
  ggplot(aes(x = Navigation.pfcount)) +
  geom_density(fill = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(y = "Probability density", x = "Navigation.pfcount t=2")

rq4b.data.pcount.t2 %>%
  ggplot(aes(x = Axial.pfcount)) +
  geom_density(fill = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(y = "Probability density", x = "Axial.pfcount t=2")

rq4b.data.pcount.t2 %>%
  ggplot(aes(x = Solution.pfcount)) +
  geom_density(fill = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(y = "Probability density", x = "Solution.pfcount t=2")

rq4b.data.pcount.t2 %>%
  ggplot(aes(x = Target.pfcount)) +
  geom_density(fill = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(y = "Probability density", x = "Target.pcount t=2")


shapiro.test(rq4b.data.pcount.t2$Fixation.Count)
# W = 0.8104, p-value = 1.565e-14

shapiro.test(rq4b.data.pcount.t2$Question.pfcount)
# W = 0.95353, p-value = 6.376e-06

shapiro.test(rq4b.data.pcount.t2$Response.pfcount)
# W = 0.90723, p-value = 1.328e-09

shapiro.test(rq4b.data.pcount.t2$Misc.pfcount)
# W = 0.85103, p-value = 9.674e-13

shapiro.test(rq4b.data.pcount.t2$Navigation.pfcount)
# W = 0.96328, p-value = 6.574e-05

shapiro.test(rq4b.data.pcount.t2$Axial.pfcount)
# W = 0.98186, p-value = 0.01377

shapiro.test(rq4b.data.pcount.t2$Solution.pfcount)
# W = 0.92026, p-value = 1.046e-08
 
shapiro.test(rq4b.data.pcount.t2$Target.pfcount)
# W = 0.97127, p-value = 0.000557

 
# Note: All p-values were below < 0.05 --> 
# p-values < alpha value --> Reject H0 that values are normally distributed
# Conclusions: None of the variables are normally distributed

# Get a table of descriptive statistics
table.desc.rq4b.count <- CreateTableOne(data = rq4b.data.pcount.t2,
                                       strata="Accuracy")
print(table.desc.rq4b.count,
      nonnormal = c('Fixation.Count','Question.pfcount','Response.pfcount','Misc.pfcount','Navigation.pfcount',
                    'Axial.pfcount','Solution.pfcount','Target.pfcount'),
      showAllLevels = TRUE)

#                                     level False                   True                   p      test   
# n                                           17                     175                               
# Accuracy (%)                      False     17 (100.0)               0 (  0.0)         <0.001        
#                                   True       0 (  0.0)             175 (100.0)                       
# Fixation.Count (median [IQR])           143.00 [132.00, 212.00] 112.00 [86.50, 167.00]  0.011 nonnorm
# Question.pfcount (median [IQR])           0.17 [0.12, 0.23]       0.18 [0.13, 0.24]     0.835 nonnorm
# Response.pfcount (median [IQR])           0.04 [0.03, 0.06]       0.04 [0.02, 0.06]     0.555 nonnorm
# Misc.pfcount (median [IQR])               0.02 [0.01, 0.03]       0.02 [0.01, 0.04]     0.298 nonnorm
# Navigation.pfcount (median [IQR])         0.28 [0.19, 0.33]       0.24 [0.17, 0.31]     0.486 nonnorm
# Axial.pfcount (median [IQR])              0.31 [0.29, 0.43]       0.28 [0.20, 0.37]     0.020 nonnorm
# Solution.pfcount (median [IQR])           0.04 [0.02, 0.06]       0.03 [0.01, 0.06]     0.370 nonnorm
# Target.pfcount (median [IQR])             0.07 [0.00, 0.10]       0.15 [0.08, 0.21]    <0.001 nonnorm


# Binary Logistic Regression Function
t2.pcount.model <- glm(formula = Accuracy ~ Question.pfcount + Response.pfcount + Misc.pfcount + Navigation.pfcount + Axial.pfcount + Solution.pfcount + Target.pfcount,
                      data = rq4b.data.pcount.t2,
                      family = binomial("logit"))
summary(object = t2.pcount.model)

# get model fit, model significance, odds ratios
odds.n.ends(mod = t2.pcount.model)


# t2.pcount.model <- glm(formula = Accuracy ~ Question.pfcount + Response.pfcount + Misc.pfcount + Navigation.pfcount + Axial.pfcount + Solution.pfcount + Target.pfcount,
#                        data = rq4b.data.pcount.t2,
#                        family = binomial("logit"))
# summary(object = t2.pcount.model)
# 
# # get model fit, model significance, odds ratios
# odds.n.ends(mod = t2.pcount.model)

# Chi-squared        d.f.           p 
# 28.945           7       <.001 
# 
# $`Contingency tables (model fit): frequency predicted`
# Number observed
# Number predicted   1   0 Sum
# 1   173  15 188
# 0     2   2   4
# Sum 175  17 192
# 
# $`Count R-squared (model fit): percent correctly predicted`
# [1] 91.14583
# 
# $`Model sensitivity`
# [1] 0.9885714
# 
# $`Model specificity`
# [1] 0.1176471
# 
# $`Predictor odds ratios and 95% CI`
# OR 2.5 % 97.5 %
#   (Intercept)        Inf   Inf    Inf
# Question.pfcount     0     0      0
# Response.pfcount     0     0      0
# Misc.pfcount         0     0      0
# Navigation.pfcount   0     0      0
# Axial.pfcount        0     0      0
# Solution.pfcount     0     0      0
# Target.pfcount       0     0      0


# Those that DO NOT include the value of 1 in the range:
# All of them do NOT include it in the range, thus unclear interpretation.


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

 

