# Test RQ4c
# after 2fa
# one more test

# Research Question 4a Analysis
# RQ4: Is the response accuracy related to the proportion of the amount of visual attention given to the different
#      components of the visual stimuli of the test coverage questions?
# RQ4c: For triplets, t=3 accuracy and AOIs proportion time

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
# experiment.data <- read.csv(file = "../../../Eye-Tracking-Visualization/Experiment-Data/Curated-Data/Complete-Experiment-Data.csv", 
#                             header=TRUE)

experiment.data <- read.csv(file = "D:/Kambiz-Project/Complete-Experiment-Data.csv", 
                            header=TRUE)



# Selects the columns for T=3, for proportion of time and proportion of count for the 7 AOIs and the aggregated total and Accuracy
rq4c.data.ptime.t3 <- experiment.data %>% filter(T==3) %>% 
  select(Accuracy,Fixation.Time, Question.pftime, Response.pftime, Misc.pftime, Navigation.pftime, Axial.pftime, Solution.pftime, Target.pftime) 

rq4c.data.ptime.t3$Accuracy <- as.factor(rq4c.data.ptime.t3$Accuracy)




############################################################################################################################
############################################################################################################################
############################################################################################################################
### Research Question 4c -  accuracy and AOIs proportion time 
### Analysis for T=3, triplets

# Binary Logistic Regression
# Outcome variable: 
#   Accuracy --> Categorical, two factor values "False" and "True"
# Predictor variables: All of them numerical values, percentage of fixation time to answer each question
#   Question.pftime, Response.pftime, Misc.pftime, Navigation.pftime, Axial.pftime, Solution.pftime, Target.pftime


# Outcome Variable Levels: Reference group is the first value = False means WITHOUT the outcome, 
#  Second value = True means WITH the outcome
# Conclusion: No need to relevel the factors prior to applying the general linear mixed regression function
# > levels(rq4a.data.ptime.t3$Accuracy)
# [1] "False" "True" 



# Exploring normal distribution on Question.pftime
rq4c.data.ptime.t3 %>%
  ggplot(aes(x = Question.pftime)) +
  geom_density(fill = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(y = "Probability density", x = "Question.pftime t=3")


rq4c.data.ptime.t3 %>%
  ggplot(aes(x = Response.pftime)) +
  geom_density(fill = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(y = "Probability density", x = "Response.pftime t=3")

rq4c.data.ptime.t3 %>%
  ggplot(aes(x = Misc.pftime)) +
  geom_density(fill = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(y = "Probability density", x = "Misc.pftime t=3")

rq4c.data.ptime.t3 %>%
  ggplot(aes(x = Navigation.pftime)) +
  geom_density(fill = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(y = "Probability density", x = "Navigation.pftime t=3")

rq4c.data.ptime.t3 %>%
  ggplot(aes(x = Axial.pftime)) +
  geom_density(fill = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(y = "Probability density", x = "Axial.pftime t=3")

rq4c.data.ptime.t3 %>%
  ggplot(aes(x = Solution.pftime)) +
  geom_density(fill = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(y = "Probability density", x = "Solution.pftime t=3")

rq4c.data.ptime.t3 %>%
  ggplot(aes(x = Target.pftime)) +
  geom_density(fill = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(y = "Probability density", x = "Target.pftime t=3")


shapiro.test(rq4c.data.ptime.t3$Fixation.Time)
#W = 0.76394, p-value = 2.837e-16

shapiro.test(rq4c.data.ptime.t3$Question.pftime)
# W = 0.96867, p-value = 0.0002709

shapiro.test(rq4c.data.ptime.t3$Response.pftime)
# W = 0.51869, p-value < 2.2e-16

shapiro.test(rq4c.data.ptime.t3$Misc.pftime)
#W = 0.6673, p-value < 2.2e-16

shapiro.test(rq4c.data.ptime.t3$Navigation.pftime)
#W = 0.94248, p-value = 6.048e-07

shapiro.test(rq4c.data.ptime.t3$Axial.pftime)
#W = 0.94822, p-value = 1.987e-06

shapiro.test(rq4c.data.ptime.t3$Solution.pftime)
# W = 0.73326, p-value < 2.2e-16

shapiro.test(rq4c.data.ptime.t3$Target.pftime)
# W = 0.95068, p-value = 3.38e-06

# Note: All p-values were below < 0.05 --> 
# p-values < alpha value --> Reject H0 that values are normally distributed
# Conclusions: None of the variables are normally distributed

# Get a table of descriptive statistics
table.desc.rq4c.time <- CreateTableOne(data = rq4c.data.ptime.t3,
                                       strata="Accuracy")
print(table.desc.rq4c.time,
      nonnormal = c('Fixation.Time','Question.pftime','Response.pftime','Misc.pftime','Navigation.pftime',
                    'Axial.pftime','Solution.pftime','Target.pftime'),
      showAllLevels = TRUE)

#                 Stratified by Accuracy
#                                 level False                         True                          p      test   
#                   n                    42                           150                                    
# Accuracy (%)                      False   42 (100.0)                 0 (  0.0)              <0.001        
#                                   True     0 (  0.0)                   150 (100.0)                            
# Fixation.Time (median [IQR])           51584.00 [34631.75, 75416.25] 54682.00 [35008.75, 82755.00]  0.548 nonnorm
# Question.pftime (median [IQR])             0.16 [0.11, 0.21]             0.15 [0.09, 0.18]          0.079 nonnorm
# Response.pftime (median [IQR])             0.04 [0.02, 0.08]             0.04 [0.02, 0.06]          0.545 nonnorm
# Misc.pftime (median [IQR])                 0.03 [0.01, 0.11]             0.03 [0.01, 0.07]          0.408 nonnorm
# Navigation.pftime (median [IQR])           0.17 [0.11, 0.31]             0.21 [0.12, 0.32]          0.466 nonnorm
# Axial.pftime (median [IQR])                0.25 [0.15, 0.40]             0.23 [0.14, 0.35]          0.498 nonnorm
# Solution.pftime (median [IQR])             0.01 [0.00, 0.05]             0.02 [0.00, 0.07]          0.490 nonnorm
# Target.pftime (median [IQR])               0.15 [0.11, 0.25]             0.17 [0.08, 0.28]          0.676 nonnorm


# Binary Logistic Regression Function
t3.ptime.model <- glm(formula = Accuracy ~ Question.pftime + Response.pftime + Misc.pftime + Navigation.pftime + Axial.pftime + Solution.pftime + Target.pftime,
                      data = rq4c.data.ptime.t3,
                      family = binomial("logit"))
summary(object = t3.ptime.model)

# get model fit, model significance, odds ratios
odds.n.ends(mod = t3.ptime.model)

# Chi-squared        d.f.           p 
# 5.837       7.000       0.559 
# 
# $`Contingency tables (model fit): frequency predicted`
# Number observed
# Number predicted   1   0 Sum
# 1   150  42 192
# 0     0   0   0
# Sum 150  42 192
# 
# $`Count R-squared (model fit): percent correctly predicted`
# [1] 78.125
# 
# $`Model sensitivity`
# [1] 1
# 
# $`Model specificity`
# [1] 0
# 
# $`Predictor odds ratios and 95% CI`
#                     OR          2.5 %        97.5 %
#   (Intercept)     5.751642e-25 1.459520e-213  2.215053e+91
# Question.pftime   2.089605e+23  6.754567e-93 6.885405e+211
# Response.pftime   1.460448e+25  2.537283e-91 7.101549e+213
# Misc.pftime       2.104016e+25  1.106937e-90 6.515998e+213
# Navigation.pftime 1.345995e+25  3.541988e-91 5.251884e+213
# Axial.pftime      4.219690e+24  9.625030e-92 1.748112e+213
# Solution.pftime   1.491921e+28  4.155071e-88 6.096953e+216
# Target.pftime     7.852243e+24  2.279144e-91 3.030834e+213



# Those that DO NOT include the value of 1 in the range:
# All of them include the value of 1 in the range. Unclear interpretation.


## Assumption 1: Linearity
## Notes: For logistic regression, the linearity is tested for each continuous predictor but not the relationship. Instead, 
# the log-odds of the predicted probability against the continuous predictors of the model.
# Continuous predictor: Question.pftime,Response.pftime, Misc.pftime, Navigation.pftime, Axial.pftime, Solution.pftime, Target.pftime 


# Question AOI
# Very close to linear, except from nelow 0.15
#make a variable of the log-odds of the predicted values
logit.t3.ptime <- log(x = t3.ptime.model$fitted.values/(1-t3.ptime.model$fitted.values))

# make a small data frame with the log-odds variable
linearity.t3.ptime.question.data <- data.frame(logit.t3.ptime, Question.pftime = t3.ptime.model$model$Question.pftime)

# create a plot (Figure 10.9)
linearity.t3.ptime.question.data %>%
  ggplot(aes(x = Question.pftime, y = logit.t3.ptime))+
  geom_point(aes(size = "Observation"), color = "gray60", alpha = .6) +
  geom_smooth(se = FALSE, aes(color = "Loess curve")) +
  geom_smooth(method = lm, se = FALSE, aes(color = "linear")) +
  theme_minimal() +
  labs(x = "Proportion Fixation Time on Question AOI", y = "Log-odds of accuracy predicted probability") +
  scale_color_manual(name="Type of fit line", values=c("dodgerblue2",
                                                       "deeppink")) +
  scale_size_manual(values = 1.5, name = "")

# Response AOI
# Very far from linear throughout the range of values

# make a small data frame with the log-odds variable and the age predictor
linearity.t3.ptime.response.data <- data.frame(logit.t3.ptime, Response.pftime = t3.ptime.model$model$Response.pftime)

# create a plot (Figure 10.9)
linearity.t3.ptime.response.data %>%
  ggplot(aes(x = Response.pftime, y = logit.t3.ptime))+
  geom_point(aes(size = "Observation"), color = "gray60", alpha = .6) +
  geom_smooth(se = FALSE, aes(color = "Loess curve")) +
  geom_smooth(method = lm, se = FALSE, aes(color = "linear")) +
  theme_minimal() +
  labs(x = "Proportion Fixation Time on Response AOI", y = "Log-odds of accuracy predicted probability") +
  scale_color_manual(name="Type of fit line", values=c("dodgerblue2",
                                                       "deeppink")) +
  scale_size_manual(values = 1.5, name = "")


# Misc AOI
# Very far from linear 

# make a small data frame with the log-odds variable and the age predictor
linearity.t3.ptime.misc.data <- data.frame(logit.t3.ptime, Misc.pftime = t3.ptime.model$model$Misc.pftime)

# create a plot (Figure 10.9)
linearity.t3.ptime.misc.data %>%
  ggplot(aes(x = Misc.pftime, y = logit.t3.ptime))+
  geom_point(aes(size = "Observation"), color = "gray60", alpha = .6) +
  geom_smooth(se = FALSE, aes(color = "Loess curve")) +
  geom_smooth(method = lm, se = FALSE, aes(color = "linear")) +
  theme_minimal() +
  labs(x = "Proportion Fixation Time on Misc AOI", y = "Log-odds of accuracy predicted probability") +
  scale_color_manual(name="Type of fit line", values=c("dodgerblue2",
                                                       "deeppink")) +
  scale_size_manual(values = 1.5, name = "")



# Navigation AOI
# Close to linear, except below 0.1 aprox

# make a small data frame with the log-odds variable and the age predictor
linearity.t3.ptime.navigation.data <- data.frame(logit.t3.ptime, Navigation.pftime = t3.ptime.model$model$Navigation.pftime)

# create a plot (Figure 10.9)
linearity.t3.ptime.navigation.data %>%
  ggplot(aes(x = Navigation.pftime, y = logit.t3.ptime))+
  geom_point(aes(size = "Observation"), color = "gray60", alpha = .6) +
  geom_smooth(se = FALSE, aes(color = "Loess curve")) +
  geom_smooth(method = lm, se = FALSE, aes(color = "linear")) +
  theme_minimal() +
  labs(x = "Proportion Fixation Time on Navigation AOI", y = "Log-odds of accuracy predicted probability") +
  scale_color_manual(name="Type of fit line", values=c("dodgerblue2",
                                                       "deeppink")) +
  scale_size_manual(values = 1.5, name = "")


# Axial AOI
# Very close to lienar except in a point around 0.3

# make a small data frame with the log-odds variable and the age predictor
linearity.t3.ptime.axial.data <- data.frame(logit.t3.ptime, Axial.pftime = t3.ptime.model$model$Axial.pftime)

# create a plot (Figure 10.9)
linearity.t3.ptime.axial.data %>%
  ggplot(aes(x = Axial.pftime, y = logit.t3.ptime))+
  geom_point(aes(size = "Observation"), color = "gray60", alpha = .6) +
  geom_smooth(se = FALSE, aes(color = "Loess curve")) +
  geom_smooth(method = lm, se = FALSE, aes(color = "linear")) +
  theme_minimal() +
  labs(x = "Proportion Fixation Time on Axial AOI", y = "Log-odds of accuracy predicted probability") +
  scale_color_manual(name="Type of fit line", values=c("dodgerblue2",
                                                       "deeppink")) +
  scale_size_manual(values = 1.5, name = "")


# Solution AOI
# Far from linear, intersect at the middle only

# make a small data frame with the log-odds variable and the age predictor
linearity.t3.ptime.solution.data <- data.frame(logit.t3.ptime, Solution.pftime = t3.ptime.model$model$Solution.pftime)

# create a plot (Figure 10.9)
linearity.t3.ptime.solution.data %>%
  ggplot(aes(x = Solution.pftime, y = logit.t3.ptime))+
  geom_point(aes(size = "Observation"), color = "gray60", alpha = .6) +
  geom_smooth(se = FALSE, aes(color = "Loess curve")) +
  geom_smooth(method = lm, se = FALSE, aes(color = "linear")) +
  theme_minimal() +
  labs(x = "Proportion Fixation Time on Solution AOI", y = "Log-odds of accuracy predicted probability") +
  scale_color_manual(name="Type of fit line", values=c("dodgerblue2",
                                                       "deeppink")) +
  scale_size_manual(values = 1.5, name = "")

# Target AOI
# Close to linear, farther in between 0.05 and 1.7 approx.

# make a small data frame with the log-odds variable and the age predictor
linearity.t3.ptime.target.data <- data.frame(logit.t3.ptime, Target.pftime = t3.ptime.model$model$Target.pftime)

# create a plot (Figure 10.9)
linearity.t3.ptime.target.data %>%
  ggplot(aes(x = Target.pftime, y = logit.t3.ptime))+
  geom_point(aes(size = "Observation"), color = "gray60", alpha = .6) +
  geom_smooth(se = FALSE, aes(color = "Loess curve")) +
  geom_smooth(method = lm, se = FALSE, aes(color = "linear")) +
  theme_minimal() +
  labs(x = "Proportion Fixation Time on Target AOI", y = "Log-odds of accuracy predicted probability") +
  scale_color_manual(name="Type of fit line", values=c("dodgerblue2",
                                                       "deeppink")) +
  scale_size_manual(values = 1.5, name = "")


# Summary of linearity findings: Question, Navigation, Axial, and Target appear to be linear. 
# The problems are in Response, Solution, and Misc. 

# Assumption: Not clear if it is met or not.


## Assumption 2: No perfect multicollinearity
# compute GVIF
vif.ptime.t3 <- car::vif(mod = t3.ptime.model)
vif.ptime.t3

# Question.pftime   Response.pftime       Misc.pftime Navigation.pftime      Axial.pftime   Solution.pftime     Target.pftime 
# 4255.955          2893.866          7243.217         16075.161         19731.154          1506.616         14818.956 

# Checking values of GVIF
vif.ptime.t3[1]^(1/(2*t3.ptime.model$df.null))
vif.ptime.t3[2]^(1/(2*t3.ptime.model$df.null))
vif.ptime.t3[3]^(1/(2*t3.ptime.model$df.null))
vif.ptime.t3[4]^(1/(2*t3.ptime.model$df.null))
vif.ptime.t3[5]^(1/(2*t3.ptime.model$df.null))
vif.ptime.t3[6]^(1/(2*t3.ptime.model$df.null))
vif.ptime.t3[7]^(1/(2*t3.ptime.model$df.null))

# The Df are 191 for all the factors, thus the thresholds for GVIF are GVIF^(1/(2*Df)), since all Df are one then GVIF^(1/2)
# Question.pftime  1.022116  
# Response.pftime  1.021084  
# Misc.pftime  1.023539  
# Navigation.pftime 1.025678  
# Axial.pftime 1.026228  
# Solution.pftime 1.019341  
# Target.pftime  1.025459  
# Threshold values < 2.5 --> meets the non multicollinearity values ( > 2.5 fails the assumption)

##### Model diagnostics

## Finding outliers with residuals
rq4c.data.ptime.t3.cleaned <- rq4c.data.ptime.t3 %>%
  mutate(standarized = rstandard(model = t3.ptime.model))

# check the residuals for large values > 2 or <-2
rq4c.data.ptime.t3.cleaned %>%
  #  drop_na(standarized) %>%
  filter(standarized >2 | standarized < -2)

# There are 3 outliers.
# Accuracy Fixation.Time Question.pftime Response.pftime Misc.pftime Navigation.pftime Axial.pftime Solution.pftime Target.pftime standarized
# 1    False         52400          0.0944          0.0428      0.1853            0.3629       0.1002          0.0881        0.1263   -2.164065
# 2    False        113759          0.1028          0.0361      0.0254            0.1238       0.4039          0.1183        0.1897   -2.040767
# 3    False         42961          0.1968          0.0432      0.0370            0.2850       0.2293          0.0713        0.1425   -2.029518

## Using df-betas for identifying influential values

# Computing influencial observations
influence.ptime.t3.mod <- influence.measures(model = t3.ptime.model)
# summarize data frame with dfbetas, cooks, leverage
summary(object = influence.ptime.t3.mod$infmat)


# dfb.1_             dfb.Qst.            dfb.Rsp.            dfb.Msc.            dfb.Nvg.            dfb.Axl.            dfb.Slt.            dfb.Trg.        
# Min.   :-0.414024   Min.   :-0.593765   Min.   :-0.592064   Min.   :-0.592160   Min.   :-0.593316   Min.   :-0.592097   Min.   :-0.596261   Min.   :-0.592262  
# 1st Qu.:-0.003455   1st Qu.:-0.010863   1st Qu.:-0.010852   1st Qu.:-0.010908   1st Qu.:-0.010318   1st Qu.:-0.010869   1st Qu.:-0.010755   1st Qu.:-0.010837  
# Median : 0.004576   Median :-0.004814   Median :-0.004287   Median :-0.004824   Median :-0.004473   Median :-0.004426   Median :-0.004388   Median :-0.004053  
# Mean   :-0.001678   Mean   : 0.001657   Mean   : 0.001741   Mean   : 0.001705   Mean   : 0.001701   Mean   : 0.001680   Mean   : 0.001757   Mean   : 0.001694  
# 3rd Qu.: 0.010646   3rd Qu.: 0.002818   3rd Qu.: 0.004345   3rd Qu.: 0.003394   3rd Qu.: 0.003675   3rd Qu.: 0.003631   3rd Qu.: 0.003219   3rd Qu.: 0.003410  
# Max.   : 0.592688   Max.   : 0.414306   Max.   : 0.413459   Max.   : 0.415026   Max.   : 0.413773   Max.   : 0.413905   Max.   : 0.413550   Max.   : 0.414310  
# dffit              cov.r            cook.d               hat          
# Min.   :-0.63649   Min.   :0.8836   Min.   :0.0001743   Min.   :0.009483  
# 1st Qu.: 0.07383   1st Qu.:1.0344   1st Qu.:0.0007443   1st Qu.:0.022280  
# Median : 0.10205   Median :1.0493   Median :0.0012841   Median :0.029746  
# Mean   : 0.04032   Mean   :1.0497   Mean   :0.0049585   Mean   :0.041667  
# 3rd Qu.: 0.13328   3rd Qu.:1.0653   3rd Qu.:0.0046858   3rd Qu.:0.041155  
# Max.   : 0.87479   Max.   :2.2713   Max.   :0.0777229   Max.   :0.552384  
 
  
# Conclusion: Only cov.r has values above 2.0, it is not one of the predictors. How to interpret the result?

# save the data frame
influence.ptime.t3 <- data.frame(influence.ptime.t3.mod$infmat)

# Filtering by Cook Distance, > 4/n where n is the number of observations
n.ptime.t3 <- nrow(rq4c.data.ptime.t3.cleaned)
influence.ptime.t3 %>% filter(cook.d > 4/n.ptime.t3) %>% nrow()
# Conclusion: There are 9 observations above the threshold for Cook distance
# How do we interpret that?

# Leverage 2 * p / n,  p=number of parameters including intercept (=4, columns if dfb beta), n=number of observations
# Since we are considering the 7 AOIs and Intercept
# https://online.stat.psu.edu/stat501/lesson/11/11.2
# Threshold value = 2 * 8 / 192
p.ptime.t3 <- 8
influence.ptime.t3 %>% filter(hat > 0.08333333) %>% nrow() # ((2*8)/192))
# Conclusion: Based on this metric, 12 influential values were found

# Finding influential values combining cook.d and hat metrics
influence.ptime.t3 %>% filter(hat > 0.08333333 & cook.d > 4/n.ptime.t3)
# There are 2 such influential values
# dfb.1_    dfb.Qst.    dfb.Rsp.    dfb.Msc.    dfb.Nvg.    dfb.Axl.    dfb.Slt.    dfb.Trg.      dffit    cov.r     cook.d        hat
# 84  -0.03197323  0.02931793  0.04819797  0.03202169  0.03247698  0.03116546  0.02917575  0.03141494  0.8747895 2.271300 0.05499434 0.55238349
# 160  0.59268807 -0.59376553 -0.59206442 -0.59216054 -0.59331558 -0.59209737 -0.59626060 -0.59226178 -0.6364887 0.972077 0.07772292 0.09343668

### Forest plots

#Box 10.2
# get odds ratio table from lib.model
odds.ptime.t3.mod <- data.frame(odds.n.ends(mod = t3.ptime.model)[6])

# make row names a variable
odds.ptime.t3.mod$var <- row.names(x = odds.ptime.t3.mod)

# change variable names for easier use
names(x = odds.ptime.t3.mod) <- c("OR", "lower", "upper", "variable")

# forest plot of odds ratios from lib.model (Figure 10.15)
odds.ptime.t3.mod %>%
  ggplot(aes(x = variable, y = OR, ymin = lower, ymax = upper)) +
  geom_pointrange(color = "#7463AC") +
  geom_hline(yintercept = 1, lty = 2, color = "deeppink",
             size = 1) +
  coord_flip() +
  labs(x = "Variable from accuracy model", y = "Odds ratio (95% CI)") +
  theme_minimal()


# clean variable names for graph
odds.ptime.t3.mod.cleaned <- odds.ptime.t3.mod %>%
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
odds.ptime.t3.mod.cleaned %>%
  ggplot(aes(x = variable, y = OR, ymin = lower, ymax = upper)) +
  geom_pointrange(color = "#7463AC") +
  geom_hline(yintercept = 1, lty = 2, color = "deeppink", size = 1) +
  scale_y_log10(breaks = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10),
                minor_breaks = NULL)+
  coord_flip() +
  labs(x = "Variable from accuracy model", y = "Odds ratio (95% CI)") +
  theme_minimal()

