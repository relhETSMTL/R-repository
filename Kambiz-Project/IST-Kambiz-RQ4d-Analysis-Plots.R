# Research Question 4a Analysis
# RQ4: Is the response accuracy related to the proportion of the amount of visual attention given to the different
#      components of the visual stimuli of the test coverage questions?
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

# experiment.data <- read.csv(file = "D:/Kambiz-Project/Complete-Experiment-Data.csv", 
#                            header=TRUE)



# Selects the columns for T=3, for proportion of count and proportion of count for the 7 AOIs and the aggregated total and Accuracy
rq4d.data.pcount.t3 <- experiment.data %>% filter(T==3) %>% 
  select(Accuracy,Fixation.Count, Question.pfcount, Response.pfcount, Misc.pfcount, Navigation.pfcount, Axial.pfcount, Solution.pfcount, Target.pfcount) 

rq4d.data.pcount.t3$Accuracy <- as.factor(rq4d.data.pcount.t3$Accuracy)


############################################################################################################################
############################################################################################################################
############################################################################################################################
### Research Question 4d -  accuracy and AOIs proportion count 
### Analysis for T=3, triplets

# Binary Logistic Regression
# Outcome variable: 
#   Accuracy --> Categorical, two factor values "False" and "True"
# Predictor variables: All of them numerical values, percentage of fixation time to answer each question
#   Question.pftime, Response.pftime, Misc.pftime, Navigation.pftime, Axial.pftime, Solution.pftime, Target.pftime


# Outcome Variable Levels: Reference group is the first value = False means WITHOUT the outcome, 
#  Second value = True means WITH the outcome
# Conclusion: No need to relevel the factors prior to applying the general linear mixed regression function
# > levels(rq4d.data.count.t3$Accuracy)
# [1] "False" "True" 


# Exploring normal distribution on Question.pftime
rq4d.data.pcount.t3 %>%
  ggplot(aes(x = Question.pfcount)) +
  geom_density(fill = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(y = "Probability density", x = "Question.pfcount t=3")


rq4d.data.pcount.t3 %>%
  ggplot(aes(x = Response.pfcount)) +
  geom_density(fill = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(y = "Probability density", x = "Response.pfcount t=3")

rq4d.data.pcount.t3 %>%
  ggplot(aes(x = Misc.pfcount)) +
  geom_density(fill = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(y = "Probability density", x = "Misc.pfcount t=3")

rq4d.data.pcount.t3 %>%
  ggplot(aes(x = Navigation.pfcount)) +
  geom_density(fill = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(y = "Probability density", x = "Navigation.pfcount t=3")

rq4d.data.pcount.t3 %>%
  ggplot(aes(x = Axial.pfcount)) +
  geom_density(fill = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(y = "Probability density", x = "Axial.pfcount t=3")

rq4d.data.pcount.t3 %>%
  ggplot(aes(x = Solution.pfcount)) +
  geom_density(fill = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(y = "Probability density", x = "Solution.pfcount t=3")

rq4d.data.pcount.t3 %>%
  ggplot(aes(x = Target.pfcount)) +
  geom_density(fill = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(y = "Probability density", x = "Target.pfcount t=3")


shapiro.test(rq4d.data.pcount.t3$Fixation.Count)
# W = 0.85237, p-value = 1.122e-12

shapiro.test(rq4d.data.pcount.t3$Question.pfcount)
# W = 0.98633, p-value = 0.06006


shapiro.test(rq4d.data.pcount.t3$Response.pfcount)
# W = 0.54203, p-value < 2.2e-16

shapiro.test(rq4d.data.pcount.t3$Misc.pfcount)
# W = 0.71452, p-value < 2.2e-16

shapiro.test(rq4d.data.pcount.t3$Navigation.pfcount)
# W = 0.96488, p-value = 9.898e-05

shapiro.test(rq4d.data.pcount.t3$Axial.pfcount)
# W = 0.96416, p-value = 8.239e-05

shapiro.test(rq4d.data.pcount.t3$Solution.pfcount)
# W = 0.79163, p-value = 2.868e-15

shapiro.test(rq4d.data.pcount.t3$Target.pfcount)
# W = 0.96705, p-value = 0.0001752


# Note: All p-values were below < 0.05 except for Question
# p-values < alpha value --> Reject H0 that values are normally distributed
# Conclusions: All except Question of the variables are normally distributed

# Get a table of descriptive statistics
table.desc.rq4d.time <- CreateTableOne(data = rq4d.data.pcount.t3,
                                       strata="Accuracy")
print(table.desc.rq4d.time,
      nonnormal = c('Fixation.Count','Response.pfcount','Misc.pfcount','Navigation.pfcount',
                    'Axial.pfcount','Solution.pfcount','Target.pfcount'),
      showAllLevels = TRUE)

#                                     level False                   True                    p      test   
# n                                           42                     150                                
# Accuracy (%)                      False     42 (100.0)               0 (  0.0)          <0.001        
# True       0 (  0.0)             150 (100.0)                        
# Fixation.Count (median [IQR])           181.50 [111.50, 256.75] 180.50 [121.25, 287.50]  0.646 nonnorm
# Question.pfcount (mean (SD))              0.19 (0.06)             0.18 (0.07)            0.735        
# Response.pfcount (median [IQR])           0.03 [0.02, 0.04]       0.02 [0.01, 0.04]      0.745 nonnorm
# Misc.pfcount (median [IQR])               0.05 [0.02, 0.11]       0.04 [0.02, 0.11]      0.370 nonnorm
# Navigation.pfcount (median [IQR])         0.21 [0.14, 0.30]       0.23 [0.16, 0.33]      0.184 nonnorm
# Axial.pfcount (median [IQR])              0.28 [0.17, 0.40]       0.25 [0.15, 0.36]      0.491 nonnorm
# Solution.pfcount (median [IQR])           0.01 [0.00, 0.03]       0.01 [0.00, 0.04]      0.752 nonnorm
# Target.pfcount (median [IQR])             0.15 [0.10, 0.26]       0.15 [0.08, 0.23]      0.721 nonnorm


# Binary Logistic Regression Function
t3.pcount.model <- glm(formula = Accuracy ~ Question.pfcount + Response.pfcount + Misc.pfcount + Navigation.pfcount + Axial.pfcount + Solution.pfcount + Target.pfcount,
                      data = rq4d.data.pcount.t3,
                      family = binomial("logit"))
summary(object = t3.pcount.model)

# get model fit, model significance, odds ratios
odds.n.ends(mod = t3.pcount.model)


# $`Logistic regression model significance`
# Chi-squared        d.f.           p 
# 3.942       7.000       0.786 
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
# OR         2.5 %        97.5 %
#   (Intercept)        1.839178e+56  5.976881e-78 6.524734e+174
# Question.pfcount   1.376165e-56 5.901152e-175  3.138294e+77
# Response.pfcount   4.345516e-56 6.733136e-175  2.076603e+78
# Misc.pfcount       3.032137e-56 1.828758e-174  5.231443e+77
# Navigation.pfcount 9.658551e-56 2.716252e-174  3.079299e+78
# Axial.pfcount      6.001809e-57 1.437432e-175  2.018871e+77
# Solution.pfcount   2.128982e-53 5.170561e-172  8.936314e+80
# Target.pfcount     6.183747e-57 1.693427e-175  1.896331e+77


# Those that DO NOT include the value of 1 in the range:
# All of them include the value of 1 in the range. Unclear interpretation.

## Assumption 1: Linearity
## Notes: For logistic regression, the linearity is tested for each continuous predictor but not the relationship. Instead, 
# the log-odds of the predicted probability against the continuous predictors of the model.
# Continuous predictor: Question.pfcount,Response.pfcount, Misc.pfcount, Navigation.pfcount, Axial.pfcount, Solution.pfcount, Target.pfcount 


# Question AOI
# Not linear
#make a variable of the log-odds of the predicted values
logit.t3.pcount <- log(x = t3.pcount.model$fitted.values/(1-t3.pcount.model$fitted.values))

# make a small data frame with the log-odds variable
linearity.t3.pcount.question.data <- data.frame(logit.t3.pcount, Question.pfcount = t3.pcount.model$model$Question.pfcount)

# create a plot (Figure 10.9)
linearity.t3.pcount.question.data %>%
  ggplot(aes(x = Question.pfcount, y = logit.t3.pcount))+
  geom_point(aes(size = "Observation"), color = "gray60", alpha = .6) +
  geom_smooth(se = FALSE, aes(color = "Loess curve")) +
  geom_smooth(method = lm, se = FALSE, aes(color = "linear")) +
  theme_minimal() +
  labs(x = "Proportion Fixation Count on Question AOI", y = "Log-odds of accuracy predicted probability") +
  scale_color_manual(name="Type of fit line", values=c("dodgerblue2",
                                                       "deeppink")) +
  scale_size_manual(values = 1.5, name = "")

# Response AOI
# Not linear 

# make a small data frame with the log-odds variable and the predictor
linearity.t3.pcount.response.data <- data.frame(logit.t3.pcount, Response.pfcount = t3.pcount.model$model$Response.pfcount)

# create a plot (Figure 10.9)
linearity.t3.pcount.response.data %>%
  ggplot(aes(x = Response.pfcount, y = logit.t3.pcount))+
  geom_point(aes(size = "Observation"), color = "gray60", alpha = .6) +
  geom_smooth(se = FALSE, aes(color = "Loess curve")) +
  geom_smooth(method = lm, se = FALSE, aes(color = "linear")) +
  theme_minimal() +
  labs(x = "Proportion Fixation Count on Response AOI", y = "Log-odds of accuracy predicted probability") +
  scale_color_manual(name="Type of fit line", values=c("dodgerblue2",
                                                       "deeppink")) +
  scale_size_manual(values = 1.5, name = "")


# Misc AOI
# Not linear  

# make a small data frame with the log-odds variable and the age predictor
linearity.t3.pcount.misc.data <- data.frame(logit.t3.pcount, Misc.pfcount = t3.pcount.model$model$Misc.pfcount)

# create a plot (Figure 10.9)
linearity.t3.pcount.misc.data %>%
  ggplot(aes(x = Misc.pfcount, y = logit.t3.pcount))+
  geom_point(aes(size = "Observation"), color = "gray60", alpha = .6) +
  geom_smooth(se = FALSE, aes(color = "Loess curve")) +
  geom_smooth(method = lm, se = FALSE, aes(color = "linear")) +
  theme_minimal() +
  labs(x = "Proportion Fixation Count on Misc AOI", y = "Log-odds of accuracy predicted probability") +
  scale_color_manual(name="Type of fit line", values=c("dodgerblue2",
                                                       "deeppink")) +
  scale_size_manual(values = 1.5, name = "")



# Navigation AOI
# Close to linear most of the range, except below 0.1 and above 0.5 

# make a small data frame with the log-odds variable and the age predictor
linearity.t3.pcount.navigation.data <- data.frame(logit.t3.pcount, Navigation.pfcount = t3.pcount.model$model$Navigation.pfcount)

# create a plot (Figure 10.9)
linearity.t3.pcount.navigation.data %>%
  ggplot(aes(x = Navigation.pfcount, y = logit.t3.pcount))+
  geom_point(aes(size = "Observation"), color = "gray60", alpha = .6) +
  geom_smooth(se = FALSE, aes(color = "Loess curve")) +
  geom_smooth(method = lm, se = FALSE, aes(color = "linear")) +
  theme_minimal() +
  labs(x = "Proportion Fixation Count on Navigation AOI", y = "Log-odds of accuracy predicted probability") +
  scale_color_manual(name="Type of fit line", values=c("dodgerblue2",
                                                       "deeppink")) +
  scale_size_manual(values = 1.5, name = "")


# Axial AOI
# Partially close to linear, above 0.4 amd between 0.1 and 0.2 

# make a small data frame with the log-odds variable and the age predictor
linearity.t3.pcount.axial.data <- data.frame(logit.t3.pcount, Axial.pfcount = t3.pcount.model$model$Axial.pfcount)

# create a plot (Figure 10.9)
linearity.t3.pcount.axial.data %>%
  ggplot(aes(x = Axial.pfcount, y = logit.t3.pcount))+
  geom_point(aes(size = "Observation"), color = "gray60", alpha = .6) +
  geom_smooth(se = FALSE, aes(color = "Loess curve")) +
  geom_smooth(method = lm, se = FALSE, aes(color = "linear")) +
  theme_minimal() +
  labs(x = "Proportion Fixation Count on Axial AOI", y = "Log-odds of accuracy predicted probability") +
  scale_color_manual(name="Type of fit line", values=c("dodgerblue2",
                                                       "deeppink")) +
  scale_size_manual(values = 1.5, name = "")


# Solution AOI
# Not linear 

# make a small data frame with the log-odds variable and the age predictor
linearity.t3.pcount.solution.data <- data.frame(logit.t3.pcount, Solution.pfcount = t3.pcount.model$model$Solution.pfcount)

# create a plot (Figure 10.9)
linearity.t3.pcount.solution.data %>%
  ggplot(aes(x = Solution.pfcount, y = logit.t3.pcount))+
  geom_point(aes(size = "Observation"), color = "gray60", alpha = .6) +
  geom_smooth(se = FALSE, aes(color = "Loess curve")) +
  geom_smooth(method = lm, se = FALSE, aes(color = "linear")) +
  theme_minimal() +
  labs(x = "Proportion Fixation Count on Solution AOI", y = "Log-odds of accuracy predicted probability") +
  scale_color_manual(name="Type of fit line", values=c("dodgerblue2",
                                                       "deeppink")) +
  scale_size_manual(values = 1.5, name = "")

# Target AOI
# Linear half of the range upto 0.2 

# make a small data frame with the log-odds variable and the age predictor
linearity.t3.pcount.target.data <- data.frame(logit.t3.pcount, Target.pfcount = t3.pcount.model$model$Target.pfcount)

# create a plot (Figure 10.9)
linearity.t3.pcount.target.data %>%
  ggplot(aes(x = Target.pfcount, y = logit.t3.pcount))+
  geom_point(aes(size = "Observation"), color = "gray60", alpha = .6) +
  geom_smooth(se = FALSE, aes(color = "Loess curve")) +
  geom_smooth(method = lm, se = FALSE, aes(color = "linear")) +
  theme_minimal() +
  labs(x = "Proportion Fixation Count on Target AOI", y = "Log-odds of accuracy predicted probability") +
  scale_color_manual(name="Type of fit line", values=c("dodgerblue2",
                                                       "deeppink")) +
  scale_size_manual(values = 1.5, name = "")


# Summary of linearity findings: Unclear when to consider them linear or not.

# Assumption: Not clear if it is met or not.


## Assumption 2: No perfect multicollinearity
# compute GVIF
vif.pcount.t3 <- car::vif(mod = t3.pcount.model)
vif.pcount.t3

# Question.pfcount   Response.pfcount       Misc.pfcount Navigation.pfcount      Axial.pfcount   Solution.pfcount     Target.pfcount 
# 3187.3279           821.6742          4556.7529          8361.4356         12094.2156           505.6188          7297.8581 
 
 
# Checking values of GVIF
vif.pcount.t3[1]^(1/(2*t3.pcount.model$df.null))
vif.pcount.t3[2]^(1/(2*t3.pcount.model$df.null))
vif.pcount.t3[3]^(1/(2*t3.pcount.model$df.null))
vif.pcount.t3[4]^(1/(2*t3.pcount.model$df.null))
vif.pcount.t3[5]^(1/(2*t3.pcount.model$df.null))
vif.pcount.t3[6]^(1/(2*t3.pcount.model$df.null))
vif.pcount.t3[7]^(1/(2*t3.pcount.model$df.null))

# The Df are 191 for all the factors, thus the thresholds for GVIF are GVIF^(1/(2*Df)), since all Df are one then GVIF^(1/2)
# Question.pftime     1.021342 
# Response.pftime     1.017724    
# Misc.pftime         1.022298 
# Navigation.pftime   1.023924  
# Axial.pftime        1.024914 
# Solution.pftime     1.016431 
# Target.pftime       1.023559 
# Threshold values < 2.5 --> meets the non multicollinearity values ( > 2.5 fails the assumption)

##### Model diagnostics

## Finding outliers with residuals
rq4d.data.pcount.t3.cleaned <- rq4d.data.pcount.t3 %>%
  mutate(standarized = rstandard(model = t3.pcount.model))

# check the residuals for large values > 2 or <-2
rq4d.data.pcount.t3.cleaned %>%
  #  drop_na(standarized) %>%
  filter(standarized >2 | standarized < -2)

# There are 6 outliers.
# Accuracy Fixation.Count Question.pfcount Response.pfcount Misc.pfcount Navigation.pfcount Axial.pfcount Solution.pfcount Target.pfcount
# 1    False            180           0.1667           0.0278       0.2111             0.2944        0.1444           0.0556         0.1000
# 2    False            439           0.1093           0.0114       0.1002             0.5057        0.1435           0.0114         0.1185
# 3    False            256           0.2070           0.0156       0.1133             0.5039        0.1016           0.0000         0.0586
# 4    False            154           0.1818           0.0195       0.1364             0.5260        0.0519           0.0000         0.0844
# 5    False            345           0.1246           0.0174       0.0087             0.3014        0.1130           0.0870         0.3478
# 6    False            474           0.1498           0.0127       0.0401             0.5000        0.1709           0.0148         0.1118
# standarized
# 1   -2.055735
# 2   -2.036638
# 3   -2.051433
# 4   -2.088570
# 5   -2.035858
# 6   -2.008871


## Using df-betas for identifying influential values

# Computing influential observations
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

# TODO Revise the whole interpretation one more time