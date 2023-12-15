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

##### Model diagnostics

## Finding outliers with residuals
rq1.data.t2.cleaned <- rq1.data.t2 %>%
   mutate(standarized = rstandard(model = t2.model))
 
# check the residuals for large values > 2 or <-2
rq1.data.t2.cleaned %>%
#  drop_na(standarized) %>%
  filter(standarized >2 | standarized < -2)

# There are two outliers.
# Visualization.Technique Number.Elements Accuracy standarized
# 1                   2D-SP             127    False   -2.770048
# 2                   2D-SP              66    False   -2.470792

# Looking at the first outlier  
rq1.data.t2.cleaned %>% filter (Number.Elements==127 & Accuracy=="False")

# From all the points that are False and 127 Number.Elements, it is the only one for 2D-SP plots
# 1                   2D-SP             127    False   -2.770048  <-- outlier
# 2                   2D-PD             127    False   -1.992816
# 3                   2D-PD             127    False   -1.992816
# 4                   2D-PD             127    False   -1.992816
# 5                   2D-PD             127    False   -1.992816

# Looking at the second outlier
rq1.data.t2.cleaned %>% filter (Number.Elements==66 & Accuracy=="False")  
# It is the only one FALSE for Number.Elements 66, all of points are for 2D-SP plots

# Conclusions: Yes, two outliers but they do not seem to be erroneous.

## Using df-betas for identifying influential values

# Computing influences
influence.t2.mod <- influence.measures(model = t2.model)
# summarize data frame with dfbetas, cooks, leverage
summary(object = influence.t2.mod$infmat)
# Conclusion: No df_beta values have a Maximum > 2, hence no influential observations


# save the data frame
influence.t2 <- data.frame(influence.t2.mod$infmat)

# Filtering by Cook Distance, > 4/n where n is the number of observations
n.t2 <- nrow(rq1.data.t2.cleaned)
influence.t2 %>% filter(cook.d > 4/n.t2) %>% nrow()
# Conclusion: There are 17 observations above the threshold for Cood distance


# Leverage 2 * p / n,  p=number of parameters including intercept (=4, columns if dfb beta), n=number of observations
# Since we are considering the Number Elements, Visualization Technique, Interaction, and Intercept
# https://online.stat.psu.edu/stat501/lesson/11/11.2
# Threshold value = 2 * 4 / 192
p.t2 <- 4
influence.t2 %>% filter(hat > 0.04166667) %>% nrow() # ((2*4)/192)) # 2*p.t2/n.t2)
# Conclusion: Based on this metric, no influential values were found

### Forest plots

#Box 10.2
# get odds ratio table from lib.model
odds.t2.mod <- data.frame(odds.n.ends(mod = t2.model)[6])

# make row names a variable
odds.t2.mod$var <- row.names(x = odds.t2.mod)

# change variable names for easier use
names(x = odds.t2.mod) <- c("OR", "lower", "upper", "variable")

# forest plot of odds ratios from lib.model (Figure 10.15)
odds.t2.mod %>%
  ggplot(aes(x = variable, y = OR, ymin = lower, ymax = upper)) +
  geom_pointrange(color = "#7463AC") +
  geom_hline(yintercept = 1, lty = 2, color = "deeppink",
             size = 1) +
  coord_flip() +
  labs(x = "Variable from accuracy model", y = "Odds ratio (95% CI)") +
  theme_minimal()


# clean variable names for graph
odds.t2.mod.cleaned <- odds.t2.mod %>%
  mutate(variable = dplyr::recode(.x = variable,   # Function name class with function from car package
                           "(Intercept)" = "Intercept",
                           "Number.Elements" = "Number Pairs",
                           "Visualization.Technique2D-SP" = "Scatter Plots",
                           "Number.Elements:Visualization.Technique2D-SP" = "Interaction"))
                           

# change scale of y-axis (flipped) to log scale for visualization
odds.t2.mod.cleaned %>%
  ggplot(aes(x = variable, y = OR, ymin = lower, ymax = upper)) +
  geom_pointrange(color = "#7463AC") +
  geom_hline(yintercept = 1, lty = 2, color = "deeppink", size = 1) +
  scale_y_log10(breaks = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10),
                minor_breaks = NULL)+
  coord_flip() +
  labs(x = "Variable from accuracy model", y = "Odds ratio (95% CI)") +
  theme_minimal()


