# Research Question 2 Analysis
# RQ1: Is there any effect of number of elements and visualization technique on the time on task of responses?
# RQ2a: For pairs, t=2
# RQ2b: For triplets, t=3
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

# Selects the columns for RQ1 for t=2, 175 True with 17 False, transform the Elapsed.Time to secs (divide by 1000)
rq2.data.t2 <- experiment.data %>% filter(T==2 & Accuracy=="True") %>% 
  select(Visualization.Technique,Number.Elements,Elapsed.Time) %>%
  droplevels() %>%
  mutate(Elapsed.Time=Elapsed.Time/1000)

# Selects the columns for RQ1 for t=3, 150 True with 42 False, transform the Elapsed.Time to secs (divide by 1000)
rq2.data.t3 <- experiment.data %>% filter(T==3 & Accuracy=="True") %>% 
  select(Visualization.Technique,Number.Elements,Elapsed.Time) %>%
  droplevels() %>%
  mutate(Elapsed.Time=Elapsed.Time/1000)


############################################################################################################################
############################################################################################################################
############################################################################################################################
### Research Question 1a -  Relationship between Visualization Techniques, Number of Pairs/Triples with Accuracy
### Analysis for T=2, pairs

# https://www.andrew.cmu.edu/user/achoulde/94842/lectures/lecture10/lecture10-94842.html

tot.by.vist.numel.t2 <- lm(formula = Elapsed.Time ~ Visualization.Technique + Number.Elements + Visualization.Technique*Number.Elements,
                              data = rq2.data.t2, na.action = na.exclude)
summary(object = tot.by.vist.numel.t2)


# Residuals:
#   Min      1Q  Median      3Q     Max 
# -51.344 -16.896  -7.009   7.105 159.181 
# 
# Coefficients:
#                                             Estimate    Std. Error    t value   Pr(>|t|)    
# (Intercept)                                  39.07775   5.90996       6.612     4.64e-10 ***
#   Visualization.Technique2D-SP              -4.00652    8.01973       -0.500    0.61801    
# Number.Elements                               0.07526    0.02454      3.067     0.00251 ** 
#   Visualization.Technique2D-SP:Number.Elements -0.04351    0.03240    -1.343    0.18110    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 29.21 on 171 degrees of freedom
# Multiple R-squared:  0.1023,	Adjusted R-squared:  0.08659 
# F-statistic: 6.499 on 3 and 171 DF,  p-value: 0.0003443


## Statistical significance of slope
## Reported in Wald test (column t value). with given p-value
## Ho: Slope of the line is equal to zero.
## HA: Slope of the line is not equal to zero.
## Considering the p-values, except for Visualization Technique, H0 is rejected for all of them. Meaning that
## for every %increase in Elapsed.Time hereis an increase of .07 or 0.04 secs. For visualization.Technique the slope is equal to zero.
### TODO revised interpretation for the intercept and Visualization.Technique

## Computing confidence intervals for regression parameters
ci.tot.by.vist.numel.t2 <- confint(object = tot.by.vist.numel.t2)
ci.tot.by.vist.numel.t2

# The slope values are within the confiden intervals 2.5% and 97.5%
# (Intercept)                                   27.41188959 50.74361185
# Visualization.Technique2D-SP                 -19.83693549 11.82389791
# Number.Elements                                0.02682618  0.12368752
# Visualization.Technique2D-SP:Number.Elements  -0.10747636  0.02044936

# Interpreting model fit
# Multiple R-squared:  0.1023,	Adjusted R-squared:  0.08659 
# R-squared means that 10.23% of the variation on time on task (Elapsed.Time) is explained by the Visualization.Technique, Number.Elements and their
# interaction. Thus nearly 90% of the variation is explained by other factors.
# Adjusted R-squared:  0.08659, considers the impact of adding more predictors. Thus in the end only 8% variation is due to the predictor 
# variables.


# Checking model assumptions

## 1) Linearity. 
## Result: Not met.
## Only with Number.Elements, again only a few values. Consider it as ordinal instead of continuous.
  rq2.data.t2 %>%
  ggplot(aes(x = Number.Elements, y = Elapsed.Time)) +
  geom_point(aes(size = "Time on Task"), color = "#7463AC", alpha = .6) +
  geom_smooth(aes(color = "Linear fit line"), method = "lm", se = FALSE) +
  geom_smooth(aes(color = "Loess curve"), se = FALSE) +
  theme_minimal() +
  labs(y = "Time on Task", x = "Number of Elements") +
  scale_color_manual(values = c("gray60", "deeppink"), name = "") +
  scale_size_manual(values = 2, name = "")

# 2) Homoscedasticity assumption - data points evenly distributed around the regression line
# Result: Met
const.var.test.t2 <- lmtest::bptest(formula = tot.by.vist.numel.t2)
const.var.test.t2

# studentized Breusch-Pagan test
# 
# data:  tot.by.vist.numel.t2
# BP = 3.1638, df = 3, p-value = 0.3671

# H0: Variance is constant.
# H1: Variance is not constant.
# With p-value=0.5, H0 is kept. So assumption is met. 

## 3) Residuals are independent.
## Result: Not met.
# test independence of residuals
lmtest::dwtest(formula = tot.by.vist.numel.t2)
# Durbin-Watson test
# 
# data:  tot.by.vist.numel.t2
# DW = 1.6623, p-value = 0.01288
# alternative hypothesis: true autocorrelation is greater than 0
#
# H0: Residuals are independent.
# H1: Residuals are not independent.
# With p-value < 0.05, reject H0. Hence assumption is not met.


## 4) Residuals are normally distributed.
# Result: close to normal distribution
# check residual plot of percent uninsured and distance to syringe program
# (Figure 9.30)
data.frame(tot.by.vist.numel.t2$residuals) %>%
  ggplot(aes(x = tot.by.vist.numel.t2.residuals)) +
  geom_histogram(fill = "#7463AC", col = "white") +
  theme_minimal() +
  labs(x = "Residual (difference between observed and predicted values)",
       y = "Number of tasks")

# check residual plot of percent uninsured and distance to syringe program
# (Figure 9.31)
data.frame(tot.by.vist.numel.t2$residuals) %>%
  ggplot(aes(sample = tot.by.vist.numel.t2.residuals)) +
  geom_abline(aes(intercept = mean(x = tot.by.vist.numel.t2.residuals),
                  slope = sd(x = tot.by.vist.numel.t2.residuals),
                  linetype = "Normally distributed"),
              color = "gray60", size = 1) +
  stat_qq(aes(size = "Task"), color = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(x="Theoretical normal distribution",
       y="Observed residuals") +
  scale_size_manual(values = 1, name = "") +
  scale_linetype_manual(values = 1, name = "")


shapiro.test(tot.by.vist.numel.t2$residuals)
# 
# Shapiro-Wilk normality test
# 
# data:  tot.by.vist.numel.t2$residuals
# W = 0.81136, p-value = 9.075e-14
# H0: Distribution is normal.
# H1: Distribution is not normal.
# p-value 9.075e-14 < alpha, then reject N0. Thus it is not normally distributed.



## 5) There is no perfect collinearity.