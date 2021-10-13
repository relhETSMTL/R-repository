# Feature Model Selection
# Objective: Randomnly selects a sample of feature models taken from 882 models from SPLOT repository
# Project: Eye-Tracking Analysis of Feature Models Comprehension
# Ecole de technologie superieure
# VERITAS team
# Authors: Elmira Sepasi, Kambiz Belouchi, Roberto E. Lopez-Herrejon

library(ggplot2)
library(tidyverse)
library(hrbrthemes)

# File fmstats.csv file contains 22 feature model metrics applied to the 882 feature models 
fmData <- read.csv(file = "fmstats.csv", header=TRUE)
attach (fmData)


# For this study, we focus on two metrics:L
# NoF: Number of Features, NoC: number of cross-tree constraints
# We select the two metrics and the file name and the feature model name
ExpData <- fmData %>% select(FileName, FeatureModel, NoF, NoC)


# Boxplot for distribution of number of features
boxplotNoF <- fmData %>% 
  ggplot(aes(y=NoF)) +
  geom_boxplot() +
  theme_minimal() +
  labs(y="Number of Features (NoF)") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  coord_flip()+
  scale_y_continuous(limits=c(10,370), breaks=seq(10,370,40))
boxplotNoF

# Note: The data set contains 5 feature models with NoC greater than 50 (range 59 to 246).
# To simplify the plotting, the 5 outliers are excluded.
histogramNoC <- fmData %>% filter(NoC < 50) %>%
  ggplot( aes(x=NoC)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) + 
  theme_minimal() +
  theme(plot.title = element_text(size=15)) +
  labs(x="Number of Constraints (NoC)")  
histogramNoC


# EDITED UP TO HERE

# TO DO:
# 1) Obtain the quantile information from the summary

summary(ExpData$NoF)



#Using boxplot for two metrics and see data distribution

histogramNoC <- fmData %>%
  ggplot( aes(x=NoC)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  scale_y_continuous(trans='log10') +
  theme_ipsum() +
  theme(plot.title = element_text(size=15))
histogramNoC

#boxplot of NoF
quantile(ExpData$NoF, c(.25, .50, .75))
summary(ExpData$NoF)
var(ExpData$NoF)
sd(ExpData$NoF)

boxstyle <- fmData %>% 
  ggplot(aes(y=NoF)) +
  geom_boxplot() +
  theme_minimal() +
  labs(y="Number of Features (NoF)") +
  scale_y_continuous(limits=c(10,360))


boxstyle


####################################################
####################################################
## Other code examples not used for publications

# Boxplot for distribution of number of constraints 
boxplotNoC <- fmData %>% 
  ggplot(aes(y=NoC)) +
  geom_boxplot() +
  theme_minimal() +
  labs(y="Number of Constraints (NoC)") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_y_continuous(trans='ln2') +
  coord_flip() 
boxplotNoC


# Histogram  with log scale on the account
histogramNoC <- fmData %>%
  ggplot( aes(x=NoC)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  scale_y_continuous(trans='log10') +
  theme_ipsum() +
  theme(plot.title = element_text(size=15))
histogramNoC
