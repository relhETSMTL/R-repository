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

#Using boxplot for two metrics and see data distribution

#boxplot of NoF
quantile(ExpData$NoF, c(.25, .50, .75))
summary(ExpData$NoF)
var(ExpData$NoF)
sd(ExpData$NoF)

boxplot(ExpData$NoF, horizontal = TRUE)

boxstyle <- fmData %>% 
  ggplot(aes(y=NoF)) +
  geom_boxplot() +
# geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_minimal() +
  labs(y="Number of Features (NoF)") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  coord_flip()+
  scale_y_continuous(limits=c(10,370), breaks=seq(10,370,40))
boxstyle

boxstyle <- fmData %>% 
  ggplot(aes(y=NoF)) +
  geom_boxplot() +
  theme_minimal() +
  labs(y="Number of Features (NoF)") +
  scale_y_continuous(limits=c(10,360))


boxstyle

