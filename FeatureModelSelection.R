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


# For this study, we focus on two metrics: 
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

# Builds the columns for the experiment table
ci1 <- c(min(ExpData$NoF), min(ExpData$NoC))  # minimum value
ci2 <- c(quantile(ExpData$NoF,0.25), quantile(ExpData$NoC, 0.5))  # first quantile, median
ci3 <- c(quantile(ExpData$NoF,0.5), quantile(ExpData$NoC, 0.75))   # second quantile, third quantile
max.NoC <- 15     # we set 15 as the maximum NoC for our experiments
ci4 <- c(quantile(ExpData$NoF,0.75), max.NoC)

exp.column.names <- c("I1","I2","I3","I4")
exp.row.names <- c("NoF","NoC")
exp.combinations <- array(c(ci1,ci2,ci3,ci4), dim=c(2,4), dimnames = list(exp.row.names,exp.column.names))
exp.combinations

## Filtering the datasets for the 12 combinations of NoF and NoC
combination.i1.i1 <- ExpData %>% 
                     filter(exp.combinations["NoF","I1"] <= NoF & NoF < exp.combinations["NoF","I2"] &
                            exp.combinations["NoC","I1"] <= NoC & NoC < exp.combinations["NoC","I2"])

combination.i1.i2 <- ExpData %>% 
                     filter(exp.combinations["NoF","I1"] <= NoF & NoF < exp.combinations["NoF","I2"] &
                            exp.combinations["NoC","I2"] <= NoC & NoC < exp.combinations["NoC","I3"])

combination.i1.i3 <- ExpData %>% 
                     filter(exp.combinations["NoF","I1"] <= NoF & NoF < exp.combinations["NoF","I2"] &
                            exp.combinations["NoC","I3"] <= NoC & NoC < exp.combinations["NoC","I4"])


combination.i2.i1 <- ExpData %>% 
                    filter(exp.combinations["NoF","I2"] <= NoF & NoF < exp.combinations["NoF","I3"] &
                           exp.combinations["NoC","I1"] <= NoC & NoC < exp.combinations["NoC","I2"])

combination.i2.i2 <- ExpData %>% 
                    filter(exp.combinations["NoF","I2"] <= NoF & NoF < exp.combinations["NoF","I3"] &
                           exp.combinations["NoC","I2"] <= NoC & NoC < exp.combinations["NoC","I3"])

combination.i2.i3 <- ExpData %>% 
                    filter(exp.combinations["NoF","I2"] <= NoF & NoF < exp.combinations["NoF","I3"] &
                           exp.combinations["NoC","I3"] <= NoC & NoC < exp.combinations["NoC","I4"])


combination.i3.i1 <- ExpData %>% 
                    filter(exp.combinations["NoF","I3"] <= NoF & NoF < exp.combinations["NoF","I4"] &
                           exp.combinations["NoC","I1"] <= NoC & NoC < exp.combinations["NoC","I2"])

combination.i3.i2 <- ExpData %>% 
                    filter(exp.combinations["NoF","I3"] <= NoF & NoF < exp.combinations["NoF","I4"] &
                           exp.combinations["NoC","I2"] <= NoC & NoC < exp.combinations["NoC","I3"])

combination.i3.i3 <- ExpData %>% 
                    filter(exp.combinations["NoF","I3"] <= NoF & NoF < exp.combinations["NoF","I4"] &
                           exp.combinations["NoC","I3"] <= NoC & NoC < exp.combinations["NoC","I4"])


combination.i4.i1 <- ExpData %>% 
                    filter(exp.combinations["NoF","I4"] <= NoF &
                           exp.combinations["NoC","I1"] <= NoC & NoC < exp.combinations["NoC","I2"])

combination.i4.i2 <- ExpData %>% 
                    filter(exp.combinations["NoF","I4"] <= NoF &
                           exp.combinations["NoC","I2"] <= NoC & NoC < exp.combinations["NoC","I3"])

combination.i4.i3 <- ExpData %>% 
                    filter(exp.combinations["NoF","I4"] <= NoF & 
                           exp.combinations["NoC","I3"] <= NoC & NoC < exp.combinations["NoC","I4"])


# Bubble plot of the 12 combinations

# creates the data frame with the number of features models in each combination
values.nof <- c("10<=NoF<14", "10<=NoF<14", "10<=NoF<14", 
                "14<=NoF<20", "14<=NoF<20", "14<=NoF<20", 
                "20<=NoF<35", "20<=NoF<35", "20<=NoF<35",
                "NoF<=35", "NoF<=35", "NoF<=35")
values.noc <- c("0<=NoC<1", "1<=NoC<3", "3<=NoC<15",
                "0<=NoC<1", "1<=NoC<3", "3<=NoC<15",
                "0<=NoC<1", "1<=NoC<3", "3<=NoC<15",
                "0<=NoC<1", "1<=NoC<3", "3<=NoC<15")
values.num.fm <- c(nrow(combination.i1.i1), nrow(combination.i1.i2), nrow(combination.i1.i3),
                   nrow(combination.i2.i1), nrow(combination.i2.i2), nrow(combination.i2.i3),
                   nrow(combination.i3.i1), nrow(combination.i3.i2), nrow(combination.i3.i3),
                   nrow(combination.i4.i1), nrow(combination.i4.i2), nrow(combination.i4.i3))

values.df <- data.frame(values.nof, values.noc, values.num.fm)

# Note: Feature models considered in experiment 847 out of 882. The remaining 35, 15<=NoC
# Sanity check OK
excluded.fm <- ExpData %>% filter(15<= NoC)
nrow(excluded.fm)

# Drawing the plot
bubble.fm.combination <-
  ggplot(values.df, aes(x = values.nof, y = values.noc, size = values.num.fm))+
  theme_minimal() +
  geom_point(alpha = 0.7, color="blue", show_guide = FALSE) +
  geom_text(label=values.num.fm, nudge_x = 0.25, nudge_y = 0.25, size = 5 ) +
  labs(title= "NoF and NoC Intervals Combinations", y="Intervals NoC", x = "Intervals NoF")
bubble.fm.combination


# Selecting the sample feature models
# Seeding the random number generator once. Selecting n=2 feature models of each combinations
set.seed(2)
num.fm <- 2

# Simple random sampling function
simpleRS = function(population, n, comb.name){
  srsIndexes = sample(1:nrow(population), n)
  df_population <- population[srsIndexes,]
  df_population %>% add_column(combination = comb.name)
}

# Obtains the samples of each combination of NoF and NoC
sample.combination.i1.i1 <- simpleRS(combination.i1.i1, num.fm, "i1.i1")
sample.combination.i1.i2 <- simpleRS(combination.i1.i2, num.fm, "i1.i2")
sample.combination.i1.i3 <- simpleRS(combination.i1.i3, num.fm, "i1.i3")

sample.combination.i2.i1 <- simpleRS(combination.i2.i1, num.fm, "i2.i1")
sample.combination.i2.i2 <- simpleRS(combination.i2.i2, num.fm, "i2.i2")
sample.combination.i2.i3 <- simpleRS(combination.i2.i3, num.fm, "i2.i3")

sample.combination.i3.i1 <- simpleRS(combination.i3.i1, num.fm, "i3.i1")
sample.combination.i3.i2 <- simpleRS(combination.i3.i2, num.fm, "i3.i2")
sample.combination.i3.i3 <- simpleRS(combination.i3.i3, num.fm, "i3.i3")

sample.combination.i4.i1 <- simpleRS(combination.i4.i1, num.fm, "i4.i1")
sample.combination.i4.i2 <- simpleRS(combination.i4.i2, num.fm, "i4.i2")
sample.combination.i4.i3 <- simpleRS(combination.i4.i3, num.fm, "i4.i3")

# Obtains the sample of all the 12 combinations of randomly selected feature models
experiment.sample <- 
  rbind(sample.combination.i1.i1, sample.combination.i1.i2,sample.combination.i1.i3,
        sample.combination.i2.i1, sample.combination.i2.i2,sample.combination.i3.i3,
        sample.combination.i3.i1, sample.combination.i3.i2,sample.combination.i3.i3,
        sample.combination.i4.i1, sample.combination.i4.i2,sample.combination.i4.i3)

# Writes the sample in a experiment-sample.csv file. Adjust path accordingly.
write.csv(experiment.sample,"experiment-sample.csv", row.names = TRUE)



