# First file in the code

fmData <- read.csv(file = "fmstats.csv", header=TRUE)
attach (fmData)
fmData
library(tidyverse)
library(hrbrthemes)
library("tibble")
library(dslabs)

ExpData <- fmData %>% select(FileName, FeatureModel, NoF, NoC)
ExpData

quantile(ExpData$NoF, c(.25, .50, .75))
summary(ExpData$NoF)
var(ExpData$NoF)
sd(ExpData$NoF)
boxplot(ExpData$NoF, horizontal = TRUE)


quantile(ExpData$NoC, c(.25, .50, .75))
summary(ExpData$NoC)
var(ExpData$NoC)
sd(ExpData$NoC)
boxplot(ExpData$NoC, horizontal = TRUE)

ExpData10to14.1 <- ExpData %>% filter(NoF>=10 & NoF<14, NoC>=0 & NoC<1)
ExpData10to14.1

sample.data10to14.1 <- ExpData10to14.1[sample
                                     (1:nrow(ExpData10to14.1), 2),]

sample.data10to14.1

write.csv(sample.data10to14.1,"C:\\Users\\ereza\\Documents\\Courses\\Experiment2021\\Thesis\\R-repository\\Sample-Data-1.csv", row.names = TRUE)

ExpData10to14.2 <- ExpData %>% filter(NoF>=10 & NoF<14, NoC>=1 & NoC<3)
ExpData10to14.2

sample.data10to14.2 <- ExpData10to14.2[sample
                                     (1:nrow(ExpData10to14.2), 2),]

sample.data10to14.2
write.csv(sample.data10to14.2,"C:\\Users\\ereza
          \\Documents\\Courses\\Experiment2021\\Thesis\\R-repository\\Sample-Data-2.csv"
          , row.names = TRUE)

ExpData10to14.3 <- ExpData %>% filter(NoF>=10 & NoF<14, NoC>=3 & NoC<15)
ExpData10to14.3 

sample.data10to14.3 <- ExpData10to14.3[sample
                                     (1:nrow(ExpData10to14.3), 2),]

sample.data10to14.3
write.csv(sample.data10to14.3,"C:\\Users\\ereza\\Documents\\Courses\\Experiment2021\\Thesis\\R-repository\\Sample-Data-3.csv"
          , row.names = TRUE)



ExpData14to20.1 <- ExpData %>% filter(NoF>=14 & NoF<20, NoC>=0 & NoC<1)
ExpData14to20.1
sample.data14to20.1 <- ExpData14to20.1[sample
                            (1:nrow(ExpData14to20.1), 2),]

sample.data14to20.1
write.csv(sample.data14to20.1,"C:\\Users\\ereza\\Documents\\Courses\\Experiment2021\\Thesis\\R-repository\\Sample-Data-4.csv", row.names = TRUE)

ExpData14to20.2 <- ExpData %>% filter(NoF>=14 & NoF<20, NoC>=1 & NoC<3)
ExpData14to20.2

sample.data14to20.2 <- ExpData14to20.2[sample
                                       (1:nrow(ExpData14to20.2), 2),]

sample.data14to20.2
write.csv(sample.data14to20.2,"C:\\Users\\ereza\\Documents\\Courses\\Experiment2021\\Thesis\\R-repository\\Sample-Data-5.csv", row.names = TRUE)

ExpData14to20.3 <- ExpData %>% filter(NoF>=14 & NoF<20, NoC>=3 & NoC<15)
ExpData14to20.3
sample.data14to20.3 <- ExpData14to20.3[sample
                                       (1:nrow(ExpData14to20.3), 2),]

sample.data14to20.3
write.csv(sample.data14to20.3,"C:\\Users\\ereza\\Documents\\Courses\\Experiment2021\\Thesis\\R-repository\\Sample-Data-6.csv", row.names = TRUE)



ExpData20to35.1 <- ExpData %>% filter(NoF>=20 & NoF<35, NoC>=1 & NoC<3)
ExpData20to35.1
sample.data20to35.1 <- ExpData20to35.1[sample
                                       (1:nrow(ExpData20to35.1), 2),]

sample.data20to35.1
write.csv(sample.data20to35.1,"C:\\Users\\ereza\\Documents\\Courses\\Experiment2021\\Thesis\\R-repository\\Sample-Data-7.csv", row.names = TRUE)

ExpData20to35.2 <- ExpData %>% filter(NoF>=20 & NoF<35, NoC>=1 & NoC<3)
ExpData20to35.2
sample.data20to35.2 <- ExpData20to35.2[sample
                                       (1:nrow(ExpData20to35.2), 2),]

sample.data20to35.2
write.csv(sample.data20to35.2,"C:\\Users\\ereza\\Documents\\Courses\\Experiment2021\\Thesis\\R-repository\\Sample-Data-8.csv", row.names = TRUE)

ExpData20to35.3 <- ExpData %>% filter(NoF>=20 & NoF<35, NoC>=3 & NoC<15)
ExpData20to35.3
sample.data20to35.3 <- ExpData20to35.3[sample
                                       (1:nrow(ExpData20to35.3), 2),]
sample.data20to35.3
write.csv(sample.data20to35.3,"C:\\Users\\ereza\\Documents\\Courses\\Experiment2021\\Thesis\\R-repository\\Sample-Data-9.csv", row.names = TRUE)


ExpDataBiggerThan35.1 <- ExpData %>% filter(NoF>=35, NoC>=0 & NoC<1)
ExpDataBiggerThan35.1
sample.dataBiggerThan35.1 <- ExpDataBiggerThan35.1[sample
                                       (1:nrow(ExpDataBiggerThan35.1), 2),]
sample.dataBiggerThan35.1
write.csv(sample.dataBiggerThan35.1,"C:\\Users\\ereza\\Documents\\Courses\\Experiment2021\\Thesis\\R-repository\\Sample-Data-10.csv", row.names = TRUE)

ExpDataBiggerThan35.2<- ExpData %>% filter(NoF>=35, NoC>=1 & NoC<3)
ExpDataBiggerThan35.2
sample.dataBiggerThan35.2 <- ExpDataBiggerThan35.2[sample
                                                   (1:nrow(ExpDataBiggerThan35.2), 2),]
sample.dataBiggerThan35.2
write.csv(sample.dataBiggerThan35.2,"C:\\Users\\ereza\\Documents\\Courses\\Experiment2021\\Thesis\\R-repository\\Sample-Data-11.csv", row.names = TRUE)
          

ExpDataBiggerThan35.3 <- ExpData %>% filter(NoF>=35, NoC>=3 & NoC<15)
ExpDataBiggerThan35.3
sample.dataBiggerThan35.3 <- ExpDataBiggerThan35.3[sample
                                                   (1:nrow(ExpDataBiggerThan35.3), 2),]

sample.dataBiggerThan35.3
write.csv(sample.dataBiggerThan35.3,"C:\\Users\\ereza\\Documents\\Courses\\Experiment2021\\Thesis\\R-repository\\Sample-Data-12.csv", row.names = TRUE)






