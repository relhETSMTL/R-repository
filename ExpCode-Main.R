# This code is written to select FM comprehension's experiment samples.
#We selected our samples randomly by considering  regions in NOF and NoC. 
#Overall we have 24 FMs: 4*3.(4 shows number of regions in NoF & 3 regions in NoC.)
# Loads the CVS file with the statistics
#install.packages("ggalt")
#install.packages('GGally')
#install.packages('ggridges')
fmData <- read.csv(file = "fmstats.csv", header=TRUE)
attach (fmData)
fmData

library(ggalt)
library(GGally)
library(ggridges)
library(tidyverse)
library(hrbrthemes)
library("tibble")
library(dslabs)

#extract columns based on selected metrics
ExpData <- fmData %>% select(FileName, FeatureModel, NoF, NoC)
ExpData

#Using boxplot for two metrics and see data distribution

#boxplot of NoF
quantile(ExpData$NoF, c(.25, .50, .75))
summary(ExpData$NoF)
var(ExpData$NoF)
sd(ExpData$NoF)
boxplot(ExpData$NoF, horizontal = TRUE)

#boxplot of NoC
quantile(ExpData$NoC, c(.25, .50, .75))
summary(ExpData$NoC)
var(ExpData$NoC)
sd(ExpData$NoC)
boxplot(ExpData$NoC, horizontal = TRUE)

     #..............................................................#

#assigning variables for first region of boxplot for metrics NoF & NOC
NoF_min <- 10
NoF_Q1 <- 14
NoC_min <- 0
NoC_Med <- 1 # Med ~ Median

#extracting FMs that meet following restriction from initial dataFrame (ExpData). 
FMs_min_Q1.1 <- ExpData %>% filter(NoF>=NoF_min & NoF<NoF_Q1, NoC>=NoC_min & NoC<NoC_Med)
FMs_min_Q1.1
#selecting 2 samples randomly
set.seed(2)
sample.min_Q1.1 <- FMs_min_Q1.1[sample
                                     (1:nrow(FMs_min_Q1.1), 2),]
sample.min_Q1.1

#creating csv file for 2 extracted samples from first region in boxplot
#write.csv(sample.min_Q1.1,"your directory path\\file-Name.csv", row.names = TRUE)

      #..............................................................#

NoF_min <-10
NoF_Q1 <- 14
NoC_Med <- 1 # Med ~ Median
NoC_Q3 <- 3 
FMs_min_Q1.2 <- ExpData %>% filter(NoF>=NoF_min & NoF<NoF_Q1, NoC>=NoC_Med & NoC<NoC_Q3)
FMs_min_Q1.2
set.seed(2)
sample.min_Q1.2 <- FMs_min_Q1.2[sample
                                     (1:nrow(FMs_min_Q1.2), 2),]
sample.min_Q1.2
#write.csv(sample.min_Q1.2,"your directory path\\file-Name.csv", row.names = TRUE)

         #..............................................................# 

NoF_min <-10
NoF_Q1 <- 14
NoC_Q3 <- 3 # med ~ median
NoC_Max<- 15 # we did not consider NoC > 15 because of limitation we had using eye-tracker technology
FMs_min_Q1.3 <- ExpData %>% filter(NoF>=NoF_min & NoF<NoF_Q1, NoC>=NoC_Q3 & NoC<NoC_Max)
FMs_min_Q1.3 
set.seed(2)
sample.min_Q1.3 <- FMs_min_Q1.3[sample
                                     (1:nrow(FMs_min_Q1.3), 2),]
sample.min_Q1.3
write.csv(sample.min_Q1.3,"your directory path\\file-Name.csv", row.names = TRUE)

              #..............................................................#

NoF_Q1 <-14
NoF_Med<- 20 #Med ~ Median
NoC_min <- 0 
NoC_Med <- 1 
FMs_Q1_Med.1 <- ExpData %>% filter(NoF>=NoF_Q1 & NoF<NoF_Med, NoC>=NoC_min & NoC<NoC_Med)
FMs_Q1_Med.1
set.seed(2)
sample.Q1_Med.1 <- FMs_Q1_Med.1[sample(1:nrow(FMs_Q1_Med.1), 2),]
sample.Q1_Med.1
#write.csv(sample.Q1_Med.1,"your directory path\\file-Name.csv", row.names = TRUE)

            #..............................................................#

NoF_Q1 <-14
NoF_Med<- 20 #Med ~ Median
NoC_Med <- 1 
NoC_Q3 <- 3
FMs_Q1_Med.2 <- ExpData %>% filter(NoF>=NoF_Q1 & NoF<NoF_Med, NoC>=NoC_Med & NoC<NoC_Q3)
FMs_Q1_Med.2
set.seed(2)
sample.Q1_Med.2 <- FMs_Q1_Med.2[sample(1:nrow(FMs_Q1_Med.2), 2),]
sample.Q1_Med.2
#write.csv(sample.Q1_Med.2,"your directory path\\file-Name.csv", row.names = TRUE)

          #..............................................................#
NoF_Q1 <-14
NoF_Med<- 20 #Med ~ Median
NoC_Q3 <- 3 
NoC_Max <- 15
FMs_Q1_Med.3 <- ExpData %>% filter(NoF>=NoF_Q1 & NoF<NoF_Med, NoC>=NoC_Q3 & NoC<NoC_Max)
FMs_Q1_Med.3
set.seed(3)
sample.Q1_Med.3 <- FMs_Q1_Med.3[sample(1:nrow(FMs_Q1_Med.3), 2),]
sample.Q1_Med.3
#write.csv(sample.Q1_Med.3,"your directory path\\file-Name.csv", row.names = TRUE)

         #..............................................................#

NoF_Med <-20 #Med ~ Median
NoF_Q3<- 35 
NoC_min <- 0 
NoC_Med <- 1
FMs_Med_Q3.1 <- ExpData %>% filter(NoF>=NoF_Med & NoF<NoF_Q3, NoC>=NoC_min & NoC<NoC_Med)
FMs_Med_Q3.1
set.seed(2)
sample.Med_Q3.1 <- FMs_Med_Q3.1[sample(1:nrow(FMs_Med_Q3.1), 2),]
sample.Med_Q3.1
#write.csv(sample.Med_Q3.1,"your directory path\\file-Name.csv", row.names = TRUE)

              #..............................................................#

NoF_Med <-20 #Med ~ Median
NoF_Q3<- 35 
NoC_Med <- 1
NoC_Q3 <- 3
FMs_Med_Q3.2 <- ExpData %>% filter(NoF>=NoF_Med & NoF<NoF_Q3, NoC>=NoC_Med & NoC<NoC_Q3)
FMs_Med_Q3.2
set.seed(2)
sample.Med_Q3.2 <- FMs_Med_Q3.2[sample(1:nrow(FMs_Med_Q3.2), 2),]
sample.Med_Q3.2
#write.csv(sample.Med_Q3.2,"your directory path\\file-Name.csv", row.names = TRUE)

          #..............................................................#

NoF_Med <-20 #Med ~ Median
NoF_Q3<- 35 
NoC_Q3 <- 3
NoC_Max <- 15
FMs_Med_Q3.3 <- ExpData %>% filter(NoF>=NoF_Med & NoF<NoF_Q3, NoC>=NoC_Q3 & NoC<NoC_Max)
FMs_Med_Q3.3
set.seed(2)
sample.Med_Q3.3 <- FMs_Med_Q3.3[sample(1:nrow(FMs_Med_Q3.3), 2),]
sample.Med_Q3.3
#write.csv(sample.Med_Q3.3,"your directory path\\file-Name.csv", row.names = TRUE)

         #..............................................................#

NoF_Q3<- 35 
NoC_min <- 0
NoC_Med <- 1
FMs_Q3_Bigger.1 <- ExpData %>% filter(NoF>=NoF_Q3, NoC>=NoC_min & NoC<NoC_Med)
FMs_Q3_Bigger.1
set.seed(2)
sample.Q3_bigger.1 <- FMs_Q3_Bigger.1[sample(1:nrow(FMs_Q3_Bigger.1), 2),]
sample.Q3_bigger.1
#write.csv(sample.Q3_bigger.1,"your directory path\\file-Name.csv", row.names = TRUE)

           #..............................................................#

NoF_Q3<- 35 
NoC_Med <- 1
NoC_Q3 <- 3
FMs_Q3_Bigger.2<- ExpData %>% filter(NoF>=NoF_Q3, NoC>=NoC_Med & NoC<NoC_Q3)
FMs_Q3_Bigger.2
set.seed(2)
sample.Q3_bigger.2 <- FMs_Q3_Bigger.2[sample(1:nrow(FMs_Q3_Bigger.2), 2),]
sample.Q3_bigger.2
#write.csv(sample.Q3_bigger.2,"your directory path\\file-Name.csv", row.names = TRUE)
   
          #..............................................................#        

NoF_Q3<- 35 
NoC_Q3 <- 3
NoC_Max <- 15
FMs_Q3_Bigger.3 <- ExpData %>% filter(NoF>=NoF_Q3, NoC>=NoC_Q3 & NoC<NoC_Max)
FMs_Q3_Bigger.3
set.seed(3)
sample.Q3_bigger.3 <- FMs_Q3_Bigger.3[sample(1:nrow(FMs_Q3_Bigger.3), 2),]
sample.Q3_bigger.3
#write.csv(sample.Q3_bigger.3,"your directory path\\file-Name.csv", row.names = TRUE)


#end...........................................................................#

#visualization of how data spread 
Collection_view<- combine(sample.min_Q1.1, sample.min_Q1.2,
                       sample.min_Q1.3, sample.Q1_Med.1, 
                       sample.Q1_Med.2, sample.Q1_Med.3,
                       sample.Med_Q3.1, sample.Med_Q3.2, sample.Med_Q3.3,
                       sample.Q3_bigger.1,sample.Q3_bigger.2,
                       sample.Q3_bigger.3)
Collection_view


#where is the last error
rlang::last_error()
rlang::last_trace()


ggplot() + 
  geom_point(data = Collection_view, mapping = aes(x = NoF, y = NoC)) + 
  geom_smooth(data = Collection_view, mapping = aes(x = NoF, y = NoC))

ggplot(data = Collection_view, mapping = aes(x = NoF, y = NoC)) + 
  geom_point() + 
  geom_smooth()


 ggplot(Collection_view ) +
  geom_line(aes(x = NoF, y = NoC), 
            lwd = 1.25, color = 'darkgreen') +

  ggtitle( "Basic Line Plot",subtitle = "Feature Models") +
  xlab("NoF") +
  ylab("NoC") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))
