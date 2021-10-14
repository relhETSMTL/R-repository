# This code is written to select FM comprehension's experiment samples.
#We selected our samples randomly by considering  regions in NOF and NoC. 
#Overall we have 24 FMs: 4*3.(4 shows number of regions in NoF & 3 regions in NoC.)
# Loads the CVS file with the statistical

fmData <- read.csv(file = "fmstats.csv", header=TRUE)
attach (fmData)
fmData

# ggplot2 for presenting data in plots
library(ggplot2)
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


#Defining an array with two dimensions for storing intervals of NoF and NoC
NoF<-c(10, 14, 20, 35)
NoC<-c(0,1,3,15)
column.names <- c("NoF_Intervals","NoC_Intervals")
row.names <- c("NoF-NoC-1","NoF-NoC-2","NoF-NoC-3", "NoF-NoC-4")
array<- array(c(NoF,NoC), dim=c(4,2), dimnames = list(row.names,column.names))
array



     #..............................................................#


#extracting FMs that meet following restriction from initial dataFrame (ExpData). 
#first intervals for Nof & NoC
FMs_min_Q1_1 <- ExpData %>% filter(NoF>=array[1, 1] & NoF<array[2, 1], NoC>=array[1, 2] & NoC<array[2, 2])
FMs_min_Q1_1
#selecting 2 samples randomly
set.seed(2)
sample.min_Q1_1 <- FMs_min_Q1_1[sample
                                     (1:nrow(FMs_min_Q1_1), 2),]
sample.min_Q1_1

#creating csv file for 2 extracted samples from first region in boxplot
#write.csv(sample.min_Q1.1,"your directory path\\file-Name.csv", row.names = TRUE)

      #..............................................................#

 #first and second intervals for Nof & NoC respectively
FMs_min_Q1_2 <- ExpData %>% filter(NoF>=array[1, 1] & NoF<array[2, 1], NoC>=array[2, 2] & NoC<array[3, 2])
FMs_min_Q1_2
set.seed(2)
sample.min_Q1_2 <- FMs_min_Q1_2[sample
                                     (1:nrow(FMs_min_Q1_2), 2),]
sample.min_Q1_2
#write.csv(sample.min_Q1.2,"your directory path\\file-Name.csv", row.names = TRUE)

         #..............................................................# 

# we did not consider NoC > 15 because of limitation we had using eye-tracker technology
#first and third intervals for Nof & NoC respectively
FMs_min_Q1_3 <- ExpData %>% filter(NoF>=array[1, 1] & NoF<array[2, 1], NoC>=array[3, 2] & NoC<array[4, 2])
FMs_min_Q1_3 
set.seed(2)
sample.min_Q1_3 <- FMs_min_Q1_3[sample
                                     (1:nrow(FMs_min_Q1_3), 2),]
sample.min_Q1_3
#write.csv(sample.min_Q1.3,"your directory path\\file-Name.csv", row.names = TRUE)

              #..............................................................#

#second and first intervals for Nof & NoC respectively
FMs_Q1_Med_1 <- ExpData %>% filter(NoF>=array[2, 1] & NoF<array[3, 1], NoC>=array[1, 2] & NoC<array[2, 2])
FMs_Q1_Med_1
set.seed(2)
sample.Q1_Med_1 <- FMs_Q1_Med_1[sample(1:nrow(FMs_Q1_Med_1), 2),]
sample.Q1_Med_1
#write.csv(sample.Q1_Med.1,"your directory path\\file-Name.csv", row.names = TRUE)

            #..............................................................#

#second intervals for Nof & NoC 
FMs_Q1_Med_2 <- ExpData %>% filter(NoF>=array[2, 1] & NoF<array[3, 1], NoC>=array[2, 2] & NoC<array[3, 2])
FMs_Q1_Med_2
set.seed(2)
sample.Q1_Med_2 <- FMs_Q1_Med_2[sample(1:nrow(FMs_Q1_Med_2), 2),]
sample.Q1_Med_2
#write.csv(sample.Q1_Med.2,"your directory path\\file-Name.csv", row.names = TRUE)

          #..............................................................#

#second and third intervals for Nof & NoC respectively
FMs_Q1_Med_3 <- ExpData %>% filter(NoF>=array[2, 1] & NoF<array[3, 1], NoC>=array[3, 2] & NoC<array[4, 2])
FMs_Q1_Med_3
set.seed(3)
sample.Q1_Med_3 <- FMs_Q1_Med_3[sample(1:nrow(FMs_Q1_Med_3), 2),]
sample.Q1_Med_3
#write.csv(sample.Q1_Med.3,"your directory path\\file-Name.csv", row.names = TRUE)

         #..............................................................#

#third and first intervals for Nof & NoC respectively
FMs_Med_Q3_1 <- ExpData %>% filter(NoF>=array[3, 1] & NoF<array[4, 1], NoC>=array[1, 2] & NoC<array[2, 2])
FMs_Med_Q3_1
set.seed(2)
sample.Med_Q3_1 <- FMs_Med_Q3_1[sample(1:nrow(FMs_Med_Q3_1), 2),]
sample.Med_Q3_1
#write.csv(sample.Med_Q3.1,"your directory path\\file-Name.csv", row.names = TRUE)

              #..............................................................#

#third and second intervals for Nof & NoC respectively
FMs_Med_Q3_2 <- ExpData %>% filter(NoF>=array[3, 1] & NoF<array[4, 1], NoC>=array[2, 2] & NoC<array[3, 2])
FMs_Med_Q3_2
set.seed(2)
sample.Med_Q3_2 <- FMs_Med_Q3_2[sample(1:nrow(FMs_Med_Q3_2), 2),]
sample.Med_Q3_2
#write.csv(sample.Med_Q3.2,"your directory path\\file-Name.csv", row.names = TRUE)

          #..............................................................#

#third intervals for Nof & NoC 
FMs_Med_Q3_3 <- ExpData %>% filter(NoF>=array[3, 1] & NoF<array[4, 1], NoC>=array[3, 2] & NoC<array[4, 2])
FMs_Med_Q3_3
set.seed(2)
sample.Med_Q3_3 <- FMs_Med_Q3_3[sample(1:nrow(FMs_Med_Q3_3), 2),]
sample.Med_Q3_3
#write.csv(sample.Med_Q3.3,"your directory path\\file-Name.csv", row.names = TRUE)

         #..............................................................#

#last and first intervals for Nof & NoC respectivly
FMs_Q3_Bigger_1 <- ExpData %>% filter(NoF>=array[4, 1], NoC>=array[1, 2] & NoC<array[2, 2])
FMs_Q3_Bigger_1
set.seed(2)
sample.Q3_bigger_1 <- FMs_Q3_Bigger_1[sample(1:nrow(FMs_Q3_Bigger_1), 2),]
sample.Q3_bigger_1
#write.csv(sample.Q3_bigger.1,"your directory path\\file-Name.csv", row.names = TRUE)

           #..............................................................#

#last and second intervals for Nof & NoC respectively
FMs_Q3_Bigger_2<- ExpData %>% filter(NoF>=array[4, 1], NoC>=array[2, 2] & NoC<array[3, 2])
FMs_Q3_Bigger_2
set.seed(2)
sample.Q3_bigger_2 <- FMs_Q3_Bigger_2[sample(1:nrow(FMs_Q3_Bigger_2), 2),]
sample.Q3_bigger_2
#write.csv(sample.Q3_bigger.2,"your directory path\\file-Name.csv", row.names = TRUE)
   
          #..............................................................#        

#last and third intervals for Nof & NoC respectively
FMs_Q3_Bigger_3 <- ExpData %>% filter(NoF>=array[4, 1], NoC>=array[3, 2] & NoC<array[4, 2])
FMs_Q3_Bigger_3
set.seed(3)
sample.Q3_bigger_3 <- FMs_Q3_Bigger.3[sample(1:nrow(FMs_Q3_Bigger_3), 2),]
sample.Q3_bigger_3
#write.csv(sample.Q3_bigger.3,"your directory path\\file-Name.csv", row.names = TRUE)


#end...........................................................................#

#visualization of how many FMs we have for each region of Nof & NoC 

Collection_view<-combine(count(NoF_Q1_NoC_1), count(FMs_min_Q1_2),
                          count(FMs_min_Q1_3), count(FMs_Q1_Med_1), 
                          count(FMs_Q1_Med_2), count(FMs_Q1_Med_3),
                          count(FMs_Med_Q3_1), count(FMs_Med_Q3_2),count(FMs_Med_Q3_3),
                          count(FMs_Q3_Bigger_1),count(FMs_Q3_Bigger_2),
                          count(FMs_Q3_Bigger_3))
Collection_view$Regions <- c("NoF_Q1_NoC_1","FMs_min_Q1_2","FMs_min_Q1_3","FMs_Q1_Med_1",
                            "FMs_Q1_Med_2", "FMs_Q1_Med_3", "FMs_Med_Q3_1", "FMs_Med_Q3_2", "FMs_Med_Q3_3", "FMs_Q3_Bigger_1" ,"FMs_Q3_Bigger_2", "FMs_Q3_Bigger_3")
Collection_view


#where is the last error
rlang::last_error()
rlang::last_trace()

#.............................................................................#

#presentation of data spread as bar-plot
p<-ggplot(data=Collection_view, aes(x=Regions, y=n)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=n), vjust=1.6, color="white", size=3.5)+
 theme_minimal()
show(p)
p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# Save the file
dev.off()

