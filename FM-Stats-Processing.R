# Processing Feature Model Statistics
# Author Roberto E. Lopez-Herrejon roberto.lopez@etsmtl.ca
# ETS Univeristy of Quebec

# Loads the CVS file with the statistics of the 
fmData <- read.csv(file = 'fmstats.csv', header=TRUE)
attach (fmData)

# Creating the histogram for the number of features

library(tidyverse)
library(hrbrthemes)

# Load dataset from github
#data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/1_OneNum.csv", header=TRUE)

# histogram plot
# https://www.r-graph-gallery.com/220-basic-ggplot2-histogram.html#binSize
p <- fmData %>%
  ggplot( aes(x=NoF)) +
  geom_histogram( binwidth=5, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 5") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )

p

# Finding: The majority are in the range of 10 to 100 features. Only 9 have more than 100.

### Boxplot depiction of number of features
IQR(fmData$NoF)


# quantiles
quantile(fmData$NoF, c(.25, .50, .75))
summary(fmData$NoF)
var(fmData$NoF)
sd(fmData$NoF)

boxplot(fmData$NoF)


# creating a box plot
boxplot(fmData)

# stylized box plot
boxstyle <- fmData %>% 
  ggplot(aes(y=NoF)) +
  geom_boxplot() +
  theme_minimal() +
  labs(y="Number of FMs")

boxstyle

###### Test with jitter
library(viridis)

# create a dataset
data <- data.frame(
  name=c( rep("A",500), rep("B",500), rep("B",500), rep("C",20), rep('D', 100)  ),
  value=c( rnorm(500, 10, 5), rnorm(500, 13, 1), rnorm(500, 18, 1), rnorm(20, 25, 4), rnorm(100, 12, 1) )
)

# Plot
data %>%
  ggplot( aes(x=name, y=value, fill=name)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot with jitter") +
  xlab("")


## Reformatting the fmData to obtain a list of Values
fmDataBox <- data.frame(
  name=c(rep("NoF",length(fmData$NoF))),
  value=c(fmData$NoF)
)

fmDataBox %>%
  ggplot( aes(x=name, y=value, fill=name)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("boxplot with jitter for NoF") +
  xlab("")


#########
# Scatterplots of NoF against all other metrics

#1 
plotNoFVP <- fmData %>%
  ggplot(aes(x=NoF, y=VP)) +
  geom_point() +
  theme_minimal() +
  labs (x="NoF = Number of Features",y="VP = Variability Points")

plotNoFVP

#2 - almost linear correlation, not useful for the selection of FM
plotNoFNLeaf <- fmData %>%
  ggplot(aes(x=NoF, y=NLeaf)) +
  geom_point() +
  theme_minimal() +
  labs (x="NoF = Number of Features",y="NLeaf = Number of Leafs")

plotNoFNLeaf

#3 -
plotNoFNM <- fmData %>%
  ggplot(aes(x=NoF, y=NM)) +
  geom_point() +
  theme_minimal() +
  labs (x="NoF = Number of Features",y="NM = Number of Mandatory features")

plotNoFNM

#4 - more spread out
plotNoFNO <- fmData %>%
  ggplot(aes(x=NoF, y=NO)) +
  geom_point() +
  theme_minimal() +
  labs (x="NoF = Number of Features",y="NO = Number of Optional features")

plotNoFNO

#5 - TO DO verify column name
plotNoFNA <- fmData %>%
  ggplot(aes(x=NoF, y=NA.)) +
  geom_point() +
  theme_minimal() +
  labs (x="NoF = Number of Features",y="NA = Number of Alternative features")

plotNoFNA

#6 
plotNoFNVF <- fmData %>%
  ggplot(aes(x=NoF, y=NVF)) +
  geom_point() +
  theme_minimal() +
  labs (x="NoF = Number of Features",y="NVF = Number of Variable Features")

plotNoFNVF

#7
plotNoFNGF <- fmData %>%
  ggplot(aes(x=NoF, y=NGF)) +
  geom_point() +
  theme_minimal() +
  labs (x="NoF = Number of Features",y="NGF = Number of Grouping Features")

plotNoFNGF

#8
plotNoFSHoF <- fmData %>%
  ggplot(aes(x=NoF, y=SHoF)) +
  geom_point() +
  theme_minimal() +
  labs (x="NoF = Number of Features",y="SHoF = Single Hotspot Features")

plotNoFSHoF

#9
plotNoFMHoF <- fmData %>%
  ggplot(aes(x=NoF, y=MHoF)) +
  geom_point() +
  theme_minimal() +
  labs (x="NoF = Number of Features",y="MHoF = Multiple Hotspot Features")

plotNoFMHoF

# Grid with metrics NoF 1-10
gridExtra::grid.arrange(plotNoFVP, plotNoFNLeaf, plotNoFNM,
                        plotNoFNO, plotNoFNA, plotNoFNVF,
                        plotNoFNGF, plotNoFSHoF, plotNoFMHoF, 
                        nrow=3, ncol=3)

#10
plotNoFSCDF <- fmData %>%
  ggplot(aes(x=NoF, y=SCDF)) +
  geom_point() +
  theme_minimal() +
  labs (x="NoF = Number of Features",y="SCDF = Single Cyclic Dependent Feature")

plotNoFSCDF

#11 
plotNoFMCDF <- fmData %>%
  ggplot(aes(x=NoF, y=MCDF)) +
  geom_point() +
  theme_minimal() +
  labs (x="NoF = Number of Features",y="SCDF = Multiple Cyclic Dependent Feature")

plotNoFMCDF

#12
plotNoFNoFC <- fmData %>%
  ggplot(aes(x=NoF, y=NoFC)) +
  geom_point() +
  theme_minimal() +
  labs (x="NoF = Number of Features",y="NoFC = Number of Features in Constraints")

plotNoFNoFC


#13
plotNoFNoC <- fmData %>%
  ggplot(aes(x=NoF, y=NoC)) +
  geom_point() +
  theme_minimal() +
  labs (x="NoF = Number of Features",y="NoC = Number of Constraints")

plotNoFNoC

#14
plotNoFDoT <- fmData %>%
  ggplot(aes(x=NoF, y=DoT)) +
  geom_point() +
  theme_minimal() +
  labs (x="NoF = Number of Features",y="DoT = Depth of Tree")

plotNoFDoT

#15
plotNoFADoT <- fmData %>%
  ggplot(aes(x=NoF, y=ADoT)) +
  geom_point() +
  theme_minimal() +
  labs (x="NoF = Number of Features",y="ADoT = Avg Depth of Tree")

plotNoFADoT

#16
plotNoFRoV <- fmData %>%
  ggplot(aes(x=NoF, y=RoV)) +
  geom_point() +
  theme_minimal() +
  labs (x="NoF = Number of Features",y="RoV = Ratio of Variability")

plotNoFRoV

#17
plotNoFBF <- fmData %>%
  ggplot(aes(x=NoF, y=BF)) +
  geom_point() +
  theme_minimal() +
  labs (x="NoF = Number of Features",y="BF = Branching Factor")

plotNoFBF

#18
plotNoFCoC <- fmData %>%
  ggplot(aes(x=NoF, y=CoC)) +
  geom_point() +
  theme_minimal() +
  labs (x="NoF = Number of Features",y="CoC = Coefficient of connectivity")

plotNoFCoC

#19
plotNoFFoC <- fmData %>%
  ggplot(aes(x=NoF, y=FoC)) +
  geom_point() +
  theme_minimal() +
  labs (x="NoF = Number of Features",y="FoC = Flexibility of Configuration")

plotNoFFoC


# Grid with metrics NoF 11-20
gridExtra::grid.arrange(plotNoFSCDF, plotNoFMCDF, plotNoFNoFC,
                        plotNoFNoC, plotNoFDoT, plotNoFADoT,
                        plotNoFRoV, plotNoFBF, plotNoFCoC, plotNoFFoC,
                        nrow=5, ncol=2)

# Broken up
gridExtra::grid.arrange(plotNoFSCDF, plotNoFMCDF, plotNoFNoFC,
                        plotNoFNoC,
                        nrow=2, ncol=2)

gridExtra::grid.arrange(plotNoFDoT, plotNoFADoT, plotNoFRoV, plotNoFBF,
                        nrow=2, ncol=2)

gridExtra::grid.arrange(plotNoFCoC, plotNoFFoC,
                        nrow=1, ncol=2)



