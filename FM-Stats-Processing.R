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


