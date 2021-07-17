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
  geom_histogram( binwidth=10, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 1") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )

p