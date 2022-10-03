# Kambiz Experiment Design Setup
#



# Loading the pairs and triplets and select randomly N=50 to try for visualization questions

library(ggplot2)
library(tidyverse)
library(hrbrthemes)

# Seeds the random number generator
set.seed(37)



### Process example for a single file

# File reads the experiment data 
pairsData <- read.csv(file = "./FM-Visualization/FM-Pairs-Triplets/FM01_2.csv", header=TRUE)
attach (pairsData)

sampleSize <- 50

# Projects only the pairs, removing the solution columns, and samples N solutions
sample <- pairsData %>% select(FEATURE1, FEATURE2) %>% sample_n(sampleSize)

# Stores the sample in a file
write.csv(sample, "./FM-Visualization/FM-Pairs-Triplets/Sample-FM01_2.csv", row.names = FALSE, quote=FALSE)



#### 
### Function that receives the sample size, the name of the pair file, and the name of the output file

generatePairs = function (sampleSize, pairFile, sampleOutputFile) {
  
  pairsData <- read.csv(file = pairFile, header=TRUE)
  attach (pairsData)
  
  # Projects only the pairs, removing the solution columns, and samples N solutions
  sample <- pairsData %>% select(FEATURE1, FEATURE2) %>% sample_n(sampleSize)
  
  # Stores the sample in a file
  write.csv(sample, sampleOutputFile, row.names = FALSE, quote=FALSE)
  
}  # end of function generatePairs



# Generating the pairs
generatePairs(50, "./FM-Visualization/FM-Pairs-Triplets/FM01_2.csv", "./FM-Visualization/FM-Pairs-Triplets/Sample-FM01_2.csv" )
generatePairs(50, "./FM-Visualization/FM-Pairs-Triplets/FM02_2.csv", "./FM-Visualization/FM-Pairs-Triplets/Sample-FM02_2.csv" )
generatePairs(50, "./FM-Visualization/FM-Pairs-Triplets/FM03_2.csv", "./FM-Visualization/FM-Pairs-Triplets/Sample-FM03_2.csv" )
generatePairs(50, "./FM-Visualization/FM-Pairs-Triplets/FM04_2.csv", "./FM-Visualization/FM-Pairs-Triplets/Sample-FM04_2.csv" )
generatePairs(50, "./FM-Visualization/FM-Pairs-Triplets/FM05_2.csv", "./FM-Visualization/FM-Pairs-Triplets/Sample-FM05_2.csv" )
generatePairs(50, "./FM-Visualization/FM-Pairs-Triplets/FM06_2.csv", "./FM-Visualization/FM-Pairs-Triplets/Sample-FM06_2.csv" )
generatePairs(50, "./FM-Visualization/FM-Pairs-Triplets/FM07_2.csv", "./FM-Visualization/FM-Pairs-Triplets/Sample-FM07_2.csv" )
generatePairs(50, "./FM-Visualization/FM-Pairs-Triplets/FM08_2.csv", "./FM-Visualization/FM-Pairs-Triplets/Sample-FM08_2.csv" )
generatePairs(50, "./FM-Visualization/FM-Pairs-Triplets/FM09_2.csv", "./FM-Visualization/FM-Pairs-Triplets/Sample-FM09_2.csv" )
generatePairs(50, "./FM-Visualization/FM-Pairs-Triplets/FM10_2.csv", "./FM-Visualization/FM-Pairs-Triplets/Sample-FM10_2.csv" )
generatePairs(50, "./FM-Visualization/FM-Pairs-Triplets/FM11_2.csv", "./FM-Visualization/FM-Pairs-Triplets/Sample-FM11_2.csv" )



