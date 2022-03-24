# Transforming the computed transitions data for plotting

library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(dplyr)



## Example of computing the transistion data and plotting for participant 05
participant05.data <- read.csv(file ="../../Experiment-Data/Eye-tracking-data-samples/PartP05/P05-Transitions-Data.csv",
                               header=TRUE)
attach (participant05.data)

# For all the participant data, create a row with the form:
# Participant, QN, 1..6 AOI ID, FN fixation number, Duration

raw.df <- data.frame()
raw.df <- raw.df %>% add_column(Participant="Participant", QN="QN", IDAOI="IDAOI", FN ="FN", Duration="Duration")

# Assumption that all the fixations of a question and participant are sequentially put together
# Set current question to the first in the data frame
current.qn <- participant05.data$QN[1]
# Keeps the counter of how many fixations per question
fixation.counter <- 1

# Loop for all the data of the participant
for (i in 1:nrow(participant05.data)) {
  row.data <- participant05.data[i,]
  
  # if a new question number is found, set current question and reset fixation counter to 1
  if (row.data$QN != current.qn) {
     current.qn <- row.data$QN
     fixation.counter <- 1
  }
  

  # Answer=1, Buttons=2, CTC=3, FM=4, Legend=5, Question=6, Window=7
  aoi.fixation <- 0 
  num.selected.aois <-0
  
  # Indicated the selected AOI of the fixation
  if (row.data$Answer=="1") {
    aoi.fixation <- "Answer"
    num.selected.aois <-  num.selected.aois + 1
  }
  if (row.data$Buttons=="1") {
    aoi.fixation <- "Buttons"
    num.selected.aois <-  num.selected.aois + 1
  }
  if (row.data$CTC=="1") {
    aoi.fixation <- "CTC"
    num.selected.aois <-  num.selected.aois + 1
  }
  if (row.data$FM =="1") {
    aoi.fixation <- "FM"
    num.selected.aois <-  num.selected.aois + 1
  }
  if (row.data$Legend=="1") {
    aoi.fixation <- "Legend"
    num.selected.aois <-  num.selected.aois + 1
  }
  if (row.data$Question=="1") {
    aoi.fixation <- "Question"
    num.selected.aois <-  num.selected.aois + 1
  }
  
  # if no AOI fixation was selected then mark it as Window, i.e. not in any AOI
  if (aoi.fixation == 0) {
    aoi.fixation <- "Window"
  }

  # Error checking
  if (row.data$Window != "1") {
    stop("Error in data")
  } 
    
  # Adds the new row to the 
  raw.df[i,] = c(row.data$Participant,row.data$QN,aoi.fixation,fixation.counter, row.data$Duration)
  
  print (i)
  # print (new.row)
  
  # increments the row number
  fixation.counter <- fixation.counter + 1
  
} # end of for all the data


## Transforming the columns into numerical values
raw.df <- raw.df %>%
  mutate(Participant = as.numeric(Participant)) %>%
  mutate(QN = as.numeric(QN)) %>%
  mutate(IDAOI = as.factor(IDAOI)) %>%
  mutate(FN = as.numeric(FN)) %>%
  mutate(Duration = as.numeric(Duration))

################################################################################################
################################################################################################
################################################################################################

# Plot test 1
# Simple scatter plot, x axis = FN, y axis = QN, color based on IDAOI values 1..7
scatter.squence.p05 <- raw.df %>%
  ggplot(aes (x=FN, y=QN)) +
  geom_point(aes(color=IDAOI),size=5) + # , size=5
  theme_minimal() +
  labs(y = "Question number", x = "Fixation sequence") +
  scale_color_brewer(palette = "Set1", name="AOI") +   
  scale_y_discrete(limits=as.factor(seq(1, 24, 1)))
scatter.squence.p05

# Plot test 2
# Plot like a tile sequence
# https://stackoverflow.com/questions/10232525/geom-tile-heatmap-with-different-high-fill-colours-based-on-factor
tile.sequence.p05 <- raw.df %>%
  ggplot(aes(x = FN, y= QN, fill=IDAOI)) + # ,alpha = z  
  geom_tile() + 
  theme_minimal() +
  labs(y = "Question number", x = "Fixation sequence", fill="AOI") +
  scale_color_brewer(palette = "Set1", name="AOI") +   
  scale_y_discrete(limits=as.factor(seq(1, 24, 1)))
tile.sequence.p05


# Plot test 3
# Plot like a tile sequence
# https://stackoverflow.com/questions/10232525/geom-tile-heatmap-with-different-high-fill-colours-based-on-factor
tile2.sequence.p05 <- raw.df %>%
  ggplot(aes(x = FN, y= QN, fill=IDAOI)) + # ,alpha = z  
  geom_tile(aes(width=0.7, height=0.7), size=3) + 
  theme_minimal() +
  labs(y = "Question number", x = "Fixation sequence", fill ="AOI") +
#  scale_color_brewer(palette = "Set1", name="AOI") +   
  scale_y_discrete(limits=as.factor(seq(1, 24, 1)))
tile2.sequence.p05


# Plot test 4
# Plot like a tile sequence with spaces
tile3.sequence.p05 <- raw.df %>%
  ggplot(aes(x = FN, y= QN)) + # ,alpha = z  
  geom_tile(aes(fill=IDAOI, width=0.7, height=0.7), size=3) + 
  theme_minimal() +
  labs(y = "Question number", x = "Fixation sequence", fill ="AOI") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_discrete(limits=as.factor(seq(1, 24, 1)))
tile3.sequence.p05

# Plot test 5
# Plot stacked charts, fractions per
stacked.chart.p05 <- raw.df %>%
  ggplot(aes(fill=IDAOI, y=FN, x=QN)) + 
  coord_flip() +
  labs(x = "Question number", y = "AOI percentage", fill ="AOI") +
  scale_x_discrete(limits=as.factor(seq(1, 24, 1))) +
  geom_bar(position="fill", stat="identity") 
stacked.chart.p05


# Plot test 6
# Plot stacked charts, fractions per
stacked2.chart.p05 <- raw.df %>%
  ggplot(aes(fill=IDAOI, y=FN, x=QN)) + 
  coord_flip() +
  labs(x = "Question number", y = "AOI percentage", fill ="AOI") +
  scale_x_discrete(limits=as.factor(seq(1, 24, 1))) +
  geom_bar(position="fill", stat="identity") 
stacked2.chart.p05


# Example of geom_rect
# http://sape.inf.usi.ch/quick-reference/ggplot2/geom_rect
d=data.frame(x1=c(1,3,1,5,4), x2=c(2,4,3,6,6), y1=c(1,1,4,1,3), y2=c(2,2,5,3,5), 
             t=c('a','a','a','b','b'), r=c(1,2,3,4,5))
ggplot() + 
  scale_x_continuous(name="x") + 
  scale_y_continuous(name="y") +
  geom_rect(data=d, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=t), color="black", alpha=0.5) +
  geom_text(data=d, aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=r), size=4) 
 # +
#  opts(title="geom_rect", plot.title=theme_text(size=40, vjust=1.5))




# Plot test 7
# Plot with rectangles to show duration and sequence

# Creates a copy of the raw.df for an individual, adds the columns for the rectangle coordinates
raw.rectangle.df <- raw.df
raw.rectangle.df$Xmin <- rep(0,nrow(raw.rectangle.df))
raw.rectangle.df$Xmax <- rep(0,nrow(raw.rectangle.df))
raw.rectangle.df$Ymin <- rep(0,nrow(raw.rectangle.df))
raw.rectangle.df$Ymax <- rep(0,nrow(raw.rectangle.df))


# Assumption that all the fixations of a question and participant are sequentially put together
# Set current row to be the first question in the data frame
current.qn <- raw.rectangle.df$QN[1]

# Initializes the counters for the rectangle coordinates
x.accum <- 0
y.accum <- 1

# Loop for all the data of the participant
for (i in 1:nrow(raw.rectangle.df)) {
  row.data <- raw.rectangle.df [i,]

  # Checks if it has reached a new question
  # If a new question number is found, set current question and reset fixation counter to 1
  if (row.data$QN != current.qn) {
    current.qn <- row.data$QN           # sets the new current row for comparison
    x.accum <- 0                        # resets x coordinate to zero
    y.accum <- y.accum + 1              # increments to the next question
  } #
  
  # Assigns coordinates to the new rectangle 
  raw.rectangle.df[i,]$Xmin <- x.accum                  # starting x accumulated
  x.accum <- x.accum + raw.rectangle.df[i,]$Duration    # increments x coordinate by width of duration
  raw.rectangle.df[i,]$Xmax <- x.accum                  # maximum is the new accumulated
  raw.rectangle.df[i,]$Ymin <- y.accum                  # starts from the current question
  raw.rectangle.df[i,]$Ymax <- y.accum + 1              # sets the maximum, one point higher

  print (c(i, 
         raw.rectangle.df[i,]$Xmin, raw.rectangle.df[i,]$Xmax, 
         raw.rectangle.df[i,]$Ymin, raw.rectangle.df[i,]$Ymax))
  
} # end of for all the data

# Computes the plot for the rectangles with the duration in milisecs
rect.plot <- raw.rectangle.df %>% 
  ggplot() + 
  geom_rect(mapping=aes(xmin=Xmin, xmax=Xmax, ymin=Ymin, ymax=Ymax, fill=IDAOI), 
            color="black", alpha=0.5) + 
  theme_minimal() +
  labs(y = "Question number", x = "Fixation sequence and duration (msec)", fill ="AOI") +
  scale_y_discrete(limits=as.factor(seq(1, 24, 1))) 
rect.plot


# Plot test 7
# Plot with rectangles without borders to show duration and sequence
# Computes the plot for the rectangles with the duration in milisecs
rect.plot2 <- raw.rectangle.df %>% 
  ggplot() + 
  geom_rect(mapping=aes(xmin=Xmin, xmax=Xmax, ymin=Ymin, ymax=Ymax, fill=IDAOI), 
            alpha=0.5) + 
  theme_minimal() +
  labs(y = "Question number", x = "Fixation sequence and duration (msec)", fill ="AOI") +
  scale_y_discrete(limits=as.factor(seq(1, 24, 1))) 
rect.plot2



################################################################################################
################################################################################################
################################################################################################


# TODO 
# 2022-03-23 
# Done - Keep the counter per question to add a sequencial number for each fixation in a question
# Done - Transform the resulting columns to numerical factors all the columns
# Done Explore first plotting examples
#
# 2022-03-24
# Explore rectangle plots for showing sequence and duration of transtitions





################################################################################################
################################################################################################
################################################################################################

#  new.row <- c(row.data$Participant, row.data$QN, 0, row.data$Duration)  
#  raw.df <- raw.df %>% add_row(Participant=row.data$Participant,
#                     QN = row.data$QN,
#                     IDAOI = row.data$Index,
#                     Duration = row.data$Duration)
