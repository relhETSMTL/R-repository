# TOSEM 24 paper
# Transitions EDA, file computations and plotting

library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(dplyr)

# Sets the dir to the current place
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

################################################################################

################
## Auxiliary functions for string processing names

# If participant number is less than 10, add a leading zero to the string
leadingZeros <- function (participantNumber) {
  
  if (participantNumber<10) paste0("0",9)
  else
    as.character(participantNumber)
} # end of leadingZeros


#########################
## Function that computes the rectangle coordinates for the transitions scarf plots
# Returns a data frame with the information of the curated transition plus min, max for X and Y for drawing rectangle
# plots either for participants or questions
computeRectangleCoordinates <- function (raw.df, perParticipant) {

  # Creates a copy of the raw.df for an individual, adds the columns for the rectangle coordinates
  raw.rectangle.df <- raw.df
  raw.rectangle.df$Xmin <- rep(0,nrow(raw.rectangle.df))
  raw.rectangle.df$Xmax <- rep(0,nrow(raw.rectangle.df))
  raw.rectangle.df$Ymin <- rep(0,nrow(raw.rectangle.df))
  raw.rectangle.df$Ymax <- rep(0,nrow(raw.rectangle.df))

  # All the questions for each participant  
  if (perParticipant==TRUE) {

    # Assumption that all the fixations of a question and participant are sequentially put together
    # Set current row to be first question for the participant
    current.qn <- 1 # raw.rectangle.df$QN[1]
    
    # # Initializes the counters for the rectangle x, y coordinates
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
    
  } else { # computes the duration for the scarf plot per question

    
    # TODO revision of      
    # Assumption that all the fixations of a question are sequentially put together 
    # per participant starting at the first
  
    # Gets the list of the participants to iterate over
    participants.ids <-unique(raw.rectangle.df$Participant) 
    
    # Set current row for the first participant, we assume that the frame has the same order of participants
    # for a given question
    current.participant.id <- raw.rectangle.df$Participant[1]
    
    # # Initializes the counters for the rectangle x, y coordinates
    x.accum <- 0
    y.accum <- 1
    
    # Loop for all the data of the question
    for (i in 1:nrow(raw.rectangle.df)) {
      row.data <- raw.rectangle.df [i,]
      
      # TODO we need to do a mapping between the participant number and the position in the participants list
      # Checks if it has reached a new participant
      # If a new participant number is found, set current question and reset fixation counter to 1
      if (row.data$Participant != current.participant.id) {
        current.participant.id <- row.data$Participant # sets the new current participant for comparison
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
        
  }  # of the computation per question
  
  # returns the data frame with the computed rectangle plot coordinates
  return (raw.rectangle.df)
  
} # end of computeRectangleCoordinates

# # Creates a copy of the raw.df for an individual, adds the columns for the rectangle coordinates
# raw.rectangle.df <- raw.df
# raw.rectangle.df$Xmin <- rep(0,nrow(raw.rectangle.df))
# raw.rectangle.df$Xmax <- rep(0,nrow(raw.rectangle.df))
# raw.rectangle.df$Ymin <- rep(0,nrow(raw.rectangle.df))
# raw.rectangle.df$Ymax <- rep(0,nrow(raw.rectangle.df))

# # Assumption that all the fixations of a question and participant are sequentially put together
# # Set current row to be the first question in the data frame
# current.qn <- raw.rectangle.df$QN[1]

# # Initializes the counters for the rectangle coordinates
# x.accum <- 0
# y.accum <- 1
# 
# # Loop for all the data of the participant
# for (i in 1:nrow(raw.rectangle.df)) {
#   row.data <- raw.rectangle.df [i,]
#   
#   # Checks if it has reached a new question
#   # If a new question number is found, set current question and reset fixation counter to 1
#   if (row.data$QN != current.qn) {
#     current.qn <- row.data$QN           # sets the new current row for comparison
#     x.accum <- 0                        # resets x coordinate to zero
#     y.accum <- y.accum + 1              # increments to the next question
#   } #
#   
#   # Assigns coordinates to the new rectangle 
#   raw.rectangle.df[i,]$Xmin <- x.accum                  # starting x accumulated
#   x.accum <- x.accum + raw.rectangle.df[i,]$Duration    # increments x coordinate by width of duration
#   raw.rectangle.df[i,]$Xmax <- x.accum                  # maximum is the new accumulated
#   raw.rectangle.df[i,]$Ymin <- y.accum                  # starts from the current question
#   raw.rectangle.df[i,]$Ymax <- y.accum + 1              # sets the maximum, one point higher
#   
#   print (c(i, 
#            raw.rectangle.df[i,]$Xmin, raw.rectangle.df[i,]$Xmax, 
#            raw.rectangle.df[i,]$Ymin, raw.rectangle.df[i,]$Ymax))
#   
# } # end of for all the data




#####################
# Loads the entire set of curated transitions
all.participants.trans <- read.csv(file = "../../../Experiment-Data/All-Participants-Transitions-Curated-Data.csv", 
                                   header=TRUE)
attach (all.participants.trans)

# Computes a vector of participants and question numbers to iterate on
participants.ids <-unique(all.participants.trans$Participant) 


# initializes the data frame to empty
rect.transitions.data <- data.frame()


# input directory
input.dir <- "../../../Experiment-Data/Eye-tracking-data-samples/"

# Loops for all the participants and all the questions, loading the matrix of transition frequencies
for (participant.id in participants.ids) {
  
  print(participant.id)
  
  # Filters the transition data for the given participant
  participant.transitions <- all.participants.trans %>% filter(Participant==participant.id)
  
  # Computes the rect plot data frame for a given participant
  transitions.rect.plot <- computeRectangleCoordinates(participant.transitions,TRUE)
    
  # Appends the rows of the data frame to the accumulating data frame
  rect.transitions.data <- rbind(rect.transitions.data, transitions.rect.plot)

} # for all the participants

# Writes out the transitions data for the rectangle plots of all the participants
write.csv(rect.transitions.data, 
    file = "../../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/Transitions-Plots-Data/All-Participants-Transitions-Rect-Plots-Per-Participant-Data.csv",
    row.names = FALSE)

################################################################################

# TODO

# Loads the entire set of curated transitions
all.participants.trans <- read.csv(file = "../../../Experiment-Data/All-Participants-Transitions-Curated-Data.csv", 
                                   header=TRUE)
attach (all.participants.trans)


# Gets the number of questions to iterate over
question.nums <-  unique(all.participants.trans$QN)

# initializes the data frame to empty
rect.transitions.data <- data.frame()


# Loops for all the participants and all the questions, loading the matrix of transition frequencies
for (question.id in question.nums) {
  
  print(question.id)
  
  # Filters the transition data for the given participant
  participant.transitions <- all.participants.trans %>% filter(QN==question.id)
  
  # Computes the rect plot data frame for a given participant
  transitions.rect.plot <- computeRectangleCoordinates(participant.transitions,FALSE)
  
  # Appends the rows of the data frame to the accumulating data frame
  rect.transitions.data <- rbind(rect.transitions.data, transitions.rect.plot)
  
} # for all the participants


# Writes out the transitions data for the rectangle plots of all the participants
write.csv(rect.transitions.data, 
          file = "../../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/Transitions-Plots-Data/All-Participants-Transitions-Rect-Plots-Per-Question-Data.csv",
          row.names = FALSE)

################################################################################

library(plyr) # rounding functions

# Function that creates the scarfplots for the transitions of a single participant with 24 questions
# Input: 
# * scarfplot.data, data frame with the rect plot information for a given participant 24 questions
#        it must have the required order of AOIs factor
# * participantNumber, is the number of participant to be used for creating the title of the plot

scarfPlotParticipant <- function (scarfplot.data, participantNumber) {

  msec2secs <- 1000     # constant for transformation to secs
  tenseconds <- 10 * msec2secs # constant for generating the ticks every 10000 msecs = 10 secs
  xmax.value <- max(scarfplot.data$Xmax)  # computes the maximum value of the Xmax coordinates for this participant
  upper.limit.x <- round_any(as.numeric(xmax.value), tenseconds, f = ceiling) # rounds up the value for next 10 secs
  sequence.numbers.labels.x <- seq(0, upper.limit.x/msec2secs, tenseconds/msec2secs) # computes the sequence of label values
  tensecs.labels.x <- as.character(sequence.numbers.labels.x) # converts the sequences to strings for relabeling
  
  scarfplot.title <- paste("Participant ",participantNumber,sep="")
  
  # Color blind palette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  cbPalette <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999","#E69F00")
  
  scarfplot.participant <- scarfplot.data %>% 
    ggplot() + 
    geom_rect(mapping=aes(xmin=Xmin, xmax=Xmax, ymin=Ymin, ymax=Ymax, fill=IDAOI), alpha=0.9) + 
    theme(panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5,size = 20),
          panel.grid.major.y = element_line(colour = "grey50"), # parallel lines to x axis
          panel.grid.major.x = element_line(colour = "grey50"), # perpendicular lines in
          axis.title.x = element_blank(),   # clears the label of the axis 
          legend.position = "bottom") +
    scale_fill_manual(values=cbPalette) +
    scale_x_continuous(breaks=seq(0,upper.limit.x,tenseconds), labels=tensecs.labels.x) +
    labs(y = "Question number", x = "Fixations sequence and duration 10 secs intervals)", fill ="AOI") +
    ggtitle(scarfplot.title) + # Title
    scale_y_discrete(limits=as.factor(seq(1, 24, 1))) 
  
  # Returns the constructed plot
  return(scarfplot.participant)
  
}  # end of function 

#################
## Testing function that generates participants scarfplots


scarfplots.data.participants <-
  read.csv(file = "../../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/Transitions-Plots-Data/All-Participants-Transitions-Rect-Plots-Per-Participant-Data.csv",
           header=TRUE)
attach(scarfplots.data.participants)

# Changes the names of AOIs to factors
scarfplots.data.participants$IDAOI <- as.factor(scarfplots.data.participants$IDAOI)

# Reorders the AOIs in a more meaningful way (the first 3 are the most important ones)
scarfplots.data.participants <-
  scarfplots.data.participants %>%
  mutate(IDAOI=fct_relevel(IDAOI,c("FM","Question","CTC","Buttons","Legend","Answer","Window")))


# TODO how to accumulate the scarf plot objects into a frame? or a vector? or list?
list.scarfplots.participants <- data.frame()
participants.ids <-unique(scarfplots.data.participants$Participant) 
for (participant.id in participants.ids) {
  print(paste("Plotting participant ",participant.id,sep=""))
  
  scarfplot.part <- scarfPlotParticipant(scarfplots.data.participants %>% filter(Participant==participant.id),
                                         participant.id) 
  scarfplot.part
  list.scarfplots.participants <- append(list.scarfplots.participants, scarfplot.part)
}

# Testing for participant 8
scarfplot.p8 <- scarfPlotParticipant(scarfplots.data.participants %>% filter(Participant==8),8) 


###################

# Test for function that computes the labels on the way axis
msec2secs <- 1000
tenseconds <- 10 * msec2secs
xmax.value <- max(scarfplots.data.participants$Xmax)  # computes the maximum value of the Xmax cordinates
upper.limit.x <- round_any(as.numeric(xmax.value), tenseconds, f = ceiling) # rounds up the value for next 10 secs
sequence.numbers.labels.x <- seq(0, upper.limit.x/msec2secs, tenseconds/msec2secs) # computes the sequence of label values
tensecs.labels.x <- as.character(sequence.numbers.labels.x)
  
# TODO
# Load the file for participant

scarfplots.data.participants <-
  read.csv(file = "../../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/Transitions-Plots-Data/All-Participants-Transitions-Rect-Plots-Per-Participant-Data.csv",
  header=TRUE)
attach(scarfplots.data.participants)

# Changes the names of AOIs to factors
scarfplots.data.participants$IDAOI <- as.factor(scarfplots.data.participants$IDAOI)

# Reorders the AOIs in a more meaningful way (the first 3 are the most important ones)
scarfplots.data.participants <-
  scarfplots.data.participants %>%
  mutate(IDAOI=fct_relevel(IDAOI,c("FM","Question","CTC","Buttons","Legend","Answer","Window")))
       
# Load the file for question

# Test of plots for participant
# Original taken from TransitionData-Plotting file


# Color blind palette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
cbPalette <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999","#E69F00")

# Example of a plot for a participant
scarfplot.data <- scarfplots.data.participants %>% filter(Participant==8) 

scarfplot.participant <- scarfplot.data %>% 
  ggplot() + 
  geom_rect(mapping=aes(xmin=Xmin, xmax=Xmax, ymin=Ymin, ymax=Ymax, fill=IDAOI), alpha=0.9) + 
  theme(panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 20),
        panel.grid.major.y = element_line(colour = "grey50"), # parallel lines to x axis
        panel.grid.major.x = element_line(colour = "grey50"), # perpendicular lines in
        axis.title.x = element_blank(),   # clears the label of the axis 
        # axis.text.x=element_blank(),      # clears the text of the x axis
        legend.position = "bottom") +
  # theme_minimal(legend.position = "bottom") +
  # scale_fill_brewer(palette="Set1") + 
  scale_fill_manual(values=cbPalette) +
  scale_x_continuous(breaks=seq(0,60000,10000), labels=c("0","10","20","30","40","50","60")) +
  labs(y = "Question number", x = "Fixations sequence and duration (msec)", fill ="AOI") +
  ggtitle("Participant 8") + # Title
  scale_y_discrete(limits=as.factor(seq(1, 24, 1))) 
scarfplot.participant
ggsave("plot.png") # Saves the plot to a file


scarfplot.participant + theme(legend.position="none") # shows the scarfplots without the legend
scarfplot.participant + theme(plot.title = element_blank()) # removes the title of the participant names

# Maximum value depicted
layer_scales(scarfplot.participant)$x$range$range

# scarfplot.participant$coordinates$limits

# https://stackoverflow.com/questions/56940147/how-to-round-integer-by-intervals-of-500-in-r

library(plyr)
round_any(as.numeric(58304), 10000, f = ceiling)


# Details for the title
# https://r-charts.com/ggplot2/titles/#google_vignette
# https://rpubs.com/Mentors_Ubiqum/ggplot_remove_elements


# Example of adjusting tick levels
# http://www.sthda.com/english/wiki/ggplot2-axis-ticks-a-guide-to-customize-tick-marks-and-labels
# https://stackoverflow.com/questions/37950511/set-tick-mark-intervals
# g + scale_x_continuous(breaks = seq(10, 60, by = 10))
# + scale_y_continuous(breaks = seq(0, 10, len = 5))

# for the plot: add participant label, , remove legend to put in a single place,
# find out about adding a check or a cross for accurate or innacurate

# Transformation to seconds instead of msecs
# Just adding a division wont work because of the pixels
# Wrong: %>% mutate(Xmin=Xmin/1000) %>% mutate(Xmax=Xmax/100)


# Test of plot for question

# Make a function that computes the selected plot for participant

# Make a function that computes the selected plot for question

# Find out how to programatically generate the image files

# Find out