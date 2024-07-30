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




#################################################################################
# Loads the entire set of curated transitions and writes out the rect transitions
# Per participant
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
# Function that creates the scarfplots for the transitions of a single participant with 24 questions
# Input: 
# * scarfplot.data, data frame with the rect plot information for a given participant 24 questions
#        it must have the required order of AOIs factor
# * participantNumber, is the number of participant to be used for creating the title of the plot
library(plyr) # rounding functions
scarfPlotParticipant <- function (scarfplot.data, participantNumber) {

  msec2secs <- 1000     # constant for transformation to secs
  tenseconds <- 10 * msec2secs # constant for generating the ticks every 10000 msecs = 10 secs
  xmax.value <- max(scarfplot.data$Xmax)  # computes the maximum value of the Xmax coordinates for this participant
  upper.limit.x <- round_any(as.numeric(xmax.value), tenseconds, f = ceiling) # rounds up the value for next 10 secs
  sequence.numbers.labels.x <- seq(0, upper.limit.x/msec2secs, tenseconds/msec2secs) # computes the sequence of label values
  tensecs.labels.x <- as.character(sequence.numbers.labels.x) # converts the sequences to strings for relabeling
  
  scarfplot.title <- paste("Participant ",participantNumber,sep="")
  
  # # Color blind palette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  # cbPalette <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999","#E69F00")

  # Color blind palette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999")
  
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
## Generates participants scarfplots
scarfplots.data.participants <-
  read.csv(file = "../../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/Transitions-Plots-Data/All-Participants-Transitions-Rect-Plots-Per-Participant-Data.csv",
           header=TRUE)
attach(scarfplots.data.participants)

# Changes the names of AOIs to factors
scarfplots.data.participants$IDAOI <- as.factor(scarfplots.data.participants$IDAOI)

# Reorders the AOIs in a more meaningful way (the first 3 are the most important ones)
scarfplots.data.participants <-
  scarfplots.data.participants %>%
  mutate(IDAOI=fct_relevel(IDAOI,c("FM","Question","CTC","Answer","Window","Buttons","Legend")))

# Accumulates the scarf plot objects into a list
list.scarfplots.participants <- list()
participants.ids <-unique(scarfplots.data.participants$Participant)
plots.index <-1 
for (participant.id in participants.ids) {
  print(paste("Plotting participant ",participant.id,sep=""))
  
  scarfplot.part <- scarfPlotParticipant(scarfplots.data.participants %>% filter(Participant==participant.id),
                                         participant.id) 
  scarfplot.part
  list.scarfplots.participants[[plots.index]] <- scarfplot.part  
  plots.index <- plots.index + 1
}



###############################################################################
# Function that saves all the plots in a list in files using the id names and 
# the path name. The file is in a given format. 
# Input: 
# * listOfPlots: list of ggplot objets that can be displayed and saved
# * listOfIDs: list of names to use as file names to be saved
# * pathname: name of the path to prepend to the IDs to save the plots into files
# * format: the String with the format type to use to save the image, e.g "png" 
savePlotsList <- function (listOfPlots, listOfIDs, pathName, format, suffixName) {
  
  index <-1
  for(id in listOfIDs){
    # computes the final path name
    fName <- paste(suffixName,id,".",format,sep="")
    print(fName)
    
    # plots the plots on the window
    print(listOfPlots[index])
    
    # Saves the file in the given 
    ggsave(filename=fName, path=pathName)
    
    # Increments the index
    index <- index + 1
    
  } # of all the IDs in the list
  
} # of savePlotsList

################################################################################

## Saves the list of scarfsplots per participant into files
savePlotsList(list.scarfplots.participants, 
              unique(scarfplots.data.participants$Participant), 
              "./ParticipantScarfplots/", "png", "Participant-")

################################################################################
################################################################################
################################################################################
# Function that creates the scarfplots for the transitions of a single question with 17 participant
# Input: 
# * scarfplot.question.data, data frame with the rect plot information for a given question for 17 participants
#        it must have the required order of AOIs factor
# * questionNumber, is the number of question to be used for creating the title of the plot

scarfPlotQuestion <- function (scarfplot.question.data, questionNumber) {
  msec2secs <- 1000     # constant for transformation to secs
  tenseconds <- 10 * msec2secs # constant for generating the ticks every 10000 msecs = 10 secs
  xmax.value <- max(scarfplot.question.data$Xmax)  # computes the maximum value of the Xmax coordinates for this participant
  upper.limit.x <- round_any(as.numeric(xmax.value), tenseconds, f = ceiling) # rounds up the value for next 10 secs
  sequence.numbers.labels.x <- seq(0, upper.limit.x/msec2secs, tenseconds/msec2secs) # computes the sequence of label values
  tensecs.labels.x <- as.character(sequence.numbers.labels.x) # converts the sequences to strings for relabeling
  
  scarfplot.title <- paste("Question ",questionNumber,sep="")
  
  # Color blind palette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  # cbPalette <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999","#E69F00")
  
  # Color blind palette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999")
  
  
  scarfplot.question <- scarfplot.question.data %>% 
    ggplot() + 
    geom_rect(mapping=aes(xmin=Xmin, xmax=Xmax, ymin=Ymin, ymax=Ymax, fill=IDAOI), alpha=0.9) + 
    theme(panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5,size = 20),
          panel.grid.major.y = element_line(colour = "grey50"), # parallel lines to x axis
          panel.grid.major.x = element_line(colour = "grey50"), # perpendicular lines in
          axis.title.x = element_blank(),   # clears the label of the axis 
          legend.position = "bottom") +
    scale_fill_manual(values=cbPalette, drop=FALSE) +
    scale_x_continuous(breaks=seq(0,upper.limit.x,tenseconds), labels=tensecs.labels.x) +
    labs(y = "Participant", x = "Fixations sequence and duration 10 secs intervals)", fill ="AOI") +
    ggtitle(scarfplot.title) + # Title
    scale_y_discrete(limits=as.factor(seq(1, 17, 1))) 
  
  # Returns the constructed plot
  return(scarfplot.question)
  
}  # end of function scarfPlotQuestion


################################################################################
# Creates the array of scarfplots for questions

scarfplots.data.questions <-
  read.csv(file = "../../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/Transitions-Plots-Data/All-Participants-Transitions-Rect-Plots-Per-Question-Data.csv",
           header=TRUE)
attach(scarfplots.data.questions)

# Changes the names of AOIs to factors
scarfplots.data.questions$IDAOI <- as.factor(scarfplots.data.questions$IDAOI)

# Reorders the AOIs in a more meaningful way (the first 3 are the most important ones)
scarfplots.data.questions <-
  scarfplots.data.questions %>%
  mutate(IDAOI=fct_relevel(IDAOI,c("FM","Question","CTC","Answer","Window","Buttons","Legend")))


# Iterates over all question numbers
# Accumulates the scarf plot objects into a list
list.scarfplots.questions <- list()
questions.ids <-unique(scarfplots.data.questions$QN)

plots.index <-1 
for (question.id in questions.ids) {
  print(paste("Plotting question ",question.id,sep=""))
  
  scarfplot.part <- scarfPlotQuestion(scarfplots.data.questions %>% filter(QN==question.id),
                                         question.id) 
  scarfplot.part
  list.scarfplots.questions[[plots.index]] <- scarfplot.part  
  plots.index <- plots.index + 1
}

################################################################################
## Saves the list of scarfsplots per participant into files
savePlotsList(list.scarfplots.questions, 
              unique(scarfplots.data.questions$QN),
              "./QuestionScarfplots/", "png","Question-")


################################################################################
# Function that creates a color step plot for the transitions of a single participant and a question
# Input: 
# * stepplot.participant.question.data, data frame with the rect plot information 
#                                       for a given participant and one question
# * participantNumber, is the number of participant to be used for creating the title of the plot
#  * questionNumber, is the number of question to be used for creating the title of the plot
library(plyr) # rounding functions

stepPlotsParticipantQuestion <- function(stepplot.participant.question.data, participantNumber,
                                         questionNumber) {
  
  levels.AOIS <- levels(scarfplots.data.participants$IDAOI)
  
  step.plot.data <- stepplot.participant.question.data %>% 
    mutate(X=Xmin) %>%
    mutate(Y=case_when(IDAOI=="FM" ~ 1,
                       IDAOI=="Question" ~ 2,
                       IDAOI=="CTC" ~ 3,
                       IDAOI=="Answer" ~ 4,
                       IDAOI=="Window" ~ 5,
                       IDAOI=="Buttons" ~ 6,
                       IDAOI=="Legend" ~ 7))

  # Color blind palette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999")
  

  msec2secs <- 1000     # constant for transformation to secs
  tenseconds <- 10 * msec2secs # constant for generating the ticks every 10000 msecs = 10 secs
  xmax.value <- max(step.plot.data$Xmax)  # computes the maximum value of the Xmax coordinates for this participant
  upper.limit.x <- round_any(as.numeric(xmax.value), tenseconds, f = ceiling) # rounds up the value for next 10 secs
  sequence.numbers.labels.x <- seq(0, upper.limit.x/msec2secs, tenseconds/msec2secs) # computes the sequence of label values
  tensecs.labels.x <- as.character(sequence.numbers.labels.x) # converts the sequences to strings for relabeling
  aois.labels.y <- c("FM","Question","CTC","Answer","Window","Buttons","Legend")
  
  if (participantNumber!=0) { 
    plot.title <- paste("Participant ", participantNumber, " Question ", questionNumber,sep="")
  }
  else
    plot.title <-""
  
  step.plot <- step.plot.data %>% ggplot() + 
    geom_step(aes(x = X, y = Y, colour = IDAOI, group=1), size=2) +
    theme(panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5,size = 20),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major.y = element_line(colour = "grey50"), # parallel lines to x axis
          panel.grid.major.x = element_line(colour = "grey50"), # perpendicular lines in y axis
          axis.text.y = element_text(color=cbPalette, size = 12, face="bold",vjust=0.3), # colors of the labels in axis y
          legend.position = "none") +
    scale_colour_manual(values = cbPalette, drop=FALSE) +
    ggtitle(plot.title) + # Title
    #labs(y = "AOIs", x = "Fixations sequence and duration 10 secs intervals)", colour ="AOI") + # Adds the labels
    scale_x_continuous(breaks=seq(0,upper.limit.x,tenseconds), labels=tensecs.labels.x) + # adds tick values
    scale_y_continuous(breaks=seq(1,7,1), labels = aois.labels.y) # add the values of AOIs
  
  
  # returns the created step plot
  return (step.plot)
  
} # of stepPlotsParticipantQuestion


################################################################################
# Creates the array of step plots for all participants and questions

stepplots.data.participants <-
  read.csv(file = "../../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/Transitions-Plots-Data/All-Participants-Transitions-Rect-Plots-Per-Participant-Data.csv",
           header=TRUE)
attach(stepplots.data.participants)

# Changes the names of AOIs to factors
stepplots.data.participants$IDAOI <- as.factor(stepplots.data.participants$IDAOI)

# Reorders the AOIs in a more meaningful way (the first 3 are the most important ones)
stepplots.data.participants <-
  stepplots.data.participants %>%
  mutate(IDAOI=fct_relevel(IDAOI,c("FM","Question","CTC","Answer","Window","Buttons","Legend")))

# Accumulates the scarf plot objects into a list
list.stepplots.participants <- list()
participants.ids <-unique(stepplots.data.participants$Participant)
plots.index <-1 
for (participant.id in participants.ids) {
  
  for (question.id in seq(1,24,1)) {
    print(paste("Plotting Participant ",participant.id," Question ",question.id, sep=""))
    
    stepplot <- stepPlotsParticipantQuestion( 
      stepplots.data.participants %>% filter(Participant==participant.id & QN==question.id),
      participant.id, question.id) 
    
    list.stepplots.participants[[plots.index]] <- stepplot  
    plots.index <- plots.index + 1  
          
  } # for all the questions
  
} # for all participant


################################################################################
## Saves the step plots into files

frame.ids <- expand.grid(unique(stepplots.data.participants$Participant),seq(1,24,1)) %>%
  arrange(Var1,Var2) %>%
  mutate(file.id=paste("P",Var1,"Q",Var2,sep="-"))

savePlotsList(list.stepplots.participants, 
              frame.ids$file.id,
              "./Stepplots/", "png","Stepplot-")


################################################################################
# Computes the step plots of the core transitions, those containing only
# AOIs: FM, CTC, Question

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
  participant.filtered.transitions <- all.participants.trans %>% 
    filter(Participant==participant.id & (IDAOI %in% c("FM","CTC","Question")))

    
  # Computes the rect plot data frame for a given participant
  transitions.rect.plot <- computeRectangleCoordinates(participant.filtered.transitions,TRUE)
  
  # Appends the rows of the data frame to the accumulating data frame
  rect.transitions.data <- rbind(rect.transitions.data, transitions.rect.plot)
  
} # for all the participants

# Writes out the transitions data for the rectangle plots of all the participants
write.csv(rect.transitions.data, 
          file = "../../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/Transitions-Plots-Data/All-Participants-Core-Transitions-Rect-Plots-Per-Participant-Data.csv",
          row.names = FALSE)


################################################################################
# Function that creates a color step plot for the core transitions of a single participant and a question
# Input: 
# * stepplot.participant.question.data, data frame with the rect plot information 
#                                       for a given participant and one question
# * participantNumber, is the number of participant to be used for creating the title of the plot
#  * questionNumber, is the number of question to be used for creating the title of the plot
library(plyr) # rounding functions

stepCorePlotsParticipantQuestion <- function(stepplot.participant.question.data, participantNumber,
                                         questionNumber) {
  
  levels.AOIS <- levels(stepplot.participant.question.data$IDAOI)
  
  step.plot.data <- stepplot.participant.question.data %>% 
    mutate(X=Xmin) %>%
    mutate(Y=case_when(IDAOI=="FM" ~ 1,
                       IDAOI=="Question" ~ 2,
                       IDAOI=="CTC" ~ 3))

  # Color blind palette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999")
  

  msec2secs <- 1000     # constant for transformation to secs
  tenseconds <- 10 * msec2secs # constant for generating the ticks every 10000 msecs = 10 secs
  xmax.value <- max(step.plot.data$Xmax)  # computes the maximum value of the Xmax coordinates for this participant
  upper.limit.x <- round_any(as.numeric(xmax.value), tenseconds, f = ceiling) # rounds up the value for next 10 secs
  sequence.numbers.labels.x <- seq(0, upper.limit.x/msec2secs, tenseconds/msec2secs) # computes the sequence of label values
  tensecs.labels.x <- as.character(sequence.numbers.labels.x) # converts the sequences to strings for relabeling
  aois.labels.y <- c("FM","Question","CTC")
  
  if (participantNumber!=0) { 
    plot.title <- paste("Participant ", participantNumber, " Question ", questionNumber,sep="")
  }
  else
    plot.title <-""
  
  step.plot <- step.plot.data %>% ggplot() + 
    geom_step(aes(x = X, y = Y, colour = IDAOI, group=1), size=2) +
    theme(panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5,size = 20),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major.y = element_line(colour = "grey50"), # parallel lines to x axis
          panel.grid.major.x = element_line(colour = "grey50"), # perpendicular lines in y axis
          axis.text.y = element_text(color=cbPalette, size = 12, face="bold",vjust=0.3), # colors of the labels in axis y
          legend.position = "none") +
    scale_colour_manual(values = cbPalette, drop=FALSE) +
    ggtitle(plot.title) + # Title
    #labs(y = "AOIs", x = "Fixations sequence and duration 10 secs intervals)", colour ="AOI") + # Adds the labels
    scale_x_continuous(breaks=seq(0,upper.limit.x,tenseconds), labels=tensecs.labels.x) + # adds tick values
    scale_y_continuous(limits=c(1,3), breaks=seq(1,3,1), labels = aois.labels.y)  # add the values of AOIs
    # scale_y_discrete(limits=as.factor(seq(1, 3, 1))) # sets the dimensions of the y axis to contain 3 values
  
  # returns the created step plot
  return (step.plot)
  
} # of stepCorePlotsParticipantQuestion


################################################################################
# Creates the array of core step plots for all participants and questions

core.stepplots.data.participants <-
  read.csv(file = "../../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/Transitions-Plots-Data/All-Participants-Core-Transitions-Rect-Plots-Per-Participant-Data.csv",
           header=TRUE)
attach(core.stepplots.data.participants)

# Changes the names of AOIs to factors
core.stepplots.data.participants$IDAOI <- as.factor(core.stepplots.data.participants$IDAOI)

# Reorders the AOIs in a more meaningful way (the first 3 are the most important ones)
core.stepplots.data.participants <-
  core.stepplots.data.participants %>%
  mutate(IDAOI=fct_relevel(IDAOI,c("FM","Question","CTC")))

# Accumulates the core step plot objects into a list
list.core.stepplots.participants <- list()
participants.ids <-unique(core.stepplots.data.participants$Participant)
plots.index <-1 
for (participant.id in participants.ids) {
  
  for (question.id in seq(1,24,1)) {
    print(paste("Plotting Core Participant ",participant.id," Question ",question.id, sep=""))
    
    stepplot <- stepCorePlotsParticipantQuestion( 
      core.stepplots.data.participants %>% filter(Participant==participant.id & QN==question.id),
      participant.id, question.id) 
    
    list.core.stepplots.participants[[plots.index]] <- stepplot  
    plots.index <- plots.index + 1  
    
  } # for all the questions
  
} # for all participant

################################################################################
## Saves the step plots into files

frame.ids <- expand.grid(unique(core.stepplots.data.participants$Participant),seq(1,24,1)) %>%
  arrange(Var1,Var2) %>%
  mutate(file.id=paste("P",Var1,"Q",Var2,sep="-"))

savePlotsList(list.core.stepplots.participants, 
              frame.ids$file.id,
              "./Stepplots/", "png","CoreStepplot-")


# Example of a test for plot without CTC
stepCorePlotsParticipantQuestion(core.stepplots.data.participants %>% filter(Participant==2 & QN==1), 2,1)



################################################################################
# Test of multiple step plots for all participants in a question
# OUTCOME: Even using other lines and a single color it is simply not possible
# to draw multiple results (participant-question) on the same plot because
# it gets too crowded and it is not possible to distinguish between them

multiple.step.plots.data <- core.stepplots.data.participants %>% 
  filter(QN==3 & (Participant==2 | Participant==3))

levels.AOIS <- levels(multiple.step.plots.data$IDAOI)

multiple.step.plots.data <- multiple.step.plots.data %>% 
  mutate(X=Xmin) %>%
  mutate(Y=case_when(IDAOI=="FM" ~ 1,
                     IDAOI=="Question" ~ 2,
                     IDAOI=="CTC" ~ 3))

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999")


msec2secs <- 1000     # constant for transformation to secs
tenseconds <- 10 * msec2secs # constant for generating the ticks every 10000 msecs = 10 secs
xmax.value <- max(multiple.step.plots.data$Xmax)  # computes the maximum value of the Xmax coordinates for this participant
upper.limit.x <- round_any(as.numeric(xmax.value), tenseconds, f = ceiling) # rounds up the value for next 10 secs
sequence.numbers.labels.x <- seq(0, upper.limit.x/msec2secs, tenseconds/msec2secs) # computes the sequence of label values
tensecs.labels.x <- as.character(sequence.numbers.labels.x) # converts the sequences to strings for relabeling
aois.labels.y <- c("FM","Question","CTC")

step.plot <- multiple.step.plots.data %>% ggplot() + 
  # geom_step(aes(x = X, y = Y, colour = IDAOI, group=Participant), size=2) + # colour = IDAOI, colors by AOI the segments
  geom_line(aes(x = X, y = Y,  group=Participant), size=1) +
  theme(panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"), # parallel lines to x axis
        panel.grid.major.x = element_line(colour = "grey50"), # perpendicular lines in y axis
        axis.text.y = element_text(color=cbPalette, size = 12, face="bold",vjust=0.3), # colors of the labels in axis y
        legend.position = "none") +
  scale_colour_manual(values = cbPalette, drop=FALSE) +
  ggtitle("Multiple") + # Title
  #labs(y = "AOIs", x = "Fixations sequence and duration 10 secs intervals)", colour ="AOI") + # Adds the labels
  scale_x_continuous(breaks=seq(0,upper.limit.x,tenseconds), labels=tensecs.labels.x) + # adds tick values
  scale_y_continuous(limits=c(1,3), breaks=seq(1,3,1), labels = aois.labels.y)  # add the values of AOIs
# scale_y_discrete(limits=as.factor(seq(1, 3, 1))) # sets the dimensions of the y axis to contain 3 values
step.plot


###############################################################################
# Function that creates the participants scarfplots with the inaccurate responses highlighted

scarfPlotParticipantAccuracy <- function () {
  
  # Loads all the participants questions rect plots information
  scarfplots.data.participants <-
    read.csv(file = "../../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/Transitions-Plots-Data/All-Participants-Transitions-Rect-Plots-Per-Participant-Data.csv",
             header=TRUE)
  attach(scarfplots.data.participants)
  
  # Changes the names of AOIs to factors
  scarfplots.data.participants$IDAOI <- as.factor(scarfplots.data.participants$IDAOI)
  
  # Reorders the AOIs in a more meaningful way (the first 3 are the most important ones)
  scarfplots.data.participants <-
    scarfplots.data.participants %>%
    mutate(IDAOI=fct_relevel(IDAOI,c("FM","Question","CTC","Answer","Window","Buttons","Legend")))
  
  
  # Loads all the participant responses for all the questions
  experiment.complete.data <-
    read.csv(file = "../../../Experiment-Data/Eye-tracking-data-samples/ExperimentCompleteDataSet.csv",
             header=TRUE)
  attach(experiment.complete.data)
  
  # delta increases for the red crosses in the overlayed graph
  delta.y <- 0.5
  delta.x <- 2/10*10000

  # Accumulates the plot objects into a list
  list.accuracy.scarfplots.participants <- list()
  participants.ids <-unique(scarfplots.data.participants$Participant)
  plots.index <-1 
  
  # for all the participants
  for (participant.id in participants.ids) {
    
    print(paste("Plotting Accuracy Participant ",participant.id,sep=""))
      
    # data of the scarfplot for the participant
    participant.scarfplot.data <- scarfplots.data.participants %>% 
        filter(Participant==participant.id)
      
    # gets the base plot of the participant
    base.plot <- scarfPlotParticipant(participant.scarfplot.data,participant.id) 

    # selects the questions of the given participant
    participant.accuracy.data <- experiment.complete.data %>% 
      filter(ParticipantID==participant.id) %>% select(ParticipantID,QNumber,Correct)
    
    # selects the maximum x positions of each questio
    xmax.positions <- participant.scarfplot.data %>% group_by(QN) %>%
      filter(Xmax==max(Xmax)) %>% select(Participant,QN,Xmax,Ymin)
      
    # joins the rect plots and the responses accuracy
    join.data <- left_join(xmax.positions,participant.accuracy.data, 
                           by=c('QN'='QNumber'))

    # keeps only those responses that are inaccurate, Correct==1
    inaccurate.responses <- join.data %>% filter(Correct==1)
    
    # creates the plot by adding the wrong responses to the scarfplots
    result.plot <- base.plot +
      geom_point(data=inaccurate.responses, aes(x=Xmax + delta.x , y=Ymin + delta.y), 
                 shape=4, fill="red", color="red", size=3, stroke = 3)
    
    
    # adds the result plo to the list
    list.accuracy.scarfplots.participants[[plots.index]] <- result.plot  
    plots.index <- plots.index + 1   

  } # for all participants
  
  # Returns the list of plots with the inaccurate answers highlighted
  return(list.accuracy.scarfplots.participants)
  
} # end of scarfPlotParticipantAccuracy


################################################################################
# Computes the participant plots with the inaccurate responses indicated
# and saves them on the files

participants.list.accuracy.scarflots <- scarfPlotParticipantAccuracy()


savePlotsList(participants.list.accuracy.scarflots, 
              unique(scarfplots.data.participants$Participant), 
              "./ParticipantScarfplots/", "png", "Accuracy-Participant-")


###############################################################################
# Testing overlaying wrong marks on top of graphs

scarfplots.data.questions <-
  read.csv(file = "../../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/Transitions-Plots-Data/All-Participants-Transitions-Rect-Plots-Per-Participant-Data.csv",
           header=TRUE)
attach(scarfplots.data.questions)

# Changes the names of AOIs to factors
scarfplots.data.questions$IDAOI <- as.factor(scarfplots.data.questions$IDAOI)

# Reorders the AOIs in a more meaningful way (the first 3 are the most important ones)
scarfplots.data.questions <-
  scarfplots.data.questions %>%
  mutate(IDAOI=fct_relevel(IDAOI,c("FM","Question","CTC","Answer","Window","Buttons","Legend")))

# Gets the base scarfplot of a participant
test.scarfplot <- scarfPlotParticipant(scarfplots.data.questions %>% filter(Participant==14), 14) 


# # Adds a line on top of the scarfplot
# line.test <- data.frame(x = seq(0, 90000, by = 3600),
#                   y = seq(0, 25, by = 1))
# test.scarfplot + geom_line(data=line.test, aes(x=x, y=y), color="red")
# 
# 
# # Adds points with cross shape to an scarfplot
# line.test <- data.frame(x = seq(3600, 90000, by = 3600),
#                         y = seq(1, 24, by = 1))
# 
# # cross shape
# test.scarfplot + 
#   geom_point(data=line.test, aes(x=x, y=y), shape=4, fill="red", color="red", size=3, stroke = 3)
# 
# # asterisk shape
# test.scarfplot + geom_point(data=line.test, aes(x=x, y=y), shape=8, fill="red", color="red", size=5)


# Testing obtaining the maximum of each question of a given participant
test.xmax <- scarfplots.data.questions %>% filter(Participant==14) %>% group_by(QN) %>%
  filter(Xmax==max(Xmax)) %>% select(Participant,QN,Xmax,Ymin)

# Testing the plots with X at the end of scarfs 
delta.y <- 0.5
delta.x <- 2/10*10000
test.scarfplot + 
  geom_point(data=test.xmax, aes(x=Xmax + delta.x , y=Ymin + delta.y), shape=4, fill="red", color="red", size=3, stroke = 3)

# Joining with the information to mark only those questions that are 

experiment.complete.data <-
  read.csv(file = "../../../Experiment-Data/Eye-tracking-data-samples/ExperimentCompleteDataSet.csv",
           header=TRUE)
attach(experiment.complete.data)

# Selects the questions of the given participant
test.participant.data <- experiment.complete.data %>% filter(ParticipantID==14) %>% select(ParticipantID,QNumber,Correct)

# Joins the data from rect plots and correct responses
test.join <- left_join(test.xmax,test.participant.data, by=c('QN'='QNumber'))

# Keeps only those responses that are inaccurate, Correct==1
inaccurate.responses <- test.join %>% filter(Correct==1)

# Testing with only those question with inaccurate response
delta.y <- 0.5
delta.x <- 2/10*10000
test.scarfplot + 
  geom_point(data=inaccurate.responses, aes(x=Xmax + delta.x , y=Ymin + delta.y), shape=4, fill="red", color="red", size=3, stroke = 3)


# joining the data


# defining the delta.x to increase the x position at the end of the scarfplot
test.all.limits <- scarfplots.data.questions %>% group_by(Participant,QN) %>%
  filter(Xmax==max(Xmax)) %>% select(Participant,QN,Xmax,Ymin)
summary(test.all.limits)

  
ggplot(df1, aes(x, log(y))) + 
  geom_line() +
  geom_line(data = df2, color = "red")


# Iterates over all question numbers
# Accumulates the scarf plot objects into a list
list.scarfplots.questions <- list()
questions.ids <-unique(scarfplots.data.questions$QN)

plots.index <-1 
for (question.id in questions.ids) {
  print(paste("Plotting question ",question.id,sep=""))
  
  scarfplot.part <- scarfPlotQuestion(scarfplots.data.questions %>% filter(QN==question.id),
                                      question.id) 
  scarfplot.part
  list.scarfplots.questions[[plots.index]] <- scarfplot.part  
  plots.index <- plots.index + 1
}





################################################################################
################################################################################
################################################################################

###################### Scratch test code #######################################

################################################################################
## Tests of line plots for the transitions of a participant and question

# mutate(Y=which(p2.trans.q1.data$IDAOI==IDAOI))

levels.AOIS <- levels(scarfplots.data.participants$IDAOI)

p2.trans.q1.data <- scarfplots.data.participants %>% # from all participants scarfplots
  filter(Participant==12 & QN==23) %>% # keep only first question for participant 2
  mutate(X=Xmin) %>%
  mutate(Y=case_when(IDAOI=="FM" ~ 1,
                     IDAOI=="Question" ~ 2,
                     IDAOI=="CTC" ~ 3,
                     IDAOI=="Answer" ~ 4,
                     IDAOI=="Window" ~ 5,
                     IDAOI=="Buttons" ~ 6,
                     IDAOI=="Legend" ~ 7))

# mutate(Y=transformAOIToCoordinate(IDAOI))


# First test
# p2.trans.q1.data %>% ggplot(aes(X, Y)) + geom_step(aes(colour=as.numeric(IDAOI)))


# Color blind palette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999")

# https://stackoverflow.com/questions/42633374/how-to-get-a-step-plot-using-geom-step-with-different-colors-for-the-segments
# https://stackoverflow.com/questions/14794599/how-to-change-line-width-in-ggplot
# https://stackoverflow.com/questions/8737454/r-ggplot2-colouring-step-plot-depending-on-value 
# https://stackoverflow.com/questions/18839731/vary-colors-of-axis-labels-in-r-based-on-another-variable

library(plyr) # rounding functions
msec2secs <- 1000     # constant for transformation to secs
tenseconds <- 10 * msec2secs # constant for generating the ticks every 10000 msecs = 10 secs
xmax.value <- max(p2.trans.q1.data$Xmax)  # computes the maximum value of the Xmax coordinates for this participant
upper.limit.x <- round_any(as.numeric(xmax.value), tenseconds, f = ceiling) # rounds up the value for next 10 secs
sequence.numbers.labels.x <- seq(0, upper.limit.x/msec2secs, tenseconds/msec2secs) # computes the sequence of label values
tensecs.labels.x <- as.character(sequence.numbers.labels.x) # converts the sequences to strings for relabeling
aois.labels.y <- c("FM","Question","CTC","Answer","Window","Buttons","Legend")

p2.trans.q1.data %>% ggplot() + 
  geom_step(aes(x = X, y = Y, colour = IDAOI, group=1), size=2) +
  theme(panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"), # parallel lines to x axis
        panel.grid.major.x = element_line(colour = "grey50"), # perpendicular lines in y axis
        axis.text.y = element_text(color=cbPalette, size = 12, face="bold",vjust=0.3), # colors of the labels in axis y
        legend.position = "none" # legend.position = "bottom", # bottom if we have to have a legend
  ) +
  scale_colour_manual(values = cbPalette, drop=FALSE) +
  #labs(y = "AOIs", x = "Fixations sequence and duration 10 secs intervals)", colour ="AOI") + # Adds the labels
  scale_x_continuous(breaks=seq(0,upper.limit.x,tenseconds), labels=tensecs.labels.x) + # adds tick values
  scale_y_continuous(breaks=seq(1,7,1), labels = aois.labels.y) # add the values of AOIs


# Example of code scarfplot to start tweaking the step plot
scarfplot.question <- scarfplot.question.data %>% 
  ggplot() + 
  geom_rect(mapping=aes(xmin=Xmin, xmax=Xmax, ymin=Ymin, ymax=Ymax, fill=IDAOI), alpha=0.9) + 
  theme(panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 20),
        panel.grid.major.y = element_line(colour = "grey50"), # parallel lines to x axis
        panel.grid.major.x = element_line(colour = "grey50"), # perpendicular lines in
        axis.title.x = element_blank(),   # clears the label of the axis 
        legend.position = "bottom") +
  scale_fill_manual(values=cbPalette, drop=FALSE) +
  scale_x_continuous(breaks=seq(0,upper.limit.x,tenseconds), labels=tensecs.labels.x) +
  labs(y = "Participant", x = "Fixations sequence and duration 10 secs intervals)", fill ="AOI") +
  ggtitle(scarfplot.title) + # Title
  scale_y_discrete(limits=as.factor(seq(1, 17, 1))) 


transformAOIToCoordinate <- function(AOI) {
  AOI.number <- which(levels.AOIS==AOI)
  print(paste("AOI received ",AOI," value=",AOI.number,sep=""))
  # Returns the index of the value
  #return (which(levels.AOIS==AOI))
  return (AOI.number)
} # of transformAOIToCoordinate


################################################################################
################################################################################
################################################################################

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

###################
# Testing participants plots - scratch code

# Testing for participant 8
scarfplot.p8 <- scarfPlotParticipant(scarfplots.data.participants %>% filter(Participant==8),8) 

# Test for function that computes the labels on the way axis
msec2secs <- 1000
tenseconds <- 10 * msec2secs
xmax.value <- max(scarfplots.data.participants$Xmax)  # computes the maximum value of the Xmax cordinates
upper.limit.x <- round_any(as.numeric(xmax.value), tenseconds, f = ceiling) # rounds up the value for next 10 secs
sequence.numbers.labels.x <- seq(0, upper.limit.x/msec2secs, tenseconds/msec2secs) # computes the sequence of label values
tensecs.labels.x <- as.character(sequence.numbers.labels.x)

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

# library(plyr)
# round_any(as.numeric(58304), 10000, f = ceiling)

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


# Test of scarfplot for question
# Load the file for questions
scarfplots.data.questions <-
  read.csv(file = "../../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/Transitions-Plots-Data/All-Participants-Transitions-Rect-Plots-Per-Question-Data.csv",
           header=TRUE)
attach(scarfplots.data.questions)

# Changes the names of AOIs to factors
scarfplots.data.questions$IDAOI <- as.factor(scarfplots.data.questions$IDAOI)

# Reorders the AOIs in a more meaningful way (the first 3 are the most important ones)
scarfplots.data.questions <-
  scarfplots.data.questions %>%
  mutate(IDAOI=fct_relevel(IDAOI,c("FM","Question","CTC","Buttons","Legend","Answer","Window")))

# Example of a plot for a participant
questionNumber <- 2
scarfplot.question.data <- scarfplots.data.questions %>% filter(QN==questionNumber) 


msec2secs <- 1000     # constant for transformation to secs
tenseconds <- 10 * msec2secs # constant for generating the ticks every 10000 msecs = 10 secs
xmax.value <- max(scarfplot.question.data$Xmax)  # computes the maximum value of the Xmax coordinates for this participant
upper.limit.x <- round_any(as.numeric(xmax.value), tenseconds, f = ceiling) # rounds up the value for next 10 secs
sequence.numbers.labels.x <- seq(0, upper.limit.x/msec2secs, tenseconds/msec2secs) # computes the sequence of label values
tensecs.labels.x <- as.character(sequence.numbers.labels.x) # converts the sequences to strings for relabeling

scarfplot.title <- paste("Question ",questionNumber,sep="")

# Color blind palette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
cbPalette <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999","#E69F00")

scarfplot.question <- scarfplot.question.data %>% 
  ggplot() + 
  geom_rect(mapping=aes(xmin=Xmin, xmax=Xmax, ymin=Ymin, ymax=Ymax, fill=IDAOI), alpha=0.9) + 
  theme(panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 20),
        panel.grid.major.y = element_line(colour = "grey50"), # parallel lines to x axis
        panel.grid.major.x = element_line(colour = "grey50"), # perpendicular lines in
        axis.title.x = element_blank(),   # clears the label of the axis 
        legend.position = "bottom") +
  scale_fill_manual(values=cbPalette, drop=FALSE) +
  scale_x_continuous(breaks=seq(0,upper.limit.x,tenseconds), labels=tensecs.labels.x) +
  labs(y = "Participant", x = "Fixations sequence and duration 10 secs intervals)", fill ="AOI") +
  ggtitle(scarfplot.title) + # Title
  scale_y_discrete(limits=as.factor(seq(1, 17, 1))) 


##############################################################################################

# Make a function that computes the selected plot for question

# Find out how to programatically generate the image files

 