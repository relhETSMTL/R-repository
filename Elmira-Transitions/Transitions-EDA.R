# Transition Analysis Experiments
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(dplyr)

########################################################################################
########################################################################################

  # Function that computes the transitions between FM, Question, CTC
  compute.transitions <- function (pid, qnum, participant.question.data, aois.factors, output.dir) {
    
    # initializes the variables for the transition counters
    counter <- 0 
    
    # initializes the current type
    current.type <- participant.question.data$IDAOI[1]  # The first entry in the transition matrix, AOI of the first transition
    index.from <- match(current.type, aois.factors)
    
    number.transitions <- nrow(participant.question.data) 
    
    # initializes the matrices of transitions
    trans.matrix <- matrix(rep(0,49), nrow=7, ncol = 7)
    
    # traverses all the fixations counting the transitions of different types
    for (aoi in participant.question.data[,3]) {
    
      # computes the index the to AOI
      index.to <- match(aoi, aois.factors)
      
      # increments the entry in the matrix by one
      trans.matrix[index.from, index.to] <- trans.matrix[index.from, index.to] + 1 
      
      # changes the index from to the current transition
      index.from <- index.to
    
      counter <- counter + 1   
      
      # print(aoi)
      # print(counter)
      
      
    } # traverses all the fixations
    
    # return a vector with the following 
    # result <- c(pid, qnum, fm.question, fm.ctc, question.fm, question.ctc, ctc.fm, ctc.question)  
    
    # changes the names of the rows and columns to the AOI names, e.g trans.matrix["Answer","CTC"]
    colnames(trans.matrix) <- aois.factors
    rownames(trans.matrix) <- aois.factors
    
    # write the output string
    out.file.name <- paste(output.dir,"P",pid,"-Q",qnum,"-Transitions-Matrix.csv",sep="")
    print(out.file.name)
    write.csv(trans.matrix, file = out.file.name, row.names = TRUE)
    
    return (trans.matrix)
    
  } # of compute transitions
  

########################################################################################
########################################################################################

# Loads the entire set of curated transitions
all.participants.trans <- read.csv(file = "../../Experiment-Data/All-Participants-Transitions-Curated-Data.csv", 
                              header=TRUE)
attach (all.participants.trans)

# Making the IDAOIs factors instead of characters
all.participants.trans <- all.participants.trans %>%
  mutate(IDAOI=as.factor(IDAOI))

# Computes a vector of participants and question numbers to iterate on
participants.ids <-unique(all.participants.trans$Participant) 
question.nums <-  unique(all.participants.trans$QN)


# Testing a matrix transition call 
# pq.data.p2.q1 <- all.participants.trans %>% filter(Participant==2 & QN==1)
# factors.aois <- as.factor(levels(all.participants.trans$IDAOI))
mt.p2.q1 <- compute.transitions(2,1,pq.data.p2.q1,factors.aois, "../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/")
# colnames(mt.p2.q1) <- factors.aois
# rownames(mt.p2.q1) <- factors.aois
# cat ("../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/","P",2,"-Q",1,"-Transitions-Matrix.csv")
# paste("../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/","P",2,"-Q",1,"-Transitions-Matrix.csv",sep="")
# write.csv(mt.p2.q1, file = "../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/P02-Q1-Transitions-Matrix.csv", 
#           row.names = TRUE)


# Computes the factors of the AOIs names
factors.aois <- as.factor(levels(all.participants.trans$IDAOI))

## For all the participants
for (participant.id in participants.ids) {
  
  for (question.num in question.nums) {
    cat("[",participant.id,"-",question.num) 
    # print('done')
 
    ## Calls the procedure that filters the transition information for the participant and the question
    # Parameters: participant.id, question.num, transitions for that participant and question.num
    
    # Transition data for the participant and given question
    pq.data <- all.participants.trans %>%
      filter(Participant==participant.id & QN==question.num)  # & (IDAOI=="Question" | IDAOI=="FM" | IDAOI=="CTC") )
    
    # This is the number of transitions per participant and question  
    cat(",",nrow(pq.data),"]")
    
     
    pq.result <- compute.transitions(participant.id,question.num,pq.data,
                                     factors.aois,
                                     "../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/")
    
    } ## for all the questions
  
} ##  for all the participants
  
## For all the questions


################################################################
# Some sanity checks
pq.data.p19.q24 <- all.participants.trans %>% filter(Participant==19 & QN==24)
mt.p19.q24 <- compute.transitions(19,24,pq.data.p19.q24,factors.aois, "../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/")


## Testing the procedure with only the data for AOIs FM, CTC, and Question
# Filtering only the AOIs required
pq.data.p19.q24.core <- all.participants.trans %>% 
  filter(Participant==19 & QN==24 & (IDAOI=="Question" | IDAOI=="FM" | IDAOI=="CTC"))
  
# Compute the matrix with the same function.  
mt.p19.q24 <- compute.transitions(19,24,pq.data.p19.q24.core,factors.aois, "../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/")

# Removing the columns and rows not required
new.mt.p19.q24 <- mt.p19.q24[,colnames(mt.p19.q24)!="Answer"] 
new.mt.p19.q24 <- new.mt.p19.q24[,colnames(new.mt.p19.q24)!="Buttons"]
new.mt.p19.q24 <- new.mt.p19.q24[,colnames(new.mt.p19.q24)!="Legend"]
new.mt.p19.q24 <- new.mt.p19.q24[,colnames(new.mt.p19.q24)!="Window"]

# M<-M[rownames(M)!="A",] --> note the , for the entire row
new.mt.p19.q24 <- new.mt.p19.q24[rownames(new.mt.p19.q24)!="Answer",]
new.mt.p19.q24 <- new.mt.p19.q24[rownames(new.mt.p19.q24)!="Buttons",]
new.mt.p19.q24 <- new.mt.p19.q24[rownames(new.mt.p19.q24)!="Legend",]
new.mt.p19.q24 <- new.mt.p19.q24[rownames(new.mt.p19.q24)!="Window",]


########################################################################################
########################################################################################

# Function that computes the transitions between FM, Question, CTC
compute.core.transitions <- function (pid, qnum, participant.question.data, aois.factors, output.dir) {
  
  # initializes the variables for the transition counters
  counter <- 0 
  
  # initializes the current type
  current.type <- participant.question.data$IDAOI[1]  # The first entry in the transition matrix, AOI of the first transition
  index.from <- match(current.type, aois.factors)
  
  number.transitions <- nrow(participant.question.data) 
  
  # initializes the matrices of transitions
  trans.matrix <- matrix(rep(0,49), nrow=7, ncol = 7)
  
  # traverses all the fixations counting the transitions of different types
  for (aoi in participant.question.data[,3]) {
    
    # computes the index the to AOI
    index.to <- match(aoi, aois.factors)
    
    # increments the entry in the matrix by one
    trans.matrix[index.from, index.to] <- trans.matrix[index.from, index.to] + 1 
    
    # changes the index from to the current transition
    index.from <- index.to
    
    counter <- counter + 1   
    
    # print(aoi)
    # print(counter)
    
    
  } # traverses all the fixations
  
  # return a vector with the following 
  # result <- c(pid, qnum, fm.question, fm.ctc, question.fm, question.ctc, ctc.fm, ctc.question)  
  
  # changes the names of the rows and columns to the AOI names, e.g trans.matrix["Answer","CTC"]
  colnames(trans.matrix) <- aois.factors
  rownames(trans.matrix) <- aois.factors
  
  
  # Removing the columns and rows not required
  trans.matrix <- trans.matrix[,colnames(trans.matrix)!="Answer"] 
  trans.matrix <- trans.matrix[,colnames(trans.matrix)!="Buttons"]
  trans.matrix <- trans.matrix[,colnames(trans.matrix)!="Legend"]
  trans.matrix <- trans.matrix[,colnames(trans.matrix)!="Window"]

  trans.matrix <- trans.matrix[rownames(trans.matrix)!="Answer",]
  trans.matrix <- trans.matrix[rownames(trans.matrix)!="Buttons",]
  trans.matrix <- trans.matrix[rownames(trans.matrix)!="Legend",]
  trans.matrix <- trans.matrix[rownames(trans.matrix)!="Window",]
  
  # write the output string including the Core suffix to indicate only the core rows
  out.file.name <- paste(output.dir,"P",pid,"-Q",qnum,"-Transitions-Matrix-Core.csv",sep="")
  print(out.file.name)
  write.csv(trans.matrix, file = out.file.name, row.names = TRUE)
  
  return (trans.matrix)
  
} # of compute core transitions


########################################################################################
########################################################################################
# Computing the core transitions = CTC, FM, Question

# Loads the entire set of curated transitions
all.participants.trans <- read.csv(file = "../../Experiment-Data/All-Participants-Transitions-Curated-Data.csv", 
                                   header=TRUE)
attach (all.participants.trans)

# Making the IDAOIs factors instead of characters
all.participants.trans <- all.participants.trans %>%
  mutate(IDAOI=as.factor(IDAOI))

# Computes a vector of participants and question numbers to iterate on
participants.ids <-unique(all.participants.trans$Participant) 
question.nums <-  unique(all.participants.trans$QN)

# Computes the factors of the AOIs names
factors.aois <- as.factor(levels(all.participants.trans$IDAOI))

## For all the participants
for (participant.id in participants.ids) {
  
  for (question.num in question.nums) {
    cat("[",participant.id,"-",question.num) 
    # print('done')
    
    ## Calls the procedure that filters the transition information for the participant and the question
    # Parameters: participant.id, question.num, transitions for that participant and question.num
    
    # Transition data for the participant and given question
    pq.data <- all.participants.trans %>%
      filter(Participant==participant.id & QN==question.num) 
    
    # This is the number of transitions per participant and question  
    cat(",",nrow(pq.data),"]")
    
    
    pq.result <- compute.core.transitions(participant.id, question.num, pq.data,
                                          factors.aois,
                                          "../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/")
    
  } ## for all the questions
  
} ##  for all the participants

## For all the questions


########################################################################################
########################################################################################

## Exploring the use of chordiagrams to show the values of the matrices

# install.packages("circlize")
library(circlize)

# Complete transitions matrix
p19.q24.trans <- 
  read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/P19-Q24-Transitions-Matrix.csv",
           header=TRUE)
                          
# Core transition matrix
p19.q24.core.trans <- 
  read.csv(file = "../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/P19-Q24-Transitions-Matrix-Core.csv",
           header=TRUE)



# https://r-charts.com/flow/chord-diagram/#google_vignette

library(reshape2)
# obtains the pairs entries of the matrix listing them by columsn
pairs <- melt(p19.q24.trans)

# Visualizes a complete chordiagram of the 7 AOIs
chordDiagram(pairs)
circos.clear()

# With given colors
colors <- c(Legend = "red",Window = "darkgrey", 
            Answer = "#FF410D", Buttons = "#6EE2FF", CTC = "#F7C530",
            FM = "#95CC5E", Question = "#D0DFE6")

chord <- chordDiagram(pairs, grid.col = colors, col = hcl.colors(49),
                      transparency = 0.3,
                      link.lwd = 1,    # Line width
                      link.lty = 1,    # Line type
                      link.border = 1)  

# Problem with the chordDiagram of this library is not possible to show it as grids
library(gridExtra)
library(grid)


# https://r-charts.com/flow/ggalluvial/#google_vignette
library(ggalluvial)

p1 <- ggplot(data = vaccinations,
       aes(axis1 = survey, axis2 = response, y = freq)) +
  geom_alluvium(aes(fill = response)) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Survey", "Response"),
                   expand = c(0.15, 0.05)) +
  theme_void()


# Default curve
p2 <- ggplot(data = pairs,
       aes(axis1 = X, axis2 = variable, y = value)) +
  geom_alluvium(aes(fill = X)) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  # scale_x_discrete(limits = c("Survey", "Response"),
  #                 expand = c(0.15, 0.05)) +
  theme_void()

# Linear curve -> very clear
p3 <- ggplot(data = pairs,
       aes(axis1 = X, axis2 = variable, y = value)) +
  geom_alluvium(aes(fill = X),curve_type = "linear") +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  # scale_x_discrete(limits = c("Survey", "Response"),
  #                 expand = c(0.15, 0.05)) +
  theme_void()

#  Cubic curve --> Also very clear
p4 <- ggplot(data = pairs,
       aes(axis1 = X, axis2 = variable, y = value)) +
  geom_alluvium(aes(fill = X),curve_type = "cubic") +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  # scale_x_discrete(limits = c("Survey", "Response"),
  #                 expand = c(0.15, 0.05)) +
  theme_void()

# Quintic curve --> also clear
p6 <- ggplot(data = pairs,
       aes(axis1 = X, axis2 = variable, y = value)) +
  geom_alluvium(aes(fill = X),curve_type = "quintic") +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  # scale_x_discrete(limits = c("Survey", "Response"),
  #                 expand = c(0.15, 0.05)) +
  theme_void()

# Sine curve
p7 <- ggplot(data = pairs,
       aes(axis1 = X, axis2 = variable, y = value)) +
  geom_alluvium(aes(fill = X),curve_type = "sine") +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  # scale_x_discrete(limits = c("Survey", "Response"),
  #                 expand = c(0.15, 0.05)) +
  theme_void()

# Curve arctangent
p8 <- ggplot(data = pairs,
       aes(axis1 = X, axis2 = variable, y = value)) +
  geom_alluvium(aes(fill = X),curve_type = "arctangent") +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  # scale_x_discrete(limits = c("Survey", "Response"),
  #                 expand = c(0.15, 0.05)) +
  theme_void()

# sigmoid
p9<- ggplot(data = pairs,
       aes(axis1 = AOI, axis2 = variable, y = value)) +
  geom_alluvium(aes(fill = AOI),curve_type = "sigmoid") +
  geom_stratum() +  # aes(fill = variable) only colors the second axis
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)),
            label.strata = FALSE) +
  # scale_x_discrete(limits = c("Survey", "Response"),
  #                 expand = c(0.15, 0.05)) +
  theme_void() + theme(legend.position = "none")

# https://stackoverflow.com/questions/27858123/how-to-pass-a-vector-of-ggplot-objects-to-grid-arrange-function
# plotting <- function(d){
#   P <- list()
#   vars <- names(d)[!names(d)%in%c('channel','label')]
#   for (var in vars){
#     p <- ggplot(d, aes_string(x='channel', y=var)) + 
#       geom_boxplot(aes(fill=label)) + ggtitle(var)
#     P <- c(P, list(p))
#   }
#   return(list(plots=P, num=length(vars)))
# }
# 
# PLOTS <- plotting(d)
# do.call(grid.arrange, c(PLOTS$plots, nrow = PLOTS$num))


#TODO test if we can define a function that returns a plot, based on the matrix data of a participant question
# Create the p

list.plots <- list(p1,p2,p3)

set.seed(1)
m <- matrix(sample(15, 15), 5, 3)
rownames(m) <- paste0("Row", 1:5)
colnames(m) <- paste0("Col", 1:3)

# Or use a data frame
df <- data.frame(from = rep(rownames(m), ncol(m)),
                 to = rep(colnames(m), each = nrow(m)),
                 value = as.vector(m))



########################################################################################
########################################################################################

# Functions for the creation of alluvial plots to illustrate the strength of the transitions
library(ggalluvial) # alluvial ggplots  
library(reshape2)   # converting a matrix/dataframe to a list of pairs


# Function that computes the transitions between FM, Question, CTC
compute.transitions.alluvial.plot <- function (pid, qnum, input.dir) {

  # Input file with the transition data
  input.file <- paste(input.dir,"P",pid,"-Q",qnum,"-Transitions-Matrix.csv",sep="")
  
  # Reads the participant and question data
  participant.question.data <- read.csv(file=input.file, header=TRUE)

  # Computes the pairs from the matrix cells
  pairs <- melt(participant.question.data)
  colnames(pairs)[1] <- "AOI"
  
  # Computes the plots 
  plot<- ggplot(data = pairs, aes(axis1 = AOI, axis2 = variable, y = value)) +
    geom_alluvium(aes(fill = AOI),curve_type = "sigmoid") +
    geom_stratum() +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)),
              label.strata = FALSE) +
    theme_void()  + 
    theme(legend.position = "none")
  
  # Returns the plot
  return (plot)
  
} # end of compute.transitions.alluvial.plot




## Function that generated alluvial plots for the 24 questions of a participant
# https://r-charts.com/flow/ggalluvial/#google_vignette


# Function that computes the transitions between FM, Question, CTC
compute.alluvial.plots.participant <- function (pid, input.dir) {
  
  q1 <- compute.transitions.alluvial.plot(pid,1,input.dir)
  q2 <- compute.transitions.alluvial.plot(pid,2,input.dir)
  q3 <- compute.transitions.alluvial.plot(pid,3,input.dir)
  q4 <- compute.transitions.alluvial.plot(pid,4,input.dir)
  q5 <- compute.transitions.alluvial.plot(pid,5,input.dir)

  q6 <- compute.transitions.alluvial.plot(pid,6,input.dir)
  q7 <- compute.transitions.alluvial.plot(pid,7,input.dir)
  q8 <- compute.transitions.alluvial.plot(pid,8,input.dir)
  q9 <- compute.transitions.alluvial.plot(pid,9,input.dir)
  q10 <- compute.transitions.alluvial.plot(pid,10,input.dir)

  q11 <- compute.transitions.alluvial.plot(pid,11,input.dir)
  q12 <- compute.transitions.alluvial.plot(pid,12,input.dir)
  q13 <- compute.transitions.alluvial.plot(pid,13,input.dir)
  q14 <- compute.transitions.alluvial.plot(pid,14,input.dir)
  q15 <- compute.transitions.alluvial.plot(pid,15,input.dir)

  q16 <- compute.transitions.alluvial.plot(pid,16,input.dir)
  q17 <- compute.transitions.alluvial.plot(pid,17,input.dir)
  q18 <- compute.transitions.alluvial.plot(pid,18,input.dir)
  q19 <- compute.transitions.alluvial.plot(pid,19,input.dir)
  q20 <- compute.transitions.alluvial.plot(pid,20,input.dir)

  q21 <- compute.transitions.alluvial.plot(pid,21,input.dir)
  q22 <- compute.transitions.alluvial.plot(pid,22,input.dir)
  q23 <- compute.transitions.alluvial.plot(pid,23,input.dir)
  q24 <- compute.transitions.alluvial.plot(pid,24,input.dir)

  list.plots <- list(q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,q11,q12,
                     q13,q14,q15,q16,q17,q18,q19,q20,q21,q22,q23,q24)
  
  return(list.plots)
  
} # of compute.alluvial.plots.participants



########################################################################################
########################################################################################

# Test of creating a plot to return it
plot.p19.q24 <- compute.transitions.alluvial.plot(19,24,"../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/")

plot.p5.q12 <- compute.transitions.alluvial.plot(5,12,"../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/")


p5 <- compute.alluvial.plots.participant(5,"../../Experiment-Data/Eye-tracking-data-samples/Transitions-Data/")


library(cowplot)
prow <- plot_grid(p5[[1]], p5[[2]], p5[[3]], p5[[4]], 
                  p5[[5]], p5[[6]], p5[[7]], p5[[8]],
                  p5[[9]], p5[[10]], p5[[11]], p5[[12]], 
                  p5[[13]], p5[[14]], p5[[15]], p5[[16]],
                  p5[[17]], p5[[18]], p5[[19]], p5[[20]], 
                  p5[[21]], p5[[22]], p5[[23]], p5[[24]],
                  align = 'vh',
                  hjust = -1, legend_b,
                  nrow = 6) #2, ncol = 2)


# Displays the 24 plots of participant 5. Correct Picture!!!
prow <- plot_grid(plotlist=p5,
                  align = 'vh',
                  hjust = -1, 
                  nrow = 6, ncol = 4)
legend_b <- get_legend(ctc.plot + theme(legend.position="bottom"))
transition.plot <- plot_grid( prow, legend_b, ncol = 1, rel_heights = c(1, .2))
transition.plot




legend_b <- get_legend(p5[[1]] + theme(legend.position="bottom"))
# transition.plot <- plot_grid( prow, legend_b, ncol = 4, rel_heights = c(1, .2))
transition.plot



prow <- plot_grid(ctc.plot + theme(legend.position="none"),
                  noctc.plot + theme(legend.position="none"),
                  align = 'vh',
                  # labels = c("CTC", "no CTC"),
                  hjust = -1,
                  nrow = 1)

legend_b <- get_legend(ctc.plot + theme(legend.position="bottom"))
fixation.plot <- plot_grid( prow, legend_b, ncol = 1, rel_heights = c(1, .2))
fixation.plot

########################################################################################
########################################################################################
