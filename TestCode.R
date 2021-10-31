################

# EDITED UP TO HERE


# ggplot(data, aes(x = rx, y = ry,size = rr, color=rcolor))+
#  geom_point(alpha = 0.7)



# TO DO:
# 1) Obtain the quantile information from the summary

summary(ExpData$NoF)

# Use [[1]] for the numeric values of the elements in summary


#Using boxplot for two metrics and see data distribution

histogramNoC <- fmData %>%
  ggplot( aes(x=NoC)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  scale_y_continuous(trans='log10') +
  theme_ipsum() +
  theme(plot.title = element_text(size=15))
histogramNoC

#boxplot of NoF
quantile(ExpData$NoF, c(.25, .50, .75))
summary(ExpData$NoF)
var(ExpData$NoF)
sd(ExpData$NoF)

boxstyle <- fmData %>% 
  ggplot(aes(y=NoF)) +
  geom_boxplot() +
  theme_minimal() +
  labs(y="Number of Features (NoF)") +
  scale_y_continuous(limits=c(10,360))


boxstyle


####################################################
####################################################
## Other code examples not used for publications

# Boxplot for distribution of number of constraints 
boxplotNoC <- fmData %>% 
  ggplot(aes(y=NoC)) +
  geom_boxplot() +
  theme_minimal() +
  labs(y="Number of Constraints (NoC)") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_y_continuous(trans='ln2') +
  coord_flip() 
boxplotNoC


# Histogram  with log scale on the account
histogramNoC <- fmData %>%
  ggplot( aes(x=NoC)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  scale_y_continuous(trans='log10') +
  theme_ipsum() +
  theme(plot.title = element_text(size=15))
histogramNoC




# Example of bubble plots
x <- c(12,23,43,61,78,54,34,76,58,103,39,46,52,33,11)
y <- c(12,54,34,76,54,23,43,61,78,23,12,34,56,98,67)
r <- c(1,5,13,8,12,3,2,16,7,40,23,45,76,8,7)

color <- c(rep("color1", 1), rep("color2", 2),
           rep("Color3", 3), rep("color4", 4),
           rep("color5", 5))
# creating the dataframe from the above columns
data <- data.frame(x, y, r, color)

ggplot(data, aes(x = x, y = y,size = r, color=color))+
  geom_point(alpha = 0.7)


rx <- c("NoF1", "NoF2", "NoF3")
ry <- c("NoC1", "NoC2", "NoC3")
rr <- c(10,20,40)
rcolor <-c("color1", "color2", "color3")
data <- data.frame(rx, ry, rr, rcolor)

ggplot(data, aes(x = rx, y = ry,size = rr))+
  geom_point(alpha = 0.7)


# Test code for writing files
# Write.csv ignored not writting the column names, it ignore the command
write.csv(newConfigurationFrame,paste("configuration-",i,".csv", sep=""),  col.names = FALSE, row.names = FALSE)
write.table(newConfigurationFrame, paste("configuration-",i,".csv", sep=""), row.names=F, col.names=F, sep=",")

# Example of appending text to a file.
cat("First line \n", file=paste("configuration-",2,".csv", sep=""), append=TRUE)
cat("Second line", file=paste("configuration-",2,".csv", sep=""), append=TRUE)


