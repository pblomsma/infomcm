#Testing
#Question 2
csv.data1 <- read.csv("C:\\Users\\adria\\OneDrive\\Documents\\tableOfDriftValuesCalibration (1).csv", sep= ",")

#subset
subset1518<- csv.data1[csv.data1$trialTime>15000,]
subset1518<- subset1518[subset1518$trialTime<18000,]
subset1518_nabs<- subset1518[subset1518$trialTime<18000,]
subset1518$posX<-abs(subset1518$posX)
#plot position x
plot(subset1518$posX~subset1518$trialTime,col = 1:20, type = "l")
subset1518$trial <- as.factor(subset1518$trial)
library(ggplot2)
ggplot(data=subset1518, aes(x=trialTime,y=posX))+geom_line(aes(colour=trial))+labs(x="Trial Time(ms)", y = "Lateral Position(m)", title = "Empirical Human data Data based on Original Model")
#Question 2 B
simdata <- data.frame(trial = integer(),timetrial=integer(),xpos=double())
for (trial in 1:20) { 
  
  distrution<- rnorm(61 , 0 , 0.13 )
  sum <- 0
  for (x in 1:61) 
  {
    sum <-  sum + distrution[x]
    simdata [nrow(simdata)+1,] = list(trial,(x-1)*50,sum)
  }
}
simdata$trial<-as.factor(simdata$trial)
ggplot(data=simdata, aes(x=timetrial,y=xpos)) +geom_line(aes(colour=trial))+labs(x = "Trial Time(ms)", y = "Lateral Position(m)", title = "Simulated Data based on Original Model")
# Question C
hist(simdata$xpos,col = "grey", main = "Histogram of car position in simulated data", xlab = "Car Position", ylab = "Frequency")       
hist(subset1518_nabs$posX, col = "grey",main = "Histogram of car position in human trials",  xlab = "Car Position", ylab = "Frequency")
# Question D
sd(simdata$xpos)
sd(subset1518_nabs$posX)
# Question E
