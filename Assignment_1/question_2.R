#Testing
#Question 2
csv.data1 <- read.csv("C:\\Users\\adria\\OneDrive\\Documents\\tableOfDriftValuesCalibration (1).csv", sep= ",")

#subset
subset1518<- csv.data1[csv.data1$trialTime>=15000,]
subset1518<- subset1518[subset1518$trialTime<=18000,]
subset1518_nabs<- subset1518[subset1518$trialTime<=18000,]
subset1518$posX<-abs(subset1518$posX)
#plot position x
plot(subset1518$posX~subset1518$trialTime,col = 1:20, type = "l")
subset1518$trial <- as.factor(subset1518$trial)
library(ggplot2)
ggplot(data=subset1518, aes(x=trialTime,y=posX))+geom_line(aes(colour=trial))+labs(x="Trial Time(ms)", y = "Lateral Position(m)", title = "Lateral position over Time based on Empirical Human Data")
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
ggplot(data=simdata, aes(x=timetrial,y=xpos)) +geom_line(aes(colour=trial))+labs(x = "Trial Time(ms)", y = "Lateral Position(m)", title = "Lateral position over Time Based on Simulated Data")
# Question C
hist(simdata$xpos,col = "grey", main = "Histogram of car position in simulated data", xlab = "Car Position", ylab = "Frequency")       
hist(subset1518_nabs$posX, col = "grey",main = "Histogram of car position in empirical human data", xlab = "Car Position", ylab = "Frequency")
# Question D
sd(simdata$xpos)
sd(subset1518_nabs$posX)

# Question E

sd_results <- data.frame(sd.input = double(), sd.output = double())
for (sd in seq(0.05, 0.07, 0.0001))
{
  simdata <- data.frame(trial = integer(),timetrial=integer(),xpos=double())
  for (trial in 1:50) { 
    
    distrution<- rnorm(61 , 0 , sd)
    sum <- 0
    for (x in 1:61) 
    {
      sum <-  sum + distrution[x]
      simdata [nrow(simdata)+1,] = list(trial,(x-1)*50,sum)
    }
  }
  sd_results [nrow(sd_results)+1,] = list(sd,sd(simdata$xpos))
}










simdata2 <- data.frame(trial = integer(),timetrial=integer(),xpos=double())
for (trial in 1:20) { 
  
  distrution<- rnorm(61 , 0 , 0.062 )
  sum <- 0
  for (x in 1:61) 
  {
    sum <-  sum + distrution[x]
    simdata2 [nrow(simdata2)+1,] = list(trial,(x-1)*50,sum)
  }
}
simdata2$trial<-as.factor(simdata2$trial)
ggplot(data=simdata2, aes(x=timetrial,y=xpos)) +geom_line(aes(colour=trial))+labs(x = "Trial Time(ms)", y = "Lateral Position(m)", title = "Lateral position over Time Based on Simulated Data")
hist(simdata$xpos,col = "grey", main = "Histogram of car position in simulated data", xlab = "Car Position", ylab = "Frequency")
sd(simdata2$xpos)
# sd for simdata2  0.4051379

