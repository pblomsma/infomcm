COG_DATA = read.csv("C:\\Projects\\COG\\git\\Assignment_1\\datakeyPressDataWithLaneDeviation.csv", sep=",")

#question 1 A
#Create subsets
no_typing_errors <- COG_DATA[COG_DATA$typingErrorMadeOnTrial == 0,]
correctSet <- no_typing_errors[no_typing_errors$Event1 == "Correct",]
dualSteerFocusSet <- correctSet[correctSet$partOfExperiment == "dualSteerFocus",]
dualDialFocus <- correctSet[correctSet$partOfExperiment == "dualDialFocus",]

#means per participant
dualSteerFocusSet_means <- aggregate(dualSteerFocusSet, by=list(dualSteerFocusSet$pp), FUN=mean)
dualDialFocus_means <- aggregate(dualDialFocus, by=list(dualDialFocus$pp), FUN=mean)

#Means dualSteerFocus
mean(dualSteerFocusSet_means$timeRelativeToTrialStart)
sd(dualSteerFocusSet_means$timeRelativeToTrialStart)
sd(dualSteerFocusSet_means$timeRelativeToTrialStart)/sqrt(length(dualSteerFocusSet_means$timeRelativeToTrialStart))

#Means dualDialFocus
mean(dualDialFocus_means$timeRelativeToTrialStart)
sd(dualDialFocus_means$timeRelativeToTrialStart)
sd(dualDialFocus_means$timeRelativeToTrialStart)/sqrt(length(dualDialFocus_means$timeRelativeToTrialStart))

#question 1 B
#create subsets without the 'correct filter'.
dualSteerFocusSet <- no_typing_errors[no_typing_errors$partOfExperiment == "dualSteerFocus",]
dualDialFocus <- no_typing_errors[no_typing_errors$partOfExperiment == "dualDialFocus",]

dualSteerFocusSet$lanePosition <- abs(dualSteerFocusSet$lanePosition)
dualDialFocus$lanePosition <- abs(dualDialFocus$lanePosition)

#means per participant
dualSteerFocusSet_means <- aggregate(dualSteerFocusSet, by=list(dualSteerFocusSet$pp), FUN=mean)
dualDialFocus_means <- aggregate(dualDialFocus, by=list(dualDialFocus$pp), FUN=mean)

#Means dualSteerFocus
mean(dualSteerFocusSet_means$lanePosition)
sd(dualSteerFocusSet_means$lanePosition)
sd(dualSteerFocusSet_means$lanePosition)/sqrt(length(dualSteerFocusSet_means$lanePosition))

#Means dualDialFocus
mean(dualDialFocus_means$lanePosition)
sd(dualDialFocus_means$lanePosition)
sd(dualDialFocus_means$lanePosition)/sqrt(length(dualDialFocus_means$lanePosition))

#question 1 C

#all data with ABS
no_typing_errors_abs_lanedeviation <- no_typing_errors
no_typing_errors_abs_lanedeviation$lanePosition <- abs(no_typing_errors_abs_lanedeviation$lanePosition)

#Two subsets
dualSteerFocus_onego <- no_typing_errors_abs_lanedeviation[no_typing_errors$partOfExperiment == "dualSteerFocus",]
dualDialFocus_onego <- no_typing_errors_abs_lanedeviation[no_typing_errors$partOfExperiment == "dualDialFocus",]

#Calculate mean, sd and length per key dualSteerFocus.
dualSteerFocus_keymean <- aggregate(dualSteerFocus_onego,by=list(dualSteerFocus_onego$phoneNrLengthAfterKeyPress),mean)
dualSteerFocus_keymeansd <- aggregate(dualSteerFocus_onego,by=list(dualSteerFocus_onego$phoneNrLengthAfterKeyPress),sd)
dualSteerFocus_keymeanlength <- aggregate(dualSteerFocus_onego,by=list(dualSteerFocus_onego$phoneNrLengthAfterKeyPress),length)
dualSteerFocus_keymean$lanePosition.sd<- dualSteerFocus_keymeansd$lanePosition
dualSteerFocus_keymean$lanePosition.count<- dualSteerFocus_keymeanlength$lanePosition
dualSteerFocus_keymean$lanePosition.se<-dualSteerFocus_keymean$lanePosition.sd/sqrt(dualSteerFocus_keymean$lanePosition.count)

#Calculate mean, sd and length per key dualDialFocus
dualDialFocus_keymean <- aggregate(dualDialFocus_onego,by=list(dualDialFocus_onego$phoneNrLengthAfterKeyPress),mean)
dualDialFocus_keymeansd <- aggregate(dualDialFocus_onego,by=list(dualDialFocus_onego$phoneNrLengthAfterKeyPress),sd)
dualDialFocus_keymeanlength <- aggregate(dualDialFocus_onego,by=list(dualDialFocus_onego$phoneNrLengthAfterKeyPress),length)
dualDialFocus_keymean$lanePosition.sd<- dualDialFocus_keymeansd$lanePosition
dualDialFocus_keymean$lanePosition.count<- dualDialFocus_keymeanlength$lanePosition
dualDialFocus_keymean$lanePosition.se<-dualDialFocus_keymean$lanePosition.sd/sqrt(dualDialFocus_keymean$lanePosition.count)

#Abbreviate for readability
sf <- dualSteerFocus_keymean
df <- dualDialFocus_keymean

#Calculate range
g_range <- range(0, sf$lanePosition - sf$lanePosition.se,sf$lanePosition + sf$lanePosition.se, df$lanePosition - df$lanePosition.se,df$lanePosition + df$lanePosition.se )

#Plot dualSteerFocus line
plot(sf$phoneNrLengthAfterKeyPress, sf$lanePosition, type = "o", pch=21, ylim = g_range, col = "blue", xlab="Keypress count", ylab="Lateral Deviation (m)", main ="Lateral Deviation per keypress")
arrows(sf$phoneNrLengthAfterKeyPress,sf$lanePosition - sf$lanePosition.se,sf$phoneNrLengthAfterKeyPress,sf$lanePosition + sf$lanePosition.se, angle=90,code=3, col = "blue", length = 0.05)

#Plot dualDialFocus line
lines(df$phoneNrLengthAfterKeyPress, df$lanePosition, type="o", pch=22, lty=2, col="red")
arrows(df$phoneNrLengthAfterKeyPress,df$lanePosition - df$lanePosition.se,df$phoneNrLengthAfterKeyPress,df$lanePosition + df$lanePosition.se, angle=90, code=3, col = "red", length = 0.05)

# Create labels and legend
legend(1, g_range[2], c("Dialing focus","Steering focus"), cex=0.8, 
       col=c("blue","red"), pch=21:22, lty=1:2)
