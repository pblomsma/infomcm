COG_DATA = read.csv("C:\\Users\\adria\\OneDrive\\Documents\\keyPressDataWithLaneDeviation.csv", sep=",")
no_typing_errors <- COG_DATA[COG_DATA$typingErrorMadeOnTrial == 0,]
# Part A
singleDialing2 <- no_typing_errors[no_typing_errors$partOfExperiment == "singleDialing2",]
singleDialing2_11 <- singleDialing2[singleDialing2$phoneNrLengthBeforeKeyPress== 11,]
singleDialing2_11$meaninterval <- singleDialing2_11$timeRelativeToTrialStart/12
mean(singleDialing2_11$meaninterval)
# Part B

#Calculate for each participant what the average interkeypressinterval is between two digits in 
#the single -  task trial data that was collected at the end of the experiment (partOfExperiment == "
#singleDialing2").


singleTaskKeyPressTimes

#calculate what on average (across participants), the average interkeypress interval is 
#for the average participant.


