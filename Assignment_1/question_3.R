COG_DATA = read.csv("data\\keyPressDataWithLaneDeviation.csv", sep=",")
no_typing_errors <- COG_DATA[COG_DATA$typingErrorMadeOnTrial == 0,]

#Calculate for each participant what the average interkeypressinterval is between two digits in 
#the single -  task trial data that was collected at the end of the experiment (partOfExperiment == "
#singleDialing2").


singleTaskKeyPressTimes

#calculate what on average (across participants), the average interkeypress interval is 
#for the average participant.


