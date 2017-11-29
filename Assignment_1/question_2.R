#Testing
a <- rnorm(10000, 10.0, 0.13)
hist(a, col="grey")

#Reading the data
TableDriftValuesCalibration = read.csv("data\\tableOfDriftValuesCalibration.csv", sep=",")
