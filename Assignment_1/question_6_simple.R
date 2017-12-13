
#Create 2*2*2 different models:

#A. Calibrating the drift function to this data set VERSUS leaving the drift function at the old value 
#of Janssen & Brumby (2010) (2 options) (NB: check the text on "engineering" after question 2 and report
#whether you only changed 1 SD value or 2 SD values)

#Model A1: Driving modelA1 with gaussDeviateSD <- 0.13 (DEFAULT)
#Model A2: Driving modelA2 with gaussDeviateSD <- 0.062

#B. Calibrating the participants IKI to the single - task values VERSUS leaving them at  the default function of Janssen & Brumby (2010) (2 options)
#Model A1B1: Driving modelA1 with gaussDeviateSD <- 0.13 (DEFAULT), singleTaskKeyPressTimes <- c(400,400,400,400,400,400,400,400,400,400,400) (DEFAULT)
#Model A1B2 Driving modelA1 with gaussDeviateSD <- 0.13 (DEFAULT), singleTaskKeyPressTimes <- c(250,250,250,250,250,250,250,250,250,250,250)

#Model A2B1: Driving modelA1 with gaussDeviateSD <- 0.062, singleTaskKeyPressTimes <- c(400,400,400,400,400,400,400,400,400,400,400) (DEFAULT)
#Model A2B2 Driving modelA1 with gaussDeviateSD <- 0.062, singleTaskKeyPressTimes <- c(250,250,250,250,250,250,250,250,250,250,250)

#C. Modeling a low number of simulations (10 per run) VERSUS running a high number of simulations (e.g., 50 per run) (2 options)
#10 vs 50

directory <- 'C:\\Projects\\COG\\git\\Assignment_1\\'
load("data\\drivingModel_2017.R")

#Model A1B1:
gaussDeviateSD <- 0.13
singleTaskKeyPressTimes <- c(400,400,400,400,400,400,400,400,400,400,400)

png( paste(directory, 'Model_A1B1_10_runAllSimpleStrategies.png'), width = 1300, height = 800 )
runAllSimpleStrategies(10, "0123456891")
dev.off()

png( paste(directory, 'Model_A1B1_50_runAllSimpleStrategies.png'), width = 1300, height = 800 )
runAllSimpleStrategies(50, "0123456891")
dev.off()

#Model A1B2
gaussDeviateSD <- 0.13
singleTaskKeyPressTimes <- c(250,250,250,250,250,250,250,250,250,250,250)

png( paste(directory, 'Model_A1B2_10_runAllSimpleStrategies.png'), width = 1300, height = 800 )
runAllSimpleStrategies(10, "0123456891")
dev.off()

png( paste(directory, 'Model_A1B2_50_runAllSimpleStrategies.png'), width = 1300, height = 800 )
runAllSimpleStrategies(50, "0123456891")
dev.off()

#Model A2B1:
gaussDeviateSD <- 0.062
singleTaskKeyPressTimes <- c(400,400,400,400,400,400,400,400,400,400,400)

png( paste(directory, 'Model_A2B1_10_runAllSimpleStrategies.png'), width = 1300, height = 800 )
runAllSimpleStrategies(10, "0123456891")
dev.off()

png( paste(directory, 'Model_A2B1_50_runAllSimpleStrategies_redo.png'), width = 1300, height = 800 )
runAllSimpleStrategies(50, "0123456891",title)
dev.off()

#Model A2B2
gaussDeviateSD <- 0.062
singleTaskKeyPressTimes <- c(250,250,250,250,250,250,250,250,250,250,250)

png( paste(directory, 'Model_A2B2_10_runAllSimpleStrategies.png'), width = 1300, height = 800 )
runAllSimpleStrategies(10, "0123456891")
dev.off()

png( paste(directory, 'Model_A2B2_50_runAllSimpleStrategies.png'), width = 1300, height = 800 )
runAllSimpleStrategies(50, "0123456891")
dev.off()