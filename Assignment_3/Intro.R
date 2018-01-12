#Read in data
COG_DATA = read.table("C:\\Projects\\COG\\git\\Assignment_3\\Input\\RSA lab assignment\\NeuralResponses", quote="\"", comment.char = "") 

noise1 <- rnorm(nrow(COG_DATA) * ncol(COG_DATA) * 12, mean = 0, sd = 1)
noise2 <- rnorm(nrow(COG_DATA) * ncol(COG_DATA) * 12, mean = 0, sd = 1)
noise3 <- rnorm(nrow(COG_DATA) * ncol(COG_DATA) * 12, mean = 0, sd = 1)
noise4 <- rnorm(nrow(COG_DATA) * ncol(COG_DATA) * 12, mean = 0, sd = 1)
noise5 <- rnorm(nrow(COG_DATA) * ncol(COG_DATA) * 12, mean = 0, sd = 1)
noise6 <- rnorm(nrow(COG_DATA) * ncol(COG_DATA) * 12, mean = 0, sd = 1)
noise7 <- rnorm(nrow(COG_DATA) * ncol(COG_DATA) * 12, mean = 0, sd = 1)
noise8 <- rnorm(nrow(COG_DATA) * ncol(COG_DATA) * 12, mean = 0, sd = 1)
noise9 <- rnorm(nrow(COG_DATA) * ncol(COG_DATA) * 12, mean = 0, sd = 1)
noise10 <- rnorm(nrow(COG_DATA) * ncol(COG_DATA) * 12, mean = 0, sd = 1)
noise11 <- rnorm(nrow(COG_DATA) * ncol(COG_DATA) * 12, mean = 0, sd = 1)
noise12 <- rnorm(nrow(COG_DATA) * ncol(COG_DATA) * 12, mean = 0, sd = 1)

subject1 <- COG_DATA + noise1
subject2 <- COG_DATA + noise2
subject3 <- COG_DATA + noise3
subject4 <- COG_DATA + noise4
subject5 <- COG_DATA + noise5
subject6 <- COG_DATA + noise6
subject7 <- COG_DATA + noise7
subject8 <- COG_DATA + noise8
subject9 <- COG_DATA + noise9
subject10 <- COG_DATA + noise10
subject11 <- COG_DATA + noise11
subject12 <- COG_DATA + noise12

