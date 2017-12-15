str(data.train)

data.test <- read.csv("http://ricknouwen.org/movie.testing.frame",
                      sep=",",
                      colClasses=c("character","character","integer"),
                      header=TRUE)
write.dcf(as.character(data.test[191,]$txt))

data.train <- read.csv("http://ricknouwen.org/moviereview.training.frame", sep=",", header=TRUE)
data.train$freq[data.train$freq==0] <- 0.001

### install.packages{"tokenizers"}
library(tokenizers)
tokenize_words("mY name \n is :-) MICHAEL Caine")



#Task 2
# Build a function in R that classifies strings (text)
classifyText <- function(text, model)  
{
  
  
 
  
}

trainModel <- function(traindata)  
{
  library(tokenizers)
  install.packages("tokenizers")

  
  sum_0 <- sum(data.train[ which(data.train$sentiment==0), 2] )
  sum_1 <- sum(data.train[ which(data.train$sentiment==1), 2] )
  
  for (i in 1:nrow(data.train)) 
  {
    if(i %% 2 == 0) #sentiment 0
    {
      data.train[i,4] <- data.train[i,2] / sum_0 
    }
    else # sentiment 1
    {
      data.train[i,4] <- data.train[i,2] / sum_1
    }
  }
  priors <- data.train[ which(data.train$sentiment==1), ]   
  
  
  
  
}
