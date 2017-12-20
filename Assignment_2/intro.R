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

#Main code:
model <- trainModel(data.train)

trainModel <- function(traindata)  
{
  #P(w_i|c = 1) = (freq w_i|c = 1) / total freq w_i

  traindata["total_freq"] <- NA
  for (i in seq(1,nrow(traindata),2)) 
  {
    total_freq <- traindata[i,2] + traindata[i+1,2]

    traindata$total_freq[i] <- total_freq
    traindata$total_freq[i+1] <- total_freq
  }
  traindata$prob <- traindata$freq / traindata$total_freq
  
  return(traindata)
}

  
  multi <- 1
  for (i in 1:length(priors$V4)) {
    multi <- multi * priors$V4[i]
    print(multi)
  }  
  print(multi)
  
}
