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

  priors <- data.frame(term = character(), probability = double()) 
  
  for (i in seq(1, length(data.train[,1]), by=2)) 
  {
    current_term <- data.train[i,1]
    frequency_0 <- data.train[i,2]
    frequency_1 <- data.train[i+1,2]

    probability <- (frequency_0)/(frequency_0 + frequency_1)
  
    
    priors 
      
        print(paste("term ", current_term))
    print(paste("Probability ", probability))

  }
  
  data.train
  
  tokenize_words("mY name \n is :-) MICHAEL Caine")
  
  
  
}
