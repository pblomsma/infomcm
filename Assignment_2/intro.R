str(data.train)

data.test <- read.csv("http://ricknouwen.org/movie.testing.frame",
                      sep=",",
                      colClasses=c("character","character","integer"),
                      header=TRUE)
write.dcf(as.character(data.test[191,]$txt))

data.train <- read.csv("http://ricknouwen.org/moviereview.training.frame", sep=",", header=TRUE)
data.train$freq[data.train$freq==0] <- 0.001
knownwords <- function (textnumber) {intersect(unlist(tokenize_words(data.test[textnumber,]$txt)),data.train$term)}
knownwords(23)
common.words <- scan("http://ricknouwen.org/stopwords.txt",
                     sep="\n", what="")
sentiment.words <-
  scan("http://ricknouwen.org/sentimentwords.txt",
       sep="\n", what="")
adjective.words <-
  scan("http://ricknouwen.org/adjectives.txt",
       sep="\n", what="")

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


#Step 1: Create the word probabilities
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

#Step 2: Preprocess the text
preprocess <- function(text, model)  
{
  library(tokenizers)
  
  #Download the lists.
  common.words <- scan("http://ricknouwen.org/stopwords.txt",sep="\n", what="")
  sentiment.words <- scan("http://ricknouwen.org/sentimentwords.txt", sep="\n", what="")
  adjective.words <-    scan("http://ricknouwen.org/adjectives.txt", sep="\n", what="")
  
  #tmp
  text <- data.test[23,]$txt
  
  #convert text to vector
  text <- unlist(tokenize_words(text))
                 
  #only known words.
  unknownwords <-  setdiff(text, model$term)
  text <- intersect(text ,model$term)
  
  #Substract the other lists from the text
  text <- setdiff(text,common.words)
  text <- setdiff(text,adjective.words)
  
  #only sentimental words.
  text <- intersect(text, sentiment.words)
  
  return(text)
}

classifier <- function(text.model){
  clas <- integer()
  pos_words <- text.model[text.model$sentiment == 1,]
  neg_words <- text.model[text.model$sentiment == 0,]
  posmulti <- 1
  negmulti <- 1
  prob_class <- 0.5
  #positive sentiment
  for (i in 1:length(pos_words$prob)) {
    posmulti <- posmulti * pos_words$prob[i]
  }  
  claspos <- prob_class*posmulti
  #negative sentiment
  for (i in 1:length(neg_words$prob)) {
    negmulti <- negmulti * neg_words$prob[i]
  }  
  clasneg <- prob_class*negmulti
  
  if(clasneg<claspos){
    clas <- 1
  } else 
    clas <- 0
  return(clas)
}    

classifier(model)
