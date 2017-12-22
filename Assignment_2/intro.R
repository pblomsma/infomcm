#FUNCTION: trainModel - Step 1: Create the word probabilities
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

#FUNCTION: preprocess functions are different per task:

#TASK 2: "Make sure that the classifier only looks at words in the stopwords.txt file."
preprocess2 <- function(text, model)  
{
  library(tokenizers)

  #convert text to vector
  text <- unlist(tokenize_words(text))
  text <- intersect(text, model$term)
  text <- intersect(text, common.words)
  return(text)
}

#TASK 3: "Change the classifier in 2. so that it instead only looks at adjectives. Report on all the evaluation scores once more."
preprocess3 <- function(text, model)  
{
  library(tokenizers)
  
  #convert text to vector
  text <- unlist(tokenize_words(text))
  text <- intersect(text, model$term)
  text <- intersect(text, adjective.words)
  return(text)
}

#TASK 4: " Change the classifier once more so that it only looks at words in the sententwords.txt file."
preprocess4 <- function(text, model)  
{
  library(tokenizers)
  
  #convert text to vector
  text <- unlist(tokenize_words(text))
  text <- intersect(text, model$term)
  text <- intersect(text, sentiment.words)
  return(text)
}

#TASK 5: "Optimize the Bayes Classifier"
preprocess5 <- function(text, model)  
{
  library(tokenizers)
  
  #convert text to vector
  text <- unlist(tokenize_words(text))
  
  text <- intersect(text, sentiment.adjective.words)
  text <- intersect(text, model$term)
  text <- setdiff(text, common.words)
  return(text)
}

#FUNCTION: classify
classifier <- function(text, model)
{
  #Get all probabilties per word.
  probabilities <- model[-c(1:nrow(model)), ]
  for(i in 1:length(text))
  {
    probabilities <- rbind(probabilities, model[model$term == text[i],])
  }
  
  if (is.null(probabilities))
  {
    return(sample(0:1,1))
  }
  
  negmulti <- prod(probabilities[probabilities$sentiment == 0,]$prob)
  posmulti <- prod(probabilities[probabilities$sentiment == 1,]$prob)
  
  if(is.na(negmulti))
  {
    negmulti <- 0
  }
  
  if(is.na(posmulti))
  {
    posmulti <- 0
  }
  
  if(negmulti<posmulti){
    return(1)
  }
  return(0)
}    

#MAIN CODE
library(tokenizers)

#Load datasets
data.test <- read.csv("http://ricknouwen.org/movie.testing.frame", sep=",", colClasses=c("character","character","integer"),header=TRUE)
data.train <- read.csv("http://ricknouwen.org/moviereview.training.frame", sep=",", header=TRUE)
common.words <- scan("http://ricknouwen.org/stopwords.txt",sep="\n", what="")
sentiment.words <-  scan("http://ricknouwen.org/sentimentwords.txt", sep="\n", what="")
adjective.words <-  scan("http://ricknouwen.org/adjectives.txt", sep="\n", what="")
sentiment.adjective.words <- union(sentiment.words, adjective.words)

#Fix freq=0 problem
data.train$freq[data.train$freq==0] <- 0.001


#Training the model
model <- trainModel(data.train)

#Testing the model
data.test["classifier_result_T2"] <- NA
data.test["classifier_result_T3"] <- NA
data.test["classifier_result_T4"] <- NA
data.test["classifier_result_T5"] <- NA


for(i in 1:nrow(data.test))
{
  current_text <- data.test[i,]$txt
  data.test[i,]$classifier_result_T2 <- classifier(preprocess2(current_text, model),model)
  data.test[i,]$classifier_result_T3 <- classifier(preprocess3(current_text, model),model)
  data.test[i,]$classifier_result_T4 <- classifier(preprocess4(current_text, model),model)
  data.test[i,]$classifier_result_T5 <- classifier(preprocess4(current_text, model),model)
}

#Creating statistics:


#Get test results.



#Step 5: count false and true positive
#data frame with number of tp,fp, fn,tn for the classes 1 and 0 in that order.  
numberof <- data.frame(int.sent = c(0,0,0,0,0,0,0,0),int.comon = c(0,0,0,0,0,0,0,0),int.adj = c(0,0,0,0,0,0,0,0))

#tp_1 <-  #numberof$int.sent[1]
#fp_1 <-  #numberof$int.sent[2]
#fn_1 <-  #numberof$int.sent[3]
#tn_1 <-  #numberof$int.sent[4]
#tp_0 <-  #numberof$int.sent[5]
#fp_0 <-  #numberof$int.sent[6]
#fn_0 <-  #numberof$int.sent[7]
#tn_0 <-  #numberof$int.sent[8]
#count false and true positive
for (i in 1:length(data.test$sent)) {
  if(data.test$sent[i]==1){
    if(data.test$sent[i]==data.test$classifier_result[i])
    {
      numberof$int.sent[1] <- numberof$int.sent[1] + 1
      numberof$int.sent[8] <- numberof$int.sent[8] + 1
    }else{
      numberof$int.sent[3] <- numberof$int.sent[3] + 1
      numberof$int.sent[6] <- numberof$int.sent[6] + 1 
    }
  }else{
    if(data.test$sent[i]==data.test$classifier_result[i])
    {
      numberof$int.sent[5] <- numberof$int.sent[5] + 1
      numberof$int.sent[4] <- numberof$int.sent[4] + 1
    }else{
      numberof$int.sent[2] <- numberof$int.sent[2] + 1
      numberof$int.sent[7] <- numberof$int.sent[7] + 1 
    }
  }
}
#returns vector c(Accuracy,Presion,Recall,F1) depends on the classification that you want and
# the number of clasificartion
evaluation_classifier <- function(class,numclass){
  eval <- c(0,0,0,0)
  if(class == 1){
    eval[1] <- (numberof$int.sent[1]+numberof$int.sent[4])/numclass
    eval[2] <-  numberof$int.sent[1]/(numberof$int.sent[1]+numberof$int.sent[2])
    eval[3] <-  numberof$int.sent[1]/(numberof$int.sent[1]+numberof$int.sent[3])
    eval[4] <-  eval[2]*eval[3]/(eval[2]+eval[3])
    if(eval[2]=="NaN"){
      eval[2] <- 0
    }
    if(eval[3]=="NaN"){
      eval[3] <- 0
    }
    if(eval[4]=="NaN"){
      eval[4] <- 1
    }
  }else if (class == 0){
    eval[1] <- (numberof$int.sent[5]+numberof$int.sent[8])/numclass
    eval[2] <-  numberof$int.sent[5]/(numberof$int.sent[5]+numberof$int.sent[6])
    eval[3] <-  numberof$int.sent[5]/(numberof$int.sent[5]+numberof$int.sent[7])
    eval[4] <- eval[2]*eval[3]/(eval[2]+eval[3])
    if(eval[2]=="NaN"){
      eval[2] <- 0
    }
    if(eval[3]=="NaN"){
      eval[3] <- 0
    }
    if(eval[4]=="NaN"){
      eval[4] <- 1
    }
  }else
    print("Not valid")
  return(eval)
}
evaluation_sent1 <- evaluation_classifier(1,length(data.test$sent))
evaluation_sent0 <- evaluation_classifier(0,length(data.test$sent))
