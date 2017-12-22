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
  
  #Remove not
  for(i in 1:length(text))
  {
    if(text[i] == "not")
    {
      text[i] = ""
      text[i+1] = ""
    }
  }
  
  text <- intersect(text, sentiment.adjective.words)
  text <- intersect(text, model$term)
  text <- setdiff(text, common.words)
  return(text)
}

#FUNCTION: classify for tak 1-4
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

#FUNCTION: classify for task 5
classifier5 <- function(text, model)
{
  return(classifier(unique(text), model))
}    


#MAIN CODE
library(tokenizers)

#Load datasets
data.test <- read.csv("http://ricknouwen.org/movie.testing.frame", sep=",", colClasses=c("character","character","integer"),header=TRUE)
data.train <- read.csv("http://ricknouwen.org/moviereview.training.frame", sep=",", header=TRUE)
data.task6.test <- read.csv("C:\\Projects\\COG\\git\\Assignment_2\\amazon_test_set.csv", sep=";")
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
  data.test[i,]$classifier_result_T5 <- classifier5(preprocess5(current_text, model),model)
}

#Testing model with other data (task 6)
data.task6.test["classifier_result_T6"] <- NA

for(i in 1:nrow(data.task6.test))
{
  current_text <- data.task6.test[i,]$txt
  data.test[i,]$classifier_result_T6 <- classifier(preprocess5(current_text, model),model)
}




#Creating statistics:


#Get test results.



#Step 5: count false and true positive
#data frame with number of tp,fp, fn,tn for the classes 1 and 0 in that order.  

#tp_1 <-  #evaluationnumbers[1]
#fp_1 <-  #evaluationnumbers[2]
#fn_1 <-  #evaluationnumbers[3]
#tn_1 <-  #evaluationnumbers[4]
#tp_0 <-  #evaluationnumbers[5]
#fp_0 <-  #evaluationnumbers[6]
#fn_0 <-  #evaluationnumbers[7]
#tn_0 <-  #evaluationnumbers[8]
#count false and true positive

test <- data.frame(c(1,0,1),c(1,0,0),c(0,0,0))

#returns vector c(Accuracy,Presion,Recall,F1) depends on the classification that you want and
# the number of clasificartion
statistics <- function(column1,column2,class){
  evaluationnumbers <- c(0,0,0,0,0,0,0,0)
  for (i in 1:length(column1)) {
    if(column1[i]==1){
      if(column1[i]==column2[i])
      {
        evaluationnumbers[1] <- evaluationnumbers[1] + 1
        evaluationnumbers[8] <- evaluationnumbers[8] + 1
      }else{
        evaluationnumbers[3] <- evaluationnumbers[3] + 1
        evaluationnumbers[6] <- evaluationnumbers[6] + 1 
      }
    }else if(column1[i]==0){
      if(column1[i]==column2[i])
      {
        evaluationnumbers[5] <- evaluationnumbers[5] + 1
        evaluationnumbers[4] <- evaluationnumbers[4] + 1
      }else{
        evaluationnumbers[2] <- evaluationnumbers[2] + 1
        evaluationnumbers[7] <- evaluationnumbers[7] + 1 
      }
    }
  }
  eval <- c(0,0,0,0)
  if(class == 1){
    eval[1] <- (evaluationnumbers[1]+evaluationnumbers[4])/length(column1)
    eval[2] <-  evaluationnumbers[1]/(evaluationnumbers[1]+evaluationnumbers[2])
    eval[3] <-  evaluationnumbers[1]/(evaluationnumbers[1]+evaluationnumbers[3])
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
    eval[1] <- (evaluationnumbers[5]+evaluationnumbers[8])/length(column1)
    eval[2] <-  evaluationnumbers[5]/(evaluationnumbers[5]+evaluationnumbers[6])
    eval[3] <-  evaluationnumbers[5]/(evaluationnumbers[5]+evaluationnumbers[7])
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
  print(evaluationnumbers)
  print(eval)
  return(eval)
}

print("Results for Task 2")
statistics(data.test$sent,data.test$classifier_result_T2,0)
statistics(data.test$sent,data.test$classifier_result_T2,1)

print("Results for Task 3")
statistics(data.test$sent,data.test$classifier_result_T3,0)
statistics(data.test$sent,data.test$classifier_result_T3,1)

print("Results for Task 4")
statistics(data.test$sent,data.test$classifier_result_T4,0)
statistics(data.test$sent,data.test$classifier_result_T4,1)

print("Results for Task 5")
statistics(data.test$sent,data.test$classifier_result_T5,0)
statistics(data.test$sent,data.test$classifier_result_T5,1)

print("Results for Task 6")
statistics(data.task6.test$sent,data.task6.test$classifier_result_T6,0)
statistics(data.task6.test$sent,data.task6.test$classifier_result_T6,1)

