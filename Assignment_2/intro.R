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


#Main code:
model <- trainModel(data.train)

#Loop over the test texts
data.test <- read.csv("http://ricknouwen.org/movie.testing.frame",
                      sep=",",
                      colClasses=c("character","character","integer"),
                      header=TRUE)

#Word lists:
common.words <- scan("http://ricknouwen.org/stopwords.txt",sep="\n", what="")
sentiment.words <- scan("http://ricknouwen.org/sentimentwords.txt", sep="\n", what="")
adjective.words <-    scan("http://ricknouwen.org/adjectives.txt", sep="\n", what="")


data.test["classifier_result"] <- NA

for(i in 1:nrow(data.test))
{
  current_text <- data.test[i,]$txt
  current_text <- preprocess(current_text, model)
  current_probs <-  retrieveprobabilties(current_text, model)
  data.test[i,]$classifier_result <- classifier(current_probs)
}



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

#Step 3: Retrieve probabilities
retrieveprobabilties <- function(text, model)  
{
  probabilities <- model[-c(1:nrow(model)), ]
  for(i in 1:length(text))
  {
    probabilities <- rbind(probabilities, model[model$term == text[i],])
  }
  return(probabilities)
}

#Step 4: Classify
classifier <- function(text.model){
  clas <- integer()
  if (is.null(text.model))
  {return(sample(0:1,1))}
  else{
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
    clas <- 0}
  return(clas)
}

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
