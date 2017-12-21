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
  
  if(clasneg<claspos){
    clas <- 1
  } else 
    clas <- 0}
  return(clas)
}    
