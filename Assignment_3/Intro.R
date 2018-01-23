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

#Now calculate the representational dissimilarity of each object pair, constructing a
#representational dissimilarity matrix (RDM). Take the responses of each object and
#calculate the dissimilarity from the responses to each other object.
RDM_orig <- 1 - cor(t(COG_DATA), t(COG_DATA))
RDM_1 <- 1 - cor(t(subject1), t(subject1))
RDM_2 <- 1 - cor(t(subject2), t(subject2))
RDM_3 <- 1 - cor(t(subject3), t(subject3))
RDM_4 <- 1 - cor(t(subject4), t(subject4))
RDM_5 <- 1 - cor(t(subject5), t(subject5))
RDM_6 <- 1 - cor(t(subject6), t(subject6))
RDM_7 <- 1 - cor(t(subject7), t(subject7))
RDM_8 <- 1 - cor(t(subject8), t(subject8))
RDM_9 <- 1 - cor(t(subject9), t(subject9))
RDM_10 <- 1 - cor(t(subject10), t(subject10))
RDM_11 <- 1 - cor(t(subject11), t(subject11))
RDM_12 <- 1 - cor(t(subject12), t(subject12))

#image plot function: http://www.phaget4.org/R/image_matrix.html
myImagePlot <- function(x, ...){
  min <- min(x)
  max <- max(x)
  yLabels <- rownames(x)
  xLabels <- colnames(x)
  title <-c()
  # check for additional function arguments
  if( length(list(...)) ){
    Lst <- list(...)
    if( !is.null(Lst$zlim) ){
      min <- Lst$zlim[1]
      max <- Lst$zlim[2]
    }
    if( !is.null(Lst$yLabels) ){
      yLabels <- c(Lst$yLabels)
    }
    if( !is.null(Lst$xLabels) ){
      xLabels <- c(Lst$xLabels)
    }
    if( !is.null(Lst$title) ){
      title <- Lst$title
    }
  }
  # check for null values
  if( is.null(xLabels) ){
    xLabels <- c(1:ncol(x))
  }
  if( is.null(yLabels) ){
    yLabels <- c(1:nrow(x))
  }
  
  layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(4,1), heights=c(1,1))
  
  # Red and green range from 0 to 1 while Blue ranges from 1 to 0
  ColorRamp <- rgb( seq(0,1,length=256),  # Red
                    seq(0,1,length=256),  # Green
                    seq(1,0,length=256))  # Blue
  ColorLevels <- seq(min, max, length=length(ColorRamp))
  
  # Reverse Y axis
  reverse <- nrow(x) : 1
  yLabels <- yLabels[reverse]
  x <- x[reverse,]
  
  # Data Map
  par(mar = c(3,5,2.5,2))
  image(1:length(xLabels), 1:length(yLabels), t(x), col=ColorRamp, xlab="",
        ylab="", axes=FALSE, zlim=c(min,max))
  if( !is.null(title) ){
    title(main=title)
  }
  axis(BELOW<-1, at=1:length(xLabels), labels=xLabels, cex.axis=0.7)
  axis(LEFT <-2, at=1:length(yLabels), labels=yLabels, las= HORIZONTAL<-1,
       cex.axis=0.7)
  
  # Color Scale
  par(mar = c(3,2.5,2.5,2))
  image(1, ColorLevels,
        matrix(data=ColorLevels, ncol=length(ColorLevels),nrow=1),
        col=ColorRamp,
        xlab="",ylab="",
        xaxt="n")
  
  layout(1)
}
#Distances
myImagePlot(RDM_1,  title=c("RDM of subject1"))
myImagePlot(RDM_orig,  title=c("RDM of original data"))

#Mean matrix
rdm_avg <- (RDM_1 + RDM_2 + RDM_3 + RDM_4 + 
              RDM_5 + RDM_6 + RDM_7 + RDM_8 + RDM_9 + RDM_10 + RDM_11 + RDM_12) / 12

myImagePlot(rdm_avg,  title=c("RDM of average data"))

# RDM animacy dissimilarity matrix predicted by our hypothesis 
RDM_animacy <-  matrix(nrow=92,ncol=92) 
for (i in 1:92) {
  for (j in 1:92) {
    if(CategoryVectors$V1[i]==CategoryVectors$V1[j]){
      RDM_animacy[i,j] <- 1
    }else{
      RDM_animacy[i,j] <- 0
    }
  }
}
myImagePlot(RDM_animacy,  title=c("RDM of animacy state"))
#RDM no face and face
RDM_face <-  matrix(nrow=92,ncol=92) 
for (i in 1:92) {
  for (j in 1:92) {
    if(CategoryVectors$V6[i]==CategoryVectors$V6[j]){
      RDM_face[i,j] <- 1
    }else{
      RDM_face[i,j] <- 0
    }
  }
}
myImagePlot(RDM_face,  title=c("RDM of animacy state"))


#FUNCTION: Hypothesis testing
hypothesistesting <- function(measured_matrix, hypothesis_matrix)  
{
  
  #Check
  if(ncol(measured_matrix) != ncol(hypothesis_matrix) )
  {
    print("Matrices have different size!")
  }
  
  #Check
  if(nrow(measured_matrix) != nrow(hypothesis_matrix) )
  {
    print("Matrices have different size!")
  }
  
  results_0 =list() 
  results_1 =list() 
  
  for(i in 1:nrow(measured_matrix))
  {
    for(j in 1:i)
    {
      if(j == i)
      {
        next
      }
      if(hypothesis_matrix[i,j]<1)
      {
        results_0 <- c(results_0, measured_matrix[i,j])
      }
      else
      {
        results_1 <- c(results_1, measured_matrix[i,j])
      }
    }
  }

  # calculate ssquared
  t.test(unlist(results_0),unlist(results_1),paired=FALSE)
}

#Question 4
hypothesistesting(rdm_avg,RDM_animacy)

#Question 5
hypothesistesting(RDM_orig,RDM_animacy)

#Question 6
hypothesistesting(RDM_orig,RDM_face)

#Question 7

#Approach1:
#FUNCTION: Hypothesis testing
hypothesistesting_2dim <- function(measured_matrix, hypothesis_matrix1, hypothesis_matrix2)  
{
  results_0 =list() 
  results_1 =list() 
  
  for(i in 1:nrow(measured_matrix))
  {
    for(j in 1:i)
    {
      if(j == i)
      {
        next
      }
      if(hypothesis_matrix1[i,j]>0)
      {
        if(hypothesis_matrix2[i,j]<1)
        {
          results_0 <- c(results_0, measured_matrix[i,j])
        }
        else
        {
          results_1 <- c(results_1, measured_matrix[i,j])
        }
      }
    }
  }
  
  # calculate ssquared
  t.test(unlist(results_0),unlist(results_1),paired=FALSE)
}

hypothesistesting_2dim(RDM_orig,RDM_animacy, RDM_face)

#Question 8
#RDM no face and face
RDM_human <-  matrix(nrow=92,ncol=92) 
for (i in 1:92) {
  for (j in 1:92) {
    if(CategoryVectors$V6[i]==CategoryVectors$V3[j]){
      RDM_human[i,j] <- 1
    }else{
      RDM_human[i,j] <- 0
    }
  }
}

hypothesistesting_2dim(RDM_orig,RDM_animacy, RDM_human)
#Question 9
# RDM animacy and faces 
RDM_animacyface <-  matrix(nrow=92,ncol=92) 
for (i in 1:92) {
  for (j in 1:92) {
    if(CategoryVectors$V1[i]==CategoryVectors$V1[j]&CategoryVectors$V1[i]==1&CategoryVectors$V6[i]==CategoryVectors$V6[j]){
      RDM_animacyface[i,j] <- 1
    }else{
      RDM_animacyface[i,j] <- 0
    }
  }
}
myImagePlot(RDM_animacyface,  title=c("RDM of animacy and face state"))

inputframe <- data.frame(diss = as.vector(RDM_orig), a = as.factor(as.vector(RDM_animacy)), af = as.factor(as.vector(RDM_animacyface)))
linearmodel <- glm(diss ~ a + af, data = inputframe)
anova(linearmodel)
summary(linearmodel)

#Question 11
#Question 11
NeuroRDM = read.table("C:\\Projects\\COG\\git\\Assignment_3\\Input\\RSA lab assignment\\NeuroRDM", quote="\"", comment.char = "") 
createvector <- function(data){
  datalower <-lower.tri(data,diag = FALSE)
  datavector <- c()
  for (i in 1:92) {
    for (j in 1:92) {
      if(datalower[i,j]){
        #datavector[j] <- data[i,j]
        datavector = c(datavector,data[i,j])
      }
    }
  }
  return(datavector)
}
rdm_avgvector <- createvector(rdm_avg)
NeuroRDMvector <- createvector(NeuroRDM)
cor.test(rdm_avgvector,NeuroRDMvector)
plot(rdm_avgvector, NeuroRDMvector, main="Scatterplot AVG vs NeuroRDM", 
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)
#Question 12
# RDM animate
RDM_animate <-  matrix(nrow=92,ncol=92) 
for (i in 1:92) {
  for (j in 1:92) {
    if(CategoryVectors$V1[i]==CategoryVectors$V1[j]&CategoryVectors$V1[i]==1){
      RDM_animate[i,j] <- rdm_avg[i,j]
    }
  }
}

RDM_animatevector <- createvector(RDM_animate)
cor.test(RDM_animatevector,NeuroRDMvector)
plot(RDM_animatevector, NeuroRDMvector, main="Scatterplot AVG vs NeuroRDM only animate", 
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)
#Question 13
# RDM inanimate
RDM_inanimate <-  matrix(nrow=92,ncol=92) 
for (i in 1:92) {
  for (j in 1:92) {
    if(CategoryVectors$V1[i]==CategoryVectors$V1[j]&CategoryVectors$V1[i]==0){
      RDM_inanimate[i,j] <- rdm_avg[i,j]
    }
  }
}

RDM_inanimatevector <- createvector(RDM_inanimate)
cor.test(RDM_inanimatevector,NeuroRDMvector)
plot(RDM_inanimatevector, NeuroRDMvector, main="Scatterplot AVG vs NeuroRDM only inanimate", 
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)

#Question 14
BehaviourRDM = read.table("C:\\Projects\\COG\\git\\Assignment_3\\Input\\RSA lab assignment\\BehaviourRDM", quote="\"", comment.char = "") 
BehaviourRDMvector <- createvector(BehaviourRDM)
cor.test(BehaviourRDMvector,rdm_avgvector)
cor.test(BehaviourRDMvector,RDM_inanimatevector)
cor.test(BehaviourRDMvector,RDM_animatevector)

#Question 15
HmaxRDM = read.table("C:\\Projects\\COG\\git\\Assignment_3\\Input\\RSA lab assignment\\HmaxRDM", quote="\"", comment.char = "") 

##How well does the HMAX RDM correlate to our 'average subject' RDM?
cor.test(as.vector(rdm_avg), as.vector(as.matrix(HmaxRDM)))

##Only look at animated objects
inputframe <- data.frame(hmax =  as.vector(as.matrix(HmaxRDM)), avg = as.vector(rdm_avg), animacy = as.factor(as.vector(RDM_animacy)), human = as.factor(as.vector(RDM_human)))
inputframe_animacy <- inputframe[inputframe$animacy == 1,]
cor.test(inputframe_animacy$hmax, inputframe_animacy$avg)

##Only look at non-animated objects
inputframe_nonanimacy <- inputframe[inputframe$animacy == 0,]
cor.test(inputframe_nonanimacy$hmax, inputframe_nonanimacy$avg)

##Only look at human objects
inputframe_human <- inputframe[inputframe$human == 1,]
cor.test(inputframe_human$hmax, inputframe_human$avg)
