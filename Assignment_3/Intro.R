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
RDM_orig <- 1 - cor(COG_DATA, COG_DATA)
RDM_1 <- 1 - cor(subject1, subject1)
RDM_2 <- 1 - cor(subject2, subject2)
RDM_3 <- 1 - cor(subject3, subject3)
RDM_4 <- 1 - cor(subject4, subject4)
RDM_5 <- 1 - cor(subject5, subject5)
RDM_6 <- 1 - cor(subject6, subject6)
RDM_7 <- 1 - cor(subject7, subject7)
RDM_8 <- 1 - cor(subject8, subject8)
RDM_9 <- 1 - cor(subject9, subject9)
RDM_10 <- 1 - cor(subject10, subject10)
RDM_11 <- 1 - cor(subject11, subject11)
RDM_12 <- 1 - cor(subject12, subject12)

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

# RDM animacy
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
