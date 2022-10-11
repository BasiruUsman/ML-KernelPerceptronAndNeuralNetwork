rdata <- read_csv("Desktop/DataScience/MachineLearning/Mini_Project/Data/rdata.csv")
library(ggplot2)
ggplot(rdata, aes(x = V41, y = V40)) + 
  geom_point(aes(colour=V1, shape=as.factor(V1)), size = 3) +
  xlab("V41") + 
  ylab("V40") + 
  ggtitle("V1 vs V40 and V41")

#Here we separate the attributes from the class
x <- rdata[, -1]
y<- rdata[,1]
#Now we implement the perceptron algorithm 
perc01 <- function(x, y, eta, niter) {
  
  # initialize weight vector
  weight <- rep(0, dim(x)[2] + 1)
  errors <- rep(0, niter)
  
  
  # loop over number of epochs niter
  for (jj in 1:niter) {
    
    # loop through training data set
    for (ii in 1:length(y)) {
      
      # Predict binary label using Heaviside activation 
      # function
      z <- sum(weight[2:length(weight)] * 
                 as.numeric(x[ii, ])) + weight[1]
      if(z < 0) { 
        ypred <- -1
      } else {
        ypred <- 1
      }
      
      # Change weight - the formula doesn't do anything 
      # if the predicted value is correct
      weightdiff <- eta * (y[ii] - ypred) * 
        c(1, as.numeric(x[ii, ]))
      weight <- weight + weightdiff
      
      # Update error function
      if ((y[ii] - ypred) != 0.0) {
        errors[jj] <- errors[jj] + 1
      }
      
    }
  }
  # weight to decide between the two species 
  print(weight)
  return(errors)
}
err <- perc01(x, y, 1, 10)
plot(1:10, err, type="l", lwd=2, col="red", xlab="epoch #", ylab="errors")
title("Errors vs epoch (with learning rate eta = 1)")
######################################################
#Now let us implement perceptron in Iris data set(Multiclass classification)
Iris_data <- read.csv("~/Desktop/DataScience/MachineLearning/Mini_Project/Data/Iris - data.csv")
#The plan is to achieve this classification in two levels
#level1: we split the data set into two(e.g sesota and versicolor), one containing 
  #two levels and build a perceptron with binary levels
# level2: We build a model on the whole dataset which classify virginica or not, and then, further
  #classify not virginica into sesota or versicolor using the first model
#Let start by spliting the data into test and training 
smp_siz = floor(0.75*nrow(Iris_data))  # creates a value for dividing the data into train and
# with test(70% as train)
set.seed(85)   # set seed to ensure you always have same random numbers generated
train_id = sample(seq_len(nrow(Iris_data)),size = smp_siz)#generate id for training set
train_Iris<-Iris_data[train_id,]#generating the training data
test_Iris<-Iris_data[-train_id,]
#Now working with training set 
#Let's look at our data 
names(train_Iris)<-c("sepallength","sepalwidth", "petallength", "petalwidth", "Species")
head(train_Iris)
library(ggplot2)
ggplot(train_Iris, aes(x =  sepallength, y = petallength)) + 
  geom_point(aes(colour=Species, shape=Species), size = 3) +
  xlab("sepal length") + 
  ylab("petal length") + 
  ggtitle("Species vs sepal and petal lengths")
ggplot(train_Iris, aes(x =  sepalwidth, y = petalwidth)) + 
  geom_point(aes(colour=Species, shape=Species), size = 3) +
  xlab("sepal width") + 
  ylab("petal width") + 
  ggtitle("Species vs sepal and petal widths")
#Now let us get the subset of the dataset with two levels(virginica and versicolor)
tIris2 <- subset(train_Iris, Species=="Iris-virginica" | Species=="Iris-versicolor", select=c( sepallength, sepalwidth, petallength, petalwidth,Species))
#Let use level virginica=1 and versicolor=-1
tIris2[, 6] <- 1
tIris2[tIris2[, 5] == "Iris-versicolor", 6] <- -1
Ix <- tIris2[, c(1, 2,3,4)]
Iy <- tIris2[, 6]
#Remember we already have a perceptron model we just need to train it and obtain the weights 
err2 <- perc01(Ix, Iy, 1, 10)
plot(1:100, err2, type="l", lwd=2, col="red", xlab="epoch #", ylab="errors")
#Now here we use the training set but with labels setosa =-1 and others =1
tIris3<-train_Iris
tIris3[,6]<- -1
tIris3[tIris3[, 5] == "Iris-setosa", 6] <- 1
#Now we run the model to the get the weight of classifying setosa vs others 
IIx<-tIris3[, c(1, 2,3,4)]
IIy<-tIris3[, 6]
err3 <- perc01(IIx, IIy, 0.1, 20)
plot(1:10, err3, type="l", lwd=2, col="red", xlab="epoch #", ylab="errors")
#Now let us test the accuracy of the model
test_Iris1<-test_Iris#just to have a duplicate that we can alter as we fit
test_Iris1[,6]<- 1#we are creating labels for setosa vs rest
test_Iris1[test_Iris1[, 5] == "Iris-setosa", 6] <- -1
Tx<-test_Iris1[,c(1,2,3,4)]
Ty<-test_Iris1[,6]
w1<-c(  0.20, 0.66,  1.42, -2.28, -1.12)#weight for classifying setosa vs others
w2<-c(-1.320, -1.254, -1.356,  1.844,  2.400)#weight of classifying virginica vs versicolor 
#Let us test the accuracy of the two perceptrons
TTx<-Tx
TTx[,5]<-1
colnames(TTx) <- NULL
p1<-zeros(38, 1)
for (ii in 1:38) {
  p1[ii,1]<-w1%*%as.double(TTx[ii,])
}
p1[p1 >= 0] = 1
p1[p1< 0] = -1
acc = sum(p1==Ty)/38
acc
#we got an accuracy of 71.05263% on classifying setosa vs others
#Now let get accuracy for classify between virginica vs versicolor
names(test_Iris)<-c( "sepallength", "sepalwidth", "petallength", "petalwidth","Species" )
test2 <- subset(test_Iris, Species=="Iris-virginica" | Species=="Iris-versicolor", 
                 select=c( sepallength, sepalwidth, petallength, petalwidth,Species))
test2[, 6] <- 1
test2[test2[, 5] == "Iris-versicolor", 6] <- -1
Bx<-test2[,c(1,2,3,4)]
By<-test2[,6]
colnames(Bx) <- NULL
Bx[,5]<-1
p2<-zeros(27, 1)
for (ii in 1:27) {
  p2[ii,1]<-w2%*%as.double(Bx[ii,])
}
p2[p2 >= 0] = 1
p2[p2< 0] = -1
acc2 = sum(p2==By)/27
acc2
#we get a test accuracy of classifying virginica vs versicolor of 40.74074%


