#install.packages("tidybayes")
#install.packages("kernlab")
#install.packages("e1071")
#install.packages("RColorBrewer")
library(e1071)
#working with the training data set
rdata <- read_csv("Desktop/DataScience/MachineLearning/Mini_Project/Data/rdata.csv")
svmfit = svm(V1~ ., data = rdata, kernel = "linear", cost = 10, scale = FALSE)
print(svmfit)#To see the number of supportvectors 
#Now let test the model 
test_data <- read_csv("Desktop/DataScience/MachineLearning/Mini_Project/Data/test_data.csv")
Sx <- test_data[,-1]
Sy <- test_data[1]
pred1 <- predict(svmfit,Sx)
pred1[pred1 >= 0]=1
pred1[pred1< 0]=-1
acc3=sum(pred1==Sy)/nrow(Sy)
#The test accuracy is  77.96537%
#######
#Now we work on the iris data set, we are still going to use the same approach as in perceptron
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
train_I<-train_Iris
test_I<-test_Iris
svmfit2 <- svm(Species ~ ., data = train_I, kernel = 'linear')
table(Prediction = predict(svmfit2, train_I),Truth = train_I$Species)
#we see here that training accuracy is 98.2%, of course we can tune the parameter gamma ans cost to try
#and obtain perfect.
#let us test our model
svmpred3 <- predict(svmfit2, test_I)
table(Prediction = svmpred3, Truth = test_I$Species)
#The test accuracy is 97.36%




