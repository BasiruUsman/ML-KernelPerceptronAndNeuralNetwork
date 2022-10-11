library(readr)
a4a <- read_csv("https://www.csie.ntu.edu.tw/%7Ecjlin/libsvmtools/datasets/binary/a4a",
                col_names = FALSE)
library(dplyr)
library(tidyr)
write.table(a4a, file = "trying.csv", sep = ",")#save the excel file in the current working directory
data2 <- read.csv("~/Desktop/DataScience/MachineLearning/Mini_Project/Data/data2.csv")
names(data2)<-c("y", "x1","x2", "x3","x4","x5", "x6","x7","x8","x9","x10","x11","x12","x13",
                "x14", "x15","x16", "x17","x18","x19", "x20","x21","x22","x23","x24","x25",
                "x26","x27","x28")
names(data2) #To view the column names
sapply(data2, function(x) sum(is.na(x)))# To count the number of missing values in each column
#Here we see that the number of missing data has about 7%, so we are going delete the number 
#of observation with the missing value
#data3<-na.omit(data2)
install.packages("optimbase")#install the required package
library(optimbase) #load the library 
dmat1<-zeros(nx=nrow(data2), ny=123) #create a matrix of zeros
dmat2<-data2[,-1]#removing the labels
dmat3<-dmat2[,-c(2,4,6,8,10,12,14,16,18,20,22,24,26,28)]#removing the columns with ones in the entries 
tdmat3<-data.frame(t(apply(dmat3, 1, na.locf)))#replacing NAs with the previous value in the row
for (i in 1:nrow(dmat3)){
  for ( j in 1:ncol(tdmat3)){
      dmat1[[i,tdmat3[i,j]]]<-1
  }
}
lab<-data2$y
lab1<-as.data.frame(lab)
data_clean<-dmat1
write.table(dmat1, file = "dmat1.csv", sep = ",")
write.table(lab1, file = "lab1.csv", sep = ",")
rdata <- read_csv("Desktop/DataScience/MachineLearning/Mini_Project/Data/rdata.csv")
install.packages("rpart.plot")# to install rpart.plot
library(rpart)
library(rpart.plot)
fit1 <- rpart(V1~., data = rdata, method = 'class', parms =  list(split = "information"))#Now building the model
rpart.plot(fit1, extra = 106)
ad1<-varImp(fit1)#To see the variable importance 
#We work on recovering and loading the test data 
a4a <- read_csv("Desktop/DataScience/MachineLearning/Mini_Project/Data/a4a.t", col_names = FALSE)
write.table(a4a, file = "testdata.csv", sep = ",")#save the excel file in the current working directory
test_data <- read.csv("~/Desktop/DataScience/MachineLearning/Mini_Project/Data/tdata.csv")#load the 
#data after seperating it in columns 
names(test_data)<-c("y", "x1","x2", "x3","x4","x5", "x6","x7","x8","x9","x10","x11","x12","x13",
                "x14", "x15","x16", "x17","x18","x19", "x20","x21","x22","x23","x24","x25",
                "x26","x27","x28")
library(optimbase) #load the library 
sapply(test_data, function(x) sum(is.na(x)))# To count the number of missing values in each column
#(Remember in this case the values are not missing!)
tmat1<-zeros(nx=nrow(test_data), ny=123) #create a matrix of zeros
tmat2<-test_data[,-1]#removing the labels from the test data
tmat3<-tmat2[,-c(2,4,6,8,10,12,14,16,18,20,22,24,26,28)]#removing the columns with ones in the entries
#from the test data
ttmat3<-data.frame(t(apply(tmat3, 1, na.locf)))#replacing NAs with the previous value in the row
#In the code below we are recovering the original test data points 
for (i in 1:nrow(tmat3)){
  for ( j in 1:ncol(ttmat3)){
    tmat1[[i,ttmat3[i,j]]]<-1
  }
}
tlab<-test_data$y
tlab1<-as.data.frame(tlab)
#In the code below we expoted the test data in order to merge it with labels
write.table(tmat1, file = "tmat1.csv", sep = ",")
write.table(tlab1, file = "tlab1.csv", sep = ",")
test_data <- read_csv("Desktop/DataScience/MachineLearning/Mini_Project/Data/test_data.csv")
#####(After Cleaning the data, the training and test data as follows)
#training data=rdata
#test data= test_data
# Now we test our decision tree model
predict_unseen <-predict(fit1, test_data, type = 'class')
table_mat <- table(test_data$V1, predict_unseen)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)#Computing the accuracy
print(paste('Accuracy for test', accuracy_Test))
#############
#For Q2. 
Iris_data <- read_csv("Desktop/DataScience/MachineLearning/Mini_Project/Data/Iris - data.csv")
smp_siz = floor(0.75*nrow(Iris_data))  # creates a value for dividing the data into train and
# with test(70% as train)
set.seed(850)   # set seed to ensure you always have same random numbers generated
train_id = sample(seq_len(nrow(Iris_data)),size = smp_siz)#generate id for training set
train_Iris<-Iris_data[train_id,]#generating the training data
train_I<-train_Iris
train_I[train_I[,5]=="Iris-setosa",6]<-1
train_I[train_I[,5]=="Iris-virginica",6]<-2
train_I[train_I[,5]=="Iris-versicolor",6]<-3
names(train_I)<-c("sepallength", "sepalwidth" , "petallength" ,"petalwidth" , "Species"  ,   "sp")
test_Iris<-Iris_data[-train_id,]
fit2 <- rpart(sp~., data = train_I, method = 'class', parms =  list(split = "information"))#Now building the model
rpart.plot(fit2, extra = 106)
test_I<-test_Iris
test_I[test_I[,5]=="Iris-setosa",6]<-1
test_I[test_I[,5]=="Iris-virginica",6]<-2
test_I[test_I[,5]=="Iris-versicolor",6]<-3
names(test_I)<-c("sepallength", "sepalwidth" , "petallength" ,"petalwidth" , "Species"  ,   "sp")
predict_unseen2 <-predict(fit2, test_I, type = 'class')
table_mat2 <- table(test_I$sp, predict_unseen2)
table_mat2
accuracy_Test2 <- sum(diag(table_mat2)) / sum(table_mat2)#Computing the accuracy
print(paste('Accuracy for test', accuracy_Test2))





