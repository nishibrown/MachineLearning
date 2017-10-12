
# get needed libraries
library(caret)
library(rattle)
library(rpart)
library(rpart.plot)
library(randomForest)
library(knitr)

## read the files and get rid of any "N/A" or "#DIV/0"

train<-read.csv("pml-training.csv", na.strings=c("NA", "#DIV/0!", ""))
validation<-read.csv("pml-testing.csv",na.strings=c("NA", "#DIV/0!", ""))


# get rid of first 7 columns since they have no predictive value

train_a<-train[, -c(1:7)]
validation_a<-validation[, -c(1:7)]

# get rid of missing values

training_data<-train_a[, colSums(is.na(train_a)) == 0]
validation_data<-validation_a[, colSums(is.na(validation_a))==0]


#break up training data into training set and testing set

inTrain<-createDataPartition(y=training_data$classe, p=0.7, list=FALSE)
training<-training_data[inTrain,]
testing<-training_data[-inTrain,]


# predict classe using trees

set.seed(1001)
modFit1 <- rpart(classe ~ ., data=training, method="class")
predict_tree<-predict(modFit1, testing, type="class")
cm_tree<-confusionMatrix(predict_tree, testing$classe)
cm_tree

# predict classe using random forest

set.seed(2003)
modFit2<-randomForest(classe~., data=training)
predict_rf<-predict(modFit2, testing, type="class")
cm_rf<-confusionMatrix(predict_rf, testing$classe)
cm_rf

# Given high accuracy value of Random Forest method, use it on validation data
predict_validation<-predict(modFit2, validation_data, type="class")
predict_validation


# Generate files for course project prediction quiz

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("test_case_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predict_validation)