## Very simple practice document with the Titanic Kaggle contest data

library(caret)
library(randomForest)

trainSet <- read.table("train.csv", sep = ",", header = TRUE)
testSet <- read.table("test.csv", sep = ",", header = TRUE)

table(trainSet$Survived, trainSet$Pclass)

library(fields)
trainSet$Survived = factor(trainSet$Survived)

table(trainSet$Survived, trainSet$Sex)

model <- train(Survived ~ Pclass + Sex + SibSp +   
                 Embarked + Parch + Fare, # Survived is a function of the variables we decided to include
               data = trainSet, # Use the trainSet dataframe as the training data
               method = "rf",# Use the "random forest" algorithm
               trControl = trainControl(method = "cv", # Use cross-validation
                                        number = 5) # Use 5 folds for cross-validation
)

model2 = train(Survived ~ Pclass + Fare + Embarked + Sex + SibSp + Age, 
               data = trainSet, 
               method = "rf", 
               trControl = trainControl (method = 'cv', number = 5)
               )

model3 = train(Survived ~ Pclass + Sex + SibSp + Age2, 
               data = trainSet, 
               method = "rf", 
               trControl = trainControl (method = 'cv', number = 5)
)

testSet$Fare <- ifelse(is.na(testSet$Fare), mean(testSet$Fare, na.rm = TRUE), testSet$Fare)

testSet$Survived3 <- predict(model3, newdata = testSet)
summary(testSet$Survived)

trainAge = trainSet$Age
trainSet$Age2 = ifelse(is.na(trainSet$Age2), 100, trainSet$Age2) 
testSet$Age2 = ifelse(is.na(testSet$Age), 100, testSet$Age) 

testSet$Survived <- predict(model3, newdata = testSet)

submission <- testSet[,c("PassengerId", "Survived")]
write.table(submission, file = "titanicSub1.csv", col.names = TRUE, row.names = FALSE, sep = ",")
