##Karar Ağaçları
install.packages("rpart")
library(rpart)

installed.packages("rattle")
library(rattle)


install.packages("mice")

library(mice)

md.pattern(Diabetes) # kayıp gözlem yok


table(Diabetes$Outcome)

set.seed(165)
trainIndex <- sample(1:nrow(Diabetes), size = 0.8*nrow(Diabetes))# sample((x, size, replace = FALSE, prob = NULL) )

trainSet <- Diabetes[trainIndex,]
testSet <- Diabetes[-trainIndex,]

nrow(trainSet)
nrow(testSet)
table(trainSet$Outcome)
table(testSet$Outcome)
table(Diabetes$Outcome)

### Model Oluşturma

trainSet$Outcome <- as.factor(trainSet$Outcome)
testSet$Outcome <- as.factor(testSet$Outcome)
modelEntropy <- rpart(Outcome ~ . , data = trainSet , method = "class" , 
                      parms = list(split = "information"))

modelEntropy2 <- rpart(Outcome ~ . , data = trainSet , method = "class" , 
                       parms = list(split = "information"))

modelGini <- rpart(Outcome ~ . , data = trainSet , method = "class" , 
                   parms = list(split = "gini"))

modelEntropy
modelEntropy2
modelGini

### Karar Ağacının Görselleştirilmesi

fancyRpartPlot(modelEntropy)

modelEntropy

### Model Detayları 

summary(modelEntropy)
summary(modelGini)

### Karar Ağaçları Hiper Parametreleri



?rpart.control

modelEntropyHyper <- rpart(Outcome ~ . , data = trainSet , method = "class" , 
                           parms = list(split = "information") ,
                           control = rpart.control(minsplit = 40 , cp = 0.02 , maxdepth = 5))

modelEntropyHyper


### Tahmin

predModelEntropy <- predict(modelEntropy , testSet , type = "class")
predModelGini <- predict(modelGini , testSet , type = "class")
predModelEntropyHyper <- predict(modelEntropyHyper , testSet , type="class")

library(caret)

confusionMatrix(predModelEntropy , testSet$Outcome)
confusionMatrix(predModelEntropy , testSet$Outcome , mode = "prec_recall")
confusionMatrix(predModelEntropy , testSet$Outcome , mode = "prec_recall" , positive = "1")


confusionMatrix(predModelEntropyHyper , testSet$Outcome)
confusionMatrix(predModelEntropyHyper , testSet$Outcome , mode = "prec_recall")
confusionMatrix(predModelEntropyHyper , testSet$Outcome , mode = "prec_recall" , positive = "1")

## Model Tuning

modelLookup("rpart")
modelLookup("rpart2")
library(e1071)

trControl <- trainControl(method="cv" , number = 5 , search = "random")
trControl2 <- trainControl(method="cv" , number = 5 , search = "grid")

modelCP <- train(Outcome ~ . , data  = trainSet ,
                 method = "rpart" ,
                 tuneLength = 20,
                 trControl = trControl
)
modelCP

modelMD <- train(Outcome ~ . , data  = trainSet ,
                 method = "rpart2" ,
                 tuneLength = 20,
                 trControl = trControl)
modelMD #max depth = 9 olarak çıktı.

modelMDGrid <- train(Outcome ~ . , data  = trainSet ,
                     method = "rpart2" ,
                     tuneGrid = expand.grid(maxdepth = 3:20),
                     trControl = trControl2
)
modelMDGrid #max depth = 6 olarak çıktı.

modelTuneMin <- tune.rpart(Outcome ~ . , data  = trainSet ,
                           minsplit = 10:15 , minbucket = 5:10 , cp = seq(0.0 , 0.2 , by = 0.01))
modelTuneMin

modelMDGrid$finalModel

predMDGrid <- predict(modelMDGrid$finalModel , testSet , type = "class")
predCP <- predict(modelCP$finalModel , testSet , type = "class")
predMD <- predict(modelMD$finalModel , testSet , type = "class")
predMin <- predict(modelTuneMin$best.model , testSet , type = "class")


confusionMatrix(predMDGrid  , testSet$Outcome , mode = "prec_recall" , positive = "1")
confusionMatrix(predCP  , testSet$Outcome , mode = "prec_recall" , positive = "1")
confusionMatrix(predMD  , testSet$Outcome , mode = "prec_recall" , positive = "1")
confusionMatrix(predMin  , testSet$Outcome , mode = "prec_recall" , positive = "1")

## Regression Tree Modeli


modelReg <- rpart(Age ~ . , data  = trainSet )
modelReg

predReg <- predict(modelReg , testSet)
predReg


summary(modelReg)

### Random Forest Modeli Oluşturma
####################################

install.packages("randomForest")
library(randomForest)

modelRF <- randomForest(Outcome ~ . , data  = trainSet , ntree = 500 ) # OOB :Hata Terimi
modelRF
modelRF$mtry # Split için kullanılması gerken değişken sayısı
modelRF$err.rate


# Random Forest Tahmin
predRF <- predict(modelRF, testSet)
predRF

library(caret)

confusionMatrix(predRF , testSet$Outcome)




