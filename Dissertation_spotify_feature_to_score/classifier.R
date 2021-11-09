set.seed(17)
train.index<-createDataPartition(df$pop_class, p=0.75, list=F)
training<-df[train.index,]
testing<-df[-train.index,]

#training control for caret
#declare 10-fold cross validation
ctrl<-trainControl(method = "cv", number = 10,  classProbs = TRUE, verboseIter = F)

#CART approach method random forest
#training model
cartgrid<-expand.grid(mtry=c(2,3,4,5,6,8,10,12))
cartfit<-train(pop_class~., data = training, method =  "rf", tuneGrid = cartgrid,
               trControl = ctrl, metric = "Accuracy")
cartfit
#predicting test data
cartpred<-predict(cartfit, newdata = testing)
cnfcart<-confusionMatrix(cartpred, testing$pop_class)
cnfcart