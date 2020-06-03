fraud<-read.csv(file.choose())
data(fraud)
View(fraud)
hist(fraud$Taxable.Income)
Risky_Good = ifelse(fraud$Taxable.Income<= 30000, "Risky", "Good")
FCtemp= data.frame(fraud,Risky_Good)
FC = FCtemp[,c(1:7)]

str(FC)
table(FC$Risky_Good)


fraud_Risky<-FC[FC$Risky_Good=="Risky",] 
fraud_Good <- FC[FC$Risky_Good=="Good",] 

fraud_train <- rbind(fraud_Risky[1:80,],fraud_Good[1:350,])
View(fraud_train)
fraud_test <- rbind(fraud_Risky[81:44,],fraud_Good[351:476,])
View(fraud_test)

library(randomForest)
fit.forest <- randomForest(Risky_Good~.,data=fraud_train, na.action=na.roughfix,importance=TRUE)
fit.forest$ntree

mean(fraud_train$Risky_Good==predict(fit.forest,fraud_train))

pred_test <- predict(fit.forest,newdata=fraud_test)
mean(pred_test==fraud_test$Risky_Good) 
library(gmodels)
# Cross table 
rf_perf<-CrossTable(fraud_test$Risky_Good, pred_test,
                    prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
                    dnn = c('actual default', 'predicted default'))

                    