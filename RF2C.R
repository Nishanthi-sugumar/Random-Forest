company<-read.csv(file.choose())
data(company)
View(company)
hist(company$Sales)
high = ifelse(company$Sales<= 9, "yes", "no")
CDtemp= data.frame(company,high)
CD = CDtemp[,c(1:12)]

str(CD)
table(CD$high)


company_yes<-CD[CD$high=="yes",] 
company_no <-CD[CD$high=="no",] 

company_train <- rbind(company_yes[1:200,],company_no[1:80,])
View(company_train)
company_test <- rbind(company_yes[201:287,],company_no[81:113,])
View(company_test)

library(randomForest)
fit.forest <- randomForest(high~.,data=company_train, na.action=na.roughfix,importance=TRUE)
fit.forest$ntree

mean(company_train$high==predict(fit.forest,company_train))

pred_test <- predict(fit.forest,newdata=company_test)
mean(pred_test==company_test$high) 
library(gmodels)
# Cross table 
rf_perf<-CrossTable(company_test$high, pred_test,
                    prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
                    dnn = c('actual default', 'predicted default'))

