# Case study 2 ; Bank Loan Default by LOgistic Regreesion 

library(readr)
credit <- read_csv("E:/R prac/Datasets Predictive/credit.csv")
View(credit)
head(credit)
credit$default[credit$default==1]=0
credit$default[credit$default==2]=1
head(credit$default)

indx =sample(1:nrow(credit),as.integer(0.8*nrow(credit)))
credit_train= credit[indx,]
credit_test=credit[-indx,]
dim(credit_train)
dim(credit_test)


prop.table(table(credit$default))*100
prop.table(table(credit_train$default))*100
prop.table(table(credit_test$default))*100


credit_train_labels = credit[indx,17]
credit_test_labels = credit[-indx,17]

#install.packages('Amelia')
library(Amelia)

missmap(credit, main = 'missed vs actual')
sapply(credit, function(x) sum(is.na(x)))

model = glm(default~.,family = binomial(link = 'logit'), data = credit_train)
summary(model)


library(margins)
margins(model)
anova(model)
fitted.results = predict(model,newdata = credit_test,type = 'response')
fitted.results = ifelse(fitted.results>0.6,1,0)
head(fitted.results)
head(credit_test$default)

par(mfrow=c(2,1))
plot(fitted.results,type='p',xlab = '',ylab = '',ylim = c(0,1),col = 'green',pch =20)
plot(credit_test$default,type = 'p',col = 'red',pch=20)

classification_error =mean(fitted.results != credit_test$default)
classification_error
paste('Accuracy=', 1-classification_error)
####################################################################
# install.packages("ROCR")
library(ROCR)
fitted.results = predict(model,newdata = credit_test,type = 'response')
pr_fitted.results = prediction(fitted.results,credit_test$default)
per_fitted.results =performance(pr_fitted.results,measure = 'tpr',x.measure = 'fpr')

plot(per_fitted.results)
abline(0,1, lwd=2,lty=2)

auc = performance(pr_fitted.results, measure = 'auc')
auc = auc@y.values[[1]]
auc
?performance

#################################
head(credit_train)
model2 = glm(default~checking_balance+months_loan_duration+credit_history+
               savings_balance+housing,family = binomial(link = 'logit'),
             data = credit_train)
summary(model2)

anova(model2,test = 'Chisq')

fitted.results = predict(model2,newdata = credit_test,type = 'response')
fitted.results = ifelse(fitted.results>0.6,1,0)
head(fitted.results)
head(credit_test$default)




classification_error =mean(fitted.results != credit_test$default)
classification_error
paste('Accuracy=', 1-classification_error)



fitted.results = predict(model2,newdata = credit_test,type = 'response')
pr_fitted.results = prediction(fitted.results,credit_test$default)
per_fitted.results =performance(pr_fitted.results,measure = 'tpr',x.measure = 'fpr')

plot(per_fitted.results)
abline(0,1, lwd=2,lty=2)

auc = performance(pr_fitted.results, measure = 'auc')
auc = auc@y.values[[1]]
auc
