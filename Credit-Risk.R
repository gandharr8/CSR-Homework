loans <- read.csv(file.choose())
loans

str(loans)
summary(loans)

loans$credit.policy = as.factor(loans$credit.policy)
loans$credit.policy
library(caTools)

set.seed(100)
spl = sample.split(loans$not.fully.paid, 0.7)
train = subset(loans, spl == TRUE)
test = subset(loans, spl == FALSE)

modLog = glm(not.fully.paid ~. -annualincome, data=train, family="binomial")
summary(modLog)

modLog2 = glm(not.fully.paid ~ purpose + int.rate + installment + log.annual.inc + inq.last.6mths + pub.rec, data=train, family="binomial")
summary(modLog2)

test$predicted.risk = predict(modLog2, newdata=test, type="response")
test$predicted.risk
table(test$not.fully.paid, as.numeric(test$predicted.risk >= 0.5))

table(test$not.fully.paid)

library(ROCR)
require(ROCR)
pred = prediction(test$predicted.risk, test$not.fully.paid)
pred
as.numeric(performance(pred, "auc")@y.values)

predictTrain = predict(modLog2, type="response")
predictTrain
ROCRpred = prediction(predictTrain, train$not.fully.paid)
ROCRpred
ROCRperf = performance(ROCRpred, "tpr", "fpr")
ROCRperf
plot(ROCRperf) 
plot(ROCRperf, colorize=TRUE)

plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7)) 
t1 = table(test$not.fully.paid, as.numeric(test$predicted.risk >= 0.35))
t1

