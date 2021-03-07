data = read.csv('/Users/chenhongyu/Desktop/training.csv')
citycode = read.csv('/Users/chenhongyu/Desktop/citycode.csv')
data1= merge(data,citycode,by.x="citycode",by.y="a")
print(head(data1))
data=data1[,c(-1)]
print(head(data))

set.seed(7777)
nn=0.8
data=data
length(data[,1])
sub<-sample(1:nrow(data),round(nrow(data)*nn))
length(sub)
train<-data[sub,]#取0.8的数据做训练集
test<-data[-sub,]#取0.2的数据做测试集
dim(test) #测试集的行数和列数

# library(mice)
# md.pattern(train)


train$amount = as.numeric(as.character(train$amount))
train$recency = as.double(as.character(train$recency))
train$time_period = as.numeric(as.character(train$time_period))
train$frequency = as.numeric(as.character(train$frequency))
train$avgamount = as.numeric(as.character(train$avgamount))
train$maxamount = as.numeric(as.character(train$maxamount))
train$minamount = as.numeric(as.character(train$minamount))
train$cityavg = as.numeric(as.character(train$cityavg))
train$recent_amount = as.numeric(as.character(train$recent_amount))
train$refe=train$recency/train$frequency

test$amount = as.numeric(as.character(test$amount))
test$recency = as.double(as.character(test$recency))
test$time_period = as.numeric(as.character(test$time_period))
test$frequency = as.numeric(as.character(test$frequency))
test$avgamount = as.numeric(as.character(test$avgamount))
test$maxamount = as.numeric(as.character(test$maxamount))
test$minamount = as.numeric(as.character(test$minamount))
test$cityavg = as.numeric(as.character(test$cityavg))
test$recent_amount = as.numeric(as.character(test$recent_amount))
test$refe=test$recency/test$frequency
summary(train)



# hist(log(train$recency))
# hist(log(train$avgamount))
# hist(log(train$maxamount))
# hist(log(train$minamount))

train$recency = log(train$recency)
train$avgamount = log(train$avgamount)
train$maxamount = log(train$maxamount)
train$minamount = log(train$minamount)
train$cityavg=log(train$cityavg)
train$recent_amount=log(train$recent_amount)

test$recency = log(test$recency)
test$avgamount = log(test$avgamount)
test$maxamount = log(test$maxamount)
test$minamount = log(test$minamount)
test$cityavg=log(test$cityavg)
test$recent_amount=log(test$recent_amount)

print(head(train))
library(xgboost)
library(Matrix)
print(head(train[,c(-1,-6,-7,-8,-9,-10,-11,-12)]))
test_x = data.matrix(train[,c(-1,-6,-7,-8,-10,-9,-11,-12)])
test_x = Matrix(test_x, sparse = T)
train_data = list(data = test_x,label = train$donation)
dtrain = xgb.DMatrix(data = train_data$data, label = train_data$label)

set.seed(8888)
xgb.fit = xgboost(data = dtrain, nrounds = 135, objective='binary:logistic', eta = 0.1, 
                  subsample = 0.7,colsample_bytree = 0.8,max_depth = 3,eval_metric ="auc")

#cv.fit = xgb.cv(max_depth = 3, eta = 0.1 ,data = dtrain, metrics = "auc", nfold = 10, 
                #nrounds = 135,objective = "binary:logistic",subsample = 0.7,colsample_bytree = 0.8)
importance_matrix <- xgb.importance(names(train[,c(-1,-6,-7,-8,-9,-10,-11,-12)]), model = xgb.fit)
xgb.plot.importance(importance_matrix[1:10,])
######输出pro
test2_x = data.matrix(test[,c(-1,-6,-7,-8,-9,-10,-11,-12)])
test2_x = Matrix(test2_x, sparse = T)
train2_data = list(data = test2_x,label = test$donation)
dtrain2 = xgb.DMatrix(data = train2_data$data, label = train2_data$label)
xgb.probs = predict(xgb.fit, dtrain2)
test$prob = xgb.probs
print(head(test))
#######amount
train1=na.omit(train)
str(train1)
amount.model = lm(formula = log(amount) ~ recency+avgamount+ 
                    maxamount+minamount+recent_amount,
                  data = train1)

summary(amount.model)
###### 将不存在的城市名替换成null


test$predicted_amount = exp(predict(object = amount.model, newdata = test))
test$score  = test$prob * test$predicted_amount
print(head(test))


####训练集的结果
test$amount[is.na(test$amount)] <- 0
test$predicted_solicit=ifelse(test$score>2.0,1,0)
test$solicit=ifelse(test$amount>2.0,1,0)
test$solicit[is.na(test$predicted_amount)] <- 0
print(head(test))
b_cm =  confusionMatrix(as.factor(test$predicted_solicit), as.factor(test$solicit))
b_cm
#financial performance
gain= sum(test$predicted_solicit*test$amount,na.rm=TRUE)
cost=2*sum(test$predicted_solicit,na.rm=TRUE)
print(gain-cost)
sum(head(test$predicted_solicit))
gain_true= sum(test$amount,na.rm=TRUE)
cost_true=sum(test$donation,na.rm=TRUE)*2
profit_true=gain_true-cost_true
profit_true
# 53257/73034
# 41742/60782
#41599






prediction = read.csv('/Users/chenhongyu/Desktop/prediction.csv')
citycode = read.csv('/Users/chenhongyu/Desktop/citycode.csv')
data2= merge(prediction,citycode,by.x="citycode",by.y="a")
print(head(data2))
prediction=data2[,c(-1)]
print(head(prediction))

prediction$amount = as.numeric(as.character(prediction$amount))
prediction$recency = as.double(as.character(prediction$recency))
prediction$time_period = as.numeric(as.character(prediction$time_period))
prediction$frequency = as.numeric(as.character(prediction$frequency))
prediction$avgamount = as.numeric(as.character(prediction$avgamount))
prediction$maxamount = as.numeric(as.character(prediction$maxamount))
prediction$minamount = as.numeric(as.character(prediction$minamount))
prediction$cityavg = as.numeric(as.character(prediction$cityavg))
prediction$refe=prediction$recency/prediction$frequency
prediction$recent_amount = as.numeric(as.character(prediction$recent_amount))
prediction$recency = log(prediction$recency)
prediction$avgamount = log(prediction$avgamount)
prediction$maxamount = log(prediction$maxamount)
prediction$minamount = log(prediction$minamount)
prediction$cityavg=log(prediction$cityavg)
prediction$recent_amount=log(prediction$recent_amount)

prediction_x = data.matrix(prediction[,c(-1,-6,-7,-8,-9,-10,-11,-12)])
prediction_x = Matrix(prediction_x, sparse = T)
prediction_data = list(data = prediction_x,label = prediction$donation)
dtrain3 = xgb.DMatrix(data = prediction_data$data, label =prediction_data$label)
xgb.probs = predict(xgb.fit, dtrain3)
prediction$prob = xgb.probs
print(head(prediction))
prediction$predicted_amount = exp(predict(object = amount.model, newdata = prediction))
prediction$score  = prediction$prob * prediction$predicted_amount
print(head(prediction))
prediction$amount[is.na(prediction$amount)] <- 0
prediction$predicted_solicit=ifelse(prediction$score>2.0,1,0)
print(head(prediction))
number=sum(prediction$predicted_solicit,na.rm=TRUE)
number

out = data.frame(contact_id = prediction$contact_id,result=prediction$predicted_solicit)
out$result[is.na(out$result)] <- 0
out=out[order(out$contact_id), ]
write.table (out, file ="ass2result5", sep = "\t", row.names =FALSE, col.names =FALSE, quote =FALSE)
