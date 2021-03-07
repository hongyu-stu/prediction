# Install and load libraries ---------------------------------------------------------------
install.packages('rpart')
install.packages('randomForest')

library(rpart)
library(randomForest)
 
# Defining Useful functions ---------------------------------------------------------

# Accuracy function: compute the accuracy between the predicted values and the real ones
## Accuracy is percentage of accurate predictions

accuracy <- function(y_pred,y_test) {
  cnt <- 0
  for (i in 1:length(y_pred)) { ####Yashodhan Joshi Edit
    if (y_pred[[i]] == y_test[[i]]) cnt = cnt + 1
  }
  return(cnt/nrow(testSet))
}

# Cross validation function: compute a k-fold cross-validation for a given model ("Logreg","Tree","RF")
# It works as follows: the dataset is split into k samples (named folds) and for each sample: the sample
# is used as test data and the k-1 remaining ones are used to train the model. The model is then evaluated
# on the k-th sample, the test one, and at the end, we compute the mean of the k accuracies to evaluate 
# the overall model performance.
cross_validation <- function(data,nfold,model) {
  
  l_accuracy = rep(0, nfold)
  # We associate a number between 1 and nfold to each row, randomly
  fold_i <- sample(rep(1:nfold, length.out = nrow(df)))
  for (k in 1:nfold) {
    
    # All the rows for which the index has been associated to the k-th fold are stored into testIdx
    testIdx = which(fold_i == k)
    # We select all the data that has not been associated to the k-th fold for the training set
    trainSet = data[-testIdx, ]
    # We select the data that has been associated to the k-th fold for the test set
    textSet = data[testIdx,]
    y_text = as.array(textSet$AddToCart)   ####Yashodhan Joshi Edit
    # Convert to factors for Random Forest:
    if (model == 'RF') {
      trainSet$AddToCart = as.factor(trainSet$AddToCart)
      textSet$AddToCart = as.factor(textSet$AddToCart)
    }
    options(warn=-1) # warnings are disabled for the fitting
    # We fit the models on the train set
    if (model == 'Logreg') fitted = glm(AddToCart ~., data=trainSet, family="binomial")
    else if (model == 'Tree') fitted = rpart(AddToCart ~., data=trainSet,method="class")
    else if (model == 'RF') fitted = randomForest(AddToCart ~., data=trainSet)
    options(warn=0) # warnings are re-activated
    
    # Predictions on the test set
    if (model == 'Tree') pred=predict(fitted,newdata=textSet)[,2] ####Yashodhan Joshi Edit
    else pred=predict(fitted,newdata=textSet,type='response')     ####Yashodhan Joshi Edit
    
    if (model == 'RF') predictions = ifelse(pred==1,1,0)
    else predictions = ifelse(pred>=0.5,1,0)
    # Computation of the accuracy for the k-th fold, stored into a vector
    l_accuracy[k] = accuracy(predictions,y_text)                  ####Yashodhan Joshi Edit
  }
  
  # We print the model and the mean of the accuracies
  cat("Model: ", model, " - Average Accuracy: ", mean(l_accuracy))
  # We return the vector of all accuracies
  return(l_accuracy)
}

# Load data ---------------------------------------------------------------

# Load data into a dataframe
df = read.csv('table.csv')
# List of initial columns
print(colnames(df))


# Feature selection ---------------------------------------------------------------

# We store all variables to drop into a list
to_drop = c("ClientId","RowId","Recency","Frequency","NbTotalSessions","NbBounces","QualityAvg",
            "DeviceCategory","Browser","OperatingSystem","NbEventHits")
# We drop the variables
df = df[ , !(names(df) %in% to_drop)]
# We fill missing values with zeros
df[is.na(df)] = 0
# List of final columns
print(colnames(df))



# Train/test split ---------------------------------------------------------------

# We split the dataset into a train and test samples
# We define the percent of our dataset that will be used for training
trainSize = 0.75
# We set a seed in order to have the same split at each run
set.seed(123)
# We create random indices that represents a given percent of the dataset (previously defined)
sample = sample.int(n = nrow(df), size = round(trainSize*nrow(df)), replace = F)
# We select a sample based on the indices: trainSet is composed of trainSize % of the initial dataset
trainSet = df[sample, ]
# The rest is the test set
testSet = df[-sample, ]
# We define the target variable (it will be used to compute models accuracies)
y_test = testSet$AddToCart
summary(y_test) #JR: to see what is unconditional proability of bying a trip: 27.9%

# Models ---------------------------------------------------------------
# ----- Logistic regression
# We fit the classifier over our training data
logreg = glm(AddToCart ~ ., data = trainSet, family = 'binomial')
summary(logreg) #JR: understand the output
# We apply our fitted model to our test set
pred.log=predict(logreg,newdata=testSet,type='response')
# We convert predicted probabilities into binary values
predictions_log = ifelse(pred.log>=0.5,1,0)
# We compute the test accuracy
accuracy(predictions_log,y_test)
 
purchase<-pred.log[y_test==1]    # Select the fitted proba for Purchasers
nonpurchase<-pred.log[y_test==0] # Select the fitted proba for Non-Purchasers
plot(density(nonpurchase),col = "red", xlim =c(-.05,1),ylim=c(0,max(c(density(nonpurchase)$y,density(purchase)$y))),main = paste("Purchased (green) vs. Non-Purchased (red)", collapse = ''))
lines(density(purchase), col = "green") # plot the purchased in green

#JR: check out the optimal threshold
Nthresholds=11;
mthreshold=array(0,dim=c(Nthresholds,2));
mthreshold[,1] = t(seq(0, 1, length.out=Nthresholds));
for(t in 1:Nthresholds)
{
  predictions_log = ifelse(pred.log>=mthreshold[t,1],1,0);
  mthreshold[t,2] = accuracy(predictions_log,y_test);
}
plot(mthreshold[,1],mthreshold[,2],type="l");
mthreshold
#JR: 0.5 threshold is not bad but a slightly lower value like 0.4 has higher accuracy

# ----- Decision tree
#JR: add this packages to make a plot of the tree
install.packages("rpart.plot")
library(rpart.plot)

# We fit the classifier over our training data
tree =  rpart(AddToCart ~., method="class", data=trainSet)
#JR: make some plots to interpret tree
#plot(tree)
plotcp(tree)
rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)

# We apply our fitted model to our test set
pred.tree=predict(tree,newdata=testSet)[,2]
#JR: based on the plot try to understand the first 5 values of pred.tree

# We convert predicted probabilities into binary values
predictions_tree = ifelse(pred.tree>=0.5,1,0)
# We compute the test accuracy
accuracy(predictions_tree,y_test)

#JR: check out the optimal threshold
Nthresholds=11;
mthreshold=array(0,dim=c(Nthresholds,2));
mthreshold[,1] = t(seq(0, 1, length.out=Nthresholds));
for(t in 1:Nthresholds)
{
  predictions_log = ifelse(pred.tree>=mthreshold[t,1],1,0);
  mthreshold[t,2] = accuracy(predictions_log,y_test);
}
plot(mthreshold[,1],mthreshold[,2],type="l");
mthreshold
#JR: 0.5 threshold is OK




# ----- Random forest
# Convert AddToCart to factor in order to perform Random Forest classification
trainSet_rf = trainSet
testSet_rf = testSet

trainSet_rf$AddToCart = as.factor(trainSet_rf$AddToCart)
testSet_rf$AddToCart = as.factor(testSet_rf$AddToCart)

# We fit the classifier over our training data
options(warn=-1) # warnings are disabled for the fitting
rf = randomForest(AddToCart ~., data=trainSet_rf)
options(warn=0) # warnings are re-activated
# We apply our fitted model to our test set
pred.rf = predict(rf,newdata=testSet_rf)
# We convert predicted probabilities into binary values #JR: are already in binary values
predictions_rf = ifelse(pred.rf==1,1,0)
# We compute the test accuracy
accuracy(predictions_rf,y_test)

# Cross validation ----------------------------------------
# JR: to test if previous results were not specific to the train and test data selection
# We define the number of folds
nb_fold = 4
# We perform cross-validation over our 3 models
acc_log = cross_validation(data=df,nfold=nb_fold,model="Logreg")
acc_tree = cross_validation(data=df,nfold=nb_fold,model="Tree")
acc_rf = cross_validation(data=df,nfold=nb_fold,model="RF")

# Results analysis ----------------------------------------
# We plot the accuracies for all models
plot(acc_log,type='l',col='blue',xlim=c(1, nb_fold), ylim=c(0, 1),
     xlab="Fold number", ylab="Accuracy",main="Accuracy per model")
lines(acc_tree,col='red')
lines(acc_rf,col='green')
legend("bottomright",legend=c('Logistic regression','Decision tree','Random forest'),
       fill=c('blue','red','green'))

