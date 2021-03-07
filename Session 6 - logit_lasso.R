
# A very fast library for elasticNet estimation
library(glmnet)

# The library needs a matrix as input, not a data frame
# Unfortunately, unlike "lm" or "multinom", there is no formula parameter
x = cbind(data1$recency, data1$frequency, data1$recency * data1$frequency, log(data1$recency), log(data1$frequency))
y = data1$donation

# when alpha=0, penalty is ridge
# when alpha=1, penalty is lasso
# when alpha is in ]0,1[, elasticNet penalty
ridge = glmnet(x, y, family = "binomial", alpha=0)
plot(ridge, xvar="lambda")

# Complete model with lasso penalty
# Step 1: fit the model for various values of lambda
lasso = glmnet(x, y, family = "binomial", alpha=1)
plot(lasso, xvar="lambda")

# Step 2: find the optimal value of lambda using cross-validation
cv.lasso = cv.glmnet(x, y, family = "binomial", alpha=1)
plot(cv.lasso)
best.lambda = cv.lasso$lambda.min

# Extract prediction data from database
query = "SELECT contact_id,
                DATEDIFF(20180626, MAX(act_date)) / 365 AS 'recency',
                COUNT(amount) AS 'frequency',
                AVG(amount) AS 'avgamount',
                MAX(amount) AS 'maxamount'
         FROM acts
         WHERE (act_type_id = 'DO')
         GROUP BY 1"
newdata = sqlQuery(db, query)

# Prepare new data as matrix
newdata2 = cbind(newdata$recency, newdata$frequency, newdata$recency * newdata$frequency,
                 log(newdata$recency), log(newdata$frequency))

# Step 3: make predictions by specifying a specific value for lambda
print(best.lambda)
results = predict(lasso, newdata2, s = best.lambda, type = "response")
print(head(results))

# Close the connection
odbcClose(db)
