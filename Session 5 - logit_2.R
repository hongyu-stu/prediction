# Load the package

library(RODBC)

# Connect to MySQL (my credentials are mysql_server_64/root/.)
db<- odbcConnect("mysqlodbc",uid="root",pwd="12345678")
sqlQuery(db, "USE ma_charity_full")
# Extract data from database
query = "SELECT    s.contact_id,
          DATEDIFF(20180626, MAX(a.act_date)) / 365 AS 'recency',
          COUNT(a.amount) AS 'frequency',
          AVG(a.amount) AS 'avgamount',
          MAX(a.amount) AS 'maxamount',
          s.donation as donation,
          c.prefix_id as prefix
FROM      assignment2 AS s
LEFT JOIN acts AS a
ON        (s.contact_id = a.contact_id) AND (a.act_type_id = 'DO')
left join contacts c
on s.contact_id = c.id
WHERE     (s.calibration = 1)
GROUP BY 1"
data = sqlQuery(db, query)

# Close the connection
odbcClose(db)
data=na.omit(data)
# Assign contact id as row names, remove id from data
rownames(data) = data$contact_id
data = data[, -1]
print(head(data))
str(data)

# One of the libraries available for (multinomial) logit model
library(nnet)

# Logit model after replacement with a new combination of terms
model3 = multinom(formula = donation ~ log(recency) + log(frequency)+avgamount+maxamount,
                 data = data)
summary(model3)

# Get coefficients, standard errors
coeff = t(summary(model3)$coefficients)
stder = t(summary(model3)$standard.errors)
zvalues = coeff / stder
pvalues = (1 - pnorm(abs(zvalues), 0, 1)) * 2
probs3  = predict(object = model3, newdata = data, type = "class")

a =  confusionMatrix(probs3, as.factor(data$donation))
a 
# Print results
print("coefficients:")
print(coeff)
print("standard deviations:")
print(stder)
print("p-values")
print(pvalues)
