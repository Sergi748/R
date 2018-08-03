
##############################################################################################
# LIBRARIES
##############################################################################################
library(rpart)
library(C50)
library(caret)
library(e1071)

##############################################################################################
# DEVELOPMENT
##############################################################################################
data(churn)
table = rbind(churnTrain, churnTest)
# Split table in train and test
set.seed(22)
row_train = sample(1:nrow(table), size = 0.9*nrow(table))
train = table[row_train, ]
test = table[-row_train, ]

tree = rpart(churn ~ ., data = train)
plot(tree, uniform = TRUE, branch = 0.6, margin = 0.1)
text(tree, all = TRUE, use.n = TRUE)

# Complexity parameters of model
printcp(tree)

# Mode predict
treePredict = predict(tree, newdata = test, type = "class")
table(test$churn, treePredict)
# Visualization of classifications made
confusionMatrix(treePredict, reference = test$churn)$table

# Improvement of model
# Use CP to remove 
minXerror = min(tree$cptable[, "xerror"])
rowMinXerror = which.min(tree$cptable[, "xerror"])

cpMinXerror = tree$cptable[rowMinXerror, "CP"]

# Now we prune the tree
treePrune = prune(tree, cp = cpMinXerror)
plot(treePrune, uniform = TRUE, branch = 0.6, margin = 0.1)
text(treePrune, all = TRUE, use.n = TRUE)

# Performance
prunedPredictions = predict(treePrune, newdata = test, type = "class")
confusionMatrix(prunedPredictions, reference = test$churn)$table
