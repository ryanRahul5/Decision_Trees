require(ISLR)
require(tree)
attach(Carseats)
hist(Sales)
High = ifelse(Sales >=8,"Yes","No")

## now what we are doing is converting Sales data into binary variables(categorical) 
Carseats = data.frame(Carseats, High)
tree.carseats = tree(High ~. -Sales, data=Carseats)
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty = 0)  # for annotating

#to see the result of every single splitting variable
tree.carseats

## 
set.seed(1011)
train = sample(1:nrow(Carseats),250)
tree.carseats = tree(High~. -Sales, Carseats,subset = train)
plot(tree.carseats); text(tree.carseats,pretty = 0)

tree.pred = predict(tree.carseats, Carseats[-train,], type = "class")
with(Carseats[-train,], table(tree.pred, High))  ## similar to apply function

# handy way of assigning a data frame as context in which to do the next command(table)

## result diagonals have the right classification
(72+33)/150
# error rate of .7 with that bushy tree


## prune using CV

cv.carseats = cv.tree(tree.carseats,FUN = prune.misclass)  ## using misclassification for the basis of pruning

cv.carseats
 ## notice deviance drops down then increases later
## $k -- cost complexity parameter in the process

plot(cv.carseats)

# picking a value near the minimum
prune.carseats = prune.misclass(tree.carseats, best = 13)

plot(prune.carseats); text(prune.carseats,pretty = 0)


## evaluating on the test dataset
tree.pred = predict(prune.carseats, Carseats[-train,], type = "class")
with(Carseats[-train,], table(tree.pred,High))

(72+32)/150
# 0.69  ...little less than the previous error rate


## often pruning doesn't hurt misclassification errors rather gave us just simple trees to interpret

########################## Random Forests and Boosting

#Boston housing data

require(randomForest)
require(MASS)
set.seed(101)

dim(Boston)
train = sample(1:nrow(Boston),300)

rf.boston = randomForest(medv~., data = Boston, subset = train)
rf.boston

# mean squared residuals basically are the out of bag errors... i.e. each obs. was predicted using the average of trees that didn't include it. implies de-biased wstimate of prediction error

## here "mtry" is used to choose no. predictors at each split

## from 13 variables we are going to mtry range through values 1-13.. and record the errors


mtry = 4

obb.error = double(13)
test.error = double(13)
for(mtry in 1:13){
  fit = randomForest(medv ~. , data = Boston, subset = train, mtry = mtry, ntree = 400)
  obb.error[mtry] = fit$mse[400]
  pred = predict(fit, Boston[-train,])
  test.error[mtry] = with(Boston[-train,], mean((medv-pred)^2))
  cat(mtry, " ")
}

matplot(1:mtry, cbind(test.error,obb.error), pch = 19, col = c("red","blue"), type = "b", ylab = "Mean Squared Error")

legend("topright", legend = c("OOB","Test"), pch = 19, col = c("red","blue"))
## pch = plotting character

##result shows OOB works best around 8 and Test works good for mtry = 4


########## boosting

# gradient boosted machines

# GBM asks for distribution which Gaussian as we are doing squared error loss

#Boosting builds lots of smaller trees. Unlike random forests, each new tree in boosting tries to patch up the deficiencies of the current ensemble.


require(gbm)
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=10000,shrinkage=0.01,interaction.depth=4)        # shrinkage here is lambda
summary(boost.boston)             # summary gives the importance of variable plot.
plot(boost.boston,i="lstat")    # lower status people in suburb, lower the housing prices
plot(boost.boston,i="rm")       # avg no. of romms increases price increases



## ???? bit of work to be done in terms of running cross validation to select the no. of trees.. this kind of work has to be done with boosting to make good models


###



n.trees=seq(from=100,to=10000,by=100)
predmat=predict(boost.boston,newdata=Boston[-train,],n.trees=n.trees)
dim(predmat)
berr=with(Boston[-train,],apply( (predmat-medv)^2,2,mean))
plot(n.trees,berr,pch=19,ylab="Mean Squared Error", xlab="# Trees",main="Boosting Test Error")    ## boosting is reluctant to overfit.

#now including best test error from the random forest
abline(h=min(test.error),col="red")


## boosting is good but requires lot of tunning of the data


## random forest is easy they won't overfit,(increasing the no. of trees, it won't overfit but stabilizes) only tuning parameter is mtry.


























































