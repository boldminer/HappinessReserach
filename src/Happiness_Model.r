#Name: Guannan Liu
#Student Id: 2286215
#Email: Liu308@mail.chapman.edu
#Course: CS510
#Assignment: Midterm project 

####What is this?####

#This is the project draft for CS510. This project was created in 2018 as the final of my first r course. 
#Three goals:
#1, apply the best pratice 
#2, Update the most recent data
#Fix the mistakes

####
#This dataset measures the happiness rank and score among 155 countries globally based on a several factors such as,
#Economy, generosity, life expectancy, family, trust in government,  and freedom. 
#The happiness score is a sum of these factors and the higher the happiness rank, the higher the happiness score.




#### Loads required packages ####
packages <- c("tree", "gbm","randomForest","testthat","rmarkdown","MVN")

package.check <- lapply(
  packages,
  FUN <- function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE, repos='http://cran.us.r-project.org')
      library(x, character.only = TRUE)
    }
  }
)

####import data####
dir()
dat2015<- read.csv("../data/2015.csv")
dat2016 <- read.csv("../data/2016.csv")
dat2017 <- read.csv("../data/2017.csv")
dat2018 <- read.csv("../data/2018.csv")
dat2019 <- read.csv("../data/2019.csv")

#clean data
dat1 = dat2019[,c(3:9)] #delete unsless variable: Country.or.region, country and rank
dat1 = na.omit(dat1)
#check data
head(dat1)
size=dim(dat1)
mvn(dat1,  mvnTest = "hz", 
    univariatePlot = "qq", multivariatePlot = "qq",
    multivariateOutlierMethod = "none",
    showOutliers = TRUE, showNewData = TRUE) #Check the Multivariate & Univariate Normality
cor(dat1)
# GDP.per.capita is the most significant, Healthy.life.expectancy to make life choices is the second

#create a training and test set to test for later use:
set.seed(1)
train = sample(1:size[1],size[1]/2)
dat.train = dat1[train,]
dim(dat.train)
#
set.seed(1)
dat1.other = dat1[-train,]
valid = sample(1:nrow(dat1.other),size[1]/2)
dat.valid = dat1.other[valid,]
dim(dat.valid)


#Part I:
#Continuous Dependent Variable - Look at the Happiness score
#elementary linear regression
reg1 = lm(Score~., data = dat1)
options(scipen = 999)
boxplot(dat1$Score)
summary(reg1)
plot(reg1)
yhat.all = predict(reg1, dat1)
MSE.all = mean((dat1$Score-yhat.all)^2)
MSE.all 
#This linear model suggests GDP,SicialSUpport, freedom and LifeExpectancy is significant
#with a overall Adjusted R-squared:  0.7703 
# MSE is 0.27187

#what about a small model, which only look at the top 2 factor
reg2 <- lm(Score~Social.support+Freedom.to.make.life.choices, data = dat1)
anova(reg2, reg1, test = "Chisq")
# P-value is near zero Reject H0
# Reg1 is better


#tree regression:
set.seed(1)
tree.all = tree(Score~., data = dat1) #take account all factors
plot(tree.all)
text(tree.all,pretty = 0)
tree.all
yhat.tree.all = predict(tree.all, dat1)
yhat.tree.all
MSE.tree.all = mean((dat1$Score-yhat.tree.all)^2)
MSE.tree.all
# MSE is 0.1921395

#prune the tree to get rid off some branch :
prune.tree.all = prune.tree(tree.all)
prune.tree.all
plot(prune.tree.all)

prune.tree = prune.tree(tree.all,best = 4) # we only look 6 branch
plot(prune.tree)
text(prune.tree, pretty = 0)
yhat.prune.tree = predict(prune.tree, dat1)
MSE.prune.tree  = mean((dat1$Score - yhat.prune.tree)^2)
MSE.prune.tree
#MSE.prune.tree in this case = 0.3107576 There is an increase in pruned tree, but easier to interpretation

#(2)finding the best pruned tree using cross-validation on the training set:
set.seed(1)
tree.train = tree(Score~., data = dat.train)
plot(tree.train)
text(tree.train,pretty = 0)
tree <- predict(tree.train, dat.valid)
mean((dat.valid$Score - tree)^2)
#
prune.tree.train = prune.tree(tree.train)
plot(prune.tree.train)
#
cv.tree.train = cv.tree(tree.train)
plot(cv.tree.train$size,cv.tree.train$dev,type = "b") #we notice the accuracy reach the best level when there is 5 branches
#We will choose 5 branches.

prune.tree.5 = prune.tree(tree.train,best=5)
plot(prune.tree.5)
text(prune.tree.5,pretty = 0)
#
yhat.prune.tree.5 = predict(tree.train,dat.train)
plot(yhat.prune.tree.5,dat.train$Score)
abline(0,1) 
MSE.prune.tree.5 = mean((dat.train$Score - yhat.prune.tree.5)^2)
MSE.prune.tree.5
#MSE.prune.tree.5 = 0.195074


#the MSE to the test data set:
yhat.tree.valid = predict(tree.train,dat.valid)
MSE.prune.tree.5.valid = mean((dat.valid$Score - yhat.tree.valid)^2)
MSE.prune.tree.5.valid
#MSE.prune.tree.5.valid = 0.47322


#bagging:
set.seed(1)
bag.happiness = randomForest(Score~., data = dat1, subset = train,
                             mtry = 11,importance = TRUE)
bag.happiness
importance(bag.happiness)
#                             %IncMSE IncNodePurity
#GDP.per.capita               20.9539698     30.989430
#Social.support               13.7814449     20.783324
#Healthy.life.expectancy      21.4888365     30.262989
#Freedom.to.make.life.choices 10.8448485      5.517545
#Generosity                   -0.5548402      2.958512
#Perceptions.of.corruption     4.8664215      3.325306

yhat.bag = predict(bag.happiness,dat.valid)
MSE.bag.test = mean((dat.valid$Score - yhat.bag)^2)
MSE.bag.test
#test.MSE.bag = 0.3101329

#Generalized Boosted Regression Models
boost <- gbm(Score~., data = dat.train, distribution = "gaussian", 
             n.trees = 5000, interaction.depth = 4)
summary(boost)
yhat.boost <- predict(boost, dat.valid, n.trees=5000)
MSE.boost <- mean((dat.valid$Score - yhat.boost)^2)
MSE.boost
#test MSE = 0.420676


#Part II: 
#Look at the created indicator "high", which stand for happier of not. 
#Create an indicator variable with logistic regression 
high<-ifelse(dat1$Score>5,1,0)    #we took the threshold of happiness score = 5, because the over all median is 5.3795
dat2 <- cbind(dat1,high)
names(dat2)
dat2 <- dat2[,c(8,2:7)]
dat2$high <- as.factor(dat2$high)   #factorize the indicator variable "high"
logregall<-glm(high~.,data = dat2,family = "binomial")    #use regression to see which factor will make a country lay in Higher happiness group
summary(logregall)
# Null deviance: 206.912  on 155  degrees of freedom
# Residual deviance:  72.576  on 149  degrees of freedom
# AIC: 86.576
yhat.logregall<-predict(logregall,dat2,type="response")
yhat.logregall.class<-ifelse(yhat.logregall>0.5,1,0)
tab.logregall<-table(dat2$high,yhat.logregall.class,dnn=c("Actual","Predicted"))
tab.logregall    #check the prediction power
mean(dat2$high != yhat.logregall.class)
# The error rate is 0.09615385(9.61%).

#Create an indicator variable with tree model
tree.high <-tree(high~.,data=dat2)
plot(tree.high)
text(tree.high,pretty=0)
summary(tree.high)
tree.high
# The tree has a number of 9 termial nodes. 
# Used variables: social support, perceptions of corruption, life expectancy, gdp, and birth rate
yhat.tree.high <- predict(tree.high, dat2,type="class")
tree.high.tab<-table(dat2$high,yhat.tree.high,dnn=c("Actual","Predicted"))
tree.high.tab
mean(dat2$high != yhat.tree.high)
# The error rate is 0.05128205(5.12%),
cv.high <- cv.tree(tree.high)
plot(cv.high$size,cv.high$dev,type="b")    # pick 4
prune.high<-prune.tree(tree.high, best = 5)
plot(prune.high)
text(prune.high, pretty=0)
yhat.prune.high <- predict(prune.high, dat2,type="class")
tab.high<-table(dat2$high,yhat.prune.high,dnn=c("Actual","Predicted"))
tab.high
mean(dat2$high != yhat.prune.high)
# The prediction error rate is (0.07051282) 0.0705%
# The tree method has a lower error rate.( 0.0708% than 9.61) It performs better than the logistic regression on 

#Create a training dataset and test dataset
set.seed(2)
train2 <- sample(1:size[1],size[1]/2)
dat.train2 <- dat2[train2,]
dat.test2 <- dat2[-train2,]

logregall.2<-glm(high~.,data = dat.train2,family = "binomial")
summary(logregall.2)
yhat.logregall.2<-predict(logregall,dat.test2,type="response")
yhat.logregall.class.2<-ifelse(yhat.logregall.2>0.5,1,0)
tab.logregall.2<-table(dat.test2$high,yhat.logregall.class.2,dnn=c("Actual","Predicted"))
tab.logregall.2
mean(dat.test2$high != yhat.logregall.class.2)
#Error rate of 0.1153846, 11.5384%

tree.high.train <-tree(high~.,data=dat.train2)
plot(tree.high.train)
text(tree.high.train,pretty=0)
summary(tree.high.train)
tree.train.2 <- predict(tree.high.train, dat.test2,type="class")
mean(dat.test2$high != tree.train.2)
# Test Error rate: 14.10256%
# Misclassification error rate: 0.07692 in training
# The tree has a number of 5 termial nodes. 
# Used variables: "Social.support"          "GDP.per.capita"          "Healthy.life.expectancy"

cv.high.train <- cv.tree(tree.high.train, FUN = prune.misclass)
plot(cv.high.train$size,cv.high.train$dev,type="b")
# pick 3 since the deviance is more stable after 2 splits.

prune.high.train<-prune.tree(tree.high.train, best = 3)
plot(prune.high.train)
text(prune.high.train, pretty=0)
yhat.prune.high.test <- predict(prune.high.train, dat.test2,type="class")
tab.high.test<-table(dat.test2$high,yhat.prune.high.test,dnn=c("Actual","Predicted"))
tab.high.test
mean(dat.test2$high != yhat.prune.high.test)
# The error rate is 15.38462%
# The prune tree perform wrose on the test dataset in terms of the classification error. 

#Bagging
bag.high <- randomForest(high~., data=dat.train2, mtry = 6,importance=TRUE)
bag.high
varImpPlot(bag.high)
#Number of trees: 500
#No. of variables tried at each split: 6
#error rate:8.97%
#Confusion matrix:
#    0  1  class.error
#0  27  4  0.12903226
#1   3 44  0.06382979
#The most important variable: social support

yhat.bag.high <- predict(bag.high,dat.test2, type = "class")
tab.bag.high.test<-table(dat.test2$high,yhat.bag.high, dnn=c("Actual","Predicted"))
tab.bag.high.test
mean(dat.test2$high != yhat.bag.high)
# The error rate is 0.08974359, 8.9744%

RF.high <- randomForest(high~., data=dat.train2, mtry = 6, importance=TRUE)
RF.high
varImpPlot(RF.high)
#Number of trees: 500
#No. of variables tried at each split: 6
#error rate:7.69%
#    Confusion matrix:
#      0   1 class.error
#    0 28  3  0.09677419
#    1  3 44  0.06382979
#The most important variable: social support and freedom to make life choices 

yhat.rf.high <- predict(RF.high,dat.test2, type="class")
tab.rf.high.test<-table(dat.test2$high,yhat.rf.high, dnn=c("Actual","Predicted"))
tab.rf.high.test
mean(dat.test2$high != yhat.rf.high)
# The error rate is 0.08974359，8.9743%

#Generalized Boosted Regression Models
dat.train2[,1] <- as.numeric(dat.train2[,1])-1
str(dat.train2)
dat.test2[,1] <- as.numeric(dat.test2[,1])-1
str(dat.test2)
boost.high <- gbm(high~., data=dat.train2, distribution = "bernoulli", 
                  n.trees = 500, interaction.depth = 4)
boost.high
summary(boost.high)
#                                                              var     rel.inf
#Social.support                                       Social.support  43.691770
#Healthy.life.expectancy                     Healthy.life.expectancy  31.482370
#GDP.per.capita                                       GDP.per.capita  15.762046
#Perceptions.of.corruption                 Perceptions.of.corruption  4.681847
#Generosity                                               Generosity   2.651387
#Freedom.to.make.life.choices           Freedom.to.make.life.choices   1.730581
yhat.boost.high <- predict(boost.high, dat.test2, n.trees=500, type = "response")
yhat.boost.class<-ifelse(yhat.boost.high>0.5,1,0)
tab.boost<-table(dat.test2$high,yhat.boost.class, dnn=c("Actual","Predicted"))
tab.boost
mean(dat.test2$high != yhat.boost.class)
# 20.00%

####END####

