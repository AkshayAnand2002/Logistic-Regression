#LOGISTIC REGRESSION WITH SINGLE PREDICTOR
glm.fit = glm(Sold~price,data=df,family=binomial)
##family=binomial tells we are running a logistic regression
summary(glm.fit)
##we get summary as we got in linear regression.
#Relation of price vs sold can be seen from above commands
#same as done in linear regression and previous exercise on carsdata.xlsx
#y=b0+b1x; b0 is intercept and b1 is coefficient of x
##we got estmate i.e. values of b0 and b1 as non-zero by maximum likelihood
#method for doing logistic regression as done above.
##we still need to check if b0 and b1 are non-zero or not as there is still a chance
##they can be zero.
#LOGISTIC REGRESSION WITH MULTIPLE REGRESSION
glm.fit = glm(Sold~.,data=df,family=binomial)
summary(glm.fit)
glm.probs = predict(glm.fit,type="response")
glm.probs[1:10]
#USING BOUNDARY CONDITION WE CAN ASSIGN CLASSES TO EACH VALUE.
#EG: ANY PROBABILITY VALUE > 0.5 CLASSIFY AS 'YES' AND 'NO' OTHERWISE.
glm.pred = rep("NO",506)## array of 506values and all values are "NO".
glm.pred[glm.probe > 0.5 ] ="YES"
##for all values of probability > 0.5 change no to yes
##below command to print confusion matrix
table(glm.pred,df$Sold)
##pred as rows and df$Sold as columns
#LINEAR DISCRIMINANT ANALYSIS ASSIGNS CONDITIONAL PROBABAILTY TO ALL CLASSES AND
#ASSIGN THE CLASS WITH HIGHEST PROBABILITY.
library(MASS)
lda.fit = lda(Sold~.,data=df)
#sold is dependent variable and all other variables are predictor variables.
lda.fit
##we get prior probabilities of group,group means and coefficients of linear discrminants
lda.pred= predict(lda.fit,df)
lda.pred$posterior##to get posterior part
lda.class = lda.pred$class
table(lda.class,df$Sold)
##from above command we get confusion matrix .
##we can compare confusion matrix of logistic reg as well as lda.
sum(lda.pred$posterior[,1]>0.8)
##posterior consists of probabilities of class 0 and class 1.
#from previous command we get no. of class 1 elements whose predicted probability
# is greater than 0.8
##for quadratic discrimant analysis we replace lda by qda.
#THE ABOVE COMMANDS
#library(MASS)
#lda.fit = lda(Sold~., data = df)#FIT MODEL BY LDA FUNC
#lda.fit
#lda.pred = predict(lda.fit, df)#PREDICT PROBABILTY
#lda.pred$posterior##USE POSTERIOR PROBABILITY TO ASSIGN CLASS BASED ON SOME 
#BOUNDARY CONDITION.
#lda.class = lda.pred$class
#table(lda.class, df$Sold)
#sum(lda.pred$posterior[ ,1]>0.8)
#training error--performance of model on previously seen data.
#test error --performance of model on unseen data. 
#Test-train split
library(caTools)
set.seed(0)
split=sample.split(df,SplitRatio = 0.8)
train_set=subset(df,split==TRUE)
test_set=subset(df,split == FALSE)
##train the model using train set and create confusion matrix by test set
##logistic regression model
train.fit = glm(Sold~.,data=train_set,family = binomial)
##above contains info for logistic regression model
##using predicted probability for test_set use predict function
test.probs = predict(train.fit,test_set,type='response')
test.pred = rep('NO',120)
##declaring an array with all 120 values as NO so that later some values will change to
#yes when we assign condition. 120 here is the size of test_set.
test.pred[test.probs >0.5] = 'YES'
##whereever probability is 0.5 is assigned the value 'YES'.
table(test.pred,test_set$Sold)
##78 correct prediction .
##below is knn in R.
install.packages("class")
library(class)
trainX = train_set[,-16] # to remove 16th column of sold.
testX= test_set[,-16]
trainy = train_set$Sold
testy = test_set$Sold
k=3#assign a k value
trainX_s = scale(trainX)##to standardize the value use scale
testX_s = scale(testX)
set.seed(0)##to get same results when run different times
knn.pred = knn(trainX_s,testX_s,trainy,k=k)
table(knn.pred,testy)
##66 out of 120 correct prediction
k=1
knn.pred = knn(trainX_s,testX_s,trainy,k=k)##to get predicted values
table(knn.pred,testy)
##now 59 correct predictions on testset
