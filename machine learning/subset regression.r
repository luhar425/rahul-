# installing packages
install.packages("ISLR2")
library(ISLR2)
library(leaps)
library(class)
data(Boston)
head(Boston)
set.seed(122)

#spiltting the data into training and test data set
inde<- sample(1:nrow(Boston),80, nrow(Boston))
bos_train <- Boston[inde,]
bos_test <- Boston[-inde,]

#best susbset selection regression
mod <- regsubsets(medv~., data = bos_train, nvmax = 14)
coef(mod ,12)
summ<- summary(mod)
plot(mod)


The above code does best susbsets selection regression
# model performance for subset regression  
summ$cp
summ$bic
summ$adjr2
summ$aic
which.min(summ$cp)
 [1 ] 10
> which.min(summ$bic)
[1] 6
> which.max(summ$adjr2)
[1] 10
par(mfrow = c(2,2))
plot(summ$cp,xlab = "no of variables" ,ylab = "cp")
plot(summ$bic, xlab = "no of variables", ylab = "bic")



## predict function for using it in cross validation
predict.regsubsets = function(object, newdata, id, ...) {
    form = as.formula(object$call[[2]])
    mat = model.matrix(form, newdata)
    coefi = coef(object, id = id)
    mat[, names(coefi)] %*% coefi
}


#cross validation ten fold
k=10
set.seed(111)
folds=sample (1:k,nrow(Boston),replace=TRUE)
cv.errors =matrix (NA,k,14, dimnames =list(NULL , paste (1:14) ))



for(j in 1:k){
  best.fit=regsubsets (medv~.,data=Boston [folds!=j,],
nvmax=12)
  for(i in 1:12){
  pred=predict (best.fit ,Boston[folds ==j,],id=i)
  cv.errors[j,i]= mean( ( Boston$medv[ folds==j]-pred)^2)
  }
  }

mean.cv.errors=apply(cv.errors ,2, mean)
mean.cv.errors
  1        2        3        4        5        6        7        8 
38.42918 30.42098 27.25540 27.46147 25.12892 25.21255 24.86169 24.36261 
       9       10       11       12       13       14 
24.76870 24.50679 23.13940 23.37255
par(mfrow=c(1,1))
plot(mean.cv.errors)












              #cross validation for five fold
k=5
set.seed(111)
folds=sample (1:k,nrow(Boston),replace=TRUE)
cv.errors =matrix (NA,k,14, dimnames =list(NULL , paste (1:14) ))


for(j in 1:k){
  best.fit=regsubsets (medv~.,data=Boston [folds!=j,],
nvmax=12)
  for(i in 1:12){
  pred=predict (best.fit ,Boston[folds ==j,],id=i)
  cv.errors[j,i]= mean( ( Boston$medv[ folds==j]-pred)^2)
  }
  }

mean.cv.errors=apply(cv.errors ,2, mean)
mean.cv.errors
1        2        3        4        5        6        7        8 
38.83885 31.20527 28.36427 28.39136 26.02643 25.91331 26.10169 25.95088 
       9       10       11       12       13       14 
25.65319 25.15751 24.12820 24.25354    

par(mfrow=c(1,1))
plot(mean.cv.errors)



# bootstrap error 
install.packages("bootstrap")
library(bootstrap)
beta.fit <- function(X,Y){
	lsfit(X,Y)	
}

beta.predict <- function(fit, X){
	cbind(1,X)%*%fit$coef
}

sq.error <- function(Y,Yhat){
	(Y-Yhat)^2
}

# Create X and Y
X <- Boston[,1:12]
Y <- Boston[,13]


# Generalize it, and search over the best possible subsets of size "k"
error_store <- c()
for (i in 1:6){
	# Pull out the model
	temp <- which(select[i,] == "*")
	res <- bootpred(X[,temp], Y, nboot = 50, theta.fit = beta.fit, theta.predict = beta.predict, err.meas = sq.error) 
	error_store <- c(error_store, res[[3]])
	
}

which.min(error_store)
[1] 10

