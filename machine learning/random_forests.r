#loading the data and creating test and train data 
Install.pacakges(“random.Forest”)
Library(randomForest)
load("spam.RData")
r_spa = spam
set.seed(124)
test_ind <- sample(1:nrow(r_spa), .20*nrow(r_spa))
test <- r_spam[test_ind,]
train <- r_spam[-test_ind,]
y_true = as.numeric(test$spam)-1
m <- c(5,10,15,20,25,30,35,40,45)
miscall_fin <- c()
rand <- c()

#exploring the sensitivity of m using random Forest (the number of randomly selected inputs)
for (i in m) {
  r_fit <- randomForest(spam~., data = train, mtry = i, ntree = 1000)
  rand <- c(rand,r_fit[4]$err.rate[1])
  y_pred = predict(r_fit, newdata = test, type = "class")
  y_pred <- as.numeric(y_pred)-1
  miscall_fit <- sum(abs(y_true- y_pred))/length(y_pred)
  miscall_fin <- c(miscall_final,miscall_fit)
}
rand
output:
[1] 0.10503282 0.09761550 0.09941520 0.10874357 0.11382114 0.10534125 0.07988381
[8] 0.09866469 0.10574713

miscall_fin
output:
[1] 0.07500000 0.08043478 0.07500000 0.07826087 0.07391304 0.07826087
 [7] 0.07391304 0.08260870 0.07934783 0.05326087
data.frame(cbind(rand,miscall_fin))
output:
         rand miscall_fin
1  0.10503282  0.07500000
2  0.09761550  0.08043478
3  0.09941520  0.07500000
4  0.10874357  0.07826087
5  0.11382114  0.07391304
6  0.10534125  0.07826087
7  0.07988381  0.07391304
8  0.09866469  0.08260870
9  0.10574713  0.07934783
10 0.10503282  0.05326087
Warning message:
In cbind(rand, miscall_fin) :
  number of rows of result is not a multiple of vector length (arg 1)

#plotting both OOB error and test error
plot(m,rand, col = "blue", type = "l",xlab = "values of m",ylab ="error")
output:


par(new = True)
plot(miscall_fin, col = "red", type = "l")
output:
