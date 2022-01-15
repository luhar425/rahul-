library(rpart)
library(rpart.plot)
load("cleveland.rdata")
head(cleveland)
dat <- subset(cleveland, select = -diag2)
set.seed(1)
train = sample(1:nrow(dat), nrow(dat)*.80)
test = -train
cleveland_train <- dat[train,]
cleveland_test <- dat[test,]
cleve_train_class <- dat$diag1[train]
cleve_test_class <- dat$diag1[test]

#plotting the cart model 
model.cont <- rpart.control(minbucket = 2, minsplit = 4, xval = 10, cp = 0)
fit_cleveland <- rpart(cleve_train_class~.,data = cleveland_train,method="class",control = model.cont)
min_cp = which.min(fit_cleveland$cptable[,4])
pruned_fit_cleveland <- prune(fit_cleveland, cp = fit_cleveland$cptable[min_cp, 1])
graphics.off()
x11()
plot(fit_cleveland,  main = "Full_Tree")
text(fit_cleveland,use.n=TRUE,cex = .5)
plot(pruned_fit_cleveland,  main = "Pruned_Tree")
text(pruned_fit_cleveland,use.n=TRUE,cex = .5)
output:



#calculating the test error 
pred.full_tree <- predict(fit_cleveland, cleveland_test)
pred.pruned_tree <- predict(pruned_fit_cleveland, cleveland_test)
pred.full_tree_frame<-as.data.frame(pred.full_tree)
predict.pruned_tree_frame<-as.data.frame(pred.pruned_tree)
y_true_test <- cleve_test_class


predicted<-pred.full_tree_frame %>%
 mutate(diag1 = case_when(
pred.full_tree_frame$buff > pred.full_tree_frame$sick ~ "buff",
pred.full_tree_frame$buff < pred.full_tree_frame$sick ~ "sick"
))

predicted<-pred.full_tree_frame %>%
 mutate(diag1 = case_when(
pred.full_tree_frame$buff > pred.full_tree_frame$sick ~ "buff",
pred.full_tree_frame$buff < pred.full_tree_frame$sick ~ "sick"
))

x<- which(predicted[,3]!=y_true_test)
length(x)
error<-length(x)/length(predicted[,3])
summary(pred.full_tree)

predict.pruned_tree_frame<-as.data.frame(pred.pruned_tree)
predicted_prune_tree<-predict.pruned_tree_frame %>%
 mutate(diag1 = case_when(
predict.pruned_tree_frame$buff > predict.pruned_tree_frame$sick ~ "buff",
predict.pruned_tree_frame$buff < predict.pruned_tree_frame$sick ~ "buff"
))

x<- which(predicted_prune_tree[,3]!=y_true_test)
error<-length(x)/length(predicted_prune_tree[,3])
[1] 0.4166667

#random forest
install.packages("randomForest")
library(randomForest)
r.fit <- randomForest(cleve_train_class~., data = cleveland_train, ntree = 10000)

x11()
varImpPlot(r.fit)

importance(r.fit)
output:
   MeanDecreaseGini
age             3.0105894
gender          1.9021018
cp              8.0632491
trestbps        2.7302223
chol            2.8079690
fbs             0.3310942
restecg         0.8368307
thalach         6.8550358
exang           3.0575015
oldpeak         6.1778835
slope           2.4177836
ca              6.6922072
thal            9.5724105
diag1          62.3782566

y_hat <- predict(r.fit, newdata = (cleveland_test), type = "response")
y_hat <- as.numeric(y_hat)-1
misclass_rf <- sum(abs(as.numeric(y_true_test) - y_hat))/length(y_hat)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
output:




#tuning the random forest
customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)

}

customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
   predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
   predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes
mtry <- sqrt(ncol(dat))
tunegrid <- expand.grid(.mtry=mtry)

customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)

}

customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
   predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
   predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes
mtry <- sqrt(ncol(dat))
tunegrid <- expand.grid(.mtry=mtry)
.mtry .ntree
output:
1      1   1000
2      2   1000
3      3   1000
4      4   1000
5      5   1000
6      6   1000
7      7   1000
8      8   1000
9      9   1000
10    10   1000
11    11   1000
12    12   1000
13    13   1000
14    14   1000
15    15   1000
16     1   1500
17     2   1500
18     3   1500
19     4   1500
20     5   1500
21     6   1500
22     7   1500
23     8   1500
24     9   1500
25    10   1500
26    11   1500
27    12   1500
28    13   1500
29    14   1500
30    15   1500
31     1   2000
32     2   2000
33     3   2000
34     4   2000
35     5   2000
36     6   2000
37     7   2000
38     8   2000
39     9   2000
40    10   2000
41    11   2000
42    12   2000
43    13   2000
44    14   2000
45    15   2000
46     1   2500
47     2   2500
48     3   2500
49     4   2500
50     5   2500
51     6   2500
52     7   2500
53     8   2500
54     9   2500
55    10   2500
56    11   2500
57    12   2500
58    13   2500
59    14   2500
60    15   2500
>
#In order to represent factor variables, we need to convert them into dummy variables.
# A dummy variable takes the N distinct values and converts it into N-1 variables. 
#We use N-1 because the final value is represented by all dummy values set to zero
table(dat$gender)
head(model.matrix(~gender, data=dat))
dat_matrix<-model.matrix(~age+gender+cp+trestbps+chol+fbs+restecg+thalach+exang+oldpeak+slope+ca+thal+diag1, data=dat) 
colnames(dat_matrix) 
col_list <- paste(c(colnames(dat_matrix[,-c(1,20)])),collapse="+") 
col_list <- paste(c("diag1sick~",col_list),collapse="") 
f <- formula(col_list)  
#neural networks
library(neuralnet)
set.seed(2)
train = sample(1:nrow(dat_matrix), nrow(dat_matrix)*.80)
test = -train
cleveland_train <- dat_matrix[train,]
cleveland_test <- dat_matrix[test,]
output<- compute(nmod, dat_matrix[,-c(1,20)],rep=1)
nmod <- neuralnet(f,data=cleveland_train,hidden=c(2,1),
                    linear.output=FALSE,threshold=0.01)
plot(nmod)
output:


results <- neuralnet::compute(nmod, cleveland_test)
result <- data.frame(actual = cleveland_test[,20], prediction = results$net.result)
#confusion matrix
roundedres<-sapply(result,round,digits=0)
rounded=data.frame(roundedres)
attach(rounded)
table(actual,prediction)
mean(actual==prediction)
0.8166667
set.seed(450)
cv.error <- NULL
k <- 10
library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)
for(i in 1:k){
    index <- sample(1:nrow(dat_matrix),round(0.9*nrow(dat_matrix)))
    train.cv <- dat_matrix[index,]
    test.cv <- dat_matrix[-index,]
    h <- summary(test.cv)
    nn <- neuralnet(f,data=train.cv,hidden=c(5,2),linear.output=T)   
    res <- neuralnet::compute(nn, test.cv[,1:19])
    pr.nn <- res$net.result*(max(dat$diag1)-min(dat$diag1))+min(dat$diag1)  
    test.cv.r <- (test.cv$diag1*(max(dat$diag1)-min(dat$diag1))+min(dat$diag1)   
    cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)    
    pbar$step()
}

Comments:
Of the three models , Random forest performs better and next is neural network  and the last is Cart 
