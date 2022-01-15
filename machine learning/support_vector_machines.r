a)Create a training set containing a random sample of 800 observations, and a test set containing the remaining observations.

require(ISLR); require(tidyverse); require(ggthemes)
require(caret); require(e1071)
set.seed(1)
data('OJ')
dat <- sample(nrow(OJ), 800, replace = FALSE)
train <- OJ[dat,]
test <- OJ[-dat,]

b)Fit a support vector classifier to the training data using cost=0.01, with Purchase as the response and the other variables as predictors. Use the summary() function to produce summary statistics, and describe the results obtained.
s_lin <- svm(Purchase ~ ., data = train,
                  kernel = 'linear',
                  cost = 0.01)
summary(s_lin)
output:
Call:
svm(formula = Purchase ~ ., data = train, kernel = "linear", cost = 0.01)
Parameters:
   SVM-Type:  C-classification 
 SVM-Kernel:  linear 
       cost:  0.01 
Number of Support Vectors:  435
 ( 219 216 )
Number of Classes:  2 
Levels: 
 CH MM

c) What are the training and test error rates?
postResample(predict(s_lin, train), train$Purchase)
output:
 Accuracy     Kappa 
0.8250000 0.6313971 

 postResample(predict(s_lin, test), test$Purchase)
output:
 Accuracy     Kappa 
0.8222222 0.6082699 

d) Use the tune() function to select an optimal cost. Consider values in the range 0.01 to 10.

s_lin_tune <- train(Purchase ~ ., data = train,
                         method = 'svmLinear2',
                         trControl = trainControl(method = 'cv', number = 10),
                         preProcess = c('center', 'scale'),
                         tuneGrid = expand.grid(cost = seq(0.01, 10, length.out = 20)))
s_lin_tune
output:
Support Vector Machines with Linear Kernel 
800 samples
 17 predictor
  2 classes: 'CH', 'MM' 
Pre-processing: centered (17), scaled (17) 
Resampling: Cross-Validated (10 fold) 
Summary of sample sizes: 721, 720, 720, 720, 721, 719, ... 
Resampling results across tuning parameters:

  cost        Accuracy   Kappa    
   0.0100000  0.8199215  0.6202565
   0.5357895  0.8273760  0.6360834
   1.0615789  0.8236101  0.6284665
   1.5873684  0.8261105  0.6333280
   2.1131579  0.8261105  0.6333280
   2.6389474  0.8273605  0.6362121
   3.1647368  0.8261105  0.6338114
   3.6905263  0.8248605  0.6309732
   4.2163158  0.8248605  0.6309732
   4.7421053  0.8261105  0.6338114
   5.2678947  0.8273605  0.6361662
   5.7936842  0.8273605  0.6361662
   6.3194737  0.8260947  0.6331693
   6.8452632  0.8260947  0.6331693
   7.3710526  0.8260947  0.6331693
   7.8968421  0.8273605  0.6361662
   8.4226316  0.8273605  0.6361662
   8.9484211  0.8273605  0.6361662
   9.4742105  0.8248447  0.6308145
  10.0000000  0.8248447  0.6308145

Accuracy was used to select the optimal model using the largest value.
The final value used for the model was cost = 0.5357895.

e) Compute the training and test error rates using this new value for cost.

 postResample(predict(s_lin_tune, train), train$Purchase)
output:
 Accuracy     Kappa 
0.8350000 0.6524601 
 
 postResample(predict(s_lin_tune, test), test$Purchase)
output:
 Accuracy     Kappa 
0.8444444 0.6585983

f) Repeat parts (b) through (e) using a support vector machine with a radial kernel. Use the default value for gamma.
svm_rad <- svm(Purchase ~ ., data = train,
                  method = 'radial',
                  cost = 0.01)
summary(svm_rad)
output:
Call:
svm(formula = Purchase ~ ., data = train, method = "radial", cost = 0.01)
Parameters:
 SVM-Type:  C-classification 
 SVM-Kernel:  radial 

cost:  0.01 
Number of Support Vectors:  634
 ( 319 315 )
Number of Classes:  2 
Levels: 
 CH MM

postResample(predict(svm_rad, train), train$Purchase)
output:
Accuracy    Kappa 
 0.60625  0.00000 

 postResample(predict(svm_rad, test), test$Purchase)
output:
 Accuracy     Kappa 
0.6222222 0.0000000 

g) Repeat parts (b) through (e) using a support vector machine with a polynomial kernel. Set degree=2.
svm_p <- svm(Purchase ~ ., data = train,
                  method = 'polynomial', degree = 2,
                  cost = 0.01)
summary(svm_p)
output:
Call:
svm(formula = Purchase ~ ., data = train, method = "polynomial", 
    degree = 2, cost = 0.01)
Parameters:
   SVM-Type:  C-classification 
 SVM-Kernel:  radial 
       cost:  0.01 
Number of Support Vectors:  634
 ( 319 315 )
Number of Classes:  2 
Levels: 
 CH MM

postResample(predict(svm_p, train), train$Purchase)
output:
Accuracy    Kappa 
 0.60625  0.00000
postResample(predict(svm_p, test), test$Purchase)
output:
Accuracy     Kappa 
0.6222222 0.0000000

svm_poly <- train(Purchase ~ ., data = training,
                         method = 'svmPoly',
                         trControl = trainControl(method = 'cv', number = 10),
                         preProcess = c('center', 'scale'),
                         tuneGrid = expand.grid(degree = 2,
                                         C = seq(0.01, 10, length.out = 20),
                                         scale = TRUE))
svm_poly
output:
Support Vector Machines with Polynomial Kernel 
800 samples
 17 predictor
  2 classes: 'CH', 'MM' 

Pre-processing: centered (17), scaled (17) 
Resampling: Cross-Validated (10 fold) 
Summary of sample sizes: 720, 719, 719, 721, 719, 720, ... 
Resampling results across tuning parameters:
C           Accuracy   Kappa    
   0.0100000  0.8137012  0.5977346
   0.5357895  0.8124979  0.6012454
   1.0615789  0.8149824  0.6062418
   1.5873684  0.8137012  0.6036783
   2.1131579  0.8087162  0.5936895
   2.6389474  0.8087320  0.5937414
   3.1647368  0.8087320  0.5937414
   3.6905263  0.8087320  0.5932934
   4.2163158  0.8087320  0.5938334
   4.7421053  0.8050129  0.5852395
   5.2678947  0.8062633  0.5882348
   5.7936842  0.8075291  0.5913598
   6.3194737  0.8075445  0.5914003
   6.8452632  0.8075287  0.5910842
   7.3710526  0.8050287  0.5863410
   7.8968421  0.8050287  0.5863410
   8.4226316  0.8075133  0.5914988
   8.9484211  0.8075133  0.5914988
   9.4742105  0.8075133  0.5914988
  10.0000000  0.8062633  0.5885986

Tuning parameter 'degree' was held constant at a value of 2
Tuning
 parameter 'scale' was held constant at a value of TRUE
Accuracy was used to select the optimal model using the largest value.
The final values used for the model were degree = 2, scale = TRUE and C
 = 1.061579.

postResample(predict(svm_poly, train), train$Purchase)
output:
Accuracy     Kappa 
0.8550000 0.6928678
postResample(predict(svm_poly, test), test$Purchase)
output:
Accuracy     Kappa 
0.8111111 0.5812808

(h) Overall, which approach seems to give the best results on this data?
Mostly the models are similar , but radial kernel does better by small margin
