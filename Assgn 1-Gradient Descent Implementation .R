library(optimbase)
library(fastDummies)
library(ISLR)
fix(Auto)
attach(Auto)

# Task 1, creating a function for NLL based Gradient Descent

gradientDescent<- function(X,y, weights,learning_rate, epoch){
  m<- length(y)
  
  for (iter in 1:epoch){
    sigma <- X %*% weights
    sigmoid <- 1/(1+exp(-sigma))
    error<- sigmoid-y
    weights<- weights-(learning_rate/m)*(t(X)%*%error)
    #if I use the below 2 lines instead of the one above, the function changes from NLL to MSE
    #sigmoid_de<- sigmoid*(1-sigmoid)
    #weights<- weights+2*((learning_rate/m)*(t(X)%*%(error*sigmoid_de)))
    
  }
  return(weights)
}

#Task 2, Creating Input and Output variables, normalising and dummy variable creation

high<- (mpg>=25)
y<-zeros(length(high),1)
y[high]<-1
results <- dummy_cols(origin, remove_first_dummy = TRUE)
dataset<-data.frame(horsepower,weight,year)
standardised.data<- scale(dataset[,1:3])
X<- cbind(ones(nrow(y),1),standardised.data,results[,2],results[,3])

#Defining Important Parameter Values, and testing the Gradient descent on the whole training set
weights<- zeros(ncol(X),1)
epoch <- 10000
learning_rate <- 0.05
weights<-runif(ncol(X), min=-0.7, max=0.7)
weights<-gradientDescent(X,y,weights,learning_rate,epoch)

#calculating prediction error on the training set and all misclassificatios

m<- length(y)
h_x<- X%*%weights
predict <- 1/(1+exp(-h_x))
probs<- (predict>0.5)
y_pred<-zeros(m,1)
y_pred[probs]<-1
print(((1/m)*sum((y-y_pred)^2))^0.5)
print(sum(y_pred!= y))

#Task 3: Splitting the data into Training set and Test set (a split in half)

size <- floor(0.5 * nrow(X))
set.seed(2810)
train_ind <- sample(seq_len(nrow(X)), size = size)
X_train <- X[train_ind, ]
X_test <- X[-train_ind, ]
y_train <- y[train_ind ]
y_test <- y[-train_ind]

#Task 4: Running the algorithm and calculating calculating MSE on Test Set 

initial_weights<-runif(ncol(X), min=-0.7, max=0.7)
epoch <- 2000
learning_rate <- 0.3
weights<-gradientDescent(X_train,y_train,initial_weights,learning_rate,epoch)
h_x<- X_train%*%weights
predict <- 1/(1+exp(-h_x))
probs<- (predict>0.5)
y_pred<-zeros(length(y_train),1)
y_pred[probs]<-1
m<- length(y_train)
print(((1/m)*sum((y_train-y_pred)^2))^0.5)
print(sum(y_pred!= y_train))
h_x<- X_test%*%weights
predict <- 1/(1+exp(-h_x))
probs<- (predict>0.5)
y_pred<-zeros(length(y_test),1)
y_pred[probs]<-1
m<- length(y_test)
print(((1/m)*sum((y_test-y_pred)^2))^0.5)
print(sum(y_pred!= y_test))

#Task 6:Running Gradient Descent 100 times with different weight initialisation, and Creating Box plot for it

m<- length(y_test)
diff_error<- zeros(100,1)
for (i in 1:100){
  weights<-runif(ncol(X), min=-0.7, max=0.7)
  weights<-gradientDescent(X_train,y_train,weights,learning_rate,epoch)
  h_x<- X_test%*%weights
  predict <- 1/(1+exp(-h_x))
  probs<- (predict>0.5)
  y_pred<-zeros(length(y_test),1)
  y_pred[probs]<-1
  diff_error[i]<-((1/m)*sum((y_test-y_pred)^2))^0.5
}
boxplot(diff_error)

#(Optional)Task 7:Running the Gradient Algorithm 4 times with different weights and choosing the prediction rule with the best MSE

h_x<- X_train%*%weights
predict <- 1/(1+exp(-h_x))
probs<- (predict>0.5)
y_pred<-zeros(length(y_train),1)
y_pred[probs]<-1
m<- length(y_train)
print(((1/m)*sum((y_train-y_pred)^2))^0.5)
print(sum(y_pred!= y_train))
m<- length(y_train)
diff_error<- zeros(4,1)
diff_weights <-zeros(ncol(X_train),4)
#learning_rate<-0.3; They were already set
#epoch<-2000 ; They were already set
for (i in 1:4){
  weights<-runif(ncol(X_train), min=-0.7, max=0.7)
  diff_weights[,i]<-gradientDescent(X_train,y_train,weights,learning_rate,epoch)
  h_x<- X_train%*%diff_weights[,i]
  predict <- 1/(1+exp(-h_x))
  probs<- (predict>0.5)
  y_pred<-zeros(length(y_train),1)
  y_pred[probs]<-1
  diff_error[i]<-((1/m)*sum((y_train-y_pred)^2))^0.5
}
best_weights<- diff_weights[,which.min(diff_error)]

h_x<- X_test%*%best_weights
predict <- 1/(1+exp(-h_x))
probs<- (predict>0.5)
y_pred<-zeros(length(y_test),1)
y_pred[probs]<-1
m<- length(y_test)
print(((1/m)*sum((y_test-y_pred)^2))^0.5)
print(sum(y_pred!= y_test))

#(Optional) Task 5: Trying different stopping rule- Stopping the algorithm if the reduction in Loss Function is less than a chosen
#but small value

gradientDescent_new<- function(X,y, weights,learning_rate, epoch){
  m<- length(y)
  
  J_history<- zeros(epoch,1)
  
  for (iter in 1:epoch){
    sigma = X %*% weights
    sigmoid = 1/(1+exp(-sigma))
    sigmoid_de<- sigmoid*(1-sigmoid)
    error<- y-sigmoid
    weights<- weights+2*((learning_rate/m)*(t(X)%*%(error*sigmoid_de)))
    J_history[iter]<-CostFunction(X,y,weights)
    if (iter>1 &&( J_history[iter-1]-J_history[iter]<0.00001)){
      break
    }
    
  }
  print(iter)
  return(weights)
}

CostFunction<- function(X , y,weights){
  m <- length(y)
  J<-0
  grad<-zeros(length(weights),1)
  z<-(X%*%weights)
  sigmoid <- 1/(1+exp(-z))
  J<- (1/m)*sum((-y*log(sigmoid))-((1-y)*log(1-sigmoid)))
  return(J)
}

weights<-gradientDescent_new(X_train,y_train,initial_weights,learning_rate,epoch)
h_x<- X_train%*%weights
predict <- 1/(1+exp(-h_x))
probs<- (predict>0.5)
y_pred<-zeros(length(y_train),1)
y_pred[probs]<-1
m<- length(y_train)
print(((1/m)*sum((y_train-y_pred)^2))^0.5)
print(sum(y_pred!= y_train))

h_x<- X_test%*%weights
predict <- 1/(1+exp(-h_x))
probs<- (predict>0.5)
y_pred<-zeros(length(y_test),1)
y_pred[probs]<-1
m<- length(y_test)
print(((1/m)*sum((y_test-y_pred)^2))^0.5)
print(sum(y_pred!= y_test))