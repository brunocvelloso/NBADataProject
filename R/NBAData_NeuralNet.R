#0. Clean up and erase

rm(list=ls())

#1. Load Relevant Packages and Set WD
library(tidyverse)
library(dplyr)
#setwd(working)

#I will implement a neural network manually constructed and compare to
#a pre-used package for 2 simple variables. The idea is to predict the
#outcome of the game (Home Team win) based on the average adjusted net margin of
#the home team and away team

#Thanks to Akshaj Verma as a reference resource which helped with some of the implementation steps: https://rviews.rstudio.com/2020/07/20/shallow-neural-net-from-scratch-using-r-part-1/

fileLinks <- c("allregdata_FINALV2.RData")
for (i in fileLinks) {
  link <- paste0("https://www.dropbox.com/scl/fo/51g9vud1seqtjmyl2gdxt/h/",i,"?rlkey=w73ql5hcbve241kau127t2a2h&dl=1",sep="")
  download.file(link,i)
  load(i)
}
home_marg = seasondata$NetAdjMarg3.x #home team net margin on the season, adjusted for strengths of opponents
away_marg = seasondata$NetAdjMarg3.y #away team net margin on the season, adjusted for strengths of opponents
homewin <- seasondata$HomeWin #0 or 1 dummy if home team wins

#create dataframe
df <- data.frame(home_marg,away_marg,homewin)
df <- df[!is.na(home_marg)&!is.na(away_marg)&!is.na(homewin),] #remove NAs
df <- df[sample(nrow(df),replace=FALSE), ] #shuffle data

#train-test split
split_index <- 0.9 * nrow(df)
train <- df[1:split_index,]
test <- df[(split_index+1): nrow(df),]
X_train <- train[, c("home_marg","away_marg")]
y_train <- train$homewin
dim(y_train) <- c(length(y_train), 1) # add extra dimension to vector
X_test <- scale(test[, c("home_marg","away_marg")])
y_test <-  test$homewin
dim(y_test) <- c(length(y_test), 1) # add extra dimension to vector

#make matrices
X_train <- as.matrix(X_train, byrow=TRUE)
X_train <- t(X_train) #transpose
y_train <- as.matrix(y_train, byrow=TRUE)
y_train <- t(y_train) #transpose
X_test <- as.matrix(X_test, byrow=TRUE)
X_test <- t(X_test) #transpose
y_test <- as.matrix(y_test, byrow=TRUE)
y_test <- t(y_test) #transpose


neuronSize <- function(X, y, hidden_neuron_size, train=TRUE) {
  n_obs <- dim(X)[1] #num observations
  n_neur <- hidden_neuron_size
  n_outcome <- dim(y)[1]
  
  size <- list("n_obs" = n_obs,
               "n_neur" = n_neur,
               "n_outcome" = n_outcome)
  
  return(size)
}

size_params <- neuronSize(X_train, y_train, hidden_neuron_size = 40)
size_params

#create weight matrices/parameters and initialize with uniform distribution to start
getParameters <- function(X, size_params){
  
  m <- dim(data.matrix(X))[2]
  
  n_obs <- size_params$n_obs
  n_neur <- size_params$n_neur
  n_outcome <- size_params$n_outcome
  
  W1 <- matrix(runif(n_neur * n_obs), nrow = n_neur, ncol = n_obs, byrow = TRUE) * 0.01
  b1 <- matrix(rep(0, n_neur), nrow = n_neur)
  W2 <- matrix(runif(n_outcome * n_neur), nrow = n_outcome, ncol = n_neur, byrow = TRUE) * 0.01
  b2 <- matrix(rep(0, n_outcome), nrow = n_outcome)
  
  params <- list("W1" = W1,
                 "b1" = b1, 
                 "W2" = W2,
                 "b2" = b2)
  
  return (params)
}

init_params <- getParameters(X_train, size_params)
lapply(init_params, function(x) dim(x))

#sigmoid activation function for output (will use tanh for hidden layer)
sigmoid <- function(x){
  return(1 / (1 + exp(-x)))
}


forwardPropagation <- function(X, params, size_params){
  
  m <- dim(X)[2] #num X variables
  n_neur <- size_params$n_neur
  n_outcome <- size_params$n_outcome
  
  W1 <- params$W1
  b1 <- params$b1
  W2 <- params$W2
  b2 <- params$b2
  
  b1_new <- matrix(rep(b1, m), nrow = n_neur)
  b2_new <- matrix(rep(b2, m), nrow = n_outcome)
  
  Z1 <- W1 %*% X + b1_new #propogate forward to hidden layer
  A1 <- tanh(Z1)
  Z2 <- W2 %*% A1 + b2_new #propogate forward to outer layer
  A2 <- sigmoid(Z2)
  
  forprop_params <- list("Z1" = Z1,
                "A1" = A1, 
                "Z2" = Z2,
                "A2" = A2)
  
  return (forprop_params)
}

forprop_params <- forwardPropagation(X_train, init_params, size_params)
lapply(forprop_params, function(x) dim(x))

#create Loss function (binary cross entropy)
lossFunction <- function(X, y, forprop_params) {
  m <- dim(X)[2]
  A2 <- forprop_params$A2
  logprobs <- (log(A2) * y) + (log(1-A2) * (1-y))
  loss <- -sum(logprobs/m)
  return (loss)
}
loss <- lossFunction(X_train, y_train, forprop_params)
loss


#here I compute the gradients for loss function
computeGradients <- function(X, y, forprop_params, params, size_params){
  
  m <- dim(X)[2]
  
  n_obs <- size_params$n_obs
  n_neur <- size_params$n_neur
  n_outcome <- size_params$n_outcome
  
  A2 <- forprop_params$A2
  A1 <- forprop_params$A1
  W2 <- params$W2
  
  dZ2 <- A2 - y
  dW2 <- 1/m * (dZ2 %*% t(A1)) 
  db2 <- matrix(1/m * sum(dZ2), nrow = n_outcome)
  db2_new <- matrix(rep(db2, m), nrow = n_outcome)
  
  dZ1 <- (t(W2) %*% dZ2) * (1 - A1^2)
  dW1 <- 1/m * (dZ1 %*% t(X))
  db1 <- matrix(1/m * sum(dZ1), nrow = n_neur)
  db1_new <- matrix(rep(db1, m), nrow = n_neur)
  
  grads <- list("dW1" = dW1, 
                "db1" = db1,
                "dW2" = dW2,
                "db2" = db2)
  
  return(grads)
}

back_prop <- computeGradients(X_train, y_train, forprop_params, init_params, size_params)
lapply(back_prop, function(x) dim(x))


updateWeights <- function(grads, params, learning_rate){
  
  W1 <- params$W1
  b1 <- params$b1
  W2 <- params$W2
  b2 <- params$b2
  
  dW1 <- grads$dW1
  db1 <- grads$db1
  dW2 <- grads$dW2
  db2 <- grads$db2
  
  
  W1 <- W1 - learning_rate * dW1
  b1 <- b1 - learning_rate * db1
  W2 <- W2 - learning_rate * dW2
  b2 <- b2 - learning_rate * db2
  
  updated_params <- list("W1" = W1,
                         "b1" = b1,
                         "W2" = W2,
                         "b2" = b2)
  
  return (updated_params)
}


update_params <- updateWeights(back_prop, init_params, learning_rate = 0.01)
lapply(update_params, function(x) dim(x))

#train the model (i.e. do everything and iterate perpetually)
trainModel <- function(X, y, num_iteration, hidden_neurons, lr){
  
  size_params <- neuronSize(X, y, hidden_neurons)
  init_params <- getParameters(X, size_params)
  loss_history <- c()
  for (i in 1:num_iteration) {
    forprop_params <- forwardPropagation(X, init_params, size_params)
    loss <- lossFunction(X, y, forprop_params)
    back_prop <- computeGradients(X, y, forprop_params, init_params, size_params)
    update_params <- updateWeights(back_prop, init_params, learning_rate = lr)
    init_params <- update_params
    if (i > 1) {
      if (abs(loss - loss_history[(i-1)])<1e-10 & (i>1)) {
        loss_history <- c(loss_history, loss)
        print(paste0("Converge at iteration: ", i,sep=""))
        break
      }
    }
    loss_history <- c(loss_history, loss)
    
    if (i %% 100 == 0) cat("Iteration", i, " | loss: ", loss, " | converge: ", abs(loss - loss_history[(i-1)]), "\n")
    if (i == num_iteration) cat("Iteration finished at ", i, " | loss: ", loss," no converge", "\n\n")
  }
  
  model_out <- list("updated_params" = update_params,
                    "loss_hist" = loss_history)
  return (model_out)
}

#set hyperparameters to for neuralnet
num_iteration = 1000
n_neur = 4
learning_rate = 0.9

#train_model
cat("STEP 1: Training Neur Net Model \n\n")
train_model <- trainModel(X_train, y_train, hidden_neurons = n_neur, num_iteration = num_iteration, lr = learning_rate)

#create reference logistic regression model
logistic_reg <- glm(homewin ~ home_marg + away_marg, data = train)
logistic_reg

logistic_predclass <- round(as.vector(predict(logistic_reg, test[, 1:2])))
logistic_pred <- as.vector(predict(logistic_reg, test[, 1:2]))
logistic_predtrainclass <- round(as.vector(predict(logistic_reg, train[, 1:2])))
logistic_predtrain <- as.vector(predict(logistic_reg, train[, 1:2]))

#make prediction for neural net
makePrediction <- function(X, y, hidden_neurons){
  size_params <- neuronSize(X, y, hidden_neurons)
  params <- train_model$updated_params
  forprop_params <- forwardPropagation(X, params, size_params)
  pred <- forprop_params$A2
  
  return (pred)
}

#both probability and classification prediction
y_pred <- makePrediction(X_test, y_test, n_neur)
y_predclass <- round(y_pred)
y_predtrain <- makePrediction(X_train, y_train, n_neur) #predictions on training data (in-sample)
y_predtrainclass <- round(y_predtrain)




#compare predictions of logarithmic with NN
tb_nn <- table(y_test, y_predclass)
tb_lr <- table(y_test, logistic_predclass)
cat("STEP 2a: Confusion Matrix for NN (out of sample test) \n\n")
print(tb_nn)
cat("\n\n")
cat("\nSTEP 2b: Confusion Matrix for Logistic Reg (out of sample test) \n\n")
print(tb_lr)
cat("\n\n")

cat("STEP 3: Calculate out-of-sample tests and compare results on many factors\n\n")
calculate_stats <- function(tb, model_name,y_test,y_pred) {
  acc <- (tb[1] + tb[4])/(tb[1] + tb[2] + tb[3] + tb[4])
  recall <- tb[4]/(tb[4] + tb[3])
  precision <- tb[4]/(tb[4] + tb[2])
  f1 <- 2 * ((precision * recall) / (precision + recall))
  
  bincrossentr = -1*mean((log(y_pred)*y_test + log(1-y_pred)*(1-y_test)),na.rm=TRUE)
  cat(model_name, " (out of sample test): \n")
  cat("\tAccuracy = ", acc*100, "%.")
  cat("\n\tPrecision = ", precision*100, "%.")
  cat("\n\tRecall = ", recall*100, "%.")
  cat("\n\tF1 Score = ", f1*100, "%.")
  cat("\n\tBin. Cross. Ent.  = ", bincrossentr, "\n\n")
}



calculate_stats(tb_nn,"NN",y_test,y_pred) ##nn out-of-sample
calculate_stats(tb_lr,"logreg",y_test,logistic_pred) #logreg out of sample

#compare predictions of logarithmic with NN
tb_nn <- table(y_train, y_predtrainclass)
tb_lr <- table(y_train, logistic_predtrainclass)
cat("STEP 4a: Confusion Matrix for NN (in-sample test) \n\n")
print(tb_nn)
cat("\n\n")
cat("\nSTEP 4b: Confusion Matrix for Logistic Reg (in-sample test) \n\n")
print(tb_lr)
cat("\n\n")

cat("STEP 5: Calculate in-sample tests and compare results on many factors\n\n")
calculate_stats2 <- function(tb, model_name,y_test,y_pred) {
  acc <- (tb[1] + tb[4])/(tb[1] + tb[2] + tb[3] + tb[4])
  recall <- tb[4]/(tb[4] + tb[3])
  precision <- tb[4]/(tb[4] + tb[2])
  f1 <- 2 * ((precision * recall) / (precision + recall))
  
  bincrossentr = -1*mean((log(y_pred)*y_test + log(1-y_pred)*(1-y_test)),na.rm=TRUE)
  cat(model_name, ": in sample test \n")
  cat("\tAccuracy = ", acc*100, "%.")
  cat("\n\tPrecision = ", precision*100, "%.")
  cat("\n\tRecall = ", recall*100, "%.")
  cat("\n\tF1 Score = ", f1*100, "%.")
  cat("\n\tBin. Cross. Ent.  = ", bincrossentr, "\n\n")
}


calculate_stats2(tb_nn,"Neur. Net.",y_train,y_predtrain) #nn in-sample
calculate_stats2(tb_lr,"logreg",y_train,logistic_predtrain) ## logreg in-sample


