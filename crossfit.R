library(data.table)
library(mlr3)
library(mltools)

partition <- function(n, k) {
  fold.size <- (n %/% k) + 1
  
  folds <- rep(c(1:k), times=fold.size)
  folds <- folds[1:n]
  folds <- folds[sample(1:n)]

  return(folds)
}

kfold.fit.predict <- function(df, target.name, k, learner.str) {
  n <- nrow(df)
  folds <- partition(n, k)
  
  df <- setDT(df)
  df <- one_hot(df)
  
  tsk <- as_task_regr(df, target=target.name)

  all.preds <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(all.preds) <- c("row_ids", "truth", "response")
  
  for (i in 1:k) {
    learner <- lrn(learner.str)
    learner$train(tsk, row_ids=c(1:n)[folds != i])
    preds <- learner$predict(tsk, row_ids=c(1:n)[folds == i])
    preds <- as.data.table(preds)
    all.preds <- rbind(all.preds, preds)
  }
  all.preds <- arrange(all.preds, row_ids)
  
  return(all.preds)
}

fit.predict <- function(train.df, test.df, target.name, learner.str) {
  n1 <- nrow(test.df)
  n2 <- nrow(train.df)
  df <- rbind(test.df, train.df)
  
  df <- setDT(df)
  df <- one_hot(df)
  
  tsk <- as_task_regr(df, target=target.name)
  
  learner <- lrn(learner.str)
  
  learner$train(tsk, row_ids=c((n1 + 1):(n1 + n2)))
  preds <- learner$predict(tsk, row_ids=c(1:n1))
  preds <- as.data.table(preds)
  preds <- arrange(preds, row_ids)
  return(preds)
}