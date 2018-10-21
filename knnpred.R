housing.df <- read.csv("BostonHousing.csv")
set.seed(1)  
train.index <- sample(row.names(housing.df), 0.6*dim(housing.df)[1])  
valid.index <- setdiff(row.names(housing.df), train.index)  
train.df <- housing.df[train.index, -14]
valid.df <- housing.df[valid.index, -14]

library(caret)
train.norm.df <- train.df
valid.norm.df <- valid.df
norm.values <- preProcess(train.df[, -13], method=c("center", "scale"))
train.norm.df[, -13] <- predict(norm.values, train.df[, -13])
valid.norm.df[, -13] <- predict(norm.values, valid.df[, -13])

# Accuracy or correct rate of k-NN predictions in validation set for various choices of k
library(caret)
library(FNN)

# initialize a data frame with two columns: k, and accuracy.
accuracy.df <- data.frame(k = seq(1, 5, 1), accuracy = rep(0, 5))

#compute knn for different k on validation
# install package e1071 if needed
# as.numeric(as.character(x)) to convert a factor column to a numeric one
for(i in 1:5) {
  knn.pred <- knn(train.norm.df[, -13], valid.norm.df[ , -13],
                  cl = train.norm.df[, 13], k = i)
  accuracy.df[i, 2] <- RMSE(as.numeric(as.character(knn.pred)), valid.df$MEDV)
}

# display the accuracy rate for various choices of k
accuracy.df


# Another way to do this using package class rather than FNN

accuracy.df <- data.frame(k = seq(1, 5, 1), RMSE = rep(0, 5))
for(i in 1:5) {
  knn.pred <- class::knn(train = train.norm.df[,-13], 
                         test = valid.norm.df[,-13], 
                         cl = train.df$MEDV, k = i)
  accuracy.df[i, 2] <- RMSE(as.numeric(as.character(knn.pred)), valid.df$MEDV)
}                
accuracy.df

# End of alternative solution

# b

new.rec <- data.frame(0.2, 0, 7, 0, 0.538, 6, 62, 4.7, 4, 307, 21, 10)
names(new.rec) <- names(train.norm.df)[-13]
new.norm.rec <- predict(norm.values, new.rec)

knn.pred <- knn(train.norm.df[, -13], new.norm.rec,
                cl = train.norm.df[, 13], k = 1)
knn.pred

# Alternatively, you can do this
knn.pred <- class::knn(train = train.norm.df[,-13], 
                       test = new.norm.rec, 
                       cl = train.df$MEDV, k = 1)
knn.pred


# c
knn.train.df.pred <- knn(train.norm.df[, -13], train.norm.df[, -13],
                         cl = train.norm.df[, 13], k = 1)
accuracy <- RMSE(as.numeric(as.character(knn.train.df.pred)), train.df$MEDV)
accuracy