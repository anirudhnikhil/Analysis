# Part a

housing.df <- read.csv("BostonHousing.csv")
set.seed(1)
train.index <- sample(row.names(housing.df), 0.6*dim(housing.df)[1])
valid.index <- setdiff(row.names(housing.df), train.index)
train.df <- housing.df[train.index, ]
valid.df <- housing.df[valid.index, ]


# Part b

#scatter plot
plot(RM ~ AGE, data=train.df[1:20,], pch=ifelse(train.df$CAT..MEDV == 1, 1, 3))
text(train.df[1:20,]$RM, train.df[1:20,]$AGE, rownames(train.df[1:20,]), pos=4)

legend("topright", c("> = 30", "< 30"), pch = c(1, 3))

# part c Normalizing data

#Running k-NN
# initialize normalized training, validation data, complete data framews to originals
train.norm.df <- train.df
valid.norm.df <- valid.df
housing.norm.df <- housing.df
# use preProcess() from the caret package to normalize. Install the package caret first.
library(caret)
# "center" method: subtract the mean from the value, "scale" method: divide the value by standard deviation
norm.values <- preProcess(housing.df[, 1:12], method=c("center", "scale"))
# normalize
train.norm.df[, 1:12] <- predict(norm.values, train.df[ , 1:12])
valid.norm.df[, 1:12] <- predict(norm.values, valid.df[ , 1:12])
housing.norm.df[, 1:12] <- predict(norm.values, housing.df[ , 1:12])

# part d find the best k

# Accuracy or correct rate of k-NN predictions in validation set for various choices of k
library(caret)
library(FNN)
# initialize a data frame with two columns: k, and accuracy.
accuracy.df <- data.frame(k = seq(1, 20, 1), accuracy = rep(0, 20))


#compute knn for different k on validation
# install package e1071 if needed
for(i in 1:20) {
  knn.pred <- knn(train.norm.df[, 1:12], valid.norm.df[ , 1:12],
                  cl = train.norm.df[, 14], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, valid.norm.df[, 14])$overall[1]
}

# display the accuracy rate for various choices of k
accuracy.df


# part e best k model display confusion matrix

knn.pred <- knn(train.norm.df[, 1:12], valid.norm.df[ , 1:12],
                cl = train.norm.df[, 14], k = 3)
confusionMatrix(knn.pred, valid.norm.df[, 14])
CM<-confusionMatrix(knn.pred, valid.norm.df[, 14])
CM$overall[1]

# part f predict 2 new cases

housingscore.df <- read.csv("BostonHousingScore.csv")
housingscore.norm.df <- housingscore.df
housingscore.norm.df[, 1:12] <- predict(norm.values, housingscore.df[ , 1:12])

pred <- knn(train = train.norm.df[, 1:12], test = housingscore.norm.df,
            cl = train.norm.df[, 14], k = 3)
pred
# closest neighbors
attr(pred, "nn.index")
# closest neighbors for the first record
rownum <- attr(pred, "nn.index")[1, 1:3]
# display the case numbers of the 3 nearest neighbors
casenum <- row.names(train.df)[rownum]
medvalue<-train.df[casenum, 14]
medvalue

rownum <- attr(pred, "nn.index")[2, 1:3]
# display the case numbers of the 3 nearest neighbors
casenum <- row.names(train.df)[rownum]
medvalue<-train.df[casenum, 14]
medvalue