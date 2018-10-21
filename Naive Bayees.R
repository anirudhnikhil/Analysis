library(e1071)
delays.df <- read.csv("FlightDelays.csv")

set.seed(1)

# Change numerical variables to categorical first
delays.df$DAY_WEEK <- factor(delays.df$DAY_WEEK)
delays.df$DEP_TIME <- factor(delays.df$DEP_TIME)
# create hourly bins departure time
delays.df$CRS_DEP_TIME <- factor(round(delays.df$CRS_DEP_TIME/100))

# Create training and validation sets.
selected.var <- c(10, 1, 8, 4, 2, 13)
train.index <- sample(c(1:dim(delays.df)[1]), dim(delays.df)[1]*0.6)
train.df <- delays.df[train.index, selected.var]
valid.df <- delays.df[-train.index, selected.var]

# run naive bayes
delays.nb <- naiveBayes(Flight.Status ~., data = train.df)
delays.nb

# use prop.table() with margin = 1 to convert a count table to a proportion table,
# where each row sums up to 1 (use margin = 2 for column sums).
# You can get the same outcomes as NB by doing this for each predictor.
table(train.df$Flight.Status, train.df$DEST)
prop.table(table(train.df$Flight.Status, train.df$DEST), margin = 1)
prop.table(table(train.df$Flight.Status, train.df$DEST), margin = 2)

table(train.df$Flight.Status, train.df$ORIGIN)
prop.table(table(train.df$Flight.Status, train.df$ORIGIN), margin = 1)
prop.table(table(train.df$Flight.Status, train.df$ORIGIN), margin = 2)

# Scoring the example flight (probability and class)
# Predict probabilities
pred.prob <- predict(delays.nb, newdata = valid.df, type = "raw")
# Predict class membership
pred.class <- predict(delays.nb, newdata = valid.df)

# Create an outcome data frame
df <- data.frame(actual = valid.df$Flight.Status, predicted = pred.class, pred.prob)

# Score a new case with certain characteristics.  Find same cases in the validation data set.
df[valid.df$CARRIER == "DL" & valid.df$DAY_WEEK == 7 & valid.df$CRS_DEP_TIME == 10 & 
     valid.df$DES == "LGA" & valid.df$ORIGIN == "DCA", ]


# Create confusion matrices for flight delay using a naive bayes classifier
library(caret)

# training confusion matrix
pred.class <- predict(delays.nb, newdata=train.df)
confusionMatrix(pred.class, train.df$Flight.Status)

# validation confusion matrix
pred.class <- predict(delays.nb, newdata=valid.df)
confusionMatrix(pred.class, valid.df$Flight.Status)


#  Lift chart of naive bayes classifier applied to flight delays data
# Install package gains if not already installed
library(gains)
gain <- gains(ifelse(valid.df$Flight.Status=="delayed", 1, 0), pred.prob[ , 1], groups=100)

plot(c(0, gain$cume.pct.of.total*sum(valid.df$Flight.Status=="delayed"))~c(0, gain$cume.obs),
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0, sum(valid.df$Flight.Status=="delayed"))~ c(0, dim(valid.df)[1]), lty=2)

# OR
plot(gain$cume.pct.of.total*sum(valid.df$Flight.Status=="delayed")~ gain$cume.obs,
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0, sum(valid.df$Flight.Status=="delayed"))~ c(0, dim(valid.df)[1]), lty=2)