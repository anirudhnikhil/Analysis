boston.housing.df <- read.csv("BostonHousing.csv", header = TRUE)
head(boston.housing.df, 9)
summary(boston.housing.df)
str(boston.housing.df)

# Compute mean, standard dev, min, max median, length, and missing values of CRIM
mean(boston.housing.df$CRIM)
sd(boston.housing.df$CRIM)
min(boston.housing.df$CRIM)
max(boston.housing.df$CRIM)
median(boston.housing.df$CRIM)
length(boston.housing.df$CRIM)
sum(is.na(boston.housing.df$CRIM))

# Compute mean, standard dev, min, max median, and length for all variables
data.frame(mean=sapply(boston.housing.df, mean), 
           sd=sapply(boston.housing.df, sd), 
           min=sapply(boston.housing.df, min),
           max=sapply(boston.housing.df, max),
           median=sapply(boston.housing.df, median),
           length=sapply(boston.housing.df, length))
#correlation table
round(cor(boston.housing.df), 2)
#Aggregation by a single variable by using table()
table(boston.housing.df$CHAS)
#Aggregate MEDV by CHAS and RM
boston.housing.df$RM.bin <- .bincode(boston.housing.df$RM, c(1:9))
boston.housing.df$RM.bin
#compute the average of MEDV by (binned) RM and CHAS
#in aggregate() use the argument by= to define the list of aggregating variables,
#and FUN= as an aggregating function.
aggregate(boston.housing.df$MEDV, by=list(RM=boston.housing.df$RM.bin, CHAS=boston.housing.df$CHAS), FUN = mean)

#Creating pivot tables using functions melt() and cast()
#use install.packages("reshape")the first time the package is used
library(reshape)
#create bins of size 1
boston.housing.df$RM.bin <- .bincode(boston.housing.df$RM, c(1:9))
#use melt() to stack a set of columns into a single column of data.
#stack MEDV values for each combination of (binned) RM and CHAS
mlt <- melt(boston.housing.df, id=c("RM.bin", "CHAS"), measure=c("MEDV"))
head(mlt, 5)

# use cast() to reshape data and generate pivot table
cast(mlt, RM.bin ~ CHAS, subset=variable=="MEDV", margins=c("grand_row", "grand_col"), mean)


#PCA on the two variables Calories and Rating
cereals.df <- read.csv("Cereals.csv")
#compute PCs on two dimensions
pcs <- prcomp(data.frame(cereals.df$calories, cereals.df$rating))
summary(pcs)
#weights
pcs$rot
#scores
scores <- pcs$x
head(scores, 5)


#PCA using all 13 numerical variables in the breakfast cereals dataset
#Remove any missing values and columns 1-3
pcs <- prcomp(na.omit(cereals.df[ , -c(1:3)]))
summary(pcs)
#weights
pcs$rot
#scores
scores <- pcs$x
head(scores, 5)


#PCA using all NORMALIZED 13 numerical variables in the breakfast cereals dataset
#Use function prcomp() with scale. = T to run PCA on normalized data
pcs.cor <- prcomp(na.omit(cereals.df[ , -c(1:3)]), scale. =T)
summary(pcs.cor)
#weights
pcs.cor$rot
#scores
scores <- pcs.cor$x
head(scores, 5)