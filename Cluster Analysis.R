
# Distance matrix between pairs of the utilities, using euclidean distance
utilities.df <- read.csv("Utilities.csv")

# set row names to the utilities column
row.names(utilities.df) <- utilities.df[,1]
# remove the utility column
utilities.df <- utilities.df[,-1]

# compute Euclidean distance
# (to compute other distance measures, change the value in method= )
d <- dist(utilities.df, method="euclidean")
d

# Distance matrix between pairs of the utilities, using euclidean distance and normalized measurements
# code for normalizing data and computing distance
utilities.df.norm <- sapply(utilities.df, scale)
# add row names: utilities
row.names(utilities.df.norm) <- row.names(utilities.df)
#compute normalized distance based on Sales (col 6) and Fuel Cost (col 8)
d.norm <- dist(utilities.df.norm[ , c(6,8)], method= "euclidean")
d.norm



# Dengrogram: Single linkage and average linkage for all 22 utilities, using all eight measures
# code for unning hierarchical clustering and generating a dengrogram
# in hclust() set argument method=
# to "ward.D", "single", "complete", "average", "median", or "centroid"
hc1 <- hclust(d.norm, method = "single")
plot(hc1, hang = -1, ann = FALSE)
# computing cluster membership by "cutting" the dengrogram
memb <- cutree(hc1, k=6)
memb

# average linkage
hc2 <- hclust(d.norm, method = "average")
plot(hc2, hang = -1, ann = FALSE)
# computing cluster membership by "cutting" the dengrogram
memb <- cutree(hc2, k=6)
memb


# Heatmap for the 22 utilities. Rows are sorted by the 
# six clusters from average linkage clustering, darker cells denote higher values within a column.
# set labels as clusters membership and utility name
row.names(utilities.df.norm) <- paste(memb, ": ", row.names(utilities.df), sep = "")
# plot heatmap
# rev() reverses the color mapping to large = dark
heatmap(as.matrix(utilities.df.norm), Colv = NA, hclustfun = hclust,
        col=rev(paste("gray", 1:99, sep="")))

# k-Means clustering of 22 utilities into k=6 clusters(sorted by cluster ID)
# load and preparocess data
utilities.df <- read.csv("Utilities.csv")
row.names(utilities.df) <- utilities.df[ , 1]
utilities.df <- utilities.df[, -1]

# normalized distance:
utilities.df.norm <- sapply(utilities.df, scale)
row.names(utilities.df.norm) <- row.names(utilities.df)

# run kmeans algorithm
km <- kmeans(utilities.df.norm, 6)
# show cluster membership
km$cluster

# cluster centroids and squared distances for k-means with k=6
km$centers

# within-cluster sum of squares
km$withinss

# cluster size
km$size

# plotting profile plot of centroids
# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type="l",
     ylim = c(min(km$centers), max(km$centers)), xlim = c(0,8))
# label x-axes
axis(1, at = c(1:8), labels = names(utilities.df))
# plot centroids
for (i in c(1:6))
  lines(km$centers[i,], lty=i, lwd=2, col=ifelse(i %in% c(1,3,5),
                                                 "black", "dark grey"))
# name clusters
text(x = 0.5, y = km$centers[,1], labels=paste("Cluster", c(1:6)))

# Euclidean distance between cluster centroids
dist(km$centers)
dist(km$centers, method="euclidean")