### Statistical Data Mining Assignment 2

### Lab 2

## 1: Clustering

# Load the iris data
data(iris)

# We want to cluster the measurement data but the dataset gives
# the true species for each flower in column 5. Thus, we remove
# column 5 from the data and store the resulting data as X.
X<-iris[,-5]

# Plot the data. Do you see clusters? Yes.
pairs(X)

# Run k-means (with K=3) and store the resulting fit in fitkm.
fitkm<-kmeans(X,centers=3)
fitkm    # k-means with k=3

# If worried about converging to local minimum:
# Run k-means (with K=3 & 10 random starts)
# Store the resulting fit in fitkm & examine the results.
fitkm<-kmeans(X,centers=3,nstart=10)
fitkm

# Let's compare the clustering results to the iris species.
# First, let's look at the true species from column 5 of the original data
iris[,5]

# For ploting, let's make a vector of numbers
# with each number corresponding to a species
colvec <- as.numeric(iris[,5])

# Plot the "true" clustering.
# The plot symbol (pch) and color (col) correspond to species
plot(X,col=colvec,pch=colvec)

# Plot the k-means clustering
# The plot symbol (pch) and color (col) correspond to cluster
plot(X,col=fitkm$cluster,pch=fitkm$cluster)

# Put them side-by-side
par(mfrow=c(1,2))
plot(X,col=colvec,pch=colvec,main="Species")
plot(X,col=fitkm$cluster,pch=fitkm$cluster,main="Cluster")
par(mfrow=c(1,1))

# Do the plots differ much? No.

# We could also have done a single plot by
# letting the colors (col) be the clusters and
# letting the symbols (pch) be the species
plot(X,col=fitkm$cluster,pch=colvec)

# Cluster centres
fitkm$centers

# Total sum of squares
fitkm$totss

# Elbow plot to guide choice of k
WGSS <- rep(0, 10)
n <- nrow(iris)
WGSS[1] <- (n - 1) * sum(apply(iris[,0:4], 2, var))
for(k in 2:10) {
  WGSS[k] <- sum(kmeans(iris[,0:4], centers=k, nstart=50)$withinss)
}

# Plot the WGSS values vs. k and look for a kink
plot(1:10, WGSS, type="b", xlab="k", ylab="WGSS", main="K-Means")

## 2: k-Medoids

# Load the cluster library
library(cluster)

# The pam() function needs the data to be in a distance matrix form.
# We make a distance matrix called d.
d<-dist(X,method="euclidean")

# We run partitioning around medoids can store the results as fitpam.
fitpam<-pam(d,k=3)

# Examine the results
fitpam

# We can construct a table to compare the k-means and PAM results
table(fitkm$cluster,fitpam$clustering)

# How do the results change if a different distance, like manhattan, is used?
d1 <- dist(X, method = 'manhattan')
# d1 is now a distance matrix
fitpam <- pam(d1, k=3)
fitpam
table(fitkm$cluster, fitpam$clustering)

# How do the results change when we change k?
d2 <- dist(X, method = 'manhattan')
fitpam <- pam(d2, k=10)
fitpam
table(fitkm$cluster, fitpam$clustering)

## 3: Scaling Data
# Scale data to have mean 0 and standard deviation 1
Xs <- scale(X)

# Do the clustering results change?
d <- dist(Xs, method = 'euclidean')
fitpam <- pam(d, k=3)
fitpam
table(fitkm$cluster, fitpam$clustering)
# Yes

## 4: US Congress Data. Homework 2

dat <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/voting-records/house-votes-84.data",sep=",")

# Give sensible column names to data.
colnames(dat) <- c("Party",paste("Vote",1:16,sep=""))

# Look at the data
dat

# Note that the ?'s are people being absent from the vote.
# We recode the ?'s as n's
dat[dat=="?"] <- "n"

# I will remove "party" from the data so that we only analyze votes.
parties<-dat[,1]
dat<-dat[,-1]

# Let's make a binary version of the data (1="y" and 0="n")
datnew <- 1*(dat=="y")
colnames(datnew) <- paste(1:16,sep="")

datnew    # binary version of data
mult <- 1.5
barplot(table(parties), col=c("dodgerblue2", "firebrick"), main="Political Party Affiliation for 1984 US House of Representatives", 
        xlab="Political Party Affiliation", ylab="No. of House of Representatives Congressmen", ylim=c(0, 300),
        cex.lab=mult, cex.axis=mult, cex.main=mult)
legend("topright", legend = c("Democrat: 267", "Republican: 168"), fill = c("dodgerblue2", "firebrick"))

# Frequency counts
count <- apply(datnew, 2, table)
prop.count <- apply(count, 2, function(x) x/sum(x))   # In terms of percentage
barplot(prop.count)
barplot(prop.count, col=c("blue", "grey"), main="1984 US House of Representatives on 16 Key Votes", 
        xlab="Vote Number", ylab="Percentage of Votes", cex.lab=mult, cex.axis=mult, cex.main=mult)
abline(h = 0.5, col="black", lty=2)
legend("topright", legend = c("Yes", "No"), fill = c("blue", "grey"))

# Frequency counts by party
pdatnew <- as.data.frame(cbind(parties, datnew))
c <- aggregate(pdatnew[,-1], by=list(pdatnew[,1]), FUN = table)
prop.c <- apply(c, 2, function(x) x/sum(x))   # In terms of percentage

cat <- c("a", "b", "b", "a", "b", "a")
df = cbind(cat, data.frame(matrix(c(0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0), nrow=6, byrow = T)))

c <- data.frame(rbind(apply(pdatnew[pdatnew$parties == "1", -1]  == 1, 2, sum),    # Democrat Yes
                      apply(pdatnew[pdatnew$parties == "2", -1]  == 1, 2, sum),    # Republican Yes 
                      apply(pdatnew[pdatnew$parties == "1", -1]  == 0, 2, sum),    # Democrat No
                      apply(pdatnew[pdatnew$parties == "2", -1]  == 0, 2, sum)))   # Republican No
colnames(c) <- 1:16    # Rename columns
prop.c <- apply(c, 2, function(x) x/sum(x))

barplot(prop.c, main="1984 US House of Representatives on 16 Key Votes", col=c("Dodgerblue4", "royalblue2", "gray66", "gainsboro"), 
        xlab="Vote Number", ylab="Percentage of Votes", cex.lab=mult, cex.axis=mult, cex.main=mult)
abline(h = 0.5, col="black", lty=2)
legend("topright", legend = c("Democrat Yes", "Republican Yes", "Democrat No", "Republican No"), 
       fill = c("Dodgerblue4", "royalblue2", "gray66", "gainsboro"))

### Look at partisan voting patterns
dem.yes <- mean(apply(prop.c, 2, function(x) x[1]/ (x[1] + x[3])))    # proporition of time yes when yes
dem.no <- mean(apply(prop.c, 2, function(x) x[3]/ (x[1] + x[3])))    # proporition of time no when no

rep.yes <- mean(apply(prop.c, 2, function(x) x[2]/ (x[2] + x[4])))    # proporition of time yes when yes
rep.no <- mean(apply(prop.c, 2, function(x) x[4]/ (x[2] + x[4])))    # proporition of time no when no

## Number of votes passed to Senate
sum(apply(prop.c, 2, function(x) x[1] + x[2]) > .5)
passed <- apply(prop.c, 2, function(x) x[1] + x[2]) > .5    # index

## Votes that passed that republicans had the majority vote in
prop.c[c(1,2),passed]
apply(prop.c, 2, function(x) x[1] > x[2])

# Elbow plot to guide choice of k
WGSS <- rep(0, 10)
n <- nrow(datnew)
WGSS[1] <- (n - 1) * sum(apply(datnew, 2, var))
for(k in 2:10) {
  WGSS[k] <- sum(kmeans(datnew, centers=k, nstart=10)$withinss)
}

# Plot the WGSS values vs. k and look for a kink
plot(1:10, WGSS, type="b", xlab="k", ylab="WGSS", main="K-Means Elbow Plot", cex.lab=mult, cex.axis=mult, cex.main=mult)

K <- 2
fitkm <- kmeans(datnew, centers=K, nstart=10)

colvec<-as.numeric(parties)    # Binary encode "true" parties

# Validate k-means algorithm
fitkm$cluster
tab <- table(colvec, fitkm$cluster)
tab

1-sum(diag(tab)) / sum(tab)    # Misclassification rate

# Try the k-medoids algorithm
library(cluster)
SWAP <- WGSS
for(k in 1:10) {
  SWAP[k] <- sum(pam(dist(datnew, method="euclidean"), k=k)$objective[2])
}

# Plot the 'swap' values vs. k and look for a kink
plot(1:10, SWAP, type="b", xlab="k", ylab="SWAP", main="K-Medoids Elbow Plot", cex.lab=mult, cex.axis=mult, cex.main=mult)

cl2 <- pam(dist(datnew, method="euclidean"), 2)

cl2 <- pam(datnew, 2, metric='manhattan')    # more suitable for binary data
cl2 <- pam(dist(datnew, method='binary'), k=2)    # same as the 'Jaccard' measure?

cl2 <- pam(dist(datnew, method='canberra'), k=2)
tab2 <- table(as.numeric(!(colvec - 1)) + 1, cl2$cluster)
tab2
1-sum(diag(tab2)) / sum(tab2) # Misclassification rate for K-medoids

library(e1071)
# Tabulate the two solutions
tab <- table(fitkm$cluster, cl2$cluster, dnn=c("K-Means", "K-Medoids"))
classAgreement(tab)

## Plot the silhouette plot
sil <- silhouette(fitkm1$cluster, d)
pdf(file=paste0(getwd(), "/silhouette.pdf"), width=10, height=10)
plot(sil)
dev.off()    # Should work?

d <- dist(datnew, method="euclidean")^2

# Compute the silhouette for each observation
sil <- silhouette(fitkm$cluster,d)

# Plot the silhouette plot
plot(sil)

silvals<-sil[,3]

#Choose the plot symbol for the positive/negative silhouette observations
pchvec<-rep(19,nrow(datnew))
pchvec[silvals<0]<-3

# Replace negative silhouettes with zero (just for plot coloring)
silvals[silvals<0]<-0

#Construct the pairs plot
pairs(X,col=rgb(silvals,0,1-silvals,alpha=0.2),pch=pchvec)

## k-medoids
d <- dist(datnew)
fitpam <- pam(d, k=3)
plot(fitpam)    # Look up lab notes for how to do this properly. 

#### Lab 3

# Load the iris data
data(iris)

# We want to cluster the measurement data but the dataset gives the
#true species for each flower in column 5.
# Thus, we remove column 5 from the data and store the resulting
# data as X.
X<-iris[,-5]

#Plot the data. Do you see clusters?
pairs(X)

# Run k-means (with K=3) and store the resulting fit in fitkm.
K<-3
fitkm<-kmeans(X,centers=K)

# Plot the data (colored by cluster)
colvec<-rainbow(K,alpha=0.5)
pairs(X,col=colvec[fitkm$cluster],pch=19)

# Adding the cluster centroids to a pairs plot such as this is a
# little more involved than the ''faithful' example we looked at in class.
# See if you can figure out what the following lines of code are doing.
pairs(rbind(X, fitkm$centers),
      pch=rep(c(19,8), c(nrow(iris), K)),
      col=c(colvec[fitkm$cluster], rep("black", K)),
      cex=rep(c(1, 2.5), c(nrow(iris), K)))

library("cluster")
# Construct a distance matrix using squared Euclidean distance
d <- dist(X, method="euclidean")^2

# Compute the silhouette for each observation
sil <- silhouette(fitkm$cluster,d)

# Plot the silhouette plot
plot(sil)

# See which observations have high silhouette (red=high, blue=low)
# Observations with negative silhouette are blue but use a + symbol.
# Store the silhouette values in a vector
silvals<-sil[,3]

# Choose the plot symbol for the positive/negative silhouette observations
pchvec<-rep(19,nrow(X))
pchvec[silvals<0]<-3

# Replace negative silhouettes with zero (just for plot coloring)
silvals[silvals<0]<-0

#Construct the pairs plot
pairs(X,col=rgb(silvals,0,1-silvals,alpha=0.2),pch=pchvec)

### 2: Rand Index
# Run both algorithms
# K-means
K<-3
fitkm<-kmeans(X,centers=K,nstart=10)

# K-medoids
d <- dist(X,method="manhattan")
fitkmed <- pam(d,k=K)

# Tabulate the results
tab<-table(fitkm$cluster,fitkmed$clustering)
tab

#Compute the Rand and adjusted Rand indices
library(e1071)
classAgreement(tab)

### US Congress Data

dat <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/voting-records/house-votes-84.data",sep=",")

# Give sensible column names to data.
colnames(dat) <- c("Party",paste("Vote",1:16,sep=""))
# Look at the data
dat
# Note that the ?'s are people being absent from the vote.
# We recode the ?'s as n's
dat[dat=="?"] <- "n"
# I will remove "party" from the data so that we only analyze votes.
parties<-dat[,1]
dat<-dat[,-1]
# Let's make a binary version of the data (1="y" and 0="n")
datnew <- 1*(dat=="y")
colnames(datnew) <- paste("Yes",1:16,sep="")

datnew    # binary version of data
K <- 3
fitkm1 <- kmeans(datnew, centers=K, nstart=10)

colvec<-as.numeric(parties)    # Binary encode "true" parties

### These plots are not useful for this data set.
plot(datnew, col=colvec, pch=colvec, main="Parties")
head(datnew)
plot(datnew, col=fitkm1$cluster, pch=fitkm1$cluster, main="Cluster")
plot(datnew, col=fitkm1$cluster, pch=colvec)

### Use this instead:
## Plot the silhouette plot
sil <- silhouette(fitkm1$cluster, d)
pdf(file=paste0(getwd(), "/silhouette.pdf"), width=10, height=10)
plot(sil)
dev.off()    # Should work?

## k-medoids
d <- dist(datnew)
fitpam <- pam(d, k=3)
plot(fitpam)    # Look up lab notes for how to do this properly. 

# fuzzy k-means using fclust

