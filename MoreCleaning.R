library(ggplot2)

setwd("/home/rohitb/Dropbox/Spring16/ExploratoryDataAnalysis/Assignments/Project2/Codes/")
data = read.csv("../GlobalFloodsRecord_1.csv")
print(names(data))

data$Nations = NULL
data$X.Affected = NULL
data$X = NULL
data$Severity.. <- as.factor(as.character(data$Severity..))
data$Affected.sq.km <- as.numeric(as.character(data$Affected.sq.km))
data$Magnitude..M... <- as.numeric(as.character(data$Magnitude..M...))
data$Damage..USD. <- as.numeric(as.character(data$Damage..USD.))
data$Displaced <- as.numeric(as.character(data$Displaced))
data$Severity.. <- as.numeric(as.character(data$Severity..))
data$Duration.in.Days <- as.numeric(as.character(data$Duration.in.Days))
data$Dead <- as.numeric(as.character(data$Dead))

summary(data)

#typeof(data$Affected.sq.km)
#kms <- data$Affected.sq.km
#kms <- kms[0:4319]
#kms <- as.numeric(as.character(kms))
#scaledData <- scale(kms)
#sum(is.na(kms))
#length(kms)
#mean(kms,na.rm = TRUE)
#scaledData

# Affected.sq.km, Magnitude, Dead, Displaced, Severity, Duration.in.Days, Damage.., 
data <- data[0:4319,]
kms <- data$Affected.sq.km
mag <- data$Magnitude..M...
#damage <- data$Damage..USD.
displaced <- data$Displaced
severity <- data$Severity..
duration <- data$Duration.in.Days
dead <- data$Dead
newFrame <- data.frame(kms,mag,displaced,severity,duration,dead)

for(i in 1:ncol(newFrame)){
  if (sum(is.na(newFrame[,i]))<15){
    newFrame[is.na(newFrame[,i]), i] <- mean(newFrame[,i], na.rm = TRUE)
  }
}

for(i in 1:ncol(newFrame)){
  if (sum(is.na(newFrame[,i]))<15){
    newFrame[,i] <- scale(newFrame[,i])
  }
}

#check <- newFrame[0:5,]
#check
distances <- dist(newFrame)

#PCA
data.pca <- prcomp(newFrame,center=TRUE,scale=TRUE)
print(data.pca)
plot(data.pca, type="l")


# MDS
fit <- cmdscale(distances,eig=TRUE, k=2) 
x <- fit$points[,1]
y <- fit$points[,2]
#plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
#     main="Metric MDS", type="n")
#text(x, y, labels = row.names(newFrame), cex=.7) 
ggplot(newFrame, aes(x= x, y= y))+geom_point()+geom_text(aes(label=row.names(newFrame)),hjust=0, vjust=0, check_overlap = TRUE)+theme(legend.position = "none")

#Clustering

#hierarchical clustering
cluster <- hclust(distances,method="ward")
plot(cluster)
groups <- cutree(cluster, k=5)
rect.hclust(cluster, k=5, border="red") 

#mcust 10 clusters gaussian with EM
library(mclust)
fit <- Mclust(newFrame)
plot(fit)
summary(fit)
clusters <- fit$classification
