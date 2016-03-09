library(ggplot2)

setwd("C:\\Users\\Admin\\Documents\\DigitalIntern2015\\RWorks\\EDAV_Proect_NOAA")
load("floods.RData")
load("pressure.RData")
floods$Nations = NULL
floods$X.Affected = NULL
floods$X = NULL
floods$severity <- as.factor(as.character(floods$severity))
floods$affected.sq.km <- as.numeric(as.character(floods$affected.sq.km))
floods$magnitude <- as.numeric(as.character(floods$magnitude))
floods$damage <- as.numeric(as.character(floods$damage))
floods$displaced <- as.numeric(as.character(floods$displaced))
floods$duration <- as.numeric(as.character(floods$duration))
floods$dead <- as.numeric(as.character(floods$dead))

summary(floods)

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
kms <- floods$affected.sq.km
mag <- floods$magnitude
displaced <- floods$displaced
severity <- floods$severity
duration <- floods$duration
dead <- floods$dead

newFrame <- data.frame(kms,mag,displaced,duration,dead)

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
#library(mclust)
#fit <- Mclust(newFrame)
#plot(fit)
#summary(fit)
#clusters <- fit$classification
