library(ggplot2)
library(mclust)
library(Hmisc)

#read nc file
#read floods table
setwd("C:\\Users\\Admin\\Documents\\DigitalIntern2015\\RWorks\\EDAV_Proect_NOAA")
load("floods.RData")
load("pressure.RData")

#leave meaningful columns, remove degenerate cases
floods <- floods[(floods$began > as.Date("1985-01-01")) & (floods$centroid.y >= 35) & (floods$centroid.y <= 70),
                 c(1,4:27)]
floods <- floods[!is.na()]

get_features <- function(x, y, began, ended, interval_length, unit_length) {
    
    index.x1 <- ifelse(floor((x+180)/2.5) + 1 - unit_length > 0,
                       floor((x+180)/2.5) + 1 - unit_length,
                       1)
    index.x2 <- ceiling((x+180)/2.5) + 1 + unit_length
    
    index.y1 <- ifelse(28-floor((y)/2.5) - unit_length > 0, 
                       28-floor((y)/2.5) - unit_length,
                       1)
    index.y2 <- ifelse(28-floor((y)/2.5)+1 + unit_length < 16,
                       28-floor((y)/2.5)+1 + unit_length,
                       15)
    
    #convert floods$began date to phi index
    if (interval_length == 0) {
        index.start <- as.numeric(began - as.Date("1985-01-01"))
        index.end <- as.numeric(ended - as.Date("1985-01-01"))
    } else {
        index.start <- ifelse(as.numeric(began - as.Date("1985-01-01")) - interval_length > 0,
                              as.numeric(began - as.Date("1985-01-01")) - interval_length,
                              1)
        index.end <- as.numeric(began - as.Date("1985-01-01"))   
    }
    
     avg <- mean(phi[index.x1:index.x2, index.y1:index.y2, index.start:index.end])
     std <- sd(phi[index.x1:index.x2, index.y1:index.y2, index.start:index.end])
     return(c(avg, std))
}

#for each flood extract the presure that was several days before within a certain range

floods$avg.int1.1 <- NA
floods$std.int1.1 <- NA
floods$avg.int1.2 <- NA
floods$std.int1.2 <- NA
floods$avg.int2.1 <- NA
floods$std.int2.1 <- NA
floods$avg.int2.2 <- NA
floods$std.int2.2 <- NA
floods$avg.int3.1 <- NA
floods$std.int3.1 <- NA
floods$avg.int3.2 <- NA
floods$std.int3.2 <- NA
floods$avg.int4.1 <- NA
floods$std.int4.1 <- NA
floods$avg.int4.2 <- NA
floods$std.int4.2 <- NA

for (i in 1:nrow(floods)) {
    if (!(is.na(floods$centroid.x[i]))) {
        int1.1 <- with(floods, 
                        get_features(centroid.x[i], centroid.y[i], began[i], ended[i], 0, 0))
        floods$avg.int1.1[i] <- int1.1[1]
        floods$std.int1.1[i] <- int1.1[2]
        
        int1.2 <- with(floods,
                       get_features(centroid.x[i], centroid.y[i], began[i], ended[i], 0, 1))
        floods$avg.int1.2[i] <- int1.2[1]
        floods$std.int1.2[i] <- int1.2[2]
        
        int2.1 <- with(floods,
                       get_features(centroid.x[i], centroid.y[i], began[i], ended[i], 5, 0))
        floods$avg.int2.1[i] <- int2.1[1]
        floods$std.int2.1[i] <- int2.1[2]
        
        int2.2 <- with(floods,
                       get_features(centroid.x[i], centroid.y[i], began[i], ended[i], 5, 1))
        floods$avg.int2.2[i] <- int2.2[1]
        floods$std.int2.2[i] <- int2.2[2]
        
        int3.1 <- with(floods,
                       get_features(centroid.x[i], centroid.y[i], began[i], ended[i], 10, 0))
        floods$avg.int3.1[i] <- int3.1[1]
        floods$std.int3.1[i] <- int3.1[2]
        
        int3.2 <- with(floods,
                       get_features(centroid.x[i], centroid.y[i], began[i], ended[i], 10, 1))
        floods$avg.int3.2[i] <- int3.2[1]
        floods$std.int3.2[i] <- int3.2[2]
        
        int4.1 <- with(floods,
                       get_features(centroid.x[i], centroid.y[i], began[i], ended[i], 15, 0))
        floods$avg.int4.1[i] <- int4.1[1]
        floods$std.int4.1[i] <- int4.1[2]
        
        int4.2 <- with(floods,
                       get_features(centroid.x[i], centroid.y[i], began[i], ended[i], 15, 1))
        floods$avg.int4.2[i] <- int4.2[1]
        floods$std.int4.2[i] <- int4.2[2]
    }
}

names(floods)

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

kms <- floods$affected.sq.km
mag <- floods$magnitude
displaced <- floods$displaced
severity <- floods$severity
duration <- floods$duration
dead <- floods$dead
p1.1.avg <- floods$avg.int1.1
p1.2.avg <- floods$avg.int1.2
p2.1.avg <- floods$avg.int2.1
p2.2.avg <- floods$avg.int2.2
p3.1.avg <- floods$avg.int3.1
p3.2.avg <- floods$avg.int3.2
p4.1.avg <- floods$avg.int4.1
p4.2.avg <- floods$avg.int4.2

p1.1.sd <- floods$std.int1.1
p1.2.sd <- floods$std.int1.2
p2.1.sd <- floods$std.int2.1
p2.2.sd <- floods$std.int2.2
p3.1.sd <- floods$std.int3.1
p3.2.sd <- floods$std.int3.2
p4.1.sd <- floods$std.int4.1
p4.2.sd <- floods$std.int4.2

newFrame_withoutP <- data.frame(kms,mag,displaced,duration,dead)
newFrame_withP <- data.frame(kms,mag,displaced,duration,dead,
                             p1.1.sd,p1.2.sd,p2.1.sd,p2.2.sd,p3.1.sd,p3.2.sd,p4.1.sd,p4.2.sd,
                             p1.1.avg,p1.2.avg,p2.1.avg,p2.2.avg,p3.1.avg,p3.2.avg,p4.1.avg,p4.2.avg)
onlyPressure <- data.frame(p1.1.sd,p1.2.sd,p2.1.sd,p2.2.sd,p3.1.sd,p3.2.sd,p4.1.sd,p4.2.sd,
                           p1.1.avg,p1.2.avg,p2.1.avg,p2.2.avg,p3.1.avg,p3.2.avg,p4.1.avg,p4.2.avg)


for(i in 1:ncol(newFrame_withoutP)){
  if (sum(is.na(newFrame_withoutP[,i]))<15){
    newFrame_withoutP[is.na(newFrame_withoutP[,i]), i] <- mean(newFrame_withoutP[,i], na.rm = TRUE)
  }
}


for(i in 1:ncol(newFrame_withP)){
  if (sum(is.na(newFrame_withP[,i]))<15){
    newFrame_withP[is.na(newFrame_withP[,i]), i] <- mean(newFrame_withP[,i], na.rm = TRUE)
  }
}


for(i in 1:ncol(onlyPressure)){
  if (sum(is.na(onlyPressure[,i]))<15){
    onlyPressure[is.na(onlyPressure[,i]), i] <- mean(onlyPressure[,i], na.rm = TRUE)
  }
}


for(i in 1:ncol(newFrame_withoutP)){
  if (sum(is.na(newFrame_withoutP[,i]))<15){
    newFrame_withoutP[,i] <- scale(newFrame_withoutP[,i])
  }
}


for(i in 1:ncol(newFrame_withP)){
  if (sum(is.na(newFrame_withP[,i]))<15){
    newFrame_withP[,i] <- scale(newFrame_withP[,i])
  }
}

for(i in 1:ncol(onlyPressure)){
  if (sum(is.na(onlyPressure[,i]))<15){
    onlyPressure[,i] <- scale(onlyPressure[,i])
  }
}
# Processing and data got ready

distances_withoutP <- dist(newFrame_withoutP)
distances_withP <- dist(newFrame_withP)
distances_p <- dist(onlyPressure)

#MDS
#Initial analysis to plot the 3 dataframes onto an MDS plot to find the distributions

fit_withoutP <- cmdscale(distances_withoutP,eig=TRUE, k=2) 
x <- fit_withoutP$points[,1]
y <- fit_withoutP$points[,2]
#plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
#     main="Metric MDS", type="n")
#text(x, y, labels = row.names(newFrame), cex=.7) 
ggplot(newFrame_withoutP, aes(x= x, y= y))+geom_point()+geom_text(aes(label=row.names(newFrame_withoutP)),hjust=0, vjust=0, check_overlap = TRUE)+theme(legend.position = "none")

fit_withP <- cmdscale(distances_withP,eig=TRUE, k=2) 
x <- fit_withP$points[,1]
y <- fit_withP$points[,2]
#plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
#     main="Metric MDS", type="n")
#text(x, y, labels = row.names(newFrame), cex=.7) 
ggplot(newFrame_withP, aes(x= x, y= y))+geom_point()+geom_text(aes(label=row.names(newFrame_withP)),hjust=0, vjust=0, check_overlap = TRUE)+theme(legend.position = "none")

fit_P <- cmdscale(distances_p,eig=TRUE, k=2) 
x <- fit_P$points[,1]
y <- fit_P$points[,2]
#plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
#     main="Metric MDS", type="n")
#text(x, y, labels = row.names(newFrame), cex=.7) 
ggplot(onlyPressure, aes(x= x, y= y))+geom_point()+geom_text(aes(label=row.names(onlyPressure)),hjust=0, vjust=0, check_overlap = TRUE)+theme(legend.position = "none")

#PCA
withoutP.pca <- prcomp(newFrame_withoutP,center=TRUE,scale=TRUE)
print(withoutP.pca)
plot(withoutP.pca, type="l")
pca_withoutP <- predict(withoutP.pca,newFrame_withoutP)


withP.pca <- prcomp(newFrame_withP,center=TRUE,scale=TRUE)
print(withP.pca)
plot(withP.pca, type="l")

p.pca <- prcomp(onlyPressure,center=TRUE,scale=TRUE)
print(p.pca)
plot(p.pca, type="l")
pca_P <- predict(p.pca,onlyPressure)


pcaAnalysis_1 <- data.frame(newFrame_withoutP,pca_P[,1])
corr_1 <- rcorr(as.matrix(pcaAnalysis_1))
corr_1

pcaAnalysis_2 <- data.frame(newFrame_withoutP,pca_P[,2])
corr_2 <- rcorr(as.matrix(pcaAnalysis_2))
corr_2

pcaAnalysis_1_2 <- data.frame(newFrame_withoutP,pca_P[,1:2])
corr_1_2 <- rcorr(as.matrix(pcaAnalysis_1_2))
corr_1_2

pcaAnalysisp_1 <- data.frame(pca_withoutP[,1],pca_P[,1])
plot(pcaAnalysisp_1)
corr_1 <- rcorr(as.matrix(pcaAnalysisp_1))
corr_1

pcaAnalysisp_2 <- data.frame(pca_withoutP[,1],pca_P[,2])
plot(pcaAnalysisp_2)
corr_1 <- rcorr(as.matrix(pcaAnalysisp_2))
corr_1

pcaAnalysisp_1_2 <- data.frame(pca_withoutP[,1],pca_P[,1:2])
corr_1 <- rcorr(as.matrix(pcaAnalysisp_1_2))
corr_1

pcaAnalysisd_1 <- data.frame(pca_withoutP[,1],onlyPressure)
corr_1 <- rcorr(as.matrix(pcaAnalysisd_1))
corr_1

pcaAnalysisd_2 <- data.frame(pca_withoutP[,1],onlyPressure)
corr_1 <- rcorr(as.matrix(pcaAnalysisd_2))
corr_1

pcaAnalysisd_1_2 <- data.frame(pca_withoutP[,1:2],onlyPressure)
corr_1 <- rcorr(as.matrix(pcaAnalysisd_1_2))
corr_1



#clustering
clusters2_withoutP <- hclust(distances_withoutP,method="ward")
plot(clusters2_withoutP)
groups <- cutree(clusters2_withoutP, k=5)
rect.hclust(clusters2_withoutP, k=5, border="red") 

clusters2_withP <- hclust(distances_withP,method="ward")
plot(clusters2_withP)
groups <- cutree(clusters2_withP, k=5)
rect.hclust(clusters2_withP, k=5, border="red") 

corr <- rcorr(as.matrix(newFrame_withP))
corr
