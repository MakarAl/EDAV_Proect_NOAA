library(RNetCDF)
library(qpcR)
library(maps)

data <- open.nc("NOAA_Daily_phi_500mb.nc")
dat <- read.nc(data)

TLen = 9496
XLen = 144
YLen = 15
T = seq(1,Len,by=1)
X = seq(-177.5, 180, by=2.5)
Y = seq(35, 70, by=2.5)
startIdx <- 13515

phi <- array(0,dim=c(XLen,YLen,TLen))
for (i in T) {
  for (j in 1:YLen) {
    phi[1:71,j,i] = dat$phi[74:XLen,YLen-j+1,i+startIdx]
    phi[72:XLen,j,i] = dat$phi[1:73,YLen-j+1,i+startIdx]
  }
}

# stretch the matrix
phiM <- array(0,dim=c(TLen, XLen*YLen))
for (i in T) {
  k <- 1
  for (n in 1:XLen) {
    for (m in 1:YLen) {
      phiM[i, k] = phi[n, m, i]
      k <- k+1
    }
  }
}

# function to reverse the matrix
transformPCA <- function(p, xlim, ylim) {
  res <- matrix(0, nrow = xlim, ncol = ylim)
  k <- 1
  for (i in 1:xlim) {
    for (j in 1:ylim) {
      res[i, j] = p[k]
      k <- k + 1
    }
  }
  return(res)
}

# run PCA on stretched matrix
pca <- prcomp(phiM)
# first component
pc1 <- pca$rotation[,1]
# second component
pc2 <- pca$rotation[,2]
# third component
pc3 <- pca$rotation[,3]

# reverse components
p1 <- transformPCA(pc1, XLen, YLen)
p2 <- transformPCA(pc2, XLen, YLen)
p3 <- transformPCA(pc3, XLen, YLen)

# graph it
filled.contour(X,Y,p1,color.palette=terrain.colors,
               xlim=c(-180,180),ylim=c(35,70),
               plot.axes = {par(usr=c(-180,180,35,70));axis(1);axis(2);map('world',add=TRUE);grid()},
               key.axes={par(fg="NA")})
filled.contour(X,Y,p2, color.palette=terrain.colors,
               xlim=c(-180,180),ylim=c(35,70), 
               plot.axes = {par(usr=c(-180,180,35,70));axis(1);axis(2);map('world',add=TRUE);grid()},
               key.axes={par(fg="NA")})
filled.contour(X,Y,p3, color.palette=terrain.colors, 
               xlim=c(-180,180),ylim=c(35,70), 
               plot.axes = {par(usr=c(-180,180,35,70));axis(1);axis(2);map('world',add=TRUE);grid()},
               key.axes={par(fg="NA")})


