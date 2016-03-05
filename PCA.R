library(RNetCDF)
library(qpcR)
library(maps)

source("Filled.contour2.R")
source("Filled.legend.R")

# transform 3-D matrix to 2-D matrix
transformMatrix <- function(origM, TT, XX, YY) {
  res <- array(0,dim=c(TT, XX*YY))
  for (i in 1:TT) {
    k <- 1
    for (n in 1:XX) {
      for (m in 1:YY) {
        res[i, k] <- origM[n, m, i]
        k <- k + 1
      }
    }
  }
  return(res)
}

# reverse the transform
reverseMatrix <- function(newM, XX, YY) {
  res <- matrix(0, nrow = XX, ncol = YY)
  k <- 1
  for (i in 1:XX) {
    for (j in 1:YY) {
      res[i, j] <- newM[k]
      k <- k + 1
    }
  }
  return(res)
}

runPCA <- function(phiM, s, e, year)
{
  phiYear <- phiM[s:e,]
  pca <- prcomp(phiYear)
  pc1 <- pca$rotation[,1]
  pc2 <- pca$rotation[,2]
  pc3 <- pca$rotation[,3]
  pca1 <- reverseMatrix(pc1, 7, 7)
  pca2 <- reverseMatrix(pc2, 7, 7)
  pca3 <- reverseMatrix(pc3, 7, 7)
  filled.contour3(long, lat, pca1, color.palette = terrain.colors,
                 ylim=c(40,55),
                 plot.title=title(xlab='Longitude', ylab='Lattitude', main=year,cex.lab=0.8, cex.main=0.9 ), 
                 plot.axes = { axis(1, seq(-7.5,7.5,by=2.5),cex.axis=0.6 ); axis(2, seq(40, 55, by = 2.5),cex.axis=0.6 );map(add=TRUE);grid()})
}

data <- open.nc("NOAA_Daily_phi_500mb.nc")
dat <- read.nc(data)

TLen = 11322
XLen = 7
YLen = 7
T = seq(1,Len,by=1)
X = seq(-7.5, 7.5, by=2.5)
Y = seq(40, 55, by=2.5)
startIdx <- 15342
# start from 1990/1/1

phi <- array(0,dim=c(XLen,YLen,TLen))
for (i in 1:TLen) {
  for (j in 1:YLen) {
    for (m in 1:4) {
      phi[3+m,j,i] = dat$phi[m,14-j,i+startIdx-1]
    }
    for (m in 1:3) {
      phi[m,j,i] = dat$phi[141+m,14-j,i+startIdx-1]
    }
  }
}

phiM <- transformMatrix(phi, TLen, XLen, YLen)
runPCA(phiM, 1, 365, 1990)
runPCA(phiM, 366, 730, 1991)
runPCA(phiM, 731, 1096, 1992)
runPCA(phiM, 1097, 1461, 1993)
runPCA(phiM, 1462, 1862, 1994)
runPCA(phiM, 1863, 2191, 1995)
runPCA(phiM, 2192, 2557, 1996)
runPCA(phiM, 2558, 2922, 1997)
runPCA(phiM, 2923, 3287, 1998)
runPCA(phiM, 3288, 3652, 1999)
runPCA(phiM, 3653, 4018, 2000)
runPCA(phiM, 4019, 4383, 2001)
runPCA(phiM, 4384, 4748, 2002)
runPCA(phiM, 4749, 5113, 2003)
runPCA(phiM, 5114, 5479, 2004)
runPCA(phiM, 5480, 5844, 2005)
runPCA(phiM, 5845, 6209, 2006)
runPCA(phiM, 6210, 6574, 2007)
runPCA(phiM, 6575, 6940, 2008)
runPCA(phiM, 6941, 7305, 2009)
runPCA(phiM, 7306, 7670, 2010)
runPCA(phiM, 7671, 8035, 2011)
runPCA(phiM, 8036, 8401, 2012)
runPCA(phiM, 8402, 8766, 2013)
runPCA(phiM, 8767, 9131, 2014)
runPCA(phiM, 9132, 9496, 2015)





