# jonashaslbeck@gmail.com; 21-08-2015


#### computing aggregates ####

#load data
setwd("C:\\Users\\jo\\Dropbox\\LineBisec\\_repository\\4_Code")
dat <- readRDS("data_preprocessed.RDS")


# mean + SE of the mean for all time steps separately for exp and left

l.stor <- list("vector", length=3) #storage object
fse <- function(x) sqrt(var(x)/length(x))

for(exp in 1:3) {
  
  exp.stor <- matrix(0,101,4)
  
  for(t in 0:100) {
    
    x_left <- dat[dat$larger == 0 & dat$exp == exp & dat$t == t,]$x
    x_right <- dat[dat$larger == 1 & dat$exp == exp & dat$t == t,]$x
    
    exp.stor[t+1,1] <- mean(x_left)
    exp.stor[t+1,2] <- mean(x_right)
    exp.stor[t+1,3] <- qt(.975, df=(length(x_left)−1)) * fse(x_left) #error margin (confidence interval)
    exp.stor[t+1,4] <- qt(.975, df=(length(x_right)−1)) * fse(x_right) #error margin (confidence interval)
    
  }
  l.stor[[exp]] <- exp.stor
}


#### plot figure, version1 ####

pdf("Figure1.pdf", height = 3, width=9)

lmat <- matrix(c(1,2,3), 1, 3)
lo <- layout(lmat, widths=c(.36,.32,.32), heights=c(6))

for(exp in 1:3) {

# setting up plotting areas

if(exp==1) {    
#E1
par(mar=c(3.5,4,3,1))
plot(1, type='n', xlim=c(0,100), ylim=c(-6,6), ylab="x (Pixel)", xlab="",
     cex.lab=1.35, main="(a) Aggregate area", 
     mgp=c(2,.5,0))
} else if (exp==2) {
#E2
par(mar=c(3.5,1,3,1))
plot(1, type='n', xlim=c(0,100), ylim=c(-6,6), ylab="", xlab="Normalized time",
     cex.lab=1.35, main="(b) Contour length", 
     mgp=c(2,.5,0))
} else {
#E3
par(mar=c(3.5,1,3,1))
plot(1, type='n', xlim=c(0,100), ylim=c(-6,6), ylab="", xlab="",
     cex.lab=1.35, main="(c) Subtended area", 
     mgp=c(2,.5,0))
}

#plotting data

dd <- l.stor[[exp]]
colnames(dd) <- c("x_left","x_right", "se_xl", "se_xr")
dd <- as.data.frame(dd)

lines(dd$x_left, type="l", ylim=c(-8,8), lty=1, lwd=2)
lines(dd$x_left+dd$se_xl, type="l", lty=1)
lines(dd$x_left-dd$se_xl, type="l", lty=1)

lines(dd$x_right, type="l", lty=2, lwd=2)
lines(dd$x_right+dd$se_xr, type="l", lty=2)
lines(dd$x_right-dd$se_xr, type="l", lty=2)

if(exp==2) {
  legend(55,5.9,
         c("9 Dot left: Mean", "9 Dot left: SE", "9 Dot right: Mean", "9 Dot right: SE"),
         lty=c(2,2,1,1), lwd=c(2,1,2,1), 
         cex=0.83)


  }


}

dev.off()


#### plot figure, version 2 ####


pdf("Figure1_v2.pdf", height = 4, width=8)

error_bars <- FALSE
lmat <- matrix(c(1,2), 1, 2)
lo <- layout(lmat, widths=c(.355,.32), heights=c(6))

for(side in 1:2) {

  #setting up plotting areas  
  if(side==1) {  
    par(mar=c(3.5,3,3,.5))
    plot(1, type='n', xlim=c(0,100), ylim=c(-6,6), ylab="<- left      Bias(Pixel)      right ->", 
         xlab="Normalized time",
         cex.lab=1.1, main="9-Dot left", 
         mgp=c(2,.5,0))
    #lines(0:100,rep(0,101), lty=1, col="grey")
  } else {
    par(mar=c(3.5,.5,3,.5))
    plot(1, type='n', xlim=c(0,100), ylim=c(-6,6), ylab="", 
         xlab="Normalized time",
         cex.lab=1.1, main="9-Dot right", yaxt="n", 
         mgp=c(2,.5,0))
    #lines(0:100,rep(0,101), lty=1, col="grey")
    
      }

  #plotting data
  
  for(exp in 1:3)
  {
    dd <- l.stor[[exp]]
    colnames(dd) <- c("x_left","x_right", "se_xl", "se_xr")
    lines(dd[,abs(side-3)], type="l", ylim=c(-8,8), lty=exp, lwd=2)

    if(error_bars==TRUE)
    {
      lines(dd[,side]+dd[,side+2], type="l", ylim=c(-8,8), lty=exp, lwd=1)
      lines(dd[,side]-dd[,side+2], type="l", ylim=c(-8,8), lty=exp, lwd=1)
      }
    
    }

  #legend
  if(side==2) {
    legend(50,6,
           c("Aggregate area", "Contour length", "Subtended area"),
           lty=c(1:3), 
           cex=0.83)
  }
  
  }

dev.off()



