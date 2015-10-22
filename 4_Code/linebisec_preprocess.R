# install & load necessary packages
library(devtools)
install_github("jmbh/mt.analysis") #package currently only on github
library(plyr)
library(mt.analysis)

#get data movement data
setwd("G:/bisectionpaper/_repository/3_Data/traj")
l_files <- list.files(getwd())
f_read_custom <- function(x) {read.csv(x, sep="\t", stringsAsFactors = FALSE)}
d_raw <- do.call(rbind, lapply(l_files, f_read_custom))
colnames(d_raw) <- c('id', 'trial', 'pic', 'x', 'y', 'time')
d_raw$ycor <- (d_raw$y-768)*-1
d_raw$b <- rep(0, nrow(d_raw))

#time normalize (see Spivey et al. 2005)
box.cor <- list("start"=c(512,200), "left"=c(512.0001,408), "right"=c(512.0001,408))
d_proc <- mt.preprocess(d_raw, box.cor, 
                        i.id=c("id", "trial", "pic"), 
                        i.measure=c("x", "ycor", "time", "b"))

#flag experiments
conds <- ddply(d_proc, c(i.id),  function(x) {
  x<-as.numeric(x$pic)
  if(x[3]<9)                 {c<-1
  }else if(x[3]>8  & x[3]<17) {c<-2} 
  else if(x[3]>16 & x[3]<25) {c<-3}
  else if(x[3]>24 & x[3]<33) {c<-4}
  else if(x[3]>32 & x[3]<41) {c<-5}
  else if(x[3]>40 & x[3]<49) {c<-6}
  else {c<-8} #detect unclassified trials
  matrix(rep(c,101), ncol=1)
} )

d_proc$larger <- 0
d_proc$larger[conds$`1`==1] <- 1
d_proc$larger[conds$`1`==2] <- 0
d_proc$larger[conds$`1`==3] <- 1
d_proc$larger[conds$`1`==4] <- 0
d_proc$larger[conds$`1`==5] <- 1
d_proc$larger[conds$`1`==6] <- 0
d_proc$exp <- 0
d_proc$exp[conds$`1`==1] <- 1
d_proc$exp[conds$`1`==2] <- 1
d_proc$exp[conds$`1`==3] <- 2
d_proc$exp[conds$`1`==4] <- 2
d_proc$exp[conds$`1`==5] <- 3
d_proc$exp[conds$`1`==6] <- 3


#add initiation time + timeoff data from response-data
setwd("G:/bisectionpaper/_repository/3_Data/bias")
l_files <- list.files(getwd())
f_read_custom <- function(x) {read.csv(x, sep="\t", stringsAsFactors = FALSE)}
d_rawb <- do.call(rbind, lapply(l_files, f_read_custom))
colnames(d_rawb) <- c('id', 'trial', 'pic', 'timeover', 'rt_total', 'rt_planning', 'control_planning_x', 'control_planning_y', 'rt_movement', 'x', 'y')

#add planning time to data frame
head(d_rawb)
rt.init <- ddply(d_rawb, .(id, trial, pic, timeover), function(x) {
  matrix(rep(x$rt_planning), 101, ncol=1) 
})
rt.init <- subset(rt.init, timeover!=1)
a <- cbind(
  ddply(d_rawb_not, .(id), function(x) length(unique(x$trial))),
  ddply(d_all, .(id), function(x) length(unique(x$trial)))
  )

d_rawb_not <- d_rawb[d_rawb$timeover==0,]
upt <- unique(d_rawb_not[,c("id", "trial")])
uptc <- paste0(upt[,1], upt[,2])

#drop timeover trials & get init time
d_all <- ddply(d_proc, .(id, trial), function(x) {
  if(paste0(x$id, x$trial)[1] %in% uptc) {
    x$rt.init <- d_rawb[d_rawb$id==x$id[1] & d_rawb$trial==x$trial[1],]$rt_planning
    x
  }
})

nrow(d_all) == nrow(d_rawb_not)*101 #check: do trials in bias-file match trials in trajectory-file?

#export
setwd("G:/bisectionpaper/_repository/4_Code")
saveRDS(d_all, "data_preprocessed.RDS")
