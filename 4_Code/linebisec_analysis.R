library(lme4)
library(plyr)

setwd("G:/bisectionpaper/_repository/4_Code")
dat <- readRDS("data_preprocessed.RDS")

#### part 1: endpoint (bias) analysis ####
datb <- dat[dat$t==100,] #last timepoint

datb$larger <- as.factor(datb$larger)
# predicting bias by 9-dot-side
#E1
exp1 <- lmer(x ~ larger + (1+larger|id), data=datb[datb$exp==1,], REML=FALSE)
summary(exp1)
exp1_null <- lmer(x ~ (1|id), data=datb[datb$exp==1,], REML=FALSE)
anova(exp1, exp1_null)
coef(exp1); coef(exp1)$id$larger<0 #same direction for ALL participants!
mean(coef(exp1)$id$larger)
sd(coef(exp1)$id$larger)
#E2
exp2 <- lmer(x ~ larger + (1+larger|id), data=datb[datb$exp==2,], REML=FALSE)
summary(exp2)
coef(exp2); coef(exp2)$id$larger<0
mean(coef(exp2)$id$larger<0)
mean(coef(exp2)$id$larger)
sd(coef(exp2)$id$larger)
exp2_null <- lmer(x ~ (1|id), data=datb[datb$exp==2,], REML=FALSE)
anova(exp2, exp2_null)
#E3
exp3 <- lmer(x ~ larger + (1+larger|id), data=datb[datb$exp==3,], REML=FALSE)
summary(exp3)
coef(exp3); coef(exp3)$id$larger<0 #same direction for ALL participants!
mean(coef(exp3)$id$larger)
sd(coef(exp3)$id$larger)
exp3_null <- lmer(x ~ (1|id), data=datb[datb$exp==3,], REML=FALSE)
anova(exp3, exp3_null)

#recoding to make experiment 3 the reference
datb$exp_rec <- 0
datb$exp_rec[datb$exp==1] <- 2
datb$exp_rec[datb$exp==2] <- 3
datb$exp_rec[datb$exp==3] <- 1
datb$exp_rec <- as.factor(datb$exp_rec)

# predicting initiation time by experiment
mod2 <- lmer(rt.init ~ exp_rec + (1+exp_rec|id), data=datb, REML=FALSE)
summary(mod2)
mod2_null <- lmer(rt.init ~ (1|id), data=datb, REML=FALSE)
anova(mod2, mod2_null)
coef(mod2)
mean(coef(mod2)$id$exp_rec3<0)

# predicting movement time by experiment
mod1 <- lmer(rt ~ exp_rec + (1+exp_rec|id), data=datb, REML=FALSE)
summary(mod1)
coef(mod1)
mod1_null <- lmer(rt ~ (1|id), data=datb, REML=FALSE)
anova(mod1, mod1_null) #not significant ...
mean(coef(mod1)$id$exp_rec2<0)
mean(coef(mod1)$id$exp_rec3<0)


#### part 2 - process analysis ####

## bias across time

#storage
stor_ttests <- list("exp1"=NULL, "exp2"=NULL, "exp3"=NULL)
stor_ttests[[1]] <- stor_ttests[[2]] <- stor_ttests[[3]] <- list("left"=matrix(0,101,3), "right"=matrix(0,101,3))

#calc
for(exp in 1:3) {
  for(side in 0:1) {
    for(t in 0:100)
    {
      subs <- dat[dat$exp==exp & dat$larger==side & dat$t == t,4]
      tobj <- t.test(subs, y=NULL, mu=0)
      stor_ttests[[exp]][[side+1]][t+1,1] <- tobj$statistic
      stor_ttests[[exp]][[side+1]][t+1,2] <- tobj$p.value
      stor_ttests[[exp]][[side+1]][t+1,3] <- tobj$parameter
      #print(tobj$statistic)
    }
  }
}

# analyze
for(exp in 1:3) {
  for(side in 1:2) {
    print(paste("Condition", exp, "Side", side))
    it <- stor_ttests[[exp]][[side]]
    print("signf. from t step")
    print(fromwhen <- 102-sum(it[,2]<.05))
    print("mean t")
    print(mean(it[,1][fromwhen:101]))
    print(mean(it[,3]))
  }
}

## xflip
boxplot(xflip ~ exp_rec, data=datb)
mod3 <- lmer(xflip ~ exp_rec + (1+exp_rec|id), data=datb, REML=FALSE)
summary(mod3)
mod3_null <- lmer(xflip ~ (1|id), data=datb, REML=FALSE)
anova(mod3,mod3_null)
coef(mod3) #interesting
ddply(datb, .(exp), function(x) mean(x$xflip)) #mean differences

mean(coef(mod3)$id$exp_rec3<0)

