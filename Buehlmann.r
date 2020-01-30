#Boosting: mainly about bias vs Bagging: mainly about variance
#Boosting: stage-wise correction
#Component-wise: if n pred. for one iter -> n pred (step length factor v: 0.1 normally)
#AIC can be used for estimating m_stop
library(mboost)

#01 glmboost()
data("bodyfat",package="TH.data")

#normal lm
lm1<-lm(DEXfat~hipcirc+kneebreadth+anthro3a,
    data=bodyfat)

coef(lm1)

#with boost
glm1<-glmboost(DEXfat~hipcirc+kneebreadth+anthro3a,
    data=bodyfat)

coef(glm1,off2int=TRUE)#offset added to intercept

glm2<-glmboost(DEXfat~.,data=bodyfat)
preds<-names(bodyfat[,names(bodyfat) !="DEXfat"])
fm<-as.formula(paste("DEXfat~",paste(
    preds,collapse="+")))
fm

coef(glm2,which="") #select all vars

#with intercept vars can't readable
#where preds come in (kinda zoom in)
plot(glm2,off2int=TRUE) 
plot(glm2,ylim=range(coef(glm2,which=preds)))

##Pred body fat
gam1<-gamboost(DEXfat~bbs(hipcirc)+bbs(kneebreadth)
    +bbs(anthro3a),data=bodyfat)


par(mfrow=c(1,3))
plot(gam1)

#stopping is important to prevent overfitting
#AIC-based stopping tends to overshoot the optimal point i.e.,mstop(cvm)
#use cross-validated est of the empirical risk instead
#convenience function cv() example
cv(weights,type=c("bootstrap","kfold","subsampling"),
    B=ifelse(type=="kfold",10,25))

gam2<-gamboost(DEXfat~.,baselearner="bbs",data=bodyfat,
    control=boost_control(trace=TRUE))
cvm<-cvrisk(gam2) #25-fold bootstrap cross-val
cvm
plot(cvm)
mstop(cvm)
gam2[mstop(cvm)] #setting stopping point
names(coef(gam2)) #selected base-learners at mstop(cvm)

#To see that nothing got lost we now increase mstop to 1000:
gam2[1000, return = FALSE] #iter 1-100 not computed
names(coef(gam2))

#Quantile Reg=>stopping point needs to be bigger
glm3<-glmboost(DEXfat~hipcirc+kneebreadth+anthro3a,data=bodyfat,
    family=QuantReg(tau=0.5),control=boost_control(mstop=500))
coef(glm3,off2int=TRUE)

#BOOSTING ALGORITHMS: REGULARIZATION,
#PREDICTION AND MODEL FITTING
#Paper demonstration






