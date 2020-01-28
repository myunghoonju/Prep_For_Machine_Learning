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

#fitting GAM:gamboost, centering?
