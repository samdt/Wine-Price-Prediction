setwd("C:\\Users\\Vaibhav\\Desktop\\Chaps\\Other")

wine=read.csv("wine.csv")
dim(wine)
model=lm(Price ~ AGST, data = wine)
summary(model)

model$coefficients
model$residuals
model$fitted.values

SST=sum((wine$Price-mean(wine$Price))^2)

SSE=sum(model$residuals ^2)
RMSE=sqrt(SSE/nrow(wine))
Rsq= 1-(SSE/SST)

model1=lm(Price~AGST+HarvestRain, data=wine)
summary(model1)
model1$coefficients
1.347e-03


model2=lm(Price~WinterRain+AGST+HarvestRain + Age, data=wine)
summary(model2)
#note age is significant

model3=lm(Price~WinterRain+AGST+HarvestRain + Age + FrancePop, data=wine)
summary(model3)
#adjusted R2 of model 2 and model 3 are same
#age not came significant now
SSE=sum(model3$residuals ^2)



pairs(wine)
cor(wine)
#Age and Population of France are correlated
cor.test(wine$Age, wine$FrancePop)#t-test
#so we reject the null hyp - they are correlated

model4=lm(Price~WinterRain+AGST+HarvestRain, data=wine)
summary(model4)
#note adjusted R2 has decreased

model5=lm(Price~WinterRain+AGST+HarvestRain+Age, data=wine)
summary(model5)
#Highest adjusted R2




library(car)
vif(model2)
#very high vif values for age and FrancePop - indicating multi-collinearity

vif(model5)
#no multi-collinearity

#predicting our model on new data wine_test
wine_test=read.csv("wine_test.csv")
wine_test
predictTest=predict(model2, newdata=wine_test)
predictTest
#actual value as seen from head (wine_test) is same as predicted value
SSE=sum((wine_test$Price-predictTest)^2)
SST=sum((wine_test$Price-mean(wine$Price))^2)
#sst is yi - y mean of train data

R_Square=1-SSE/SST
R_Square #pretty good model - R2 value is similar to that of train data
# we need more data 
#Out of sample R2 can be negative
#R2 value can not be negative


#Case Study 2 Baseball-------------------------------------------------------
setwd("C:\\Users\\Vaibhav\\Desktop\\Chaps\\Other")
baseball=read.csv("baseball.csv")
str(baseball)
?subset
moneyball=subset(baseball, Year<2002)
dim(moneyball)
#W represents wins
#RA represents runs allowed
#RS=Runs Scored
moneyball$RD=moneyball$RS-moneyball$RA 
#Runs difference = runs allowed and runs scored

plot(moneyball$RD, moneyball$W)#linear correlation

WinsReg=lm(W~RD, data=moneyball)
summary(WinsReg)

#We have to win more than 95 games
#if Wins >=95, then what should be the RD
W=95
RD=(W- WinsReg$coefficients[1])/WinsReg$coefficients[2]
RD
#so if RD>=135, i will win more than 95 games

#Pg 21
RD=713-614
W=WinsReg$coefficients[1]+RD*WinsReg$coefficients[2]
W #if run diff = 99, then i'll win 91 games

# using data prove that 95 games are sufficient to reach the play-offs
plot(baseball$W, baseball$Playoffs)
t=table(W=baseball$W, Playoff=baseball$Playoffs)
27/32
#note for 95 wins, playoff_ptage=.84
#obp - on base %age (measure of speed) in how much time the hitter will throw the ball
#slg - slugging %age how far the person can hit
#RS = runs scored
RunsReg=lm(RS~OBP+SLG+BA, data=moneyball)
summary(RunsReg)#R2=.93
#BA has unexpected coefficients. This is counter-intuitive. Y?
cor(moneyball$RS, moneyball$BA) #+ve correlation


#so BA is mostly coming -ve in RunsReg because of multi-collinearity
#2 ways to find the multi-collinearity - using vif or finding correlation coef


library(car)
vif(RunsReg)
# 4 is pretty high, vif should be near to 1


df=data.frame(moneyball$RS, moneyball$OBP, moneyball$SLG, moneyball$BA)
names(df)
pairs(df)
#All looking correlated
cor(df)#correlation value > .7 is strong correlation
cor(moneyball[, c("RS", "OBP", "SLG", "BA")])
#so now we'll drop some variables, we need to know where v get 



model1=lm(RS~OBP+SLG, data=moneyball)
summary(model1) #R2 =.9294
model2=lm(RS~OBP+BA, data=moneyball)
summary(model2)#R2 reduced from model1 .836
model3=lm(RS~SLG+BA, data=moneyball)
summary(model3)#R2=.8757
model4=lm(RS~SLG, data=moneyball)
summary(model4)
#So, model1 has best R2, and BA is not required as without it model 1 also giving same R2

#OOBP-Opponents on base %tage
#OSLG - opponents slugging %age
moneyball$RA
model5=lm(RA ~ OOBP + OSLG, data=moneyball)
summary(model5)

#Find the Runs Scored, given OBP is .311 and SLG is .405
#using model 1 
model1$coefficients
model1$coefficients[1]+.311*model1$coefficients[2]+.405*model1$coefficients[3]
#Ans 688.7068

#Can we predict how many games the 2002 Oakland A's will win using our model, using OBP=.339, SLG=.430
newdata=data.frame(OBP=.339, SLG=.430)
predict(model1, newdata = newdata)

#for predcting RA for 2002
OOBP=.307
OSLG=.373
RA1=model5$coefficients[1]+OOBP*model5$coefficients[2] + OSLG*model5$coefficients[3]
#to use predict function
RA2=predict(model5,data.frame(OOBP,OSLG))
RA1
RA2

table(baseball$Year, baseball$Playoffs)
#after 1984 only 8 teams go for world series

teamRank=c(1,2,3,3,4,4,4,4,5,5)
