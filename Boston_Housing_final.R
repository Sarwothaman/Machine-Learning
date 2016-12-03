setwd("E:/Data Science/Boston")
traindata=read.csv("Housing_data.csv")
str(traindata)

cor(traindata$MEDI,traindata$CRIMINAL) # -0.39
cor(traindata$MEDI,traindata$ZN)  # .36
#cor(traindata$MEDI,traindata$INDUS) # -.48 -5 
cor(traindata$MEDI,traindata$CHAS) # .17
cor(traindata$MEDI,traindata$NOX) # -.42
cor(traindata$MEDI,traindata$RM) # .69 -2 
cor(traindata$MEDI,traindata$AGE) # -.37
cor(traindata$MEDI,traindata$DISTANCE) # .25
cor(traindata$MEDI,traindata$RAD) # -.38
#cor(traindata$MEDI,traindata$TAX) # -.47 -6
cor(traindata$MEDI,traindata$PTRATIO) # -.50 - 4
cor(traindata$MEDI,traindata$BLACK) # -.58 -3
cor(traindata$MEDI,traindata$LSTAT) # -.74 -1

library(caTools)
splitvar = sample.split(traindata$MEDI,SplitRatio = .7)

# this will create a logical vector.

splitvar
traindsn  =subset(traindata,splitvar == TRUE)
testdsn  =subset(traindata,splitvar == FALSE)

fit = lm(MEDI ~ LSTAT+RM+BLACK+PTRATIO, traindsn)

summary(fit)
plot(fit$residuals)
abline(h=0)

testdsn$MEDIAN=testdsn$MEDI
testdsn$MEDI=NULL
testdsn$MEDI=predict(fit,newdata=testdsn)
testdsn
plot(fit$residuals)
abline(h=0)

write.csv(testdsn,file="Boston_Predict.csv")
