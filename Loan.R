setwd("E:/Data Science/Loan_Data")
traindata=read.csv("train.csv")
testdata=read.csv("test.csv")
testdata$Loan_Status="R"
combined=rbind(traindata,testdata)
str(traindata)
library(dplyr)
combined %>% distinct(Dependents)
levels(combined$Gender)
levels(combined$Gender)[1] = "Others"
levels(combined$Married)
levels(combined$Married)[1] = "TBC"
levels(combined$Dependents)
levels(combined$Dependents)[1] = "-1"
levels(combined$Education)

combined$ApplicantIncome[is.na(combined$ApplicantIncome)] <- mean(combined$ApplicantIncome, na.rm = TRUE)

combined$CoapplicantIncome[is.na(combined$CoapplicantIncome)] <- 0

filter(combined,!is.na(LoanAmount)) %>% group_by(Education) %>% 
  summarise(df=mean(LoanAmount)) %>% filter(Education == "Graduate") %>% select(df)

filter(combined,!is.na(LoanAmount)) %>% group_by(Education) %>% 
  summarise(df=mean(LoanAmount)) %>% filter(Education == "Not Graduate") %>% select(df)


combined[(is.na(combined$LoanAmount) & combined$Education == "Graduate"),"LoanAmount"] = 149.29

combined[(is.na(combined$LoanAmount) & combined$Education == "Not Graduate"),"LoanAmount"] = 118.47

filter(combined,Married=="TBC") %>% select(Married,Dependents,Gender,CoapplicantIncome)

combined[(combined$Married =="TBC" & combined$CoapplicantIncome == "754"),"Married"] = "Yes"

combined[(combined$Married =="TBC" & combined$CoapplicantIncome == "0"),"Married"] = "No"

filter(combined,Gender=="Others" & Married == "No") %>%
  select(Married,Dependents,Gender,ApplicantIncome,CoapplicantIncome,Self_Employed,Education)

filter(combined,Married == "No" & Gender %in% c("Male","Female")) %>%
  group_by(Education,Gender) %>% summarise(df=median(ApplicantIncome))

combined[(combined$Gender == "Others" & combined$Education == "Graduate" & 
          combined$ApplicantIncome > 4040),"Gender"] = "Male"

combined[(combined$Gender == "Others" & combined$Education == "Graduate" & 
            combined$ApplicantIncome < 4040),"Gender"] = "Female"

combined[(combined$Gender == "Others" & combined$Education == "Not Graduate" & 
            combined$ApplicantIncome > 3270),"Gender"] = "Male"

combined[(combined$Gender == "Others" & combined$Education == "Not Graduate" & 
            combined$ApplicantIncome < 3270),"Gender"] = "Female"

filter(combined,Gender=="Others" & Married == "No") %>%
  select(Married,Dependents,Gender,ApplicantIncome,CoapplicantIncome,Self_Employed,Education)

filter(combined,Married == "TBC")

combined %>% group_by(Gender,Married,Dependents) %>% summarise(df=n())

combined[(combined$Dependents == "-1"),"Dependents"] = "0"

class(combined$CoapplicantIncome)

names(combined)

combined$Gender <- factor(combined$Gender)
combined$Married <- factor(combined$Married)
combined$Dependents <- factor(combined$Dependents)
combined$Education <- factor(combined$Education)

levels(combined$Gender)
levels(combined$Married)
levels(combined$Dependents)
levels(combined$Education)
levels(combined$Self_Employed)

str(combined)

combined %>% distinct(Loan_Status)

combined %>% distinct(Property_Area)

combined %>% filter(Loan_Status == "Y") %>% group_by(Credit_History,Loan_Amount_Term,Loan_Status) %>% summarise(df=n())

combined %>% filter(!is.na(Loan_Amount_Term)) %>% group_by(Loan_Amount_Term) %>% summarise(df=n())

combined[(is.na(combined$Loan_Amount_Term)),"Loan_Amount_Term"] = 360

combined[(is.na(combined$Credit_History) & combined$Loan_Amount_Term == 480),"Credit_History"] = 1

combined %>% filter(is.na(Credit_History)) %>% group_by(Loan_Amount_Term) %>% summarise(df=n())

combined %>% filter(!is.na(Credit_History) , Loan_Status !=  "R") %>% 
                                     group_by(Loan_Amount_Term,Credit_History) %>% 
                                        summarise(df=n())

combined %>% filter(is.na(Credit_History) , Loan_Status !=  "R") %>% 
  group_by(Loan_Status,Credit_History) %>% 
  summarise(df=n())

combined[is.na(combined$Credit_History) & combined$Loan_Status == "Y","Credit_History"] = 1

combined[is.na(combined$Credit_History) & combined$Loan_Status == "N","Credit_History"] = 0

combined[is.na(combined$Credit_History) & combined$Loan_Status == "R","Credit_History"] = 1

combined %>% filter(is.na(Credit_History) , Loan_Status ==  "R") %>% 
  group_by(Loan_Amount_Term,Credit_History) %>% 
  summarise(df=n())

combined$Credit_History <- factor(combined$Credit_History)

levels(combined$Credit_History)

levels(combined$Self_Employed)

levels(combined$Self_Employed)[1] = "Others"

combined$Self_Employed <- factor(combined$Self_Employed)

combined %>% group_by(Self_Employed,Education) %>%
         summarise(df=median(ApplicantIncome))

combined[(combined$Self_Employed == "Others") & combined$Education == "Graduate" 
         & combined$ApplicantIncome > 5825,"Self_Employed"] = "Yes" 

combined[(combined$Self_Employed == "Others") & combined$Education == "Not Graduate" 
         & combined$ApplicantIncome < 4344 & combined$ApplicantIncome > 3870,"Self_Employed"] = "Yes" 

combined[(combined$Self_Employed == "Others") & combined$Education == "Graduate" 
               & combined$ApplicantIncome > 3870,"Self_Employed"] = "No" 

combined[(combined$Self_Employed == "Others") & combined$Education == "Not Graduate" 
         & combined$ApplicantIncome < 3270,"Self_Employed"] = "No" 

combined[(combined$Self_Employed == "Others") & combined$Education == "Not Graduate" 
         & combined$ApplicantIncome < 3270,"Self_Employed"] = "No" 

combined[(combined$Self_Employed == "Others") & combined$Education == "Not Graduate" 
         & combined$ApplicantIncome < 3270,"Self_Employed"] = "No" 

combined %>% filter(combined$Self_Employed == "Others") %>% 
  group_by(Education) %>% 
  summarise(df=n(),df1=median(ApplicantIncome))

combined[(combined$Self_Employed == "Others"), "Self_Employed"] = "Yes"

names(combined)

combined %>% filter(is.na(Loan_Status)) %>% 
  group_by(Education) %>% 
  summarise(df=n(),df1=median(ApplicantIncome))

new_train <- combined[1:nrow(traindata),]

new_test <- combined[-(1:nrow(traindata)),]

levels(new_train$Loan_Status)

new_train$Loan_Status <- factor(new_train$Loan_Status)

levels(new_test$Loan_Status)

new_test$Loan_Status <- factor(new_test$Loan_Status)

library(caTools)

splitvar = sample.split(new_train$Loan_Status,SplitRatio = .7)

# this will create a logical vector.

splitvar
traindsn  =subset(new_train,splitvar == TRUE)
testdsn  =subset(new_train,splitvar == FALSE)

traindsn
str(traindsn)
str(testdsn)

prop.table(table(traindsn$Loan_Status))
prop.table(table(testdsn$Loan_Status))
prop.table(table(traindata$Loan_Status))

str(traindsn)
traindsn$Loan_ID = NULL
library(randomForest)
traindata$Loan_Status = as.factor(traindata$Loan_Status)
traindsn$Loan_Status = as.factor(traindsn$Loan_Status)
testdsn$Loan_Status = as.factor(testdsn$Loan_Status)
rfmod = randomForest(Loan_Status ~  .,data=traindsn)
predTrain = predict(rfmod)
rfmod
tt=table(traindsn$Loan_Status,predTrain)
tt
sum(diag(tt))/sum(tt)

predTest = predict(rfmod,newdata=testdsn)
predTest
rfmod
tt=table(testdsn$Loan_Status,predTest)
tt
sum(diag(tt))/sum(tt)

varImpPlot(rfmod)

predTest = predict(rfmod,newdata=new_test)

predTest
new_test$Loan_Status=predTest
write.csv(new_test,file="Loan_Predict.csv")

rfmod
tt=table(new_test$Loan_Status,predTest)
tt
sum(diag(tt))/sum(tt)




