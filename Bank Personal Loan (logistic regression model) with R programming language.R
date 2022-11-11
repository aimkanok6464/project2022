# Logistic Regression

library(readxl)
data <- read_excel("Bank_Personal_Loan.xlsx", sheet = "Data")
data

colnames(data)
colnames(data) <- c("ID","Age","Experience","Income","ZIPCode","Family",
                    "CCAvg","Education","Mortgage","PersonalLoan",
                    "SecuritiesAccount","CDAccount","Online","CreditCard")
colnames(data)

#type
sapply(data, class)

### Multicollinearity
install.packages("DAAG",dependencies = TRUE)
library(DAAG)
data <- data[, !(colnames(data) %in% c("ID","ZIPCode"))]
#vif(glm(`Personal Loan`~., data=data, family=binomial))
# VIF = 1: there is no corrilation anomg predictors
# VIF >4 : moderate multicollinearity occurs
# VIF >10 : serious multicollinearity occurs
# so, we drop the variable which has larger Variance Inflation Factor (VIF) : "Age" variable

#vif(glm(`Personal Loan`~.-Age, data=data, family=binomial))
# VIF = 1: there is no corrilation anomg predictors
# VIF >4 : moderate multicollinearity occurs
# VIF >10 : serious multicollinearity occurs


# Drop the columns of the dataframe 
data<- subset(data, select = -c(Experience,income2))
data


#How to deal with Class Imbalance?
table(data$PersonalLoan)

# convert numeric to factors 
data$Education<- as.character(as.numeric(data$Education))

# convert numeric to factors
data$SecuritiesAccount<- factor(data$SecuritiesAccount, levels = c(0, 1))
data$CDAccount<- factor(data$CDAccount, levels = c(0, 1))
data$Online<- factor(data$Online, levels = c(0, 1))
data$CreditCard<- factor(data$CreditCard, levels = c(0, 1))
data$PersonalLoan<-factor(data$PersonalLoan,levels = c(0, 1))


#check graph Transform
logitplot<-function(y,x,ncat=10,...) 
{
  brksx<-unique(quantile(x,probs=(0:ncat)/ncat))
  nbrksx<-length(brksx)
  cutx<-cut(x,breaks=brksx,include.lowest=TRUE)
  yt<-table(data.frame(y,cutx))
  mx<-tapply(x,cutx,FUN=mean)
  logity<-log((yt[2,]+0.5)/(yt[1,]+0.5))
  plot(mx,logity,...)
}
par(mfrow=c(2,3))
logitplot(data$PersonalLoan,data$Age,xlab="Age",ylab="adjusted sample logit")
logitplot(data$PersonalLoan,data$Income,xlab="Income",ylab="adjusted sample logit")
logitplot(data$PersonalLoan,data$CCAvg,xlab="CCAvg",ylab="adjusted sample logit")
logitplot(data$PersonalLoan,data$Mortgage,xlab="Mortgage",ylab="adjusted sample logit")
logitplot(data$PersonalLoan,data$Family,xlab="Family",ylab="adjusted sample logit")

par(mfrow=c(2,3))
logitplot(data$PersonalLoan,data$Age,xlab="Age",ylab="adjusted sample logit")
logitplot(data$PersonalLoan,data$Income,xlab="Income",ylab="adjusted sample logit")
logitplot(data$PersonalLoan,data$CCAvg^2,xlab="CCAvg",ylab="adjusted sample logit")
logitplot(data$PersonalLoan,data$Mortgage^2,xlab="Mortgage",ylab="adjusted sample logit")
logitplot(data$PersonalLoan,data$Family,xlab="Family",ylab="adjusted sample logit")

data$income2<-data$Income^2

#train-test split
#install.packages('caret', dependencies = TRUE)
library(caret)
'%ni%' <- Negate('%in%')  
options(scipen=999)  


# # Prep Training and Test data.
# set.seed(1)
# trainDataIndex <- createDataPartition(data$PersonalLoan, p=0.7, list = F)  # 70% training data
# trainData <- data[trainDataIndex, ]
# testData <- data[-trainDataIndex, ]
# 
table(data$PersonalLoan)


# Up Sample
set.seed(1)
up_data <- upSample(x = data[,colnames(data) %ni% "PersonalLoan"],
                    y = data$PersonalLoan,
                    yname = "PersonalLoan")

table(up_data$PersonalLoan)


# Build Logistic Model
fullmodel<- glm(PersonalLoan~.,family = binomial,data=up_data)
summary(fullmodel)

# convert numeric to factors 
up_data$PersonalLoan<- as.numeric(as.character(up_data$PersonalLoan))

# Hosmer-Lemeshow Goodness-of-fit test for Binary response
hosmerlem <- function (y, yhat, g = 3)
{
  cutyhat <- cut(yhat, breaks = quantile(yhat, probs = seq(0,1, 1/g)), include.lowest = T)
  obs <- xtabs(cbind(1 - y, y) ~ cutyhat)
  expect <- xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  chisq <- sum((obs - expect)^2/expect)
  P <- 1 - pchisq(chisq, g - 2)
  c("X^2" = chisq, Df = g - 2, "P(>Chi)" = P)
}
hosmerlem(up_data$PersonalLoan, polymodel$fitted.values)


#AIC
model1<-step(fullmodel,trace=F)
summary(model1)

#backward
drop1(glm(PersonalLoan~Age+Income+Family+CCAvg+Mortgage+Education+SecuritiesAccount+CDAccount+Online+CreditCard,data=up_data),test="F")
#drop Mortgage
drop1(glm(PersonalLoan~Age+Income+Family+CCAvg+Education+SecuritiesAccount+CDAccount+Online+CreditCard,data=up_data),test="F")
#drop Age
model2<-glm(PersonalLoan~Income+Family+CCAvg+Education+SecuritiesAccount+CDAccount+Online+CreditCard,family = binomial,data=up_data)
summary(model2)


#transform
model3<-glm(PersonalLoan~Age+Income+I(Income^2)+Family+CCAvg+I(CCAvg^2)+Mortgage+I(Mortgage^2)+Education+SecuritiesAccount+CDAccount+Online+CreditCard,family = binomial,data=up_data)
summary(model3)

#BIC
model4<-step(fullmodel,k=log(768),trace=F)
summary(model4)
