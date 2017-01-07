setwd("/Users/chenkf/Book/Textbooks/Courses/UMSI/SI 601&618/SI 618-Project")
library('ggplot2')
'
This research employed a binary variable, DEFAULT payment (Yes = 1, No = 0), as the response variable. This study reviewed the literature and used the following 23 variables as explanatory variables: 
X1: Amount of the given credit (NT dollar): it includes both the individual consumer credit and his/her family (supplementary) credit. 
X2: Gender (1 = male; 2 = female). 
X3: Education (1 = graduate school; 2 = university; 3 = high school; 4 = others). 
X4: Marital status (1 = married; 2 = single; 3 = others). 
X5: Age (year). 
X6 - X11: History of past payment. We tracked the past monthly payment records (from April to September, 2005) as follows: X6 = the repayment status in September, 2005; X7 = the repayment status in August, 2005; . . .;X11 = the repayment status in April, 2005. The measurement scale for the repayment status is: -1 = pay duly; 1 = payment delay for one month; 2 = payment delay for two months; . . .; 8 = payment delay for eight months; 9 = payment delay for nine months and above. 
X12-X17: Amount of bill statement (NT dollar). X12 = amount of bill statement in September, 2005; X13 = amount of bill statement in August, 2005; . . .; X17 = amount of bill statement in April, 2005. 
X18-X23: Amount of previous payment (NT dollar). X18 = amount paid in September, 2005; X19 = amount paid in August, 2005; . . .;X23 = amount paid in April, 2005. 
'

dt=read.csv("credit.csv",header=TRUE,stringsAsFactors=F)
# take out abnormal record
abnormal = which(dt$EDUCATION == 0 | dt$EDUCATION == 5 | dt$EDUCATION == 6 | dt$MARRIAGE == 0)
dt = dt[-abnormal,]

# build a new data frame to use subset of the original data
credit = dt[,c("LIMIT_BAL","SEX","EDUCATION","MARRIAGE","AGE")]
credit = cbind(credit,apply(dt[,7:12],1,sum))
credit = cbind(credit,apply(dt[,13:18],1,mean))
credit = cbind(credit,apply(dt[,19:24],1,mean))
credit = cbind(credit,dt[,"default.payment.next.month"])

# extract dimension
n=dim(credit)[1]
#[1] 29601
p=dim(credit)[2]
#[1] 9
# change variable names
colnames(credit)[6:9]=c("delay.sum","bill.mean","repayment.mean","DEFAULT")

#[1] "LIMIT_BAL"      "SEX"            "EDUCATION"      "MARRIAGE"      
#[5] "AGE"            "delay.sum"      "bill.mean"      "repayment.mean"
#[9] "DEFAULT"   

# code some variables as factor
credit$DEFAULT=as.factor(credit$DEFAULT)
credit$SEX=as.factor(credit$SEX)
credit$EDUCATION=as.factor(credit$EDUCATION)
credit$MARRIAGE=as.factor(credit$MARRIAGE)

# change factor level names
levels(credit$SEX)[levels(credit$SEX)==1]='Male'
levels(credit$SEX)[levels(credit$SEX)==2]='Female'
levels(credit$EDUCATION)[levels(credit$EDUCATION)==1]='graduate school'
levels(credit$EDUCATION)[levels(credit$EDUCATION)==2]='university'
levels(credit$EDUCATION)[levels(credit$EDUCATION)==3]='high school'
levels(credit$EDUCATION)[levels(credit$EDUCATION)==4]='others'
levels(credit$MARRIAGE)[levels(credit$MARRIAGE)==1]='married'
levels(credit$MARRIAGE)[levels(credit$MARRIAGE)==2]='single'
levels(credit$MARRIAGE)[levels(credit$MARRIAGE)==3]='others'

# Q1
# visualization
par(mar=c(3,3,3,3))
# age
qplot(AGE, data = dt, geom = "histogram", binwidth = 1, alpha = I(.5), main = "Histograms of Age")
# education
education=as.numeric(table(credit$EDUCATION))
a=plot(credit$EDUCATION,main="Education",col=c("red","blue","green","yellow"),ylim=c(0,max(education)+2000))
legend(x="topright",legend=c("graduate school","university","high school","others"),col=c("red","blue","green","yellow"),pch=15)
text(a, education+1000, sprintf("%.1f %%", 100*education/sum(education)))
# marriage
marriage=as.numeric(table(credit$MARRIAGE))
b=plot(credit$MARRIAGE,main="Marriage",col=c("cyan3","darkorchid",257),ylim=c(0,max(marriage)+2000))
legend(x="topright",legend=c("married","single","other"),col=c("cyan3","darkorchid",257),pch=15)
text(b, marriage+1000, sprintf("%.1f %%", 100*marriage/sum(marriage)))
# sex
sex=as.numeric(table(credit$SEX))
c=plot(credit$SEX,main="Gender",col=c("blue2","violetred1"),ylim=c(0,max(sex)+2000))
legend(x="topleft",legend=c("male","female"),col=c("blue2","violetred1"),pch=15)
text(c, sex+1000, sprintf("%.1f %%", 100*sex/sum(sex)))

# Q2
ggplot(credit, aes(x=SEX, y=LIMIT_BAL,col=SEX)) + geom_boxplot() + xlab("Gender") + ylab("Credit Amount") + theme(text = element_text(size=13))
ggplot(credit, aes(x=EDUCATION, y=LIMIT_BAL,col=EDUCATION)) + geom_boxplot() + xlab("Education") + ylab("Credit Amount") + theme(text = element_text(size=13))
ggplot(credit, aes(x=MARRIAGE, y=LIMIT_BAL,col=EDUCATION)) + facet_grid(.~EDUCATION) + geom_boxplot() + ylab("Credit Amount") + theme(text = element_text(size=13))

# Q3
ggplot(credit, aes(x=EDUCATION, y=delay.sum, col=EDUCATION)) + geom_boxplot() + xlab("Education") + ylab("Sum of Days of Delay Payment") + theme(text = element_text(size=15))
ggplot(credit, aes(x=MARRIAGE, y=delay.sum, col=MARRIAGE)) + geom_boxplot() + xlab("x") + ylab("y") + theme(text = element_text(size=15))

# Q4
#variable selection
library(leaps)
regfit.full = regsubsets(DEFAULT~., data=credit,method="exhaustive",all.best=T)
summary(regfit.full)
rs=summary(regfit.full)
# cp
plot(2:9, rs$cp, xlab="No. of Parameters",ylab="Cp")
abline(0, 1)
which.min(rs$cp)
#[1] 8
# adjusted r^2
plot(2:9, rs$adjr2, xlab="No. of Parameters",ylab="Adjusted R^2")
which.max(rs$adjr2)
#[1] 8
# bic
plot(2:9, rs$bic, xlab="No. of Parameters",ylab="BIC",main="BIC versus No. of Parameters",pch=16,col=10)
lines(2:9, rs$bic)
which.min(rs$bic)
#[1] 5
#"LIMIT_BAL","SEX","MARRIAGE","delay.sum","repayment.mean"

# split the data into 3 groups for the purpose of cross-validation
set.seed(1)
dt1=sample(1:n, size=n/3, replace=FALSE)
set.seed(2)
dt2=sample((1:n)[!(1:n %in% dt1)], size=n/3, replace=FALSE)
dt3=(1:n)[!(1:n %in% c(dt1,dt2))]
ls=list(dt1,dt2,dt3)

#Logit
logit.error=numeric(3)
logit.FOR=numeric(3)
for (i in 1:3) {
  testing.id=ls[[i]]
  training.id=(1:n)[!(1:n %in% testing.id)]
  set.seed(100)
  logit.fit=glm(DEFAULT~LIMIT_BAL+SEX+MARRIAGE+delay.sum+repayment.mean,data=credit,subset=training.id,family=binomial)
  logit.prob=predict(logit.fit,credit[testing.id,],type="response")
  threshold=seq(0.1,0.5,by=0.01)
  best.threshold=0
  min.error=1
  min.FOR=1
  for (j in threshold) {
    logit.pred=rep(0,length(ls[[i]]))
    logit.pred[logit.prob>j]=1
    error=mean(logit.pred!=credit[testing.id,"DEFAULT"])
    x=table(logit.pred,credit[testing.id,"DEFAULT"])
    FOR=prop.table(x,2)[1,2]
    if (error<0.5 & FOR<min.FOR) {
      best.threshold=j
      min.error=error
      min.FOR=FOR
    }
  }
  logit.error[i]=min.error
  logit.FOR[i]=min.FOR
}
best.threshold
#[1] 0.16
mean(logit.error)
#[1] 0.4826526
mean(logit.FOR)
#[1] 0.1530215

## random forest
library(randomForest)
rf.error=numeric(3)
rf.FOR=numeric(3)
for (i in 1:3) {
  testing.id=ls[[i]]
  training.id=(1:n)[!(1:n %in% testing.id)]
  set.seed(100)
  rf.fit=randomForest(DEFAULT~.,data=credit,subset=training.id,mtry=4,importance=T)
  rf.pred=predict(rf.fit,newdata=credit[testing.id,])
  rf.error[i]=mean(rf.pred!=credit[testing.id,"DEFAULT"])
  x=table(rf.pred,credit[testing.id,"DEFAULT"])
  rf.FOR[i]=prop.table(x,2)[1,2]
}
mean(rf.error)
#[1] 0.2047566
mean(rf.FOR)
#[1] 0.6674119
importance(rf.fit)
varImpPlot(rf.fit)

library(gbm)
name=c("Average bill statement", "Average monthly repayment",
       "Sum of months with delayed payment","Age","Limit balance",
       "Education","Marriage","Gender")
name=rev(name)
par(mar=c(3,13,2,2))
barplot(sort(rf.fit$importance[,4]),horiz=T,col="blue",names.arg=name,cex.names=0.8,las=1,main="Variable Importance")