library(ggplot2)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caTools)
library(descr)
library(party)
library(MASS)
library(tidyverse)
library(caret)
library(ISLR)
library(ROCR)

#get working directory
getwd()

# import needed dataset into our global environment
Bank=read.table(file = "C:/Users/DELL G5/Downloads/bank (2).csv", header = TRUE, sep=";")


#summarise the data and check for any missing values.
str(Bank)
dim(Bank)
names(Bank) 
head(Bank)
summary(Bank)
sum(is.na(Bank))
sum(duplicated.array(Bank))

#converting data to numeric/factor where applicable
BankNum<- Bank
BankNum$age <- as.numeric(BankNum$age)
BankNum$day <as.numeric(BankNum$day)
BankNum$previous <- as.numeric(BankNum$previous)
BankNum$pdays <- as.numeric(BankNum$pdays)
BankNum$duration <- as.numeric(BankNum$duration)
BankNum$campaign <- as.numeric(BankNum$campaign)
BankNum$y<-as.factor(BankNum$y)
BankNum[sapply(BankNum, is.character)] <- lapply(BankNum[sapply(BankNum, is.character)], as.factor)
BankNum[sapply(BankNum, is.integer)] <- lapply(BankNum[sapply(BankNum, is.integer)], as.numeric)
sapply(BankNum,class)
BankNum <- BankNum %>% mutate(y = ifelse(y == "no",0,1))
BankNum$y<- as.factor(BankNum$y)
str(BankNum)

#What is the predominant output for variable y in our dataset
prop.table(table(BankNum$y))

#correlation analysis 
ggcorr(BankNum, label = TRUE, label_alpha = TRUE)  



#Conduct Exploratory data analysis to check how some different factors affect the output variable y. In this case we 
#check to see the effect that the individuals job, education level and age has on the output variable y.
barplot(table(BankNum$y, BankNum$job), main = "How does the job of an individual affect output y?", xlab = "jobs", col = c("purple","pink"), legend=rownames(table(BankNum$y, BankNum$job)), beside= TRUE)
barplot(table(BankNum$y, BankNum$education), main = "How does the education level of an individual affect output y?", xlab = "education level", col = c("red","blue"), legend=rownames(table(BankNum$y, BankNum$education)), beside= TRUE)
barplot(table(BankNum$y, BankNum$age), main = "How does the age of an individual affect output y?", xlab = "age", col = c("black","pink"), legend=rownames(table(BankNum$y, BankNum$age)), beside= TRUE)

#further exploratory analysis
ggplot(banks, aes(x=Medu, y=G3))+geom_point()+geom_smooth(method = lm)


#Determine how likely it is for a given output to occur for each variable using plots 
par(mfrow=c(2,2))
for(i in 1:length(Bank))
{barplot(prop.table(table(Bank[,i])) ,col = "green", xlab=names(Bank[i]), ylab= "probability of this output occuring")}


#create training & testing set and build classification tree 
set.seed(1200)
ind<- sample(2, nrow(BankNum), replace = TRUE, prob = c(0.7,0.3))
train.data <- BankNum[ind ==1, ]
test.data <- BankNum[ind ==2, ]
classtree<- rpart(y~., train.data)
rpart.plot(classtree)

#LOGISTIC REGRESSION
BankLogi<- glm(y~., data= train.data, family= "binomial")
summary(BankLogi)
BankLR<- predict(BankLogi, test.data, type = "response")
PredBank= round(BankLR)
LogiTable<- table(PredictedOutput=PredBank, Reference= test.data$y)
LogiTable
accuracy= sum(diag(LogiTable))/sum(LogiTable)
accuracy 

#AUC ROC
BankROC<- prediction(BankLR, test.data$y)
BankCurve<- performance(BankROC, "tpr","fpr")
plot(BankCurve)
BankAUC<- performance(BankROC, "auc")
BankAUC@y.values[[1]]

#OUTPUT PREDICTION USING CLASSIFICATION TREES and K-NEAREST NEIGHBOURS.

#CLASSIFICATION TREE
#predict the output using the classification tree we have identified earlier 
classtree<- rpart(y~., train.data)
#rpart.plot(classtree)
bank_predict<- predict(classtree, test.data, type= "class")
confusionMatrix(bank_predict, as.factor(test.data$y))

#validate using cross-validation and check for how effective the current testing and training datasets are
CrossTable(as.factor(test.data$y), bank_predict, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,dnn = c( 'real output', 'predicted output'))


#KNN 
# using K nearest neighbours method to use a set of input values to predict an output
banKnn<-train(y~., data = train.data, method="knn", maximize= TRUE, trControl= trainControl(method = "cv",number= 10))
KnnPred<- predict(banKnn, newdata= test.data )
confusionMatrix(KnnPred, test.data$y)


#validate the above using cross validation table 
CrossTable(test.data$y, KnnPred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,dnn = c( 'real output', 'predicted output'))
