library(ISLR)
library(GGally)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(MASS)
library(dplyr)
library(corrplot)
library(tidyverse)
library(caret)
library(leaps)
library(rattle)
library(randomForest)

#get working directory
getwd()

#import needed dataset into global environment
school1 <-read.table(file="C:/Users/DELL G5/Downloads/student-mat (1).csv", sep=";", header=TRUE)
school2 <-read.table(file="C:/Users/DELL G5/Downloads/student-por (1).csv", sep=";", header=TRUE)
schools <- rbind(school1,school2)


#summarise data and check for any missing value
dim(schools)
names(schools)
str(schools)
head(schools)
sum(is.na(schools))
sum(duplicated.array(schools))
summary(schools)

#we remove g1 and g2 from the data as we cannot use it in our analysis
schools<- schools[, -c(31:32)]


#Perform correlation analysis to determine if there is a correlation between the variables, and to determine the strength of it if there is. 
ggcorr(schools, label = TRUE, label_alpha = TRUE)     

#CHecking what the predominant age group and gender is in the dataset, and check for predominant result of G3
ggplot(schools, aes(x= age)) + geom_histogram(stat = "count", color="orange", fill="orange")
ggplot(schools, aes(x= sex)) + geom_histogram(stat = "count", color="purple", fill="purple")
ggplot(schools, aes(x= G3)) + geom_bar(stat = "count", color= "red", fill="red")
ggplot(schools, aes(x= internet)) + geom_bar(stat = "count", color= "green", fill="green")
ggplot(schools, aes(x= failures)) + geom_bar(stat = "count", color= "green", fill="green")

#EXPLORATORY DATA ANALYSIS

#check for any association between select variables and G3
ggplot(schools, aes(x=age, y=G3))+geom_point()+geom_smooth(method = lm)
ggplot(schools, aes(x=studytime, y=G3))+geom_point()+geom_smooth(method = lm)
ggplot(schools, aes(x=failures, y=G3))+geom_point()+geom_smooth(method = lm)
ggplot(schools, aes(x=higher, y=G3))+geom_point()+ geom_smooth(method = lm)
ggplot(schools, aes(x=Medu, y=G3))+geom_point()+geom_smooth(method = lm)
ggplot(schools, aes(x=famrel, y=G3))+geom_point()+geom_smooth(method = lm) #no effect
ggplot(schools, aes(x=absences, y=G3))+geom_point()+geom_smooth(method = lm)

#is there an association between the age of a student and the number of failures they have experienced with regards to G3?
ggplot(schools, aes(x=failures, y=age))+geom_point()+geom_smooth(method = lm)

#convert dichotomous variables for gender, address and internet to dummy variable to allow for easier analysis
schools <- schools %>% mutate(internet = ifelse(internet == "no",0,1))
schools <- schools %>% mutate(address = ifelse(address == "R",0,1))
schools <- schools %>% mutate(sex = ifelse(sex == "F",0,1))
schools <- schools %>% mutate(schoolsup = ifelse(schoolsup == "no",0,1))
schools <- schools %>% mutate(famsup = ifelse(famsup == "no",0,1))
schools <- schools %>% mutate(Pstatus = ifelse(Pstatus == "A",0,1))


#create training and test dataset to assess the predictive performance of the regression models
set.seed(1200)
ind<- sample(2, nrow(schools), replace = TRUE, prob = c(0.7,0.3))
train.data <- schools[ind ==1, ]
test.data <- schools[ind ==2, ]


#CONDUCT REGRESSION ANALYSIS

#define mlr model including all variables in the dataset, except G1 & G2 obviously.
gmodel<-lm(G3~., data=train.data)
summary(gmodel)

#create stepwise regression model in order to identify subset of variables that result in the best performing model
step.G3<- stepAIC(gmodel, direction = "both", trace = FALSE)
summary(step.G3)

#we can further confirm the presence of certain variables in the model by seeing a visual of the stepwise regression
G3Model<- regsubsets(G3~., data = schools, nvmax=6, method = "seqrep")
summary(G3Model)


#We conduct cross validation in order to check for validity of the model we have produced.
set.seed(1200)
train.control<- trainControl(method = "cv", number=10)
step.G3<- train(G3~., data=schools, method = "leapSeq", tunegrid= data.frame( nvmax = 0:6), trControl= train.control)
step.G3$results

#check to see which model gives us the best predictions
step.G3$bestTune
#from the above, we identify that the 4th model is the best model. 

summary(step.G3$finalModel)


#therefore, create model including only the significant variables that we have identified as a result of the methods used above
coef(step.G3$finalModel, 4)

#the model would be: G3= 9.251+0.445Medu-1.918failures-0.92paid+1.788higher


#classification tree
Schoolsclasstree<- rpart(G3~., train.data)
rpart.plot(Schoolsclasstree)
GTreePredict<- predict(Schoolsclasstree, test.data)
G3Predicted<- round(GTreePredict)
G3Table<- table(PredictedOutput=G3Predicted, Reference= test.data$G3)
G3Table
accuracyG3= sum(diag(G3Table))/sum(G3Table)
accuracyG3 

