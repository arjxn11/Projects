library(tidyverse)
library(ggplot2)
library(GGally)
library(ISLR)
library(ggfortify)
library(cluster)
library(factoextra)
library(FactoMineR)

#get working directory
getwd()


#import needed dataset into our global environment
ewcs<-read.table("EWCS_2016.csv", header = TRUE, sep = ",")
ewcs[,][ewcs[, ,] == -999] <- NA
kk=complete.cases(ewcs)
ewcs=ewcs[kk,]

#summarise dataset and check for missing values
str(ewcs)
dim(ewcs)
names(ewcs)
summary(ewcs)
sum(is.na(ewcs))
view(ewcs)

#Determining the predominant age and gender of the individuals in our sample
ewcs%>% count(Q2a, sort = FALSE) 
ggplot(ewcs, aes(x= Q2b)) + geom_histogram(stat = "count", color="black", fill="red")


# Perform Correlation analysis to determine if there is any correlation between the questions.
ggcorr(ewcs, label = TRUE, label_alpha = TRUE)

# Determine count for the responses to each question
ewcs%>% count(Q87a, sort = FALSE) 
ewcs%>% count(Q87b, sort = FALSE)
ewcs%>% count(Q87c, sort = FALSE)
ewcs%>% count(Q87d, sort = FALSE)
ewcs%>% count(Q87e, sort = FALSE)
ewcs%>% count(Q90a, sort = FALSE)
ewcs%>% count(Q90b, sort = FALSE)
ewcs%>% count(Q90c, sort = FALSE)
ewcs%>% count(Q90f, sort = FALSE)

#Create barcharts to help visualise the answers given by respondents to the survey
SurveyResponse<- ewcs[, 3:11]
ggplot(gather(SurveyResponse), aes(value)) + geom_bar(stat="count", color="red", fill="red") +  facet_wrap(~key, scales = 'free')



# Principal Components Analysis
pca.out<-prcomp(ewcs, scale. = TRUE)
names(pca.out)
biplot(pca.out, scale = 0, cex=0.6)
summary(pca.out)

#Corrected first two principal components biplot
pca.out$rotation= -pca.out$rotation
pca.out$x=-pca.out$x
biplot(pca.out, scale =, cex=0.6)

#Determine loadings
pca.load<- function(loading, comp.sdev){loading*comp.sdev}
loading<- pca.out$rotation
comp.sdev<-pca.out$sdev 

pca.vars<- t(apply(loading, 1, pca.load, comp.sdev))
pca.vars[, 1:2]

#determine variance of each principal component. the standard deviation for each principal component is given through prcomp()
pca.var<- pca.out$sdev^2
pca.var

#calculate Proportion of Variance Explained (PVE) by each principal component
pve<- pca.var/sum(pca.var)
pve

#plot a scree plot to help visualise the PVE
plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0,1), type = 'b')


#get eigenvalues and plot scree plot for the same as above
eig.val<- get_eigenvalue(pca.out)
eig.val
fviz_eig(pca.out)


#CLUSTER ANALYSIS 

#first, we set seed, state a maximum number of clusters that we accept and also scale the dataset in order to simplify the task
set.seed(1200)
EWCScale<- scale(ewcs)
fviz_nbclust(EWCScale, kmeans, method = "wss") 
#using elbow method, we can identify that optimal number of clusters is 2
StatsGap= clusGap(EWCScale, FUN= kmeans, nstart=20, K.max = 10, B=20)
fviz_gap_stat(StatsGap) #from this, we verify that the optimal number of clusters we identified earlier, 2, is correct.

#use the optimal number of clusters identified above to perform cluster analysis
set.seed(100)
ClusAnalysis<- kmeans(EWCScale, centers = 2, nstart = 20)
ClusAnalysis
fviz_cluster(ClusAnalysis, data = EWCScale) 

#determine the number of members in each cluster followed by the mean of both the clusters you have defined
ClusAnalysis.count<-ClusAnalysis$cluster
table(ClusAnalysis.count)
aggregate(ewcs, by=list(cluster=ClusAnalysis$cluster), mean)







