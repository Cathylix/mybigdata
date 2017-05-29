
li############################################################################################
# BIG DATA GROUP PROJECT 2
# GROUP 6
# ANALYSIS OF MUSHROOM DATA 
# GROUP MEMBERS : AFREEN, ANANYA, AUPAMA, POOJA
############################################################################################
# Set the working directory
setwd("~/Desktop/GWU-Spring2016/BigData/GroupProject2")

#load all csv's from mushroom data set
rawMushroomData <- read.table("agaricus-lepiota.data",sep=",")
rawMushroomData <- rawMushroomData[complete.cases(rawMushroomData),]
rawMushroomDF <- as.data.frame(rawMushroomData)
colnames(rawMushroomDF) <- c("EdibleOrPoisonous","capShape","capSurface","capColor","bruises","odor","gAttachment","gSpacing","gSize","gColor","stalkShape","stalkRoot","stalksurfAboveRing","stalksurfbelowRing","stalkColorAboveRing","stalkColorBelowRing","veilType","veilColor","ringNo","ringType","sporePrintcolor","population","habitat")
rawMushroomDF$veilType <-NULL
rawMushroomDF$stalkRoot <-NULL
rawMushroomDF <- rawMushroomDF[complete.cases(rawMushroomDF),]
# Convert the character factor data to respective numeric data
for (i in names(rawMushroomDF))
{
  rawMushroomDF[,i] <-as.numeric(rawMushroomDF[,i])
}
################# 2-D BAR PLOTS TO STUDY CORRELATION OF EACH ATTRIBUTE AGAINST OUTPUT ATTRIBUTE EDIBLE/POISONOUS
#----------------------------------------------------------------------------------------
EorP_By_Odor <- table(rawMushroomDF$EdibleOrPoisonous,rawMushroomDF$odor)
barplot(as.matrix(EorP_By_Odor),beside=TRUE,col=c("blue","red"),xlab="Distribution by odor",legend=rownames(EorP_By_Odor))

#################### 3D frequency tables and plots #########################################
#-----------------------------------------------------------------------------------------
x<-ftable(rawMushroomDF[c("EdibleOrPoisonous","capShape","capSurface")])
y<-ftable(rawMushroomDF[c("EdibleOrPoisonous","capShape","capColor")])
z<-ftable(rawMushroomDF[c("EdibleOrPoisonous","ringType")])
library(rgl)
plot3d(rawMushroomDF$EdibleOrPoisonous ~ rawMushroomDF$odor|rawMushroomDF$capColor, type='h', lwd=10, col=rainbow(3))
############################################################################################
##################### coorelation plots using pairs function ###############################

pairs(rawMushroomDF$EdibleOrPoisonous ~ rawMushroomDF$odor+ rawMushroomDF$capColor+ rawMushroomDF$bruises)

#===============================================================================
####################   Divide data into training and validation  ###############
#===============================================================================
#========================== 70-30 division ========================================================
set.seed(12345)
row=nrow(rawMushroomDF)
trainindex <- sample(nrow(rawMushroomDF),row*0.7 , replace=FALSE)
trainingdata60 <- rawMushroomDF[trainindex,]
testdata40 <- rawMushroomDF[-trainindex,]
#==================================================================================
# Dimensionality reduction technique no. 1 -- Principal component analysis
#===================================================================================
library("psych")
pc <- principal(trainingdata70,nfactors=6,scores=TRUE)
pc
#==================================================================================
# Dimensionality reduction technique no. 2 --CORRELATION table and plot
#==================================================================================
library(corrplot)
corNumTable<-cor(rawMushroomDF)
corrplot(corNumTable)
#==================================================================================
# Dimensionality reduction technique no. 3 -- STEP AIC to select/discard attributes
#==================================================================================
library("MASS")
trainfit<-glm(EdibleOrPoisonous~capShape+ capSurface+capColor+bruises+odor+gAttachment+gSpacing+gSize+gColor+stalkShape+stalksurfAboveRing+stalksurfbelowRing+stalkColorAboveRing+stalkColorBelowRing+veilColor+ringNo+ringType+sporePrintcolor+population+habitat, data=trainingdata70)
stepTrain<-stepAIC(trainfit,direction="both")
stepTrain$anova
coefficients(stepTrain)
summary(stepTrain)
PredValidation<-predict(trainfit,testdata,se.fit=TRUE)
table(testdata$EdibleOrPoisonous) 
PredValidation
#=========================================================================
######################## Clustering ######################################
#-------------------------------------------------------------------------

myvars = c(1,3,5:10,12:19,21)  # select the attributes by column number returned by STEP AIC
rawMushroomDFReduced = rawMushroomDF[myvars]  

######################## K-MEANS ######################################

wssplot <- function(data, nc=15, seed=1234)
{
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc)
  {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)
  }
  plot(1:nc, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
}
mushDF <- scale(rawMushroomDFReduced)    # Normalize the dataset so kmeans can be applied
wssplot(mushDF)   # plot to decide aptimum number of clusters
#===============================================================================
################# Divide the reduced and normalized data into training and validation
#===============================================================================
#========================= 70-30 division ======================================
set.seed(12345)
row=nrow(mushDF)
trainindex <- sample(nrow(mushDF),row*0.7 , replace=FALSE)
mushtrainingdata70 <- mushDF[trainindex,]
mushtestdata30 <- mushDF[-trainindex,]

#Divide data into training and validation
#===============================================================================
#======================== 50-50 division =======================================
set.seed(12345)
row=nrow(mushDF)
trainindex <- sample(nrow(mushDF),row*0.5 , replace=FALSE)
mushtrainingdata50 <- mushDF[trainindex,]
mushtestdata50 <- mushDF[-trainindex,]
#===============================================================================
#===============================================================================
#======================== 60-40 division =======================================
set.seed(12345)
row=nrow(mushDF)
trainindex <- sample(nrow(mushDF),row*0.6 , replace=FALSE)
mushtrainingdata60 <- mushDF[trainindex,]
mushtestdata40 <- mushDF[-trainindex,]
#===============================================================================
# Apply k-means for 2 clusters, 3 clusters, 5 clusters and 7 clusters for each division
#70-30##########################
library(NbClust)
set.seed(1234)
fit.km <- kmeans(mushtrainingdata70, 7, nstart=25)   
library("clue")
x <-cl_predict(fit.km, mushtestdata30)
fit.km$size
library(cluster)
library("NbClust")
set.seed(1234)
cluster.km70 <- kmeans(mushtraindata70, 7, nstart=25)
library("clue")
pred70 <-cl_predict(cluster.km70, mushtestdata30)
library("cluster")
clusplot(mushtraindata70, cluster.km70$cluster, color=TRUE, shade=TRUE,lines=0)

#summary(fit.km)
#============== below lines count the number of points each cluster of test data =========
length(which(x==1))
length(which(x==2))
length(which(x==3)) #Only if number of clusters is 3 or 5 
length(which(x==4)) #Only if number of clusters is 5
length(which(x==5)) #Only if number of clusters is 5
length(which(x==6)) #Only if number of clusters is 7
length(which(x==7)) #Only if number of clusters is 7


#60-40##########################
library(NbClust)
set.seed(1234)
fit.km <- kmeans(mushtrainingdata60, 7, nstart=25)   
library("clue")
y <-cl_predict(fit.km, mushtestdata40)
library(cluster)
clusplot(mushtrainingdata60, fit.km$cluster, color=TRUE, shade=TRUE,lines=0)#3
#summary(fit.km)
fit.km$size
#============== below lines count the number of points each cluster of test data =========
length(which(y==1))
length(which(y==2))
length(which(y==3)) #Only if number of clusters is 3 or 5 
length(which(y==4)) #Only if number of clusters is 5
length(which(y==5)) #Only if number of clusters is 5
length(which(y==6)) #Only if number of clusters is 7
length(which(y==7)) #Only if number of clusters is 7

#50-50###########################
library(NbClust)
set.seed(1234)
fit.km <- kmeans(mushtrainingdata50, 7, nstart=25)   
library("clue")
z <- cl_predict(fit.km, mushtestdata50)
fit.km$size
library(cluster)
clusplot(mushtrainingdata60, fit.km$cluster, color=TRUE, shade=TRUE,lines=0)#3
#summary(fit.km)
fit.km$size
#============== below lines count the number of points each cluster of test data =========
length(which(z==1))
length(which(z==2))
length(which(z==3)) #Only if number of clusters is 3 or 5 
length(which(z==4)) #Only if number of clusters is 5
length(which(z==5)) #Only if number of clusters is 5
length(which(z==6)) #Only if number of clusters is 7
length(which(z==7)) #Only if number of clusters is 7
#========================================================================
##################### heirarchical clustering ###########################
#========================================================================
library("vegan")
library("plyr")
Mydata_grid <-  count(mushDF)
dist_grid <- vegdist(Mydata_grid, method="jaccard")
clusters_d <- hclust(dist_grid, method="ward.D2")
plot(clusters_d,  labels = FALSE, hang = -1, main = "Original Tree")
cluster <- cutree(clusters_d, k = 5) # k= number of clusters 
cluster
length(which(cluster==1))
length(which(cluster==2))
length(which(cluster==3)) #Only if number of clusters is 3 or 5 
length(which(cluster==4)) #Only if number of clusters is 5
length(which(cluster==5)) #Only if number of clusters is 5
cent <- NULL
for(k in 1:5){
  cent <- rbind(cent, colMeans(Mydata_grid[cluster == k, ,drop = FALSE]))
}
hc1 <- hclust(dist(cent)^2, method = "cen", members = table(cluster))
plot(hc1,  labels = FALSE, hang = -1, main = "Cut Tree")

#========================================================================
# using pvclust
#========================================================================
library(pvclust)
fit.h <- pvclust(mushDF, method.hclust="ward",method.dist="correlation")
plot(fit.h) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit.h, alpha=.95) 

#=======================================================================
#using PAM and daisy - we wanted to try these but they dont work on our version of R
#=======================================================================
library("pam")
#doesnt work on R 3.1.2
#===========================================================================
################### K NEAREST NEIGHBOURS ###################################
KNN_DATA <- rawMushroomDFReduced[2:17]
KNN_DEC_VAR_DATA <- rawMushroomDFReduced[1]
#normalize
KNN_DATA_NORMALIZED <- scale(KNN_DATA)

#divide into test and train for input data
set.seed(12345)
row=nrow(KNN_DATA_NORMALIZED)
knntrainindex <- sample(nrow(KNN_DATA_NORMALIZED),row*0.7 , replace=FALSE)
knntrain <- KNN_DATA_NORMALIZED[knntrainindex,]
knntest <- KNN_DATA_NORMALIZED[-knntrainindex,]
#divide into test and train labels
set.seed(12345)
row=nrow(KNN_DEC_VAR_DATA)
knntrainlabelindex <- sample(nrow(KNN_DEC_VAR_DATA),row*0.7 , replace=FALSE)
knntrainLABEL <- KNN_DEC_VAR_DATA[knntrainlabelindex,]
knntestLABEL <- KNN_DEC_VAR_DATA[-knntrainlabelindex,]
library("class")
mush_test_pred <- knn(train=knntrain,test=knntest,cl=knntrainLABEL,k=90) #k is square root of total records -1
library("gmodels")
CrossTable(x=knntestLABEL,y=mush_test_pred,prop.chisq=FALSE)
#=======================================================================
# Decision Trees
#=======================================================================

install.packages('tree')
library(tree)
# Set a unique seed number so you get the same results everytime you run the below model,
# The number does not matter
set.seed(12)
# Create a decision tree model using the target field as the response and all 93 features as inputs
fit1 <- tree(rawMushroomDFReduced$EdibleOrPoisonous~rawMushroomDFReduced$capShape+ rawMushroomDFReduced$capSurface+rawMushroomDFReduced$capColorringType)
plot(fit1)
title(main="tree")
text(fit1)
# Test the tree model on the holdout test dataset
fit1.pred <- predict(fit1, stest, type="class")
table(fit1.pred,stest$target)

#========================================================================================
############################ MONTE CARLO SIMULATION #####################################
#========================================================================================
# we used mcmc package (Markov chain monte carlo) and explored a few functions from this packages
library("mcmc")
library("MCMCpack")
myMCMCvars = c(1,5,8,20)
mushDataMCMC <- data.frame(rawMushroomDF[myMCMCvars])
posterior <- MCMClogit(EdibleOrPoisonous~bruises+gSpacing + population, data=mushDataMCMC)
plot(posterior)
summary(posterior)

