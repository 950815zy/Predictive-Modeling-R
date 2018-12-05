library(caret)
library(neuralnet)
library(ggplot2)
rm(list=ls())
setwd("/Users/si/Documents/BIS-448predictive analysis of business")
organics.df <- read.csv("organics.csv")
#organics.df = organics.df[complete.cases(organics.df),]

# Visualization
par(mfrow=c(2,2))
boxplot(organics.df$DemAffl ~ organics.df$TargetBuy,col=heat.colors(2),ylab="DemAffl")
legend("bottomleft", inset=.02,c('NotBuy','Buy'), fill=heat.colors(2), horiz=TRUE, cex=0.8)
boxplot(organics.df$DemAge ~ organics.df$TargetBuy,col=heat.colors(2),ylab="DemAge")
boxplot(organics.df$PromSpend ~ organics.df$TargetBuy,col=heat.colors(2),ylab="PromSpend")
boxplot(organics.df$PromTime ~ organics.df$TargetBuy,col=heat.colors(2),ylab="DemAffl")


## Distribution of categorical variables based on TargetBuy
library(gridExtra)
library(grid)
hist1 <- ggplot(data=organics.df, aes(x=TargetBuy))+
  geom_histogram(binwidth=0.2, color="red", aes(fill=DemReg)) + 
  xlab("TargetBuy") +  
  ylab("Frequency") + 
  ggtitle("Distribution of DemReg")+
  geom_vline(data=organics.df, aes(xintercept = mean(TargetBuy)),linetype="dashed",color="grey")

hist2 <- ggplot(data=organics.df, aes(x=TargetBuy))+
  geom_histogram(binwidth=0.2, color="red", aes(fill=DemClusterGroup)) + 
  xlab("TargetBuy") +  
  ylab("Frequency") + 
  ggtitle("Distribution of DemClusterGroup")+
  geom_vline(data=organics.df, aes(xintercept = mean(TargetBuy)),linetype="dashed",color="grey")

hist3 <- ggplot(data=organics.df, aes(x=TargetBuy))+
  geom_histogram(binwidth=0.2, color="red", aes(fill=DemGender)) + 
  xlab("TargetBuy") +  
  ylab("Frequency") + 
  ggtitle("Distribution of DemGender")+
  geom_vline(data=organics.df, aes(xintercept = mean(TargetBuy)),linetype="dashed",color="grey")

hist4 <- ggplot(data=organics.df, aes(x=TargetBuy))+
  geom_histogram(binwidth=0.2, color="red", aes(fill=PromClass)) + 
  xlab("TargetBuy") +  
  ylab("Frequency") + 
  ggtitle("Distribution of PromClass")+
  geom_vline(data=organics.df, aes(xintercept = mean(TargetBuy)),linetype="dashed",color="grey")

grid.arrange(hist1, hist2, hist3, hist4, nrow = 2,
  top = textGrob("Distribution of Categorical features", gp=gpar(fontsize=15)))

##relashionship between variables
scatter1 <- ggplot(data=organics.df, aes(x = PromSpend, y = DemAffl)) +
  geom_point(aes(color=TargetBuy)) +
  xlab("PromSpend") + 
  ylab("DemAffl") +
  ggtitle("PromSpend vs DemAffl")

scatter2 <- ggplot(data=organics.df, aes(x = PromTime, y = DemAffl)) +
  geom_point(aes(col=TargetBuy)) + 
  xlab("PromTime") + 
  ylab("DemAffl") +
  ggtitle("PromTime vs DemAffl")

scatter3 <-ggplot(data=organics.df, aes(x = PromTime, y = DemAge)) +
  geom_point(aes(col=TargetBuy)) + 
  xlab("PromTime") + 
  ylab("DemAge") +
  ggtitle("PromTime vs DemAge")

scatter4 <- ggplot(data=organics.df, aes(x = DemAffl, y = DemAge)) +
  geom_point(aes(col=TargetBuy)) + 
  xlab("DemAffl") + 
  ylab("DemAge") +
  ggtitle("DemAffl vs DemAge")

grid.arrange(scatter1, scatter2, scatter3, scatter4,nrow = 2,
    top = textGrob("scatter plot of numeric features", gp=gpar(fontsize=15)))
par(mfrow=c(2,2))
dataforbar <- aggregate(organics.df$DemCluster,by = list(organics.df$DemClusterGroup),FUN=mean)
names(dataforbar) <- c("DemClusterGroup","DemCluster")
barplot(dataforbar$DemCluster, names.arg = dataforbar$DemClusterGroup,ylab = "mean_DemCluster", xlab = "DemClusterGroup")
omitdata <- na.omit(organics.df)
dataforbar <- aggregate(omitdata$PromSpend,by = list(omitdata$DemClusterGroup),FUN=mean)
names(dataforbar) <- c("DemClusterGroup","PromSpend")
barplot(dataforbar$PromSpend,names.arg = dataforbar$DemClusterGroup,ylab = "mean_PromSpend", xlab = "DemClusterGroup")

dataforbar <- aggregate(omitdata$PromSpend,by = list(omitdata$PromClass),FUN=mean)
names(dataforbar) <- c("PromClass","PromSpend")
barplot(dataforbar$PromSpend, names.arg = dataforbar$PromClass,ylab = "mean_PromSpend", xlab = "PromClass")

dataforbar <- aggregate(omitdata$PromTime,by = list(omitdata$PromClass),FUN=mean)
names(dataforbar) <- c("PromClass","PromTime")
barplot(dataforbar$PromTime, names.arg = dataforbar$PromClass,ylab = "mean_PromTime", xlab = "PromClass")

par(mfrow=c(2,2))
dataforbar <- aggregate(omitdata$PromSpend,by = list(omitdata$TargetBuy),FUN=mean)
names(dataforbar) <- c("TargetBuy","PromSpend")
barplot(dataforbar$PromSpend, names.arg = dataforbar$TargetBuy,ylab = "mean_PromSpend", xlab = "TargetBuy")

dataforbar <- aggregate(omitdata$DemAffl,by = list(omitdata$TargetBuy),FUN=mean)
names(dataforbar) <- c("TargetBuy","DemAffl")
barplot(dataforbar$DemAffl, names.arg = dataforbar$TargetBuy,ylab = "mean_DemAffl", xlab = "TargetBuy")

dataforbar <- aggregate(omitdata$DemAge,by = list(omitdata$TargetBuy),FUN=mean)
names(dataforbar) <- c("TargetBuy","DemAge")
barplot(dataforbar$DemAge, names.arg = dataforbar$TargetBuy,ylab = "mean_DemAge", xlab = "TargetBuy")

dataforbar <- aggregate(omitdata$PromTime,by = list(omitdata$TargetBuy),FUN=mean)
names(dataforbar) <- c("TargetBuy","PromTime")
barplot(dataforbar$PromTime, names.arg = dataforbar$TargetBuy,ylab = "mean_PromTime", xlab = "TargetBuy")

## Delete DemAffl > 30 because the grade of it is in the range (0,30) and outliers
organics.df <- organics.df[which (organics.df$DemAffl <=30 & organics.df$PromTime <= 50 & organics.df$PromSpend <= 60000 ),]
## Delete ID and TargetAmt
head(organics.df)
organics.df <- (organics.df[,-c(1,13)])

# replace all missing data with NA

for (i in seq(1,11,1)){
  organics.df[,i][organics.df[,i]==""]<-NA
  organics.df[,i][organics.df[,i]=="U"]<-NA
}

library(naniar)
gg_miss_var(organics.df)
#print out how many are those NA
for (i in seq(1,11,1)){
  print(colnames(organics.df)[i])
  print (sum(is.na(organics.df[,i])))
}



# categorical variabl to dummy variable
organics.df$PromClass <- as.numeric(factor(organics.df$PromClass , levels=c("Tin", "Silver", "Gold","Platinum")))
organics.df$DemTVReg <- as.numeric(factor(organics.df$DemTVReg , levels=c("Wales & West",	"Midlands",	"N West",	"East",	"N East",	
                                                                                    "S & S East",	"London",	"S West",	"Ulster",
                                                                                    "Yorkshire",	"Border",	"C Scotland",	"N Scot")))

organics.df$DemReg <- as.numeric(factor(organics.df$DemReg , levels=c("Midlands",	"North",	"South East",	"South West",	"Scottish")))
organics.df$DemGender <- as.numeric(factor(organics.df$DemGender , levels=c("M","F")))
organics.df$DemClusterGroup <- as.numeric(factor(organics.df$DemClusterGroup , levels=c("A",	"B",	"C",	"D",	"E",	"F")))


#organics.df = mice(organics.df, seed=111) 

## impute numerical missing values
library(Hmisc)
library(DMwR)
organics.df <- knnImputation(organics.df[, !names(organics.df) %in% "median"], k = 10)
#organics.df$DemAge <- impute(organics.df$DemAge, mean)
#organics.df$DemCluster <- impute(organics.df$DemCluster, mean)



## scale the all variables
nor <- preProcess(organics.df[, 1:11], method=c("range"))
organics.df[, 1:11] <- predict(nor, organics.df[,1:11]) 



dim(organics.df)


# Find high correlations between the variables 
cormatrix = cor(organics.df[c(1:11)],use = "complete.obs")
library(reshape2)
subset(melt(cormatrix), value > .70 & value < 1) 
heatmap(cor(organics.df), Rowv = NA, Colv = NA)
# London can be found in the South East of England
# CScotland and Scottish always show up together
organics.df <- organics.df[,-c(4,6,8)]



# Change names of the variables for further use
name=c()
for (i in names(organics.df)){
  name = c(name,gsub("&","",gsub(" ", "", i)))
}
colnames(organics.df) <- name


## partition the data
set.seed(100000)
train.index <- sample(c(1:dim(organics.df)[1]), dim(organics.df)[1]*0.6)
train.df <- organics.df[train.index, ]
valid.df <- organics.df[-train.index, ]

## Combine all the variables together
f <- as.formula(paste("TargetBuy ~", paste(name[!name %in% "TargetBuy"], collapse = "+")))
f


## Logistic Regression
logit.reg <- glm(TargetBuy ~ ., data = train.df, family = "binomial")
options(scipen=999)
summary(logit.reg)
#use predict() to compute predicted probabilities
logit.reg.pred <- predict(logit.reg, valid.df[ , -c(8)], type = "response")

accuracy(logit.reg.pred, valid.df$TargetBuy)

#remove TargetBut and ClusterGroupE and ClusterGroupD since they were combined
head(data.frame(actual = valid.df$TargetBuy, predicted = round(logit.reg.pred,4)))

cmatrix <- confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.5, 1, 0)), as.factor(valid.df$TargetBuy))
cmatrix
cmatrixAccuracy <- confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.5, 1, 0)), as.factor(valid.df$TargetBuy))$overall[1]
cmatrixAccuracy

#Vary the cutoff in increments of 0.05
accuracy.logit <- data.frame(k =  seq(0.00, 0.95, 0.05), accuracy = rep(0, 20)) #inialize a data frame
for(i in 1:20) {
  accuracy.logit[i, 2] <- confusionMatrix(as.factor(ifelse(logit.reg.pred > (i/20), 1, 0)), as.factor(valid.df$TargetBuy))$overall[1]
} 
accuracy.logit


#plot lift chart
library(gains)
gain <- gains(valid.df$TargetBuy, logit.reg.pred, groups=10)
gain

data.frame("depth" =  gain["depth"], "obs" = gain["obs"], "cume.obs" = gain["cume.obs"], 
           "mean.resp" = gain["mean.resp"], "cume.mean.resp" = gain["cume.mean.resp"], "cume.pct.of.total" = gain["cume.pct.of.total"], 
           "lift" = gain["lift"], "cume.lift" = gain["cume.lift"], "mean.prediction" = gain["mean.prediction"])

sort <- data.frame("target" = valid.df$TargetBuy, "response" = round(logit.reg.pred, 4))[order(logit.reg.pred, decreasing=TRUE), ]
sort[1:200, ]

# plot lift chart
plot(c(0, gain$cume.pct.of.total*sum(valid.df$TargetBuy)) ~ c(0, gain$cume.obs),
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.df$TargetBuy))~c(0, dim(valid.df)[1]), lty=2)

#compute deciles and plot decile-wise chart
heights <- gain$mean.resp/mean(valid.df$TargetBuy)
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9), 
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")




# Neural Network
organics.nn <- neuralnet(f,data = train.df, linear.output = F, hidden = c(4,4), stepmax = 1e7, threshold = 0.1)

organics.nn$weights
prediction(organics.nn)
plot(organics.nn, rep="best")


organics.nn.pred<-compute(organics.nn, valid.df[,-c(8)])
organics.nn.pred$net.result

RMSE.nn <- (sum((valid.df$TargetBuy - organics.nn.pred$net.result)^2)/nrow(valid.df))^0.5
MSE.nn <- sum((valid.df$TargetBuy - organics.nn.pred$net.result)^2)/nrow(valid.df)


cmatrixAccuracy <- confusionMatrix(as.factor(ifelse(organics.nn.pred$net.result > 0.1 , 1, 0)), as.factor(valid.df$TargetBuy))$overall[1]
cmatrixAccuracy

accuracy.nn <- data.frame(k =  seq(0.05, 0.95, 0.05), accuracy = rep(0, 19)) #inialize a data frame
for(i in 1:19) {
  accuracy.nn[i, 2] <- confusionMatrix(as.factor(ifelse(organics.nn.pred$net.result > (i/20), 1, 0)), as.factor(valid.df$TargetBuy))$overall[1]
} 
accuracy.nn

plot(accuracy ~ seq(1, 9, 1), xlab = "Cutoff", ylab = "", type = "l",ylim=c(0,1))
lines( 1-accuracy ~ seq(1, 9, 1), type = "l", lty = 2)
legend("topright", c("accuracy", "overall error"), lty = c(1,2), merge = T)



library(gains)
gain <- gains(valid.df$TargetBuy, organics.nn.pred, groups=10)
gain

data.frame("depth" =  gain["depth"], "obs" = gain["obs"], "cume.obs" = gain["cume.obs"], 
           "mean.resp" = gain["mean.resp"], "cume.mean.resp" = gain["cume.mean.resp"], "cume.pct.of.total" = gain["cume.pct.of.total"], 
           "lift" = gain["lift"], "cume.lift" = gain["cume.lift"], "mean.prediction" = gain["mean.prediction"])

sort <- data.frame("target" = valid.df$TargetBuy, "response" = round(organics.nn.pred, 4))[order(organics.nn.pred, decreasing=TRUE), ]
sort[1:200, ]

# plot lift chart
plot(c(0, gain$cume.pct.of.total*sum(valid.df$TargetBuy)) ~ c(0, gain$cume.obs),
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.df$TargetBuy))~c(0, dim(valid.df)[1]), lty=2)

#compute deciles and plot decile-wise chart
heights <- gain$mean.resp/mean(valid.df$TargetBuy)
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9), 
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")


## KNN

#install.packages("FNN")
library(FNN)

#initializing data frame to record accuracy of each KNN model
acc.df <- data.frame(k = seq(1, 1000, 1), accuracy = rep(0, 1000))

#testing different values for k from 1:1000
for (i in 1:1000) {
  organics.knn <- knn(train = train.df[, c(1:10)], test = valid.df[, c(1:10)], cl = train.df[, 11], k = i)
  acc.df[i, 2] <- confusionMatrix(organics.knn, factor(valid.df[, 11]))$overall[1]
}

#graphing accuracy
plot(acc.df$k, acc.df$accuracy, xlab = "Value for k", ylab = "Predictive Accuracy")

#summary to find maximum value for accuracy
summary(acc.df)

#k = 13 is the best model with an accuracy of .7922







score.df <- read.csv("scoreorganicswithoutcomes.csv",stringsAsFactors = TRUE)

## Delete DemAffl > 30 because the grade of it is in the range (0,30)
score.df <- score.df[which (score.df$DemAffl <=30 & score.df$PromTime <= 50 & score.df$PromSpend <= 60000 ),]
score.df <- (score.df[,-c(1)])


# replace all missing data with NA
for (i in seq(1,11,1)){
  score.df[,i][score.df[,i]==""]<-NA
  score.df[,i][score.df[,i]=="U"]<-NA
}

#print out how many are those NA
for (i in seq(1,11,1)){
  print(colnames(score.df)[i])
  print (sum(is.na(score.df[,i])))
}

#impute the most frequent value of each column to categorical missing value
#score.df$DemClusterGroup[which(is.na(score.df$DemClusterGroup))] = 'C'
#score.df$DemGender[which(is.na(score.df$DemGender))] = 'F'
#score.df$DemReg[which(is.na(score.df$DemReg))] = 'South East'
#score.df$DemTVReg[which(is.na(score.df$DemTVReg))] = 'London'


# categorical variabl to dummy variable
score.df$PromClass <- as.numeric(factor(score.df$PromClass , levels=c("Tin", "Silver", "Gold","Platinum")))

score.df$DemTVReg <- as.numeric(factor(score.df$DemTVReg , levels=c("Wales & West",	"Midlands",	"N West",	"East",	"N East",	
                                                                          "S & S East",	"London",	"S West",	"Ulster",
                                                                          "Yorkshire",	"Border",	"C Scotland",	"N Scot")))
score.df$DemReg <- as.numeric(factor(score.df$DemReg , levels=c("Midlands",	"North",	"South East",	"South West",	"Scottish")))

score.df$DemGender <- as.numeric(factor(score.df$DemGender , levels=c("M","F")))

score.df$DemClusterGroup <- as.numeric(factor(score.df$DemClusterGroup , levels=c("A",	"B",	"C",	"D",	"E",	"F")))



## impute numerical missing values
library(Hmisc)
library(DMwR)
score.df <- knnImputation(score.df[, !names(score.df) %in% "medv"])


#score.df <- score.df[,-c(4:8)]
score.df <- score.df[,-c(4,6,8)]

name=c()
for (i in names(score.df)){
  name = c(name,gsub("&","",gsub(" ", "", i)))
}
colnames(score.df) <- name

nor1 <- preProcess(score.df[, 1:8], method=c("range"))
score.df[, 1:8] <- predict(nor1, score.df[,1:8]) 




score.logit.pred <- predict(logit.reg, score.df[,-c(11)] ,type = "response")
logitcmatrixAccuracy <- confusionMatrix(as.factor(ifelse(score.logit.pred > 0.5, 1, 0)), as.factor(score.df$TargetBuy))$overall[1]
logitcmatrixAccuracy

score.accuracy.logit <- data.frame(k =  seq(0.50, 0.95, 0.05), accuracy = rep(0, 10)) #inialize a data frame
for(i in 1:10) {
  score.accuracy.logit[i, 2] <- confusionMatrix(as.factor(ifelse(score.logit.pred > (i/20)+0.45, 1, 0)), as.factor(score.df$TargetBuy))$overall[1]
} 
score.accuracy.logit


## neural network
score.nn.pred <- compute(organics.nn,score.df[,-c(8)]) 
nncmatrixAccuracy <- confusionMatrix(as.factor(ifelse(score.nn.pred$net.result > 0.49, 1, 0)), as.factor(score.df$TargetBuy))$overall[1]
nncmatrixAccuracy
score.accuracy.nn <- data.frame(k =  seq(0.05, 0.95, 0.05), accuracy = rep(0, 19)) #inialize a data frame
for(i in 1:19) {
  score.accuracy.nn[i, 2] <- confusionMatrix(as.factor(ifelse(score.nn.pred$net.result > (i/20), 1, 0)), as.factor(score.df$TargetBuy))$overall[1]
} 
score.accuracy.nn
