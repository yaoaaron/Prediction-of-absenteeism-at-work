# import all libraries
library(ggplot2)
library(corrplot)
library(factoextra)
library(NbClust)
library(cluster)
library(class)
library(purrr)
library(MASS)
library(gridExtra)
library(tree)
library(Metrics)
library(randomForest)
library(naivebayes)
library(C50) 
library(kernlab)
library(caret)
library(rpart)
library(rpart.plot)
library(gmodels)
library(dplyr)
library("car")

absenteeism <-read.csv("~/Absenteeism_at_work_train.csv", header=TRUE)

########----------------------  Data Processing ------------------------########

## Zero in Reason.for.absence for absence is not a valid reason code. ICD and non-ICD codes do not support it. 
## Removed observations zero in Reason code. 
# range(absenteeism$Reason.for.absence)
# absenteeism <- absenteeism[!(absenteeism$Reason.for.absence == 0),]
## Remove any rows that absenteeism in hours is zero or less than zero but reson for absence is greater than 0
remove <- subset(absenteeism, Absenteeism.time.in.hours <= 0 & Reason.for.absence > 0, c(ID, Reason.for.absence, Absenteeism.time.in.hours))
remove
## Reason code 27 refers to physiotherapy. This observation doesn't make sense since an employee cannot take an absenteeism reason without actually beeing absent. Remove this row.  
absenteeism = absenteeism[!(absenteeism$Absenteeism.time.in.hours==0 & absenteeism$Reason.for.absence > 0) ,]
str(absenteeism)
## Missing value analysis
as.matrix(colSums(is.na(absenteeism)))
## There is one missing value in Hit target and two in Weight. Replace missing values with mean of respective variables. 
for(i in 1:ncol(absenteeism)){
  absenteeism[is.na(absenteeism[,i]), i] <- mean(absenteeism[,i], na.rm = TRUE)
}
as.matrix(colSums(is.na(absenteeism)))

## Box plot of Absenteeism time in hours with Reason for absence. 
ggplot(absenteeism,
       aes_string(y=absenteeism$Absenteeism.time.in.hours,x=as.factor(absenteeism$Reason.for.absence))) +
  geom_boxplot() + 
  xlab('Reason for Absence') +
  ylab('Absenteeism Time (Hours)')

## Absenteeism time. Highly right skewed due to presence of outliers. 
hist(absenteeism$Absenteeism.time.in.hours, breaks = 60, #prob = TRUE,
     xlab = 'Absenteeism Time (Hours)', main = " Absenteeism Time Distribution", col = "light blue")

boxplot(absenteeism$Absenteeism.time.in.hours, main = "Box plot of Absenteeism Time in hours",col="grey")

## Outlier Analysis
par(mfrow=c(3,3))
boxplot(absenteeism$Transportation.expense, main = "Transportation Expense",col="grey")
boxplot(absenteeism$Distance.from.Residence.to.Work, main = "Home-Work Distance",col="grey")
boxplot(absenteeism$Service.time, main = "Service Time",col="grey")
boxplot(as.numeric(absenteeism$Age), main = "Age",col="grey")
boxplot(absenteeism$Hit.target, main = "Hit Target",col="grey")
boxplot(absenteeism$Weight, main = "Weight",col="grey")
boxplot(absenteeism$Height, main = "Height",col="grey")
boxplot(absenteeism$Body.mass.index, main = "Body Mass Index",col="grey")
boxplot(as.numeric(absenteeism$Work.load.Average.day), main = "Daily Work Load",col="grey")

## Replace outliers in each variabel with min/max values
aggregate( absenteeism$Transportation.expense~ absenteeism$ID, absenteeism, function(x) length(unique(x)))
absenteeism$Transportation.expense[absenteeism$ID == 14] <- 155
aggregate( absenteeism$Age~ absenteeism$ID, absenteeism, function(x) length(unique(x)))
absenteeism$Age[absenteeism$ID == 3] <- 38
absenteeism$Age[absenteeism$ID == 23] <- 36

absenteeism$Age<-as.numeric(absenteeism$Age)
absenteeism$Work.load.Average.day<-as.numeric(absenteeism$Work.load.Average.day)

for (i in c('Transportation.expense','Service.time','Age','Work.load.Average.day','Hit.target','Height','Absenteeism.time.in.hours')){
  q = quantile(absenteeism[,i],c(0.25,0.75))
  iqr1 = q[2]-q[1]
  min1 = q[1]-1.5*iqr1
  max1 = q[2]+1.5*iqr1
  absenteeism[,i][absenteeism[,i]<min1] = min1
  absenteeism[,i][absenteeism[,i]>max1] = max1
}

## Data independence, Multicollinearity test. 
## First categorical variables. 
categorical_var = c("Reason.for.absence","Month.of.absence","Day.of.the.week",
                    "Seasons", "Education", "Social.drinker",
                    "Social.smoker")#, "Son", "Pet")


## Transform categorical variables into factors. 
absenteeism[,categorical_var ] <- lapply(absenteeism[,categorical_var], factor)
str(absenteeism)

# Chi-square test for relationship between attributes. 
pvalue = c()

#Calculating & storing p-values in vector pval from chisquare test
for(i in categorical_var){ 
  for(j in categorical_var){
    chi2 = chisq.test(absenteeism[,i],absenteeism[,j])
    simulate.p.value = T
    pvalue = c(pvalue,chi2$p.value)
  }
}

length(pvalue)
m1 <- matrix(pvalue, ncol = 7)
df <- data.frame(m1)
row.names(df) <- categorical_var
colnames(df) <- categorical_var
print(df)

## As per the chisquare test, except Reason.for.absence and Day.of.the.week, all categorical variables are related to Reason.for.absence, as the p-values are less than 0.005. So, we removed all categorical variables correlated to Reason.for.absence but Day.of.the.week.
absenteeism1 <- absenteeism[, -c(3, 5, 12,13,14, 15, 16,17)]
str(absenteeism1)

## Correltaion matrix for continuous attribute
n <- cor(absenteeism1[,4:13])
corrplot(n,order = "hclust", tl.srt = 30, tl.col = "black", addrect = 3, method = "circle")

#absenteeism2 = absenteeism1[,-10]  #ML:which one to keep and to remove should be done in the modeling stage. It depends on the model

## Test for linearity in the data
#pairs(absenteeism2[, -c(1:3)])  #ML: A lot of these variables are Categorical variables. This test does not make sense.

## Data is not linear. So, linear models will not be a good choice for this data. 

# Aggregating Absenteeism.time.in.hours by Reason.for.absence
Reasons = aggregate(absenteeism2$Absenteeism.time.in.hours, by=list(Category=absenteeism2$Reason.for.absence), FUN=sum)
print(as.data.frame(Reasons))
Reasons$Absence = (Reasons$x/sum(absenteeism2$Absenteeism.time.in.hours))*100
Reasons = Reasons[order(Reasons$Absence, decreasing = T),]
print(Reasons)
barplot(Reasons$Absence, names.arg = Reasons$Category, xlab = "Reason for Absence", ylab = "Absence(%)", col = "light blue", 
        main = "Proportion of Each Reason in Absenteeism")


########----------------------  Models ------------------------########
## load Train Data
head(absenteeism)
str(absenteeism)
summary(absenteeism)

## ----------Data Cleaning---------------

## Remove any rows that absenteeism in hours is zero or less than zero but reson for absence is greater than 0
absenteeism = absenteeism[!(absenteeism$Absenteeism.time.in.hours <= 0 & absenteeism$Reason.for.absence > 0),]

## Reason code 27 refers to physiotherapy. This observation doesn't make sense since an employee cannot take an absenteeism reason without actually beeing absent. Remove this row.  
absenteeism = absenteeism[!(absenteeism$Absenteeism.time.in.hours==0 & absenteeism$Reason.for.absence > 0) ,]
nrow(absenteeism)
##
absenteeism=na.omit(absenteeism)
## Replace outliers in each variabel with min/max values
aggregate( absenteeism$Transportation.expense~ absenteeism$ID, absenteeism, function(x) length(unique(x)))
absenteeism$Transportation.expense[absenteeism$ID == 14] <- 155
aggregate( absenteeism$Age~ absenteeism$ID, absenteeism, function(x) length(unique(x)))
absenteeism$Age[absenteeism$ID == 3] <- 38
absenteeism$Age[absenteeism$ID == 23] <- 36

absenteeism$Age<-as.numeric(absenteeism$Age)
absenteeism$Work.load.Average.day<-as.numeric(absenteeism$Work.load.Average.day)

for (i in c('Transportation.expense','Service.time','Age','Work.load.Average.day','Hit.target','Height','Absenteeism.time.in.hours')){
  q = quantile(absenteeism[,i],c(0.25,0.75))
  iqr1 = q[2]-q[1]
  min1 = q[1]-1.5*iqr1
  max1 = q[2]+1.5*iqr1
  absenteeism[,i][absenteeism[,i]<min1] = min1
  absenteeism[,i][absenteeism[,i]>max1] = max1
}

categorical_var = c("Reason.for.absence","Month.of.absence","Day.of.the.week",
                    "Seasons", "Education", "Social.drinker",
                    "Social.smoker","Disciplinary.failure")#, "Son", "Pet")

## Transform categorical variables into factors. 
absenteeism[,categorical_var ] <- lapply(absenteeism[,categorical_var], factor)

absenteeism$age1=as.numeric(as.character(absenteeism$Age))
absenteeism$workload=as.numeric(gsub(",", "", as.character(absenteeism$Work.load.Average.day)))

# Multiple Regression
abs_data=absenteeism[,-c(1,9,10)]

Qfit1 <- lm(Absenteeism.time.in.hours ~ ., data=abs_data[,-9])
summary(Qfit1)
vif(Qfit1)

#For extremely high VIF, Weight and Body.mass.index were removed from the model. 
Qfit2 <- lm(Absenteeism.time.in.hours ~ ., data=abs_data[,-c(9,15,17)])
summary(Qfit2)
vif(Qfit2)

#Not all predictors are significant. A forward selection method is employed to build a working model. 
reg_data=na.omit(abs_data[,-c(9,15,17)])
Qfit3 <- step(lm(Absenteeism.time.in.hours ~ 1,reg_data), scope=list(lower=~1,upper = ~Reason.for.absence+
                                                                       Month.of.absence+Day.of.the.week+Seasons+
                                                                       Transportation.expense+Distance.from.Residence.to.Work+
                                                                       Service.time+Hit.target+Education+
                                                                       Son+Social.drinker+Social.smoker+
                                                                       Pet+Height+workload+age1), direction="forward")

# A Working Model
Qfit4 <- lm(Absenteeism.time.in.hours ~ Reason.for.absence + Height + Son + 
              age1 + workload, data=reg_data)
summary(Qfit4)
vif(Qfit4)

par(mfrow=c(1,2), oma = c(3,2,3,2) + 0.1, mar = c(1,1,1,1) + 0.1)
truehist(residuals(Qfit4), h = 0.25, col="slategray3")
qqPlot(residuals(Qfit4), pch=19, col="darkblue", cex=0.6)
mtext("Distribution of Residuals", outer=T, side=1, line = 2)

par(mfrow=c(1,1))
pred.val <- round(fitted(Qfit4))
plot(pred.val, residuals(Qfit4))
ts.plot(residuals(Qfit4))
residualPlots(Qfit4, pch=19, col="blue", cex=0.6)
influencePlot(Qfit4,  id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

std.del.res<-studres(Qfit4)
truehist(std.del.res, h = 0.25, col="slategray3")
mtext("Histigram of Studentized Deleted Residuals", side=1, line=2, cex=0.8)
d.fit <- dffits(Qfit4)
truehist(std.del.res, h = 0.25, col="slategray3")
truehist(d.fit, h = 0.25, col="slategray3")
mtext("Histigram of Studentized Deleted Residuals", side=1, line=2, cex=0.8)

cook.d <- cooks.distance(Qfit4)
ts.plot(cook.d)

par(mfrow=c(1,2), oma = c(3,2,3,2) + 0.1, mar = c(1,1,1,1) + 0.1)
truehist(std.del.res, h = 0.55, col="slategray3")
mtext("Studentized Deleted Residuals", side=1, line=2, cex=0.8)
truehist(d.fit, h = 0.05, col="slategray3")
mtext("DFITS", side=1, line=2, cex=0.8)
par(mfrow=c(1,1), oma = c(3,2,3,2) + 0.1, mar = c(1,1,1,1) + 0.1)
ts.plot(cook.d, col="dark blue")
mtext("Cook's Distance")

reg_data$std.de.res<-studres(Qfit4)
reg_data$d.fit <- dffits(Qfit4)
reg_data$cook.d <- cooks.distance(Qfit4)

reg_data1<-subset(reg_data,(std.de.res>=(-3)&std.de.res<3&d.fit>=(-1)&d.fit<=1))

#Only 17 points are identified as outliers according to the above criteria. A final model is fit after eliminating these points and a slight improvement in the R2 value is noted.
Qfit5 <- lm(Absenteeism.time.in.hours ~ Reason.for.absence + Height + Son + 
              age1 + workload, data=reg_data1)
summary(Qfit5)
vif(Qfit5)

residualPlots(Qfit5, pch=19, col="blue", cex=0.6)

par(mfrow=c(1,2), oma = c(3,2,3,2) + 0.1, mar = c(1,1,1,1) + 0.1)
truehist(residuals(Qfit5), h = 0.25, col="slategray3")
qqPlot(residuals(Qfit5), pch=19, col="darkblue", cex=0.6)
mtext("Distribution of Residuals", outer=T, side=1, line = 2)

#
Qfit6 <- lm(Absenteeism.time.in.hours ~ Reason.for.absence + Height + Son + 
              poly(age1,2) + workload, data=reg_data1)
summary(Qfit6)
residualPlots(Qfit6, pch=19, col="blue", cex=0.6)

# Applying Tree-Based Methods
d <- density(abs_data$Absenteeism.time.in.hours) # returns the density data 
plot(d) # plots the results
hist(abs_data$Absenteeism.time.in.hours,breaks = 120)

## Transform continous abs hours to 7 groups, run either this part or next part
abs_data$abs=ifelse(abs_data$Absenteeism.time.in.hours<=0,0,
                    ifelse(abs_data$Absenteeism.time.in.hours==1,1,
                           ifelse(abs_data$Absenteeism.time.in.hours==2,2,
                                  ifelse(abs_data$Absenteeism.time.in.hours==3,3,
                                         ifelse((abs_data$Absenteeism.time.in.hours<=7 & abs_data$Absenteeism.time.in.hours>=4),4,
                                                ifelse(abs_data$Absenteeism.time.in.hours==8,5,6))))))

## Transform continuous abs hours to category 0, less than one day, one day or more.These categories are chosen because they gave 
## the hoghest model performance in the classification models below.
abs_data$abs=ifelse(abs_data$Absenteeism.time.in.hours<=0,0,ifelse(abs_data$Absenteeism.time.in.hours<=7,1,2))

abs_data$abs=as.factor(abs_data$abs)
table(abs_data$abs)
prop.table(table(abs_data$abs))

Tree <- tree(abs ~ Reason.for.absence+
               Month.of.absence+Day.of.the.week+Seasons+
               Transportation.expense+Distance.from.Residence.to.Work+
               Service.time+Hit.target+Education+
               Son+Social.drinker+Social.smoker+
               Pet+Height+workload+age1+Weight+Body.mass.index+Disciplinary.failure, data=abs_data, method="class")
Tree
plot(Tree)
text(Tree, pretty=0, cex=0.6)
misclass.tree(Tree, detail=T)
misclass.tree(Tree, detail=F)
#### in-sample error rate
Treefit1 <- predict(Tree, abs_data, type="class")
table(Treefit1, abs_data$abs)
prop.table(table(Treefit1, abs_data$abs))
## Test sample error rate
testfit1 <- predict(Tree, test, type="class")
table(testfit1, test$abs)
table(testfit1)
prop.table(table(testfit1, test$abs))

## Another way to do a simple tree analysis
m.part=rpart(abs_data$abs ~ ., data=abs_data[,-c(18,21)])
m.part
par(mfrow=c(1,1))
rpart.plot(m.part, digits=4, fallen.leaves = TRUE, type = 3, extra = 101)
Treefit2 <- predict(m.part, abs_data, type="class")
prop.table(table(Treefit2, abs_data$abs))
error<-mean(Treefit2 != abs_data$abs)
(mat <- table(Treefit2, abs_data$abs, dnn=c("Prediction","Actual")))
(accuracy <- sum(diag(mat))/sum(mat)*100)  ##--accuracy is 55.5%

## Random forest
## unsupervised random forest
require(randomForest)
abs_data_nomissing=na.omit(abs_data)
rf <- randomForest(abs_data_nomissing$abs ~ . , data=abs_data_nomissing[,-c(18,21)], ntree=150, importance=T, proximity=T)
summary(rf)
rf_pred <- predict(rf, abs_data_nomissing, type="class")
table(rf_pred, abs_data_nomissing$abs)
prop.table(table(rf_pred, abs_data_nomissing$abs))
mean(rf_pred == abs_data_nomissing$abs)
plot(rf, main="")#Error plot
varImpPlot(rf,  main="Variable Importance Plot", cex=0.8)

##KNN
nn.result <- knn(abs_data_nomissing, abs_data_nomissing, cl=abs_data_nomissing$abs, k=5)
nn.result
table(nn.result, abs_data_nomissing$abs,dnn=c("Prediction","Actual"))
prop.table(table(nn.result, abs_data_nomissing$abs))
mean(nn.result == abs_data_nomissing$abs)

##Naive Bayes

naive <- naive_bayes(abs_data_nomissing$abs ~ ., data=abs_data_nomissing[,-c(18,21)])
nb_pred <- predict(naive, abs_data_nomissing, type="class")
(mat <- table(nb_pred, abs_data_nomissing$abs, dnn=c("Prediction","Actual")))
(accuracy <- sum(diag(mat))/sum(mat)*100)
mean(nb_pred == abs_data_nomissing$abs)

plot(naive)
################----------- Model Evaluation-----------#####################

# load Test Data
test <-read.csv("~/Documents/LearningMaterials/ANLY530/Absenteeism_at_work_test.csv", header=TRUE)
categorical_var <- c("Reason.for.absence","Month.of.absence","Day.of.the.week",
                    "Seasons", "Education", "Social.drinker",
                    "Social.smoker","Disciplinary.failure")
test[,categorical_var ] <- lapply(test[,categorical_var], factor)
test$age1<-as.numeric(as.character(test$Age))
test$workload<-as.numeric(gsub(",", "", as.character(test$Work.load.Average.day)))
test<-test[,-c(1,9,10)]
## Transform continous abs hours to 7 groups, run either this part or next part
test$abs=ifelse(test$Absenteeism.time.in.hours<=0,0,
                    ifelse(test$Absenteeism.time.in.hours==1,1,
                           ifelse(test$Absenteeism.time.in.hours==2,2,
                                  ifelse(test$Absenteeism.time.in.hours==3,3,
                                         ifelse((test$Absenteeism.time.in.hours<=7 & test$Absenteeism.time.in.hours>=4),4,
                                                ifelse(test$Absenteeism.time.in.hours==8,5,6))))))
## Transform continous abs hours to 3 groups
test$abs<-ifelse(test$Absenteeism.time.in.hours<=0,0,ifelse(test$Absenteeism.time.in.hours<=7,1,2))
test$abs=as.factor(test$abs)
colnames(test)
colnames(abs_data_nomissing)

#### 1. Test Regression / R-squared is 53.86. The model explains 53.86% variability of the data
# RSS <- c(crossprod(Qfit6$residuals))
# MSE <- RSS / length(Qfit6$residuals)
# RMSE <- sqrt(MSE)
lm_pred = predict(Qfit6, newdatga = test)
# A simple correlation between the actuals and predicted values can be used as a form of accuracy measure. 
# A higher correlation accuracy implies that the actuals and predicted values have similar directional movement, 
# i.e. when the actuals values increase the predicteds also increase and vice-versa.
lm_actuals_preds <- data.frame(cbind(actuals=test$Absenteeism.time.in.hours, predicteds=lm_pred))
correlation_accuracy <- cor(actuals_preds)
head(actuals_preds)
min_max_accuracy <- mean(apply(lm_actuals_preds, 1, min) / apply(lm_actuals_preds, 1, max))  
mape <- mean(abs((lm_actuals_preds$predicteds - lm_actuals_preds$actuals))/lm_actuals_preds$actuals) 

#### 2. Test Tree (rpart) / in-sample accuracy rate is 89.43%

test2<-test[!test$Month.of.absence==0,] # Remove due to Level 0 does not exist in train data
test_tree<- predict(m.part, test2, type="class")
prop.table(table(test_tree, test2$abs))
print(paste0('The accuracy ratio of decision tree is ', mean(test_tree==test2$abs)))

#### 3. Test Random Forest / in-sample accuracy rate is 98.64%

as.matrix(colSums(is.na(test))) # No NA values in the test data

# Identify the columns that have new levels in Test data
ind<-c()
for (i in 1:ncol(test)) {
  if (is.factor(test[,i])&!all(unique(test[,i]) %in% unique(abs_data_nomissing[,i]))) {
    ind<-c(ind, i)
    print(paste0('Column ', i, ' ', colnames(test[i]), ' contains levels that do not exist in the train data.'))
  }
}
# Identify different new levels if any
for(n in ind){
  diff<-setdiff(unique(test[,n]), unique(abs_data_nomissing[,n]))
  print(paste0(colnames(test[n]), ' should remove ', diff))
}
# str(test)
# str(test2)
# str(abs_data_nomissing)
# all<-rbind(abs_data_nomissing,test2)
# test3<-anti_join(all, abs_data_nomissing)

# Level 0 still exists after value 0 was removed-----Re-factor using levels of train data
test2$Month.of.absence <- factor(test2$Month.of.absence, levels = levels(abs_data_nomissing$Month.of.absence))
levels(test2$Month.of.absence)
unique(test2$Month.of.absence)
# Reformatting followng variables to the same types as those of train data
test2$Transportation.expense<-as.numeric(test2$Transportation.expense)
test2$Service.time<-as.numeric(test2$Service.time)
test2$Hit.target<-as.numeric(test2$Hit.target)
test2$Height<-as.numeric(test2$Height)
test2$Absenteeism.time.in.hours<-as.numeric(test2$Absenteeism.time.in.hours)
# Re-Factor due to follwoing variables do not match the type of train data (for RF), and are missing levels from train data (for NB)
levels(test2$Reason.for.absence)
unique(test2$Reason.for.absence)
test2$Reason.for.absence <- factor(test2$Reason.for.absence, levels = levels(abs_data_nomissing$Reason.for.absence))
test2$Seasons <- factor(test2$Seasons, levels = levels(abs_data_nomissing$Seasons))

test_rf <- predict(rf, test2, type="class")
table(test_rf, test2$abs)
prop.table(table(test_rf, test2$abs))
print (paste0('The accuracy ratio of Random Forest is ', mean(test_rf==test2$abs)))

#### 4. Test KNN / in-sample accuracy rate is 87.61%

test_knn <- knn(abs_data_nomissing, test, cl=abs_data_nomissing$abs, k=5)
table(test_knn, test$abs)
prop.table(table(test_knn, test$abs))
print (paste0('The accuracy ratio of KNN is ', mean(test_knn==test$abs)))

#### 5. Test Naive Bayes / in-sample accuracy rate is 85.95%

test_nb <- predict(naive, test2, type="class")
prop.table(table(test_nb, test2$abs))
print (paste0('The accuracy ratio of Naive Bayes is ', mean(test_nb==test2$abs)))

################--------- Cross Validation --------------###################
# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 5)
# lm Regression
ref.cv <- train(Absenteeism.time.in.hours ~ Reason.for.absence + Height + Son + 
                 poly(age1,2) + workload, data=reg_data1, method = "lm",
               trControl = train.control)
print(ref.cv)
# Decision Tree
install.packages("e1071")
tree.cv <- train(abs ~ ., data=abs_data[,-18], method = "rpart",
                   trControl = train.control)
print(tree.cv)
# Random Forest
rf.cv <- train(abs ~ . , data=abs_data_nomissing[,-c(18)], method = "rf",
                    ntree=150, trControl = train.control)
print(rf.cv)
confusionMatrix(rf.cv)
# Naive Bayes
nb.cv <- train(abs ~ . , data=abs_data_nomissing[,-c(18)], method = "nb",
                    trControl = train.control)
print(nb.cv)
confusionMatrix(nb.cv)