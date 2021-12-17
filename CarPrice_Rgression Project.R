#Import file using "Import Dataset OR read.csv"
data1=read.csv("C:/Users/shilpi.x.singh/Downloads/CarPrice_Assignment.csv")
head(data1)
#code for obtaining descriptive statistics
summary(data1)
library(pastecs)
stat.desc(data1$price)
### Install and activate package 'ggplot2' needed for histogram and box plot
install.packages("ggplot2")
library(ggplot2)
### Histogram of the response variable ###
qplot(data1$price,
      geom="histogram",
      binwidth=1000,  
      main="Histogram for Car Price", 
      xlab="Price", 
      fill=I("gray"), 
      col=I("red"))+theme_bw()
#Box Plot of response variable
ggplot(data1, aes(y=price)) + geom_boxplot()+ scale_fill_grey() + theme_classic()
# checking correlation between datapoints ,install Corrplot
install.packages("corrplot")
library(corrplot)
data2=data1[,c(2,4:16,18:28)]
res=cor(data2)
round(res,2)
# Plot Correlation chart
corrplot(res, type = "upper", order = "hclust",tl.col = "black", tl.srt = 45)
#Splitting data into train and test data
install.packages("caret")
library(caret)
require(caTools)
sample=sample.split(data1,SplitRatio=.7)
train1 =subset(data1,sample==TRUE) # creates a training dataset named train1 with rows which are marked as TRUEdata
View(train1)
test1 =subset(data1,sample==FALSE)# creates a test dataset named test1 with rows which are marked as False
View(test1)
# mod1: the assigned model structure
mod1=lm((price)~symboling+as.factor(CarName_1)+as.factor(fueltype)+as.factor(doornumber)+as.factor(carbody)+as.factor(drivewheel)+as.factor(enginelocation)+wheelbase+carlength+carwidth+carheight+curbweight+as.factor(cylindernumber_bin)+enginesize+stroke+compressionratio+peakrpm+citympg+highwaympg,data=train1)
summary(mod1)
# using Step AIC method to select features 
install.packages("MASS")
library(MASS)
step.mod1=stepAIC(mod1,direction = "both")
summary(step.mod1)
# rebuilding model with filtered features
mod2=lm((price)~as.factor(CarName_1)+as.factor(carbody)+as.factor(enginelocation)+wheelbase+carlength+carwidth+carheight+curbweight+enginesize,data=train1)
### Cross validation of the model using out-of-sample data ###
pred_val=predict(mod2,test1)#The predicted values of price assigned to the variable named 'pred_val'.
# finding correlation between actual values in test1 and predicted value of price from the model
cor(test1$price,pred_val)
#calculate the root mean squared error of the residuals(RMSE)
sqrt(mean((test1$price-pred_val)^2))
#to check normality of errors
plot(mod2)
#shapiro wilk test to check weather normality assumption is satisfied with null Hypothesis as data is normally distributed
shapiro.test(mod2$residuals)
# using Durbin watson test to check independance of errors.
install.packages("lmtest")
library(lmtest)
dwtest(mod2)
#Log transformation of response variable to bring normality in model
library(MASS)
bc_mod1=boxcox(mod2,lambda=seq(-3,3))
best.lam=bc_mod1$x[which(bc_mod1$y==max(bc_mod1$y))]
# Recreating model with lambda value
fullmodel=lm((price)^0.19~as.factor(CarName_1)+as.factor(carbody)+as.factor(enginelocation)+wheelbase+carlength+carwidth+carheight+curbweight+enginesize,data=train1)
# Checking Normality of transformed model
shapiro.test(fullmodel$residuals)
plot(fullmodel)
# using Durbin watson test to check independance of errors.
library(lmtest)
dwtest(fullmodel)
#Using VIF to check multicollinearty between predictors
install.packages("car")
library(car)
vif(fullmodel)
#Rework on model by removing Carname
fullmodel_1=lm((price)^0.19~as.factor(carbody)+as.factor(enginelocation)+wheelbase+carlength+carwidth+carheight+curbweight+enginesize,data=train1)
#Rechecking multicollinearty between predictors using VIF()
vif(fullmodel_1)
#Rework on model by removing Curbweight
fullmodel_2=lm((price)^0.19~as.factor(carbody)+as.factor(enginelocation)+wheelbase+carlength+carwidth+carheight+enginesize,data=train1)
#Rechecking multicollinearty between predictors using VIF()
vif(fullmodel_2)
summary(fullmodel_2)
### Cross validation of the model using out-of-sample data ###
pred_val=predict(fullmodel_2,test1)#The predicted values of price assigned to the variable named 'pred_val'.
# finding correlation between actual values in test1 and predicted value of price from the model
cor(test1$price,pred_val)
#calculate the root mean squared error of the residuals(RMSE)
sqrt(mean((test1$price-pred_val)^2))
write.csv(test1,"C:/Users/shilpi.x.singh/Downloads/test1.csv")

