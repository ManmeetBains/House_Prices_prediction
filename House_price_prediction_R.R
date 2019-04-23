## 1....Load Package



library('ggplot2')
library('ggthemes') 
library('scales')
library('dplyr') 
library('mice')
library('randomForest') 
library('data.table')
library('gridExtra')
library('corrplot') 
library('GGally')
library('e1071')


## 2.....Read Train and Test data



train <-read.csv('Data/train.csv', stringsAsFactors = F)
test  <-read.csv('Data/test.csv', stringsAsFactors = F)

## Structure of the data

dim(train)
str(train)
dim(test)
str(test)

#The housing train data set has 1460 rows and 81 features with the target feature Sale Price. The housing test data set has 1459 rows and 80 features with 
the target feature Sale Price.


#Count the number of columns that consists of text data

sum(sapply(train[,1:81], typeof) == "character")

#Count the number of columns that consists of numerical data

sum(sapply(train[,1:81], typeof) == "integer")



#We have 43 columns that consist of text and 38 columns are numerical. The text data could be challenging to work with. For those that are numerical,let's 
#take a look at some descriptive statistics
# Obtain summary statistics

summary(train[,sapply(train[,1:81], typeof) == "integer"])


cat('Train has', dim(train)[1], 'rows and', dim(train)[2], 'columns.')
cat('Test has', dim(test)[1], 'rows and', dim(test)[2], ' columns.')

# The percentage of data missing in train

sum(is.na(train)) / (nrow(train) *ncol(train))

# The percentage of data missing in test

sum(is.na(test)) / (nrow(test) * ncol(test))

# Check for duplicated rows

cat("The number of duplicated rows are", nrow(train) - nrow(unique(train)))




## 3.....Combine data

#Since test dataset has no "Saleprice" variable. We will create it and then combine.


test$SalePrice<-rep(NA,1459)
house<-bind_rows(train,test)

## Data Exploration

str(house)
summary(house)
head(house)

## 4.....Data Visualization


cat_var <- names(train)[which(sapply(train, is.character))]
cat_car <- c(cat_var, 'BedroomAbvGr', 'HalfBath', ' KitchenAbvGr','BsmtFullBath', 'BsmtHalfBath', 'MSSubClass')
numeric_var <- names(train)[which(sapply(train, is.numeric))]


## Creating one training dataset with categorical variable and one with numeric variable. We will use this for data visualization.

train1_cat<-train[cat_var]
train1_num<-train[numeric_var]

## Bar plot/Density plot function

## Bar plot function

plotHist <- function(data_in, i) 
{
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}

## Density plot function

plotDen <- function(data_in, i){
  data <- data.frame(x=data_in[[i]], SalePrice = data_in$SalePrice)
  p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(p)
  
}

## Function to call both Bar plot and Density plot function

doPlots <- function(data_in, fun, ii, ncol=3) 
{
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}


## Barplots for the categorical features

doPlots(train1_cat, fun = plotHist, ii = 1:4, ncol = 2)
doPlots(train1_cat, fun = plotHist, ii  = 5:8, ncol = 2)
doPlots(train1_cat, fun = plotHist, ii = 9:12, ncol = 2)
doPlots(train1_cat, fun = plotHist, ii = 13:18, ncol = 2)
doPlots(train1_cat, fun = plotHist, ii = 19:22, ncol = 2)


#Boxplot

ggplot(train, aes(x = Neighborhood, y = SalePrice)) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()


#conclusion: boxplot between the neighboorhoods and sale price shows that BrookSide and South & West of Iowa State University have cheap houses. While Northridge
#and Northridge Heights are rich neighborhoods with several outliers in terms of price.


#Density plots for numeric variables

doPlots(train1_num, fun = plotDen, ii = 2:6, ncol = 2)
doPlots(train1_num, fun = plotDen, ii = 7:12, ncol = 2)
doPlots(train1_num, fun = plotDen, ii = 13:17, ncol = 2)


#Conclusion: Density plots of the features indicates that the features are skewed. The denisty plot for YearBuilt shows that the data set contains a mix of new 
#and old houses. It shows a downturn in the number of houses in recent years, possibily due to the housing crisis.


#Histogram for few numeric variable


doPlots(train1_num, fun = plotHist, ii = 18:23, ncol = 2)


#Conclusion: The histograms below show that majority of the houses have 2 full baths, 0 half baths, and have an average of 3 bedrooms.

#Explore the correlation


correlations <- cor(na.omit(train1_num[,-1]))


correlations

row_indic <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)
correlations<- correlations[row_indic ,row_indic ]
corrplot(correlations, method="square")



## 5.....Missing Value


# Looking at the distribution and summary of the target variable

summary(train$SalePrice)

quantile(train$SalePrice)

# Conclusion: From summary, it was observed that minimum price is greater than 0

## Histogram for target variable

hist(train$SalePrice)

## Conclusion: From Histogram, we could see that it deviates from normal distribution and has positive skewness.


# Plotting 'GrLivArea' too see if there are any outliers

ggplot(train,aes(y=SalePrice,x=GrLivArea))+geom_point()
summary(train$GrLivArea)

# There are outliers in 'GrLivArea' field. Let's remove those outliers.

train <- train[train$GrLivArea<=4000,]


## To find number of missing value for all variable in train dataset

colSums(sapply(train, is.na))

## To find number of missing value for all variable in combined dataset (Train+Test)

sapply(house[,1:80], function(x) sum(is.na(x)))

## Taking all the missing data indices in one variables

Missing_indices <- sapply(train,function(x) sum(is.na(x)))
Missing_Summary <- data.frame(index = names(train),Missing_Values=Missing_indices)
Missing_Summary[Missing_Summary$Missing_Values > 0,]


#Combining train and test data for quicker data prep


test$SalePrice <- NA
train$isTrain <- 1
test$isTrain <- 0
house <- rbind(train,test)


## MasVnrArea

house$MasVnrArea[which(is.na(house$MasVnrArea))] <- mean(house$MasVnrArea,na.rm=T)


## Alley

#Changing NA in Alley to None


house$Alley1 <- as.character(house$Alley)
house$Alley1[which(is.na(house$Alley))] <- "None"
table(house$Alley1)
house$Alley <- as.factor(house$Alley1)
house <- subset(house,select = -Alley1)


## MasVnrType


house$MasVnrType1 <- as.character(house$MasVnrType)
house$MasVnrType1[which(is.na(house$MasVnrType))] <- "None"
house$MasVnrType <- as.factor(house$MasVnrType1)
house <- subset(house,select = -MasVnrType1)
table(house$MasVnrType)


## LotFrontage

house$LotFrontage[which(is.na(house$LotFrontage))] <- median(house$LotFrontage,na.rm = T)

## FireplaceQu


house$FireplaceQu1 <- as.character(house$FireplaceQu)
house$FireplaceQu1[which(is.na(house$FireplaceQu))] <- "None"
house$FireplaceQu <- as.factor(house$FireplaceQu1)
house <- subset(house,select = -FireplaceQu1)


## PoolQC


house$PoolQC1 <- as.character(house$PoolQC)
house$PoolQC1[which(is.na(house$PoolQC))] <- "None"
house$PoolQC <- as.factor(house$PoolQC1)
house <- subset(house,select = -PoolQC1)


## Fence


house$Fence1 <- as.character(house$Fence)
house$Fence1[which(is.na(house$Fence))] <- "None"
house$Fence <- as.factor(house$Fence1)
house <- subset(house,select = -Fence1)


## MiscFeature

house$MiscFeature1 <- as.character(house$MiscFeature)
house$MiscFeature1[which(is.na(house$MiscFeature))] <- "None"
house$MiscFeature <- as.factor(house$MiscFeature1)
house <- subset(house,select = -MiscFeature1)


## GarageType


house$GarageType1 <- as.character(house$GarageType)
house$GarageType1[which(is.na(house$GarageType))] <- "None"
house$GarageType <- as.factor(house$GarageType1)
house <- subset(house,select = -GarageType1)


## GarageYrBlt


house$GarageYrBlt[which(is.na(house$GarageYrBlt))] <- 0 


## GarageFinish




house$GarageFinish1 <- as.character(house$GarageFinish)
house$GarageFinish1[which(is.na(house$GarageFinish))] <- "None"
house$GarageFinish <- as.factor(house$GarageFinish1)
house <- subset(house,select = -GarageFinish1)


## GarageQual


house$GarageQual1 <- as.character(house$GarageQual)
house$GarageQual1[which(is.na(house$GarageQual))] <- "None"
house$GarageQual <- as.factor(house$GarageQual1)
house <- subset(house,select = -GarageQual1)


## GarageCond


house$GarageCond1 <- as.character(house$GarageCond)
house$GarageCond1[which(is.na(house$GarageCond))] <- "None"
house$GarageCond <- as.factor(house$GarageCond1)
house <- subset(house,select = -GarageCond1)


## BsmtQual


house$BsmtQual1 <- as.character(house$BsmtQual)
house$BsmtQual1[which(is.na(house$BsmtQual))] <- "None"
house$BsmtQual <- as.factor(house$BsmtQual1)
house <- subset(house,select = -BsmtQual1)


## BsmtCond


house$BsmtCond1 <- as.character(house$BsmtCond)
house$BsmtCond1[which(is.na(house$BsmtCond))] <- "None"
house$BsmtCond <- as.factor(house$BsmtCond1)
house <- subset(house,select = -BsmtCond1)


## BsmtExposure


house$BsmtExposure1 <- as.character(house$BsmtExposure)
house$BsmtExposure1[which(is.na(house$BsmtExposure))] <- "None"
house$BsmtExposure <- as.factor(house$BsmtExposure1)
house <- subset(house,select = -BsmtExposure1)


## BsmtFinType1


house$BsmtFinType11 <- as.character(house$BsmtFinType1)
house$BsmtFinType11[which(is.na(house$BsmtFinType1))] <- "None"
house$BsmtFinType1 <- as.factor(house$BsmtFinType11)
house <- subset(house,select = -BsmtFinType11)


## BsmtFinType2


house$BsmtFinType21 <- as.character(house$BsmtFinType2)
house$BsmtFinType21[which(is.na(house$BsmtFinType2))] <- "None"
house$BsmtFinType2 <- as.factor(house$BsmtFinType21)
house <- subset(house,select = -BsmtFinType21)


## Electrical


house$Electrical1 <- as.character(house$Electrical)
house$Electrical1[which(is.na(house$Electrical))] <- "None"
house$Electrical <- as.factor(house$Electrical1)
house <- subset(house,select = -Electrical1)


## Factorizing


house$MSZoning<- factor(house$MSZoning)
house$Street <- factor(house$Street)
house$LotShape <-factor(house$LotShape )
house$LandContour<-factor(house$LandContour)
house$Utilities<-factor(house$Utilities)
house$LotConfig<-factor(house$LotConfig)
house$LandSlope<-factor(house$LandSlope)
house$Neighborhood<-factor(house$Neighborhood)
house$Condition1<-factor(house$Condition1)
house$Condition2<-factor(house$Condition2)
house$BldgType<-factor(house$BldgType)
house$HouseStyle<-factor(house$HouseStyle)
house$RoofStyle<-factor(house$RoofStyle)
house$RoofMatl<-factor(house$RoofMatl)
house$Exterior1st<-factor(house$Exterior1st)
house$Exterior2nd<-factor(house$Exterior2nd)
house$ExterQual<-factor(house$ExterQual)
house$ExterCond<-factor(house$ExterCond)
house$Foundation<-factor(house$Foundation)
house$Heating<-factor(house$Heating)
house$HeatingQC<-factor(house$HeatingQC)
house$CentralAir<-factor(house$CentralAir)
house$KitchenQual<-factor(house$KitchenQual)
house$Functional<-factor(house$Functional)
house$PavedDrive<-factor(house$PavedDrive)
house$SaleType<-factor(house$SaleType)
house$SaleCondition<-factor(house$SaleCondition)
str(house)


#Taking all the column classes in one variable so as to seperate factors from numerical variables.


Column_classes <- sapply(names(house),function(x){class(house[[x]])})
numeric_columns <-names(Column_classes[Column_classes != "factor"])

#determining skew of each numeric variable

skew <- sapply(numeric_columns,function(x){skewness(house[[x]],na.rm = T)})

# Let us determine a threshold skewness and transform all variables above the treshold.

skew <- skew[skew > 0.75]

# transform excessively skewed features with log(x + 1)

for(x in names(skew)) 
{
  house[[x]] <- log(house[[x]] + 1)
}



#Train and test dataset creation


train <- house[1:1460,]
test <- house[1461:nrow(house),]


## setting the seed to make the partition reproducible



## 7.....Build the model


library(randomForest)
house_model <- randomForest(SalePrice~.,
                            data = train)


## 8...Variable importance


importance    <- importance(house_model)
varImpPlot(house_model)


## 9...Final Prediction


# Predict using the test set

prediction <- predict(house_model,test)

# Evaluation RMSE function

RMSE <- function(x,y){
  a <- sqrt(sum((log(x)-log(y))^2)/length(y))
  return(a)
}


RMSE


RMSE1 <- RMSE(prediction, validate$SalePrice)
RMSE1
RMSE1 <- round(RMSE1, digits = 5)


#Roc Curve
library("pROC")
test_prob <- predict( house_model, reference = test, type = "response")
test_roc <- roc(test$SalePrice ~  plot = TRUE, print.auc = TRUE)
as.numeric(test_roc$auc)

#Output file


prediction[which(is.na(prediction))] <- mean(prediction,na.rm=T)
final <- data.frame(Id=test$Id,SalePrice=prediction)
write.csv(submit,file="House_Price_Prediction.csv",row.names=F)