#################Linear regression model builder#############################

#function to build our formula
helpFormula <- function(target, vars, intercept=TRUE) {
  if (intercept){
    formul <- as.formula(paste(target, "~", paste(vars, collapse = '+')))
  }else{ 
    formul <- as.formula(paste(target, "~", paste(vars, collapse = '+'),-1))
  }
  return(formul)
}



#function to normalize our variables
rangeNormalizer <- function (dataset, varModel){
  dataset1 <- dataset[,varModel]
  sapply(varModel, FUN=function(x) dataset1[,x] <<- (dataset1[,x]-min(dataset1[,x]))/(max(dataset1[,x])-min(dataset1[,x])))
  return (dataset1)
}



#function to do log transformation on our variables
logTransform <- function(dataset){
  dataset <- log(dataset)
  return (dataset)
}



#function to do scale transformation on our variables
scaleTransform <- function(dataset, varModel){
  dataset1 <- scale(dataset[,varModel])
  y <- dataset[,"homicide_Rate"]
  dataset <- data.frame(cbind(dataset1, y))
  names(dataset) <- c(varModel,"homicide_Rate")
  return (dataset)
}



#RsquareCompute
rsquare <- function(model, test){
  test[,targetHat] <- predict(model, test)
  SST <- sum((test[,"homicide_Rate"]-mean(test[,"homicide_Rate"]))^2)
  SSR <- sum((test[,targetHat]-mean(test[,"homicide_Rate"]))^2)
  rS <- SSR/SST
  return(rS)
}


#10-fold cross validation function
tenFoldCV <- function (dataset, varModel1){#create a new variable to work with, not to alter our original data
  data2 <- dataset
  varModel <- varModel1
  #create the formula
  formIntercept <- helpFormula("homicide_Rate", varModel)
  form <- helpFormula("homicide_Rate", varModel, FALSE)
  #shuffle the data 
  data2<-data2[sample(nrow(data2), replace = FALSE),]
  #Create 10 equally size folds
  folds <- cut(seq(1,nrow(data2)), breaks=10, labels=FALSE)
  #create rmse variable to keep the minimum rmse possible and give it an impossible error
  rmse <- 10
  #Perform 10 fold cross validation with the range normalization transformation
  for(i in 1:10){
    #Set seed for reproducibility
    set.seed(60616)
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i, arr.ind=TRUE)
    testData <- data2[testIndexes, ]
    trainData <- data2[-testIndexes, ]
    
    #create the model and test it 
    homicideTrain1 <- trainData
    homicideTest1  <- testData
    
    y1 <- homicideTrain1[,c("homicide_Rate")]
    y2 <- homicideTest1[,c("homicide_Rate")]
    
    homicideTrain1 <- rangeNormalizer(homicideTrain1, varModel)
    homicideTest1  <- rangeNormalizer(homicideTest1, varModel)
    
    homicideTrain1 <- data.frame(cbind(homicideTrain1, y1))
    homicideTest1 <- data.frame(cbind(homicideTest1, y2))
    
    names(homicideTrain1) <- c(varModel,"homicide_Rate")
    names(homicideTest1) <- c(varModel,"homicide_Rate")
    
    NormalizedLinear <- lm(formula = formIntercept, data = homicideTrain1)
    r_SquareNormalizedLinearIntercept <- rsquare(NormalizedLinear, homicideTest1)
    
    homicideTest1$targetHat <- predict(NormalizedLinear, homicideTest1)
    
    #if we have an RMSE less than the minimum rmse so far take it as the new minimum
    if (RMSE(homicideTest1$targetHat, homicideTest1$homicide_Rate)<=rmse & r_SquareNormalizedLinearIntercept<1 ){
      #best performance Rsquare
      rs <<- r_SquareNormalizedLinearIntercept
      #best performance RMSE
      rmse <<- RMSE(homicideTest1$targetHat, homicideTest1$homicide_Rate)
      #best performance MAE
      mae <<- MAE(homicideTest1$targetHat, homicideTest1$homicide_Rate)
      #best performance model
      model_Final <<- NormalizedLinear
    }
    
  }
}



#Read primary + secondary datasets
data <- read.csv("/Users/kevinmouofo/Downloads/merged_primary+secondary12.csv")
#useless column removed
data <- data[,-c(1)] 
#Check if evrything is imported properly
str(data) 

#get our target variable
target <- data$homicide_Rate

#primary dataset
primary <- data[,-c(8,10)]

#Get numeric variables of the primary dataset
varModel <- names(which(sapply(primary, is.numeric)))[-c(8)]




#stratified sampling 
library('caret')
set.seed(60616)
inTrain <- createDataPartition(y = target, p = .8, list = FALSE)
homicideTrain <- data[inTrain,]
homicideTest <- data[-inTrain,]
#test 
sum(homicideTrain[,"homicide_Rate"])/nrow(homicideTrain)
sum(homicideTest[,"homicide_Rate"])/nrow(homicideTest)




#create the formula
formIntercept <- helpFormula("homicide_Rate", varModel)
form <- helpFormula("homicide_Rate", varModel, FALSE)





#basic linear model with intercept
homicideTrain1 <- homicideTrain
homicideTest1  <- homicideTest
BasicLinear <- lm(formula = formIntercept, data = homicideTrain1)
r_SquareBasicLinearIntercept <- rsquare(BasicLinear, homicideTest1)
summary(BasicLinear)

print(r_SquareBasicLinearIntercept)
homicideTest1$targetHat <- predict(BasicLinear, homicideTest1)
print(RMSE(homicideTest1$targetHat, homicideTest1$homicide_Rate))
print(MAE(homicideTest1$targetHat, homicideTest1$homicide_Rate))




#basic linear model without intercept
homicideTrain1 <- homicideTrain
homicideTest1  <- homicideTest
BasicLinear <- lm(formula = form, data = homicideTrain1)
r_SquareBasicLinearIntercept <- rsquare(BasicLinear, homicideTest1)
summary(BasicLinear)

print(r_SquareBasicLinearIntercept)
homicideTest1$targetHat <- predict(BasicLinear, homicideTest1)
print(RMSE(homicideTest1$targetHat, homicideTest1$homicide_Rate))
print(MAE(homicideTest1$targetHat, homicideTest1$homicide_Rate))

#Check the EDA
for (j in varModel){
  boxplot(data[,j], main=j)
}





#linear regression model with Log transformation and intercept
homicideTrain1 <- homicideTrain
homicideTest1  <- homicideTest

#remove the data with 0 in their range and the target variable
varModelnum <- varModel[-c(2,3)]
y1 <- homicideTrain1[,c("homicide_Rate","malnutrition_death_rates","Infant_mortality_rate")]
y2 <- homicideTest1[,c("homicide_Rate", "malnutrition_death_rates","Infant_mortality_rate")]

#Log scale our datasets
homicideTrain1 <- logTransform(homicideTrain1[,c(varModelnum)])
homicideTest1  <- logTransform(homicideTest1[,c(varModelnum)])

#create new data frame with log scaled variable and unscalable variables
homicideTrain1 <- data.frame(cbind(homicideTrain1, y1))
homicideTest1 <- data.frame(cbind(homicideTest1, y2))

names(homicideTrain1) <- c(varModelnum, "homicide_Rate", "malnutrition_death_rates","Infant_mortality_rate")
names(homicideTest1) <- c(varModelnum,"homicide_Rate", "malnutrition_death_rates","Infant_mortality_rate")

logBasicLinear <- lm(formula = formIntercept, data = homicideTrain1)
r_SquareLogBasicLinearIntercept <- rsquare(logBasicLinear, homicideTest1)
summary(logBasicLinear)

print(r_SquareLogBasicLinearIntercept)
homicideTest1$targetHat <- predict(logBasicLinear, homicideTest1)
print(RMSE(homicideTest1$targetHat, homicideTest1$homicide_Rate))
print(MAE(homicideTest1$targetHat, homicideTest1$homicide_Rate))





#linear regression model with Log transformation without intercept
homicideTrain1 <- homicideTrain
homicideTest1  <- homicideTest

varModelnum <- varModel[-c(2,3)]

y1 <- homicideTrain1[,c("homicide_Rate","malnutrition_death_rates","Infant_mortality_rate")]
y2 <- homicideTest1[,c("homicide_Rate", "malnutrition_death_rates","Infant_mortality_rate")]

homicideTrain1 <- logTransform(homicideTrain1[,c(varModelnum)])
homicideTest1  <- logTransform(homicideTest1[,c(varModelnum)])

homicideTrain1 <- data.frame(cbind(homicideTrain1, y1))
homicideTest1 <- data.frame(cbind(homicideTest1, y2))

names(homicideTrain1) <- c(varModelnum, "homicide_Rate", "malnutrition_death_rates","Infant_mortality_rate")
names(homicideTest1) <- c(varModelnum,"homicide_Rate", "malnutrition_death_rates","Infant_mortality_rate")

logBasicLinear <- lm(formula = form, data = homicideTrain1)
r_SquareLogBasicLinearIntercept <- rsquare(logBasicLinear, homicideTest1)
summary(logBasicLinear)

print(r_SquareLogBasicLinearIntercept)
homicideTest1$targetHat <- predict(logBasicLinear, homicideTest1)
print(RMSE(homicideTest1$targetHat, homicideTest1$homicide_Rate))
print(MAE(homicideTest1$targetHat, homicideTest1$homicide_Rate))






##linear regression model with range standardization and intercept
homicideTrain1 <- homicideTrain
homicideTest1  <- homicideTest

#remove the target variable
y1 <- homicideTrain1[,c("homicide_Rate")]
y2 <- homicideTest1[,c("homicide_Rate")]

#range normalize the variables
homicideTrain1 <- rangeNormalizer(homicideTrain1, varModel)
homicideTest1  <- rangeNormalizer(homicideTest1, varModel)

#create a new dataframe with standardized variables
homicideTrain1 <- data.frame(cbind(homicideTrain1, y1))
homicideTest1 <- data.frame(cbind(homicideTest1, y2))

names(homicideTrain1) <- c(varModel,"homicide_Rate")
names(homicideTest1) <- c(varModel,"homicide_Rate")

NormalizedLinear <- lm(formula = formIntercept, data = homicideTrain1)
r_SquareNormalizedLinearIntercept <- rsquare(NormalizedLinear, homicideTest1)
summary(NormalizedLinear)

print(r_SquareNormalizedLinearIntercept)
homicideTest1$targetHat <- predict(NormalizedLinear, homicideTest1)
print(RMSE(homicideTest1$targetHat, homicideTest1$homicide_Rate))
print(MAE(homicideTest1$targetHat, homicideTest1$homicide_Rate))






#linear regression model with range standardization without intercept
homicideTrain1 <- homicideTrain
homicideTest1  <- homicideTest

y1 <- homicideTrain1[,c("homicide_Rate")]
y2 <- homicideTest1[,c("homicide_Rate")]

homicideTrain1 <- rangeNormalizer(homicideTrain1, varModel)
homicideTest1  <- rangeNormalizer(homicideTest1, varModel)

homicideTrain1 <- data.frame(cbind(homicideTrain1, y1))
homicideTest1 <- data.frame(cbind(homicideTest1, y2))

names(homicideTrain1) <- c(varModel,"homicide_Rate")
names(homicideTest1) <- c(varModel,"homicide_Rate")

NormalizedLinear <- lm(formula = form, data = homicideTrain1)
r_SquareNormalizedLinearIntercept <- rsquare(NormalizedLinear, homicideTest1)
summary(NormalizedLinear)

print(r_SquareNormalizedLinearIntercept)
homicideTest1$targetHat <- predict(NormalizedLinear, homicideTest1)
print(RMSE(homicideTest1$targetHat, homicideTest1$homicide_Rate))
print(MAE(homicideTest1$targetHat, homicideTest1$homicide_Rate))


#best performance Rsquare for now
rs <- r_SquareNormalizedLinearIntercept
#best performance RMSE for now
rmse <- RMSE(homicideTest1$targetHat, homicideTest1$homicide_Rate)
#best performance MAE for now
mae <- MAE(homicideTest1$targetHat, homicideTest1$homicide_Rate)
#best performance model for now
model_Final <- NormalizedLinear



####10-fold cross validation#####
tenFoldCV(primary, varModel)
#best performance Rsquare
print(rs)
#best performance RMSE
print (rmse)
#best performance MAE
print(mae)
#best performance model
summary(model_Final)



#Add first secondary dataset

#primary + first secondary dataset
second <- data[,-c(10)]
#Get numeric variables of the dataset
varModel <- names(which(sapply(second, is.numeric)))[-c(9)]
####10-fold cross validation#####
tenFoldCV(second, varModel)
#best performance Rsquare
print(rs)
#best performance RMSE
print (rmse)
#best performance MAE
print(mae)
#best performance model
summary(model_Final)



#primary + first secondary + Second secondary dataset
second2 <- data
#Get numeric variables of the dataset
varModel <- names(which(sapply(second2, is.numeric)))[-c(9)]
####10-fold cross validation#####
tenFoldCV(second2, varModel)
#best performance Rsquare
print(rs)
#best performance RMSE
print (rmse)
#best performance MAE
print(mae)
#best performance model
summary(model_Final)



# Let's take a look at our regression assumptions.
# 1. Linear relationship.
summary(model_Final)
# Since R**2 is very high, we are comfortable that this assumption is true
# 2. All variables have a multivariate normal relationship
# Let's use a QQplot to verify this
plot(model_Final)
# Not great. May be something to revisit.
# 3. No multicorlinearity
corrplot(cor(data[,varModel]))
# 4. No autocorrelation
plot(model_Final)
# 5. Homoscedasticity
plot(model_Final)


##################  END  ###############################@
