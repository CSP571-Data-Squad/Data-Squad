model_df <- read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Final\\final_Cleaned.csv')
#summary(model_df)


nor <-function(x) { (x -min(x))/(max(x)-min(x))   }
df_norm <- model_df
df_norm[,c(5,6,7,8,9,10)] <-  as.data.frame(lapply(model_df[,c(5,6,7,8,9,10)],nor))
#summary(df_norm)

library('caret')
set.seed(3049)
split <- createDataPartition(df_norm$homicide_Rate,p = 0.7,list = FALSE)
norm_train <- df_norm[split,]
norm_test <- df_norm[-split,]

#install.packages('FNN')
library('FNN')
x <- knn.reg(norm_train[,c(5:10)],test = norm_test[,c(5:10)],y = norm_train$homicide_Rate,k = 10,algorithm = 'brute')
x
# 
# knnk <- function(norm_train,n)
# {
# fit <- knnreg(norm_train[,c(5:10)],norm_train$homicide_Rate, k = n)
# norm_test$predicted_tar <- predict(fit,norm_test[,c(5:10)])
# norm_test[,'residuals'] <- norm_test[,'homicide_Rate'] - norm_test[,'predicted_tar']
# SST <- sum((norm_test[,'homicide_Rate'] - mean(norm_test[,'homicide_Rate']))^2)
# SSR <- sum((norm_test[,'predicted_tar'] - mean(norm_test[,'homicide_Rate']))^2)
# r2 <- SSR/SST
# r2
# }
# 
# 
# r2cal <- list()
# for (i in c(1:20)) {
#   print(i)
# r2cal[i] = knnk(norm_train,i)
# }
# r2cal
# plot(c(1:20),r2cal)
#k-fold cross validation k = 10
tc <- trainControl(method = 'repeatedcv',number = 10,repeats = 3)

# createModelFormula <- function(targetVar, xVars, includeIntercept = TRUE){
#   if(includeIntercept){
#     modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ ')))
#   } else {
#     modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ '), -1))
#   }
#   return(modelForm)
# }
#formula1 <-  createModelFormula(names(norm_train[,c(5:10)]),targetVar = 'homicide_Rate')
fit2 <- train(x = norm_train[,c(5:10)],y = norm_train$homicide_Rate,method = "knn",trControl = tc,data = norm_train,tuneGrid   = expand.grid(k = 1:20))
fit2
norm_test$predicted_tar <- predict(fit2,norm_test[,c(5:10)])
norm_test[,'residuals'] <- norm_test[,'homicide_Rate'] - norm_test[,'predicted_tar']
SST <- sum((norm_test[,'homicide_Rate'] - mean(norm_test[,'homicide_Rate']))^2)
SSR <- sum((norm_test[,'predicted_tar'] - mean(norm_test[,'homicide_Rate']))^2)
r2 <- SSR/SST
r2
#plot(norm_test$homicide_Rate,norm_test$predicted_tar)


first_df <- read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Final\\finalFirstSecondaryData.csv')
#summary(first_df)
colnames(first_df)[1] <- 'Country_Code'
nrow(merge(df_norm,first_df,all.x = FALSE, by = 'Country_Code'))


second_df <- read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Final\\Secondary2_Cleaned.csv')
colnames(second_df)[2] <- 'Country_Code'
second_merge <- merge(df_norm,second_df,all= FALSE,by = 'Country')
#summary(second_merge)

keeps <- c("Year.y",'Country_Code.y','Country','poverty_gap',"malnutrition_death_rates","Infant_mortality_rate","GDP_per_capita","annual_health_care_per_capita"
           ,"gross_enrol_secondary","gross_enrol_primary","lower_secondary","gov_expen",'homicide_Rate')

second_merge <- second_merge[,keeps,drop= FALSE]
names(second_merge)[c(1,2)] <- c('Year','Country_Code')
#summary(second_merge)
second_norm <- second_merge
second_norm[,c(8,9,10,11,12)] <- as.data.frame(lapply(second_norm[,c(8,9,10,11)], nor))
#summary(second_norm)

set.seed(3049)
split_second <- createDataPartition(second_norm$homicide_Rate,p = 0.7,list = FALSE)
snorm_train <- second_norm[split_second,]
snorm_test <- second_norm[-split_second,]
# 
# sknnk <- function(norm_train,n)
# {
#   fit <- knnreg(norm_train[,c(4:12)],norm_train$homicide_Rate, k = n)
#   snorm_test$predicted_tar <- predict(fit,snorm_test[,c(4:12)])
#   snorm_test[,'residuals'] <- snorm_test[,'homicide_Rate'] - snorm_test[,'predicted_tar']
#   SST <- sum((snorm_test[,'homicide_Rate'] - mean(snorm_test[,'homicide_Rate']))^2)
#   SSR <- sum((snorm_test[,'predicted_tar'] - mean(snorm_test[,'homicide_Rate']))^2)
#   r2 <- SSR/SST
#   r2
# }
# sr2cal <- list()
# for (i in c(1:20)) {
#   #print(i)
#   sr2cal[i] = sknnk(snorm_train,i)
# }
# sr2cal
# 
# knnregTrain(norm_train[,c(5:10)],norm_test[,c(5:10)],norm_train$homicide_Rate,k=3,use.all = TRUE)
formula3 <- createModelFormula(xVars = names(snorm_train[,c(4:12)]),targetVar = snorm_train$homicide_Rate)
fit3 <- train(x = snorm_train[,c(4:12)],y = snorm_train$homicide_Rate,method = "knn",trControl = tc,data = snorm_train,tuneGrid = expand.grid(k = 1:20))
fit3

snorm_test$predicted_tar <- predict(fit3,snorm_test[,c(4:12)])
snorm_test[,'residuals'] <- snorm_test[,'homicide_Rate'] - snorm_test[,'predicted_tar']
SST <- sum((snorm_test[,'homicide_Rate'] - mean(snorm_test[,'homicide_Rate']))^2)
SSR <- sum((snorm_test[,'predicted_tar'] - mean(snorm_test[,'homicide_Rate']))^2)
sr2 <- SSR/SST
sr2
save(fit3)