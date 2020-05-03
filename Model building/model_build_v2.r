library('caret')

final_t_df <- read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Final\\merged_primary+secondary12.csv')
names(final_t_df)
final_t_df <- final_t_df[,c(2:11)]

normalization <-function(x) { (x -min(x))/(max(x)-min(x))   }
p_norm <-  final_t_df
p_norm[,c(1:8,10)] <- as.data.frame(lapply(final_t_df[,c(1:8,10)],normalization))
summary(p_norm)

primary_norm <- p_norm[,c(1:7,9)]
ps1_norm <- p_norm[,c(1:9)]

set.seed(60616)
split_final <- createDataPartition(p_norm$homicide_Rate,p = 0.8,list = FALSE)
final_train <-  p_norm[split_final,]
final_test <-  p_norm[-split_final,]
final_tc <- trainControl(method = 'repeatedcv',number = 10,repeats = 3)
model_final2 <- train(homicide_Rate~.,data = final_train,trainControl = final_tc, method = "knn",tuneGrid = expand.grid(k = c(1:30)),metric = 'RMSE')
final_test$predicted_tar <- predict(model_final2,final_test[,c(1:8,10)])
print(RMSE(final_test$predicted_tar,final_test$homicide_Rate))
print(MAE(final_test$predicted_tar,final_test$homicide_Rate))
model_final2


# 
# set.seed(60616)
# final_tc <- trainControl(method = 'repeatedcv',number = 10,repeats = 3)
# model_final2 <- train(homicide_Rate~.,data = primary_norm,trainControl = final_tc, method = "knn",tuneGrid = expand.grid(k = c(1:50)))
# p_norm$predicted_tar <- predict(model_final2,primary_norm[,c(1:7)])
# print(RMSE(p_norm$predicted_tar,p_norm$homicide_Rate))
# print(MAE(final_test$predicted_tar,final_test$homicide_Rate))
# 
plot(homicide_Rate ~ predicted_tar, data = final_test, cex = .8, col = "dodgerblue", main = "k = 21")
lines(final_test$homicide_Rate, final_test$predicted_tar, col = "darkorange", lwd = 2)
# 
# ?save  
# save(model_final2,file = 'C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Final\\model.rda')
# 
# createModelFormula(xVars = names(p_norm[,c(1:8,10)]),targetVar = 'homicide_Rate')
save(model_final2)

load(file = 'C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\saved_model_rdf_s1_s2.rdf')
predict(final_tuned_nn_model_deploy,p_norm[1,c(1:8,10)])
p_norm[1,c(1:8,10)]
round((predict(final_tuned_nn_model_deploy,c(85,47,37,72,23,15,57,47,23))[,1] * 16.9666368) + 0.5294892,4)
