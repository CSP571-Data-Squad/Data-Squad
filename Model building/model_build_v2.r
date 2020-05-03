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

save(model_final2)
