library('dplyr')
primary <-  read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Final\\Final DataSet.csv')
colnames(primary) <- c('Country','Country_Code','Year','poverty_gap','public_health_exp','malnutrition_death_rates','Infant_mortality_rate','GDP_per_capita','annual_health_care_per_capita','homicide_Rate')
summary(primary)
nrow(primary)

library('readr')
f <- read_file('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\primary\\sql')

#removing the rows in which there are NA's in target value.
primary_nona <- primary[!is.na(primary$homicide_Rate),]
primary_nona <- primary_nona[primary_nona$Country_Code != "",]
primary_nona$Country <-  factor(primary_nona$Country)
primary_nona$Country_Code <- factor(primary_nona$Country_Code)
primary_nona$poverty_gap <- as.numeric(format(primary_nona$poverty_gap,scientific = FALSE))
primary_before <- primary_nona
summary(primary_before)

normalization <-function(x) { (x -min(x))/(max(x)-min(x))   }
#summary(normalization(primary_before$malnutrition_death_rates))
primary_before <- primary_before[!primary_before$Country == 'World',]
primary_before$poverty_gap <- log10(primary_before$poverty_gap + 1)
primary_before$poverty_gap[is.na(primary_before$poverty_gap)] <- median(primary_before$poverty_gap,na.rm = TRUE)

primary_before <- primary_before[!is.na(primary_before$Infant_mortality_rate),]
primary_before$Infant_mortality_rate <- sqrt(primary_before$Infant_mortality_rate)
primary_before$malnutrition_death_rates <- (primary_before$malnutrition_death_rates)^(1/4)

#resource from https://knoema.com/search?query=public%20health%20expenditure%20south%20sudan
primary_before$public_health_exp[which(primary_before$Country == 'South Sudan')] <-  1.09

primary_before$GDP_per_capita[is.na(primary_before$GDP_per_capita)] <- median(primary_before$GDP_per_capita,na.rm = TRUE)
primary_before$GDP_per_capita <- log10(primary_before$GDP_per_capita)

primary_before$public_health_exp[is.na(primary_before$public_health_exp)] <- median(primary_before$public_health_exp,na.rm = TRUE)
primary_before$public_health_exp <- log10(primary_before$public_health_exp)

primary_before$annual_health_care_per_capita[is.na(primary_before$annual_health_care_per_capita)] <- median(primary_before$annual_health_care_per_capita,na.rm = TRUE)
primary_before$annual_health_care_per_capita <- log10(primary_before$annual_health_care_per_capita)

library('rvest')
library('tidyr')

missCounts <- sapply(primary_before,function(x) sum(is.na(x)))
missCounts
# primary_nona$poverty_gap[is.na(primary_nona$poverty_gap)] <- median(primary_nona$poverty_gap,na.rm = TRUE)
# primary_nona$public_health_exp[is.na(primary_nona$public_health_exp)] <- median(primary_nona$public_health_exp,na.rm = TRUE)
# primary_nona$Infant_mortality_rate[is.na(primary_nona$Infant_mortality_rate)] <- median(primary_nona$Infant_mortality_rate,na.rm = TRUE)
# primary_nona$GDP_per_capita[is.na(primary_nona$GDP_per_capita)] <- median(primary_nona$GDP_per_capita,na.rm = TRUE)
# primary_nona$annual_health_care_per_capita[is.na(primary_nona$annual_health_care_per_capita)] <- median(primary_nona$annual_health_care_per_capita,na.rm = TRUE)
# missCounts <- sapply(primary_nona,function(x) sum(is.na(x)))
# missCounts
# summary(primary_nona)

write.csv(primary_before,'C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Final\\final_Cleaned_v2.csv')
library('Amelia')
missmap(primary_nona, main = "Missing values")

pairs(primary_nona[c('poverty_gap','public_health_exp','malnutrition_death_rates','Infant_mortality_rate','GDP_per_capita','annual_health_care_per_capita','homicide_Rate')])
#relationship is kind of wierd()

pdf(file = "C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\EDA\\histograms_EDA.pdf")
#plotting only for numeric variables
for (i in names(which(sapply(primary_before,is.numeric)))) 
{
  if(names(primary_before[i]) != 'Year')
  {
    hist(primary_before[,i], data = primary_before,main = paste(i,"histogram"))
  }
}
dev.off() 


pdf(file = "C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\EDA\\boxplots_EDA.pdf")
#plotting only for numeric variables
for (i in names(which(sapply(primary_before,is.numeric)))) 
  {
    if(names(primary_before[i]) != 'Year')
    {
      boxplot(primary_before[i], data = primary_before,main = paste(i,"boxplot"))
    }
  }
dev.off() 

library("corrplot")
primarynona.cor <- cor(primary_before[which(sapply(primary_before, is.numeric))],method = 'spearman')
primarynona.cor <- primarynona.cor[2:8,2:8]
pdf(file = "C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\EDA\\coorrelationplots_EDA.pdf")
corrplot(primarynona.cor,title = "correlation plot for BostonHousing dataset")
dev.off()

#seems like there is almost no correlation between the predictors to the target variable



#Secondary dataset - EDA
#########################################################################
#secondary 2

lower_secondary <- read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Secondary2\\completion-rate-of-lower-secondary-education.csv')
names(lower_secondary) <- c(names(lower_secondary)[-4], "lower_secondary")
lower_secondary[,"Code"] <- as.character(lower_secondary[,"Code"])
nrow(lower_secondary)
#data2_ex <-  data2
nrow(lower_secondary[as.numeric(lower_secondary[,"Year"]) == 2012,])
lower_secondary <- lower_secondary[lower_secondary[,"Year"] == 2012,]
lower_secondary <- lower_secondary[lower_secondary[,"Code"] != "",]
lower_secondary_code <- melt(table(lower_secondary["Code"]))[,"Var1"]



gov_expen <- read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Secondary2\\government-expenditure-on-education.csv')
names(gov_expen) <- c(names(gov_expen)[-4], "gov_expen")
gov_expen[,"Code"] <- as.character(gov_expen[,"Code"])
nrow(gov_expen)
#data2_ex <-  data2
nrow(gov_expen[as.numeric(gov_expen[,"Year"]) == 2012,])
gov_expen <- gov_expen[gov_expen[,"Year"] == 2012,]
gov_expen <- gov_expen[gov_expen[,"Code"] != "",]
gov_expen_code <- melt(table(gov_expen["Code"]))[,"Var1"]


gross_enrol_primary <- read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Secondary2\\gross-enrollment-ratio-in-primary-education.csv')
names(gross_enrol_primary) <- c(names(gross_enrol_primary)[-4], "gross_enrol_primary")
gross_enrol_primary[,"Code"] <- as.character(gross_enrol_primary[,"Code"])
nrow(gross_enrol_primary)
#data2_ex <-  data2
nrow(gross_enrol_primary[as.numeric(gross_enrol_primary[,"Year"]) == 2012,])
gross_enrol_primary <- gross_enrol_primary[gross_enrol_primary[,"Year"] == 2012,]
gross_enrol_primary <- gross_enrol_primary[gross_enrol_primary[,"Code"] != "",]
gross_enrol_primary_code <- melt(table(gross_enrol_primary["Code"]))[,"Var1"]


gross_enrol_secondary <- read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Secondary2\\gross-enrollment-ratio-in-secondary-education.csv')
names(gross_enrol_secondary) <- c(names(gross_enrol_secondary)[-4], "gross_enrol_secondary")
gross_enrol_secondary[,"Code"] <- as.character(gross_enrol_secondary[,"Code"])
nrow(gross_enrol_secondary)
#data2_ex <-  data2
nrow(gross_enrol_secondary[as.numeric(gross_enrol_secondary[,"Year"]) == 2012,])
gross_enrol_secondary <- gross_enrol_secondary[gross_enrol_secondary[,"Year"] == 2012,]
gross_enrol_secondary <- gross_enrol_secondary[gross_enrol_secondary[,"Code"] != "",]
gross_enrol_secondary_code <- melt(table(gross_enrol_secondary["Code"]))[,"Var1"]


data5 <- read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Primary\\homicides-per-100000-people-per-year.csv')
names(data5) <- c(names(data5)[-4], "homicide_rate")
data5[,"Code"] <- as.character(data5[,"Code"])
nrow(data5)
#data2_ex <-  data2
nrow(data5[as.numeric(data5[,"Year"]) == 2012,])
data5 <- data5[data5[,"Year"] == 2012,]
data5 <- data5[data5[,"Code"] != "",]
data5_code <- melt(table(data5["Code"]))[,"Var1"]
nrow(data5)



#outa join
secondary_data2 <- merge(lower_secondary[,c(2,4)], gov_expen, by="Code", all=TRUE)
secondary_data2 <- merge(gross_enrol_primary[,c(2,4)], secondary_data2, by="Code", all=TRUE)
secondary_data2 <- merge(gross_enrol_secondary[,c(2,4)], secondary_data2, by="Code", all=TRUE)
secondary_data2 <-  merge(data5[,c(2,4)],secondary_data2, by = "Code", all = TRUE)
secondary_data2[,"Year"] <- rep(2012, nrow(secondary_data2))

Entit_2 <- data.frame(Code=c(lower_secondary[,"Code"],gov_expen[,"Code"], gross_enrol_primary[,"Code"], gross_enrol_secondary[,"Code"],data5[,"Code"]),
                    Entity=c(as.character(lower_secondary[,"Entity"]), as.character(gov_expen[,"Entity"]), as.character(gross_enrol_primary[,"Entity"]), as.character(gross_enrol_secondary[,"Entity"]),as.character(data5[,'Entity'])))
names(Entit_2)
Entit_2 <- unique(Entit_2)
secondary_data2_final <- merge(secondary_data2, Entit_2, by="Code", all.x = TRUE)
keeps <- c("homicide_rate","lower_secondary","gov_expen","gross_enrol_secondary","gross_enrol_primary","Entity.y","Year","Code")
secondary_data2_final <-   secondary_data2_final[,keeps,drop = FALSE]
colnames(secondary_data2_final)[6] <-  "Country"


miss_s2 <- sapply(secondary_data2_final,function(x) sum(is.na(x)))
missing_percent <- (miss_s2/nrow(secondary_data2_final))*100
miss_s2
missing_percent
secondary_temp <-  secondary_data2_final

#cleaning dataset
secondary_data2_final <- secondary_data2_final[!is.na(secondary_data2_final$homicide_rate),]
secondary_data2_final$Country <- factor(secondary_data2_final$Country)


#gov_expen has no outliers and histogram also seems normal distribution so using mean
secondary_data2_final$gov_expen[which(is.na(secondary_data2_final$gov_expen))] <- mean(secondary_data2_final$gov_expen,na.rm = TRUE)

#gross_enrol_primary having outliers so replacing my median
secondary_data2_final$gross_enrol_primary[which(is.na(secondary_data2_final$gross_enrol_primary))] <- median(secondary_data2_final$gross_enrol_primary,na.rm = TRUE)


#gross_enrol_Secondary is being replaced by mean for NA's.Since
#for the NA's the corresponding the gross_enrol_primary rate is pretty good
#and has minimum of 80%.the mean is around 80% for secondary. So it would be reasonable
secondary_data2_final$gross_enrol_secondary[which(is.na(secondary_data2_final$gross_enrol_secondary))] <-  mean(secondary_data2_final$gross_enrol_secondary,na.rm = TRUE)

#since, we have considered the minimum value, completion rate for lower secondary
#is also assumed it would be lower. as mean is lower trying to impute it by mean
secondary_data2_final$lower_secondary[which(is.na(secondary_data2_final$lower_secondary))] <-  mean(secondary_data2_final$lower_secondary,na.rm = TRUE)

library('Amelia')
missmap(secondary_data2_final, main = "Missing values")


pdf(file = "C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\EDA\\histograms_EDA_secondary2.pdf")
#plotting only for numeric variables
for (i in names(which(sapply(secondary_data2_final,is.numeric)))) 
{
  if(names(secondary_data2_final[i]) != 'Year')
  {
    hist(secondary_data2_final[,i], data = secondary_data2_final,main = paste(i,"histogram"))
  }
}
dev.off()

pdf(file = "C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\EDA\\boxplots_EDA_secondary2.pdf")
#plotting only for numeric variables
for (i in names(which(sapply(secondary_data2_final,is.numeric)))) 
{
  if(names(secondary_data2_final[i]) != 'Year')
  {
    boxplot(secondary_data2_final[i], data = secondary_data2_final,main = paste(i,"boxplot"))
  }
}
dev.off() 

write.csv(secondary_data2_final,'C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Final\\Secondary2_Cleaned.csv')

secondary2_cor <- cor(secondary_data2_final[which(sapply(secondary_data2_final,is.numeric))])
secondary2_cor <- secondary2_cor[c(1,2,3,5,6),c(1,2,3,5,6)]
corrplot(secondary2_cor)
#seems there is no correaltion between predictors to the target variable.