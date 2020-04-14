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
summary(primary_nona)

 
library('rvest')
library('tidyr')
html_countries <-  read_html('https://developers.google.com/public-data/docs/canonical/countries_csv')
countries_table <- html_nodes(html_countries,css = "table")
countries_df <- html_table(countries_table)[[1]]
countries <- countries_df[-(1:3)]
list_of_countries <- countries[order(countries),]


missCounts <- sapply(primary_nona,function(x) sum(is.na(x)))
missCounts
primary_nona$poverty_gap[is.na(primary_nona$poverty_gap)] <- median(primary_nona$poverty_gap,na.rm = TRUE)
primary_nona$public_health_exp[is.na(primary_nona$public_health_exp)] <- median(primary_nona$public_health_exp,na.rm = TRUE)
primary_nona$Infant_mortality_rate[is.na(primary_nona$Infant_mortality_rate)] <- median(primary_nona$Infant_mortality_rate,na.rm = TRUE)
primary_nona$GDP_per_capita[is.na(primary_nona$GDP_per_capita)] <- median(primary_nona$GDP_per_capita,na.rm = TRUE)
primary_nona$annual_health_care_per_capita[is.na(primary_nona$annual_health_care_per_capita)] <- median(primary_nona$annual_health_care_per_capita,na.rm = TRUE)
missCounts <- sapply(primary_nona,function(x) sum(is.na(x)))
missCounts

write.csv(primary_nona,'C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Final\\final_Cleaned.csv')
library('Amelia')
missmap(primary_nona, main = "Missing values")

pairs(primary_nona[c('poverty_gap','public_health_exp','malnutrition_death_rates','Infant_mortality_rate','GDP_per_capita','annual_health_care_per_capita','homicide_Rate')])
#relationship is kind of wierd()

pdf(file = "C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\EDA\\histograms_EDA.pdf")
#plotting only for numeric variables
for (i in names(which(sapply(primary_nona,is.numeric)))) 
{
  if(names(primary_nona[i]) != 'Year')
  {
    hist(primary_nona[,i], data = primary_nona,main = paste(i,"histogram"))
  }
}
dev.off() 


pdf(file = "C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\EDA\\boxplots_EDA.pdf")
#plotting only for numeric variables
for (i in names(which(sapply(primary_nona,is.numeric)))) 
  {
    if(names(primary_nona[i]) != 'Year')
    {
      boxplot(primary_nona[i], data = primary_nona,main = paste(i,"boxplot"))
    }
  }
dev.off() 

library("corrplot")
primarynona.cor <- cor(primary_nona[which(sapply(primary_nona, is.numeric))])
primarynona.cor <- primarynona.cor[2:8,2:8]
pdf(file = "C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\EDA\\coorrelationplots_EDA.pdf")
corrplot(primarynona.cor,title = "correlation plot for BostonHousing dataset")
dev.off()

#seems like there is almost no correlation between the predictors to the target variable



#Secondary dataset - EDA.
##Filtering and merging secondary 1 data##

#Load data, rename columns, Filter data to have only 2012
data1 <- read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Secondary1\\firms-with-female-top-manager-of-firms-bars.csv')
names(data1) <- c(names(data1)[-4], "Firms_with_female_top_manager")
nrow(data1)

nrow(data1[as.numeric(data1[,"Year"]) == 2012,])
data1 <- data1[data1[,"Year"] == 2012, ]
data1 <- data1[data1[,"Code"] != "", ]
data1[,"Code"] <- as.character(data1[,"Code"])
str(data1)
library('reshape2')
code1 <- melt(table(data1["Code"]))[,"Var1"]



data2 <- read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Secondary1\\proportion-of-women-in-senior-and-middle-management-positions.csv')
names(data2) <- c(names(data2)[-4], "Female_senior_and_middle_management_position")
data2[,"Code"] <- as.character(data2[,"Code"])
nrow(data2)
data2_ex <-  data2

nrow(data2[as.numeric(data2[,"Year"]) == 2012,])
data2 <- data2[data2[,"Year"] == 2012,]
data2 <- data2[data2[,"Code"] != "",]
code2 <- melt(table(data2["Code"]))[,"Var1"]



data3 <- read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Secondary1\\seats-held-by-women-in-national-parliaments.csv')
names(data3) <- c(names(data3)[-4], "Women_in_parlements")
data3[,"Code"] <- as.character(data3[,"Code"])
nrow(data3)

nrow(data3[data3[,"Year"] == 2012,])
data3 <- data3[data3[,"Year"] == 2012,]
data3 <- data3[data3[,"Code"] != "" ,]
code3 <- melt(table(data3["Code"]))[,"Var1"]



data4 <- read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Secondary1\\youth-literacy-female.csv')
names(data4) <- c(names(data4)[-4], "youth_literacy_female")
data4[,"Code"] <- as.character(data4[,"Code"])
nrow(data4)
data4_ex <-  data4

nrow(data4[data4[,"Year"] == 2012,])
data4 <- data4[data4[,"Year"] == 2012,]
data4 <- data4[data4[,"Code"] != "" ,]
code4 <- melt(table(data4["Code"]))[,"Var1"]

data5 <- read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Primary\\homicides-per-100000-people-per-year.csv')
names(data5) <- c(names(data5)[-4], "homicide_rate")
data5[,"Code"] <- as.character(data5[,"Code"])
nrow(data5)



nrow(data5[data5[,"Year"] == 2012,])
data5 <-  data5[data5[,"Year"] == 2012,]
data5 <-  data5[data5[,"Code"] != "",]
code5 <- melt(table(data5["Code"]))[,"Var1"]

#outa join
data <- merge(data1[,c(2,4)], data2, by="Code", all=TRUE)
data <- merge(data3[,c(2,4)], data, by="Code", all=TRUE)
data <- merge(data4[,c(2,4)], data, by="Code", all=TRUE)
data <-  merge(data5[,c(2,4)],data, by = "Code", all = TRUE)
data[,"Year"] <- rep(2012, nrow(data))

Entit <- data.frame(Code=c(data1[,"Code"],data2[,"Code"], data3[,"Code"], data4[,"Code"],data5[,"Code"]),
                    Entity=c(as.character(data1[,"Entity"]), as.character(data2[,"Entity"]), as.character(data3[,"Entity"]), as.character(data4[,"Entity"]),as.character(data5[,"Entity"])))
names(Entit)
Entit <- unique(Entit)

#data <- data[,-c(5)]

secondary_data1 <- merge(data, Entit, by="Code", all.x = TRUE)
keeps <- c("Code","homicide_rate","youth_literacy_female","Women_in_parlements","Year","Female_senior_and_middle_management_position","Entity.y","Firms_with_female_top_manager")
secondary_data1 <-   secondary_data1[,keeps,drop = FALSE]
colnames(secondary_data1)[7] <-  "Country"
miss_s1 <- sapply(secondary_data1,function(x) sum(is.na(x)))

##missing values are more in youth_literacy_female, female_senior and middle
hist(secondary_data1$Women_in_parlements)
#replace by median
secondary_data1$Women_in_parlements[which(is.na(secondary_data1$Women_in_parlements))] <- median(secondary_data1$Women_in_parlements,na.rm = TRUE)


data2012 <- data2_ex[data2_ex[,"Year"] == 2012,]
data2011 <- data2_ex[data2_ex[,"Year"] == 2011,]
data4_2012 <- data4_ex[data4_ex[,"Year"] == 2012,]
data4_2011 <- data4_ex[data4_ex[,"Year"] == 2011,]





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

#outa join
secondary_data2 <- merge(lower_secondary[,c(2,4)], gov_expen, by="Code", all=TRUE)
secondary_data2 <- merge(gross_enrol_primary[,c(2,4)], secondary_data2, by="Code", all=TRUE)
secondary_data2 <- merge(gross_enrol_secondary[,c(2,4)], secondary_data2, by="Code", all=TRUE)
secondary_data2 <-  merge(data5[,c(2,4)],secondary_data2, by = "Code", all = TRUE)
secondary_data2[,"Year"] <- rep(2012, nrow(secondary_data2))

Entit_2 <- data.frame(Code=c(lower_secondary[,"Code"],gov_expen[,"Code"], gross_enrol_primary[,"Code"], gross_enrol_secondary[,"Code"],data5[,"Code"]),
                    Entity=c(as.character(lower_secondary[,"Entity"]), as.character(gov_expen[,"Entity"]), as.character(gross_enrol_primary[,"Entity"]), as.character(gross_enrol_secondary[,"Entity"]),as.character(data5[,"Entity"])))
names(Entit_2)
Entit_2 <- unique(Entit_2)

#data <- data[,-c(5)]

secondary_data2_final <- merge(secondary_data2, Entit_2, by="Code", all.x = TRUE)
keeps <- c("Code","homicide_rate","gov_expen","gross_enrol_secondary","Year","gross_enrol_primary","Entity.y","lower_secondary")
secondary_data2_final <-   secondary_data2_final[,keeps,drop = FALSE]
colnames(secondary_data2_final)[7] <-  "Country"
nrow(secondary_data2_final)
