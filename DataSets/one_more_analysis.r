p_filtered <- read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\primary_cleaned_extra_indicators.csv')
summary(p_filtered)
nrow(p_filtered)
p_filtered <-  p_filtered[!p_filtered$Country == 'Eritrea' ,]
p_copy <- p_filtered

p_filtered$public_health_exp[is.na(p_filtered$public_health_exp)] <- mean(p_filtered$public_health_exp,na.rm = TRUE)
p_filtered$malnutrition_death_rates <- log10(p_filtered$malnutrition_death_rates)
p_filtered$Infant_mortality_rate  <-  log10(p_filtered$Infant_mortality_rate)

p_filtered$GDP_per_capita[is.na(p_filtered$GDP_per_capita)] <- median(p_filtered$GDP_per_capita,na.rm = TRUE)
p_filtered$GDP_per_capita <-  log10(p_filtered$GDP_per_capita)

p_filtered$annual_health_care_per_capita[is.na(p_filtered$annual_health_care_per_capita)] <- median(p_filtered$annual_health_care_per_capita,na.rm = TRUE)
p_filtered$annual_health_care_per_capita <-   log10(p_filtered$annual_health_care_per_capita)

p_filtered$median_age_2010[is.na(p_filtered$median_age_2010)] <- mean(p_filtered$median_age_2010,na.rm = TRUE)

p_filtered$fertility[is.na(p_filtered$fertility)] <- median(p_filtered$fertility,na.rm = TRUE)

cor(p_filtered[5:13],p_filtered$homicide_Rate, method = 'spearman')

write.csv(p_filtered[,c(5:13)],'C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Final\\primary_transformed.csv')

miss <- sapply(p_filtered,function(x) sum(is.na(x)))
miss

pdf(file = "C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\EDA\\histograms_EDA_filtered.pdf")
#plotting only for numeric variables
for (i in names(which(sapply(p_filtered,is.numeric)))) 
{
  if(names(p_filtered[i]) != 'Year')
  {
    hist(p_filtered[,i], data = p_filtered,main = paste(i,"histogram"))
  }
}
dev.off()
pdf(file = "C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\EDA\\boxplots_EDA_filtered.pdf")
#plotting only for numeric variables
for (i in names(which(sapply(p_filtered,is.numeric)))) 
{
  if(names(p_filtered[i]) != 'Year')
  {
    boxplot(p_filtered[i], data = p_filtered,main = paste(i,"boxplot"))
  }
}
dev.off() 


s_filtered <- read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\secondary1_cleaned_extra_indicators.csv')
summary(s_filtered)
nrow(s_filtered)
s_filterd_copy <- s_filtered

s_filterd_copy <- s_filterd_copy[,c(1,4,7,8,11,12,13,14)]
summary(s_filterd_copy)
summary(log10(s_filterd_copy$earnings_ratio_gap + 1))
  s_filterd_copy[s_filterd_copy$earnings_ratio_gap > 3.153e+07,]

s_filterd_copy$employment_ratio_gap[is.na(s_filterd_copy$employment_ratio_gap)] <- median(s_filterd_copy$employment_ratio_gap,na.rm = TRUE)
cor(log10(s_filterd_copy$employment_ratio_gap + 1 ),s_filterd_copy$homicide_rate,method = 'spearman')

cor(log10(s_filterd_copy$earnings_ratio_gap + 1),s_filterd_copy$homicide_rate)
s_filterd_copy$female_top_managers_2013[is.na(s_filterd_copy$female_top_managers_2013)] <- mean(s_filterd_copy$female_top_managers_2013,na.rm = TRUE)

x <- cor(p_filtered[,c(5:13)],method = 'spearman')
corrplot(x)






s2_filtered_copy$gross_enrol_primary[s2_filtered_copy$Country == 'Armenia'] <- 98.5
s2_filtered_copy$gross_enrol_primary[s2_filtered_copy$Country == 'Uzbekistan'] <- 91.813
s2_filtered_copy$gross_enrol_primary[s2_filtered_copy$Country == 'Bahrain'] <- 94.9
s2_filtered_copy$gross_enrol_primary[s2_filtered_copy$Country == 'Iraq'] <- 96.5
s2_filtered_copy$gross_enrol_primary[s2_filtered_copy$Country == 'Myanmar'] <- 96.7
s2_filtered_copy$gross_enrol_primary[s2_filtered_copy$Country == 'Philippines'] <- 116.8
s2_filtered_copy$gross_enrol_primary[s2_filtered_copy$Country == 'Qatar'] <- 106.9
s2_filtered_copy$gross_enrol_primary[s2_filtered_copy$Country == 'Turkmenistan'] <- 89.4

s2_filtered_copy$gross_enrol_primary[is.na(s2_filtered_copy$gross_enrol_primary)] <- median(s2_filtered_copy$gross_enrol_primary,na.rm = TRUE)
boxplot(s2_filtered_copy$gross_enrol_primary)

s2_filtered <- read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\secondary2_cleaned_extra_indicators.csv')
summary(s2_filtered)
nrow(s2_filtered)
s2_filtered_copy <- s2_filtered
summary(s2_filtered_copy)
s2_filtered_copy$gov_expen[s2_filtered_copy$Country == 'Albania'] <- 3.54
s2_filtered_copy$gov_expen[s2_filtered_copy$Country == 'Andorra'] <- 0.71
s2_filtered_copy$gov_expen[s2_filtered_copy$Country == 'Belgium'] <- 11.71
s2_filtered_copy$gov_expen[s2_filtered_copy$Country == 'Bhutan'] <- 14
s2_filtered_copy$gov_expen[s2_filtered_copy$Country == 'Cyprus'] <- 15.5
s2_filtered_copy$gov_expen[s2_filtered_copy$Country == 'Croatia'] <- 9
s2_filtered_copy$gov_expen[s2_filtered_copy$Country == 'Hungary'] <- 9
s2_filtered_copy$gov_expen[s2_filtered_copy$Country == 'Italy'] <- 8.3
s2_filtered_copy$gov_expen[s2_filtered_copy$Country == 'Kazakhstan'] <- 12
s2_filtered_copy$gov_expen[s2_filtered_copy$Country == 'Myanmar'] <- 0.42
s2_filtered_copy$gov_expen[s2_filtered_copy$Country == 'North Korea'] <- 10.7
s2_filtered_copy$gov_expen[s2_filtered_copy$Country == 'Oman'] <- 11.1
s2_filtered_copy$gov_expen[s2_filtered_copy$Country == 'Timor'] <- 9.6
s2_filtered_copy$gov_expen[s2_filtered_copy$Country == 'Uzbekistan'] <- 7.28
#s2_filtered_copy[is.na(s2_filtered_copy$gov_expen),]

s2_filtered_copy$gov_expen[is.na(s2_filtered_copy$gov_expen)] <- median(s2_filtered_copy$gov_expen,na.rm = TRUE)
boxplot((s2_filtered_copy$gov_expen))


s2_filtered_copy$lower_secondary[is.na(s2_filtered_copy$lower_secondary)] <- median(s2_filtered_copy$lower_secondary,na.rm = TRUE)
boxplot((s2_filtered_copy$lower_secondary)^2)








s2_filtered_copy$teritary_share_2010[is.na(s2_filtered_copy$teritary_share_2010)] <- mean(s2_filtered_copy$teritary_share_2010,na.rm = TRUE)
boxplot(s2_filtered_copy$teritary_share_2010)
s2_filtered_copy$teritary_share_2010 <-  (s2_filtered_copy$teritary_share_2010)^(1/2)




pdf(file = "C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\EDA\\histograms_EDA__s2_filtered.pdf")
#plotting only for numeric variables
for (i in names(which(sapply(s2_filtered,is.numeric)))) 
{
  if(names(s2_filtered[i]) != 'Year')
  {
    hist(s2_filtered[,i], data = s2_filtered,main = paste(i,"histogram"))
  }
}
dev.off()
pdf(file = "C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\EDA\\boxplots_EDA_s2filtered.pdf")
#plotting only for numeric variables
for (i in names(which(sapply(s2_filtered,is.numeric)))) 
{
  if(names(s2_filtered[i]) != 'Year')
  {
    boxplot(s2_filtered[i], data = s2_filtered,main = paste(i,"boxplot"))
  }
}
dev.off() 


summary(s2_filtered_copy$gross_enrol_primary)

x <- s2_filtered_copy

boxplot(log10(x$gross_enrol_primary))
boxplot(s2_filtered$gross_enrol_primary)

names(x)


merged_x <- merge(p_filtered,x[,c(3,9)],all = FALSE,by = 'Country')
names(merged_x)
cor(merged_x[,c(5:12)],merged_x$gross_enrol_primary,method = 'spearman')
plot(merged_x$gross_enrol_primary)
summary(merged_x)
write.csv(merged_x,file = 'C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Final\\merged_primary+secondary.csv')



merged_x$gov_exp_per_capita <- merged_x$gov_expen * merged_x$GDP_per_capita
hist((merged_x$gov_exp_per_capita))
cor(log10(merged_x$gov_exp_per_capita),merged_x$homicide_Rate,method = 'spearman')
merged_x <- merged_x[,c(5:13,15)]
corrplot(cor(merged_x,method = 'spearman'))
write.csv(merged_x,file = 'C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Final\\merged_primary+secondary12.csv')
