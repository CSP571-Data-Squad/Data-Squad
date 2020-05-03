library('dplyr')
library('reshape2')
p <-  read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Final\\Final DataSet.csv')
colnames(p) <- c('Country','Code','Year','poverty_gap','public_health_exp','malnutrition_death_rates','Infant_mortality_rate','GDP_per_capita','annual_health_care_per_capita','homicide_Rate')
summary(p)
nrow(p)
p <- p[!is.na(p$homicide_Rate),]
p <- p[p$Code != "",]
p$Country <-  factor(p$Country)
p$Code <- factor(p$Code)

life_exp <- read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Primary\\life-expectancy.csv')
names(life_exp) <- c(names(life_exp)[-4], "life_exp")
life_exp[,"Code"] <- as.character(life_exp[,"Code"])
nrow(life_exp)
#data2_ex <-  data2
nrow(life_exp[as.numeric(life_exp[,"Year"]) == 2012,])
life_exp <- life_exp[life_exp[,"Year"] == 2012,]
life_exp <- life_exp[life_exp[,"Code"] != "",]
life_exp_code <- melt(table(life_exp["Code"]))[,"Var1"]



median_age <- read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Primary\\median-age.csv')
names(median_age) <- c(names(median_age)[-4], "median_age_2010")
median_age[,"Code"] <- as.character(median_age[,"Code"])
nrow(median_age)
#data2_ex <-  data2
nrow(median_age[as.numeric(median_age[,"Year"]) == 2010,])
median_age <- median_age[median_age[,"Year"] == 2010,]
median_age <- median_age[median_age[,"Code"] != "",]
median_age_code <- melt(table(median_age["Code"]))[,"Var1"]

fertility <- read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Primary\\fertility-rate-complete-gapminder.csv')
names(fertility) <- c(names(life_exp)[-4], "fertility")
fertility[,"Code"] <- as.character(fertility[,"Code"])
nrow(fertility)
#data2_ex <-  data2
nrow(fertility[as.numeric(fertility[,"Year"]) == 2012,])
fertility <- fertility[fertility[,"Year"] == 2012,]
fertility <- fertility[fertility[,"Code"] != "",]
fertility_code <- melt(table(fertility["Code"]))[,"Var1"]


p_data <-  merge(p,life_exp[,c(2:4)],all = TRUE, by = 'Code')
p_data <- merge(fertility[,c(2:4)],p_data, all = TRUE, by = 'Code')
p_data <- merge(median_age[,c(2:4)],p_data, all = TRUE, by = 'Code')
p_data[,"Year"] <- rep(2012, nrow(p_data))

Entit_p <- data.frame(Code=c(p[,"Code"],fertility[,"Code"], median_age[,"Code"], life_exp[,"Code"]),
                      Entity=c(as.character(p[,"Country"]), as.character(fertility[,"Entity"]), as.character(median_age[,"Entity"]), as.character(life_exp[,"Entity"])))
names(Entit_p)
Entit_p <- unique(Entit_p)
p_data2_final <- merge(p_data, Entit_p, by="Code", all.x = TRUE)
keeps <- c('Country','Code','Year','poverty_gap','public_health_exp','malnutrition_death_rates','Infant_mortality_rate','GDP_per_capita','annual_health_care_per_capita','life_exp','median_age_2010','fertility','homicide_Rate')
p_data2_final <-   p_data2_final[,keeps,drop = FALSE]
nrow(p_data2_final)
summary(p_data2_final)


p_data2_final <- p_data2_final[!is.na(p_data2_final$Country),]
p_data2_final <- p_data2_final[!p_data2_final$Country == 'World',]
p_data2_final <- p_data2_final[!is.na(p_data2_final$Infant_mortality_rate),]



write.csv(p_data2_final,'C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Final\\primary_cleaned_extra_indicators.csv')
###############################################################################
s1_data5 <- read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Primary\\homicides-per-100000-people-per-year.csv')
names(s1_data5) <- c(names(s1_data5)[-4], "homicide_rate")
s1_data5[,"Code"] <- as.character(s1_data5[,"Code"])
nrow(s1_data5)
#data2_ex <-  data2
nrow(s1_data5[as.numeric(s1_data5[,"Year"]) == 2012,])
s1_data5 <- s1_data5[s1_data5[,"Year"] == 2012,]
s1_data5 <- s1_data5[s1_data5[,"Code"] != "",]
s1_data5_code <- melt(table(s1_data5["Code"]))[,"Var1"]
nrow(s1_data5)

female_top_managers <- read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Secondary1\\firms-with-female-top-manager-of-firms-bars.csv')
names(female_top_managers) <- c(names(female_top_managers)[-4], "female_top_managers_2013")
female_top_managers[,"Code"] <- as.character(female_top_managers[,"Code"])
nrow(female_top_managers)
nrow(female_top_managers[as.numeric(female_top_managers[,"Year"]) == 2013,])
female_top_managers <- female_top_managers[female_top_managers[,"Year"] == 2013,]
female_top_managers <- female_top_managers[female_top_managers[,"Code"] != "",]
female_top_managers_code <- melt(table(female_top_managers["Code"]))[,"Var1"]


female_prp <- read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Secondary1\\proportion-of-women-in-senior-and-middle-management-positions.csv')
names(female_prp) <- c(names(female_prp)[-4], "female_prp")
female_prp[,"Code"] <- as.character(female_prp[,"Code"])
nrow(female_prp)
nrow(female_prp[as.numeric(female_prp[,"Year"]) == 2012,])
female_prp <- female_prp[female_prp[,"Year"] == 2012,]
female_prp <- female_prp[female_prp[,"Code"] != "",]
female_prp_code <- melt(table(female_prp["Code"]))[,"Var1"]

female_parliament <- read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Secondary1\\seats-held-by-women-in-national-parliaments.csv')
names(female_parliament) <- c(names(female_parliament)[-4], "female_parliament")
female_parliament[,"Code"] <- as.character(female_parliament[,"Code"])
nrow(female_parliament)
nrow(female_parliament[as.numeric(female_parliament[,"Year"]) == 2012,])
female_parliament <- female_parliament[female_parliament[,"Year"] == 2012,]
female_parliament <- female_parliament[female_parliament[,"Code"] != "",]
female_parliament_code <- melt(table(female_parliament["Code"]))[,"Var1"]

female_literacy <- read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Secondary1\\youth-literacy-female.csv')
names(female_literacy) <- c(names(female_literacy)[-4], "female_literacy_2011")
female_literacy[,"Code"] <- as.character(female_literacy[,"Code"])
nrow(female_literacy)
nrow(female_literacy[as.numeric(female_literacy[,"Year"]) == 2011,])
female_literacy <- female_literacy[female_literacy[,"Year"] == 2011,]
female_literacy <- female_literacy[female_literacy[,"Code"] != "",]
female_literacy_code <- melt(table(female_literacy["Code"]))[,"Var1"]

male_literacy <- read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Secondary1\\youth-literacy-males.csv')
names(male_literacy) <- c(names(male_literacy)[-4], "male_literacy_2011")
male_literacy[,"Code"] <- as.character(male_literacy[,"Code"])
nrow(male_literacy)
nrow(male_literacy[as.numeric(male_literacy[,"Year"]) == 2011,])
male_literacy <- male_literacy[male_literacy[,"Year"] == 2011,]
male_literacy <- male_literacy[male_literacy[,"Code"] != "",]
male_literacy_code <- melt(table(male_literacy["Code"]))[,"Var1"]

wage_gap <- read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Secondary1\\gender-wage-gap-oecd.csv')
names(wage_gap) <- c(names(wage_gap)[-4], "wage_gap")
wage_gap[,"Code"] <- as.character(wage_gap[,"Code"])
nrow(wage_gap)
nrow(wage_gap[as.numeric(wage_gap[,"Year"]) == 2012,])
wage_gap <- wage_gap[wage_gap[,"Year"] == 2012,]
wage_gap <- wage_gap[wage_gap[,"Code"] != "",]
wage_gap_code <- melt(table(wage_gap["Code"]))[,"Var1"]


employmen_ratio <- read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Secondary1\\employment-to-population-ratio-men-vs-women.csv')
names(employmen_ratio)[c(4:6)] <- c("men_employment",'women_employmet','employment_ratio_gap')
employmen_ratio[,"Code"] <- as.character(employmen_ratio[,"Code"])
nrow(employmen_ratio)
nrow(employmen_ratio[as.numeric(employmen_ratio[,"Year"]) == 2012,])
employmen_ratio <- employmen_ratio[employmen_ratio[,"Year"] == 2012,]
employmen_ratio <- employmen_ratio[employmen_ratio[,"Code"] != "",]
employmen_ratio_code <- melt(table(employmen_ratio["Code"]))[,"Var1"]


earnings_ratio <- read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Secondary1\\hourly-earnings-male-vs-female.csv')
names(earnings_ratio)[c(4:6)] <- c("men_earings",'women_earnings','earnings_ratio_gap')
earnings_ratio[,"Code"] <- as.character(earnings_ratio[,"Code"])
nrow(earnings_ratio)
nrow(earnings_ratio[as.numeric(earnings_ratio[,"Year"]) == 2012,])
earnings_ratio <- earnings_ratio[earnings_ratio$Year == 2012,]
earnings_ratio <- earnings_ratio[earnings_ratio[,"Code"] != "",]
earnings_ratio_code <- melt(table(earnings_ratio["Code"]))[,"Var1"]

s1_data <-  merge(data5,female_top_managers[,c(2:4)],all = TRUE,by = 'Code')
s1_data <-  merge(female_prp[,c(2:4)],s1_data,all = TRUE,by = 'Code')
s1_data <-  merge(female_parliament[,c(2:4)],s1_data,all = TRUE,by = 'Code')
s1_data <-  merge(female_literacy[,c(2:4)],s1_data,all = TRUE,by = 'Code')
s1_data <-  merge(male_literacy[,c(2:4)],s1_data,all = TRUE,by = 'Code')
s1_data <-  merge(wage_gap[,c(2:4)],s1_data,all = TRUE,by = 'Code')
s1_data <-  merge(employmen_ratio[,c(2:6)],s1_data,all = TRUE,by = 'Code')
s1_data <-  merge(earnings_ratio[,c(2:6)],s1_data,all = TRUE,by = 'Code')

Entit_s1 <- data.frame(Code=c(female_top_managers[,"Code"],female_prp[,"Code"], female_parliament[,"Code"], female_literacy[,"Code"],male_literacy[,"Code"],wage_gap[,"Code"],employmen_ratio[,"Code"],earnings_ratio[,"Code"],data5[,"Code"]),
                      Entity=c(as.character(female_top_managers[,"Entity"]),as.character(female_prp[,"Entity"]),as.character(female_parliament[,"Entity"]),as.character(female_literacy[,"Entity"]),as.character(male_literacy[,"Entity"]),as.character(wage_gap[,"Entity"]), as.character(employmen_ratio[,"Entity"]), as.character(earnings_ratio[,"Entity"]), as.character(data5[,"Entity"])))
names(Entit_s1)
Entit_s1 <- unique(Entit_s1)
s1_data2_final <- merge(s1_data, Entit_s1, by="Code", all.x = TRUE)
keeps <- c("homicide_rate","men_earings","women_earnings","earnings_ratio_gap" ,'men_employment',"women_employmet","employment_ratio_gap","wage_gap","male_literacy_2011",'female_literacy_2011','female_prp','female_top_managers_2013',"Entity.y","Code")
s1_data2_final <-   s1_data2_final[,keeps,drop = FALSE]
names(s1_data2_final)[13] <- 'Country'
nrow(s1_data2_final)
summary(s1_data2_final)

s1_data2_final <- s1_data2_final[!is.na(s1_data2_final$homicide_rate),]
s1_data2_final <- s1_data2_final[!s1_data2_final$Country ==  'World',]
s1_data2_final <- s1_data2_final[!((s1_data2_final$Country == 'Guam') |(s1_data2_final$Country == 'Syria') |(s1_data2_final$Country == 'Puerto Rico') |(s1_data2_final$Country == 'Northern Mariana Islands') |(s1_data2_final$Country == 'Greenland') |(s1_data2_final$Country == 'Bermuda') |(s1_data2_final$Country == 'American Samoa') |(s1_data2_final$Country == 'Taiwan') | (s1_data2_final$Country == 'United States Virgin Islands')),]

write.csv(s1_data2_final,'C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Final\\secondary1_cleaned_extra_indicators.csv')

#############################################################################################################

summary(secondary_temp)
secondary_temp <- secondary_temp[!is.na(secondary_temp$homicide_rate),]


gov_expen_ter <- read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Secondary2\\government-expenditure-on-tertiary-education-by-country.csv')
names(gov_expen_ter) <- c(names(gov_expen_ter)[-4], "gov_expen_ter_2010")
gov_expen_ter[,"Code"] <- as.character(gov_expen_ter[,"Code"])
nrow(gov_expen_ter)
nrow(gov_expen_ter[as.numeric(gov_expen_ter[,"Year"]) == 2010,])
gov_expen_ter <- gov_expen_ter[gov_expen_ter[,"Year"] == 2010,]
gov_expen_ter <- gov_expen_ter[gov_expen_ter[,"Code"] != "",]
gov_expen_ter_code <- melt(table(gov_expen_ter["Code"]))[,"Var1"]

grad_per_f <- read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Secondary2\\share-graduates-stem-female.csv')
names(grad_per_f) <- c(names(grad_per_f)[-4], "grad_per_f")
grad_per_f[,"Code"] <- as.character(grad_per_f[,"Code"])
nrow(grad_per_f)
nrow(grad_per_f[as.numeric(grad_per_f[,"Year"]) == 2012,])
grad_per_f <- grad_per_f[grad_per_f[,"Year"] == 2012,]
grad_per_f <- grad_per_f[grad_per_f[,"Code"] != "",]
grad_per_f_code <- melt(table(grad_per_f["Code"]))[,"Var1"]

teritary_share <- read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Secondary2\\share-of-the-population-with-completed-tertiary-education.csv')
names(teritary_share) <- c(names(teritary_share)[-4], "teritary_share_2010")
teritary_share[,"Code"] <- as.character(teritary_share[,"Code"])
nrow(teritary_share)
nrow(teritary_share[as.numeric(teritary_share[,"Year"]) == 2010,])
teritary_share <- teritary_share[teritary_share[,"Year"] == 2010,]
teritary_share <- teritary_share[teritary_share[,"Code"] != "",]
teritary_share_code <- melt(table(teritary_share["Code"]))[,"Var1"]


s_data <-  merge(secondary_temp,gov_expen_ter[,c(2:4)],all = TRUE, by = 'Code')
s_data <- merge(grad_per_f[,c(2:4)],s_data, all = TRUE, by = 'Code')
s_data <- merge(teritary_share[,c(2:4)],s_data, all = TRUE, by = 'Code')
s_data[,"Year"] <- rep(2012, nrow(s_data))
Entit_s <- data.frame(Code=c(secondary_temp[,"Code"],gov_expen_ter[,"Code"], grad_per_f[,"Code"], teritary_share[,"Code"]),
                      Entity=c(as.character(secondary_temp[,"Country"]), as.character(gov_expen_ter[,"Entity"]), as.character(grad_per_f[,"Entity"]), as.character(teritary_share[,"Entity"])))
names(Entit_s)
Entit_s <- unique(Entit_s)
s_data2_final <- merge(s_data, Entit_s, by="Code", all.x = TRUE)
keeps <- c("homicide_rate","lower_secondary","gov_expen","gross_enrol_secondary","gross_enrol_primary",'gov_expen_ter_2010','grad_per_f','teritary_share_2010',"Country","Year","Code")
s_data2_final <-   s_data2_final[,keeps,drop = FALSE]
nrow(s_data2_final)
summary(s_data2_final)


s_data2_final <- s_data2_final[!is.na(s_data2_final$homicide_rate),]
s_data2_final <- s_data2_final[!s_data2_final$Country ==  'World',]
s_data2_final <- s_data2_final[!((s_data2_final$Country == 'Guam') |(s_data2_final$Country == 'Syria') |(s_data2_final$Country == 'Puerto Rico') |(s_data2_final$Country == 'Northern Mariana Islands') |(s_data2_final$Country == 'Greenland') |(s_data2_final$Country == 'Bermuda') |(s_data2_final$Country == 'American Samoa') |(s_data2_final$Country == 'Taiwan') | (s_data2_final$Country == 'United States Virgin Islands')),]
nrow(s_data2_final)

write.csv(s_data2_final,'C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Final\\secondary2_cleaned_extra_indicators.csv')

final <- merge(p_data2_final,s1_data2_final,all = FALSE,by ='Country')
final <- merge(s_data2_final,final,all = FALSE,by = 'Country')
nrow(final)
names(final)
keep_d <- c('Country','Code','poverty_gap','public_health_exp','malnutrition_death_rates','Infant_mortality_rate','GDP_per_capita','annual_health_care_per_capita','life_exp','median_age_2010','fertility',"lower_secondary","gov_expen","gross_enrol_secondary","gross_enrol_primary",'gov_expen_ter_2010','grad_per_f','teritary_share_2010',"men_earings","women_earnings","earnings_ratio_gap" ,'men_employment',"women_employmet","employment_ratio_gap","wage_gap","male_literacy_2011",'female_literacy_2011','female_prp','female_top_managers_2013','homicide_Rate')
final <- final[,keep_d,drop= FALSE]
nrow(final)
summary(final)
write.csv(final,'C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Final\\combined_all_indicators.csv')
