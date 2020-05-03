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

nrow(data2[as.numeric(data2[,"Year"]) == 2012,])
data2 <- data2[data2[,"Year"] == 2012,]
data2 <- data2[data2[,"Code"] != "",]
code2 <- melt(table(data2["Code"]))[,"Var.1"]



data3 <- read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Secondary1\\seats-held-by-women-in-national-parliaments.csv')
names(data3) <- c(names(data3)[-4], "Women_in_parlements")
data3[,"Code"] <- as.character(data3[,"Code"])
nrow(data3)

nrow(data3[data3[,"Year"] == 2012,])
data3 <- data3[data3[,"Year"] == 2012,]
data3 <- data3[data3[,"Code"] != "" ,]
code3 <- melt(table(data3["Code"]))[,"Var.1"]



data4 <- read.csv('C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Secondary1\\youth-literacy-female.csv')
names(data4) <- c(names(data4)[-4], "youth_literacy_female")
data4[,"Code"] <- as.character(data4[,"Code"])
nrow(data4)

nrow(data4[data4[,"Year"] == 2012,])
data4 <- data4[data4[,"Year"] == 2012,]
data4 <- data4[data4[,"Code"] != "" ,]
code4 <- melt(table(data4["Code"]))[,"Var.1"]

#outa join
data <- merge(data1[,c(2,4)], data2, by="Code", all=TRUE)
data <- merge(data3[,c(2,4)], data, by="Code", all=TRUE)
data <- merge(data4[,c(2,4)], data, by="Code", all=TRUE)

data[,"Year"] <- rep(2012, nrow(data))

Entit <- data.frame(Code=c(data1[,"Code"],data2[,"Code"], data3[,"Code"], data4[,"Code"]),
                    Entity=c(as.character(data1[,"Entity"]), as.character(data2[,"Entity"]), as.character(data3[,"Entity"]), as.character(data4[,"Entity"])))

Entit <- unique(Entit)

data <- data[,-c(5)]

data <- merge(data, Entit, by="Code", all.x = TRUE)

head(data)

install.packages("xlsx")
library("xlsx")
write.xlsx(data, "/Users/kevinmouofo/repos/CSP571/DataSets/Secondary1Final.xlsx", sheetName = "Secondary1Final", 
           col.names = TRUE, append = FALSE)

##Filtering and merging secondary 2 data##

#Load data, rename columns, Filter data to have only 2012
data5 <- read.csv("/Users/kevinmouofo/repos/CSP571/DataSets/secondary2/completion-rate-of-lower-secondary-education.csv")
names(data5) <- c(names(data5)[-4], "completion_rate_secondary_education")
nrow(data5)

nrow(data5[as.numeric(data5[,"Year"]) == 2012,])
data5 <- data5[data5[,"Year"] == 2012, ]
data5 <- data5[data5[,"Code"] != "", ]
data5[,"Code"] <- as.character(data5[,"Code"])
code5 <- melt(table(data5["Code"]))[,"Var.1"]



data6 <- read.csv("/Users/kevinmouofo/repos/CSP571/DataSets/secondary2/government-expenditure-on-education.csv")
names(data6) <- c(names(data6)[-4], "government_expanditure_education")
data6[,"Code"] <- as.character(data6[,"Code"])
nrow(data6)

nrow(data6[as.numeric(data6[,"Year"]) == 2012,])
data6 <- data6[data6[,"Year"] == 2012,]
data6 <- data6[data6[,"Code"] != "", ]
code6 <- melt(table(data6["Code"]))[,"Var.1"]



data7 <- read.csv("/Users/kevinmouofo/repos/CSP571/DataSets/secondary2/gross-enrollment-ratio-in-primary-education.csv")
names(data7) <- c(names(data7)[-4], "Gross_enrollement_ratio_primary")
data7[,"Code"] <- as.character(data7[,"Code"])
nrow(data7)

nrow(data7[data7[,"Year"] == 2012,])
data7 <- data7[data7[,"Year"] == 2012,]
data7 <- data7[data7[,"Code"] != "" ,]
code7 <- melt(table(data7["Code"]))[,"Var.1"]



data8 <- read.csv("/Users/kevinmouofo/repos/CSP571/DataSets/secondary2/gross-enrollment-ratio-in-secondary-education.csv")
names(data8) <- c(names(data8)[-4], "Gross_enrollement_ratio_secondary")
data8[,"Code"] <- as.character(data8[,"Code"])
nrow(data8)

nrow(data8[data8[,"Year"] == 2012,])
data8 <- data8[data8[,"Year"] == 2012,]
data8 <- data8[data8[,"Code"] != "" ,]
code8 <- melt(table(data8["Code"]))[,"Var.1"]

#outa join
data_ <- merge(data5[,c(2,4)], data6, by="Code", all=TRUE)
data_ <- merge(data7[,c(2,4)], data_, by="Code", all=TRUE)
data_ <- merge(data8[,c(2,4)], data_, by="Code", all=TRUE)

data_[,"Year"] <- rep(2012, nrow(data_))

Entit <- data.frame(Code=c(data5[,"Code"],data6[,"Code"], data7[,"Code"], data8[,"Code"]),
                    Entity=c(as.character(data5[,"Entity"]), as.character(data6[,"Entity"]), as.character(data7[,"Entity"]), as.character(data8[,"Entity"])))

Entit <- unique(Entit)

data_ <- data_[,-c(5)]

data_ <- merge(data_, Entit, by="Code", all.x = TRUE)

head(data_)
nrow(data_)

write.xlsx(data_, "/Users/kevinmouofo/repos/CSP571/DataSets/Secondary2Final.xlsx", sheetName = "Secondary2Final", 
           col.names = TRUE, append = FALSE)

