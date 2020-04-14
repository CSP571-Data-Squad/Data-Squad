library('readxl')
file <- '/Users/kevinmouofo/repos/CSP571/DataSets/Secondary1Final.xlsx'
firstSecondary <- read_excel(file, sheet = "Secondary1Final")
firstSecondary <- as.data.frame(firstSecondary)

#remove the row name useless
firstSecondary <- firstSecondary[,-1]

###  female top manager ##########
name <- "Firms_with_female_top_manager"
numberOfMissing <- sum(is.na(firstSecondary[,name]))
numberOfObservation <- nrow(firstSecondary)

# test the percentage of common country code 
#with our primary data
sum(firstSecondary[,"Code"]%in%finalCode)

#To feel the missing values of female manager
# lets assume that the number of female manager has not changed 
#in the past years if we have not the value of 2012 and thus, lets take the 
#the previous value registered

#Load the data
femaleManager <- read.csv("/Users/kevinmouofo/repos/CSP571/DataSets/secondary1/firms-with-female-top-manager-of-firms-bars.csv")
names(femaleManager) <- c(names(femaleManager)[-4], name)
femaleManager[,"Code"] <- as.character(femaleManager[,"Code"])

#Go through the data and assign the previous value of female manager as the current value
i=0
for (na in firstSecondary[,name]){
  i=i+1
  if (is.na(na)){
    firstSecondary[i,name] <- femaleManager[femaleManager[,"Code"]==firstSecondary[i,"Code"] & femaleManager[,"Year"] <= 2012,name][1]
  }
}

numberOfMissing <- sum(is.na(firstSecondary[,name]))
numberOfObservation <- nrow(firstSecondary)

#Go through the data and assign the next value of female manager as the current value
#Lets try next values assignment
i=0
for (na in firstSecondary[,name]){
  i=i+1
  if (is.na(na)){
    firstSecondary[i,name] <- femaleManager[femaleManager[,"Code"]==firstSecondary[i,"Code"] & femaleManager[,"Year"] >= 2012,name][1]
  }
}

# test if we could get rid of the missing values by testing the percentage of common country code 
#with our primary data
sum(firstSecondary[!is.na(firstSecondary[,name]),"Code"]%in%finalCode)
numberOfMissing <- sum(is.na(firstSecondary[,name]))
numberOfObservation <- nrow(firstSecondary)

#As we have 125 common country in common we could get rid of the country with missing values

#check
head(firstSecondary) 



###Function to that for the rest of the first secondary variables

directories <- c("/Users/kevinmouofo/repos/CSP571/DataSets/secondary1/proportion-of-women-in-senior-and-middle-management-positions.csv",
                 "/Users/kevinmouofo/repos/CSP571/DataSets/secondary1/seats-held-by-women-in-national-parliaments.csv",
                 "/Users/kevinmouofo/repos/CSP571/DataSets/secondary1/youth-literacy-female.csv")

column_names <- c("Female_senior_and_middle_management_position", "Women_in_parlements","youth_literacy_female")


clean <-  function(directory, name){

#Load the data
variableData <- read.csv(directory)
names(variableData ) <- c(names(variableData)[-4], name)
variableData[,"Code"] <- as.character(variableData[,"Code"])

#Variable
print(name)
# percentage of common country code with our primary data
print(sum(firstSecondary[,"Code"]%in%finalCode))

#Go through the data and assign the previous value of female manager as the current value
i=0
for (na in firstSecondary[,name]){
  i=i+1
  if (is.na(na)){
    firstSecondary[i,name] <<- variableData [variableData [,"Code"]==firstSecondary[i,"Code"] & variableData[,"Year"] <= 2012,name][1]
  }
}

#Go through the data and assign the next value of female manager as the current value
#Lets try next values assignment
i=0
for (na in firstSecondary[,name]){
  i=i+1
  if (is.na(na)){
    firstSecondary[i,name] <<- variableData [variableData [,"Code"]==firstSecondary[i,"Code"] & variableData [,"Year"] >= 2012,name][1]
  }
}

# test if we could get rid of the missing values by testing the percentage of common country code 
#with our primary data
print(sum(firstSecondary[!is.na(firstSecondary[,name]),"Code"]%in%finalCode))

}

for (j in 1:3) {
  clean(directories[j], column_names[j])
}

#As you can see "Female_senior_and_middle_management_position" gives unsable amout of values
#thus we get rid of that variables
firstSecondary <- firstSecondary[,-c(6)]

#check
names(firstSecondary)

#Lets remove all lines with missing values
firstSecondary <- na.omit(firstSecondary)

sum(firstSecondary[!is.na(firstSecondary[,name]),"Code"]%in%finalCode)
nrow(firstSecondary)
names(firstSecondary)
head(firstSecondary)

#filter to have only the country in common with the primary data
firstSecondary <- firstSecondary[firstSecondary[!is.na(firstSecondary[,name]),"Code"]%in%finalCode,]


###EDA final first secondary data ###
for (name in c("youth_literacy_female", "Women_in_parlements", "Firms_with_female_top_manager")){
  hist(firstSecondary[,name], main = name, xlab = name)
}

for (name in c("youth_literacy_female", "Women_in_parlements", "Firms_with_female_top_manager")){
  boxplot(firstSecondary[,name], main = name, xlab = name)
}

write.csv(firstSecondary,"/Users/kevinmouofo/repos/CSP571/DataSets/secondary1/finalFirstSecondaryData.csv",
          row.names = FALSE)
  
