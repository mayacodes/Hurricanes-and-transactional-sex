#---------------------------------------------
#         CODE 1: DATA PREP
#---------------------------------------------
# title: "Hurricanes and transactional sex study"
# author: "Maya Luetke"
# date: "April 1, 2019"
# last update: "December 17, 2019"
#---------------------------------------------



#---------------------------------------------
#         Clean environment
#---------------------------------------------
#rm(list=ls())
#---------------------------------------------



#---------------------------------------------
#         Set working directory
#---------------------------------------------
getwd()
setwd('C:\\users\\luetke\\Box Sync\\Research\\Haiti Hurricane Dec 2018\\Transact Sex, Poverty, & Hurricanes\\Data') #For school computer
setwd("/Users/mayaluetke/Box Sync/Research/Haiti Hurricane Dec 2018/Transact Sex, Poverty, & Hurricanes/Data") #For desktop computer
setwd("/Users/maya/Desktop/Box Sync/Research/Haiti Hurricane Dec 2018/Transact Sex, Poverty, & Hurricanes/Data") #For laptop computer
getwd()
#---------------------------------------------



#---------------------------------------------
#         Import data
#---------------------------------------------
mydata <- read.csv(file="semdataset.csv", header=TRUE, sep=",")
#---------------------------------------------



#---------------------------------------------
#         Install/load packages
#---------------------------------------------
#Load packages & libraries
#install.packages('xtable')
#install.packages('lavaan')
#install.packages('semPlot')
library(xtable)
library(lavaan)     # Load the packages
library(semPlot)
#---------------------------------------------



#---------------------------------------------
#         Look at data 
#---------------------------------------------
str(mydata)
names(mydata)
#---------------------------------------------



#---------------------------------------------
#         Recode: house damaged
#---------------------------------------------
#house damaged
table(mydata$natural_disaster5)
mydata$house_damaged[mydata$natural_disaster5==1]<- 1 #yes
mydata$house_damaged[mydata$natural_disaster5==2]<- 0 #no
table(mydata$house_damaged)
#---------------------------------------------



#---------------------------------------------
#         Recode: stuff damaged
#---------------------------------------------
#stuff damaged
table(mydata$natural_disaster8)
mydata$stuff_damaged[mydata$natural_disaster8==1]<- 1 #yes
mydata$stuff_damaged[mydata$natural_disaster8==2]<- 0 #no
table(mydata$stuff_damaged)
#---------------------------------------------



#---------------------------------------------
#         Recode: income
#---------------------------------------------
#income
hist(mydata$received_income, exclude=mydata$received_income<20000)
mydata$income<- ifelse(mydata$received_income>0, mydata$received_income, NA)
#mydata$income<- ifelse(mydata$received_income<15000, mydata$received_income, NA)
hist(mydata$income)
#log transformation of non-normal variables
mydata$log_income <- log(mydata$income)
#check recoding
hist(mydata$log_income)
var(mydata$log_income, use = "complete.obs")
table(mydata$income)
#---------------------------------------------



#---------------------------------------------
#         Recode: current assets
#---------------------------------------------
#household assets
#mydata$acqasset #when bought
mydata$currasset #current value--use this one

#mydata$log_assets_then <- log(mydata$acqasset)
mydata$log_assets_now <- log(mydata$currasset)

hist(mydata$currasset)
mydata$asset_desp <- -(log(mydata$currasset))
hist(mydata$asset_desp)

hist(mydata$log_assets_now)

mydata$neg_log_assets <- -mydata$log_assets_now
hist(mydata$neg_log_assets)
#---------------------------------------------



#---------------------------------------------
#         Recode: wind speed
#---------------------------------------------
#wind speed
table(mydata$speed_mph)
mydata$wind_speed[mydata$speed_mph==90]<- 1 #yes
mydata$wind_speed[mydata$speed_mph==100]<- 1 #yes
mydata$wind_speed[mydata$speed_mph==80]<- 0 #no
mydata$wind_speed[mydata$speed_mph==70]<- 0 #no
table(mydata$wind_speed)
#---------------------------------------------



#---------------------------------------------
#         Recode: no meat in diet
#---------------------------------------------
#meat in diet
table(mydata$drink_eat8)
mydata$eat_meat[mydata$drink_eat8==1]<- 1 #yes
mydata$eat_meat[mydata$drink_eat8==2]<- 0 #no
mydata$eat_meat[mydata$drink_eat8==99]<- NA #NA
mydata$eat_meat[mydata$drink_eat8==98]<- NA #NA
table(mydata$eat_meat)

#NO meat in diet
table(mydata$drink_eat8)
mydata$no_meat[mydata$drink_eat8==1]<- 0 #meat
mydata$no_meat[mydata$drink_eat8==2]<- 1 #no meat
mydata$no_meat[mydata$drink_eat8==99]<- NA #NA
mydata$no_meat[mydata$drink_eat8==98]<- NA #NA
table(mydata$no_meat)
#---------------------------------------------



#---------------------------------------------
#         Recode: transactional sex
#---------------------------------------------
#REcode trans
table(mydata$trans, exclude=FALSE)
mydata$trans[mydata$sex_partner>0 & mydata$trans==1] <- 1
mydata$trans[mydata$sex_partner>0 & mydata$trans==0] <- 0                
table(mydata$trans, exclude=FALSE)

mydata$trans2[(mydata$money_sex==1 | mydata$grocery_clothes_sex==1) & mydata$sex_partner>0 & mydata$gifts_money_sex==1] <- 1
mydata$trans2[(mydata$money_sex==1 | mydata$grocery_clothes_sex==1) & mydata$sex_partner>0 & mydata$gifts_money_sex==2] <- 0
mydata$trans2[mydata$money_sex==2 & mydata$grocery_clothes_sex==2 & mydata$sex_partner>0] <- 0
table(mydata$trans2)
#---------------------------------------------



#---------------------------------------------
#     Double=-check coding of transactional sex 
#---------------------------------------------
#transactional sex: money_sex + grocery_clothes_sex + gifts_money_sex;
table(mydata$trans3)
mydata$trans3[(mydata$money_sex==1 | mydata$grocery_clothes_sex==1) & mydata$sex_partner>0 & mydata$gifts_money_sex==1] <- 1
table(mydata$trans3)
mydata$trans3[(mydata$money_sex==1 | mydata$grocery_clothes_sex==1) & mydata$sex_partner>0 & mydata$gifts_money_sex==2] <- 0
table(mydata$trans3)
mydata$trans3[mydata$money_sex==2 & mydata$grocery_clothes_sex==2 & mydata$sex_partner>0] <- 0
table(mydata$trans3, exclude=FALSE)
mydata$trans3[mydata$sex_partner==0] <- NA
table(mydata$trans3, exclude=FALSE)
mydata$trans3[((mydata$money_sex==98|mydata$money_sex==99) | (mydata$grocery_clothes_sex==98|mydata$grocery_clothes_sex==99)) & (mydata$gifts_money_sex==98|mydata$gifts_money_sex==99)] <- NA
table(mydata$trans3, exclude=FALSE)
#mydata$trans3[mydata$sex_partner>0 & (mydata$money_sex!=1 & mydata$grocery_clothes_sex!=1) & (mydata$gifts_money_sex!=1)] <- 98
#---------------------------------------------



#---------------------------------------------
#         Check time bounds of transactional sex measure
#---------------------------------------------
CrossTable(mydata$end_of_relationship,mydata$trans)
names(mydata)
print(mydata$end_of_relationship)
table(mydata$end_of_relationship)
# use as.Date( ) to convert strings to dates
mydata <- subset(mydata,end_of_relationship!="2004-06-15" & end_of_relationship!="2009-05-11")
#---------------------------------------------



#---------------------------------------------
#         Recode: distance from hurricane
#---------------------------------------------
#dist_miles
mydata$dist_100 <- mydata$Dist_miles/100
mydata$dist_100

mydata$neg_dist <- -mydata$Dist_miles
hist(mydata$neg_dist)
#---------------------------------------------



#---------------------------------------------
#         Recode: household size
#---------------------------------------------
table(mydata$hhsize)
mydata$hhsize <- mydata$hhsize+1
table(mydata$hhsize)
#---------------------------------------------



#---------------------------------------------
#         Basic characteristics of study pop, stratified by exposure
#---------------------------------------------
library(gmodels)
CrossTable(mydata$no_meat, mydata$injrkill, digits=round(3))
chisq.test(mydata$no_meat, mydata$injrkill)
table(mydata$no_meat, mydata$injrkill, exclude=FALSE)

CrossTable(mydata$income_destroy, mydata$injrkill, digits=round(3))
chisq.test(mydata$income_destroy, mydata$injrkill)
table(mydata$income_destroy, mydata$injrkill, exclude=FALSE)

CrossTable(mydata$food_insecure, mydata$injrkill, digits=round(3))
chisq.test(mydata$food_insecure, mydata$injrkill)
table(mydata$food_insecure, mydata$injrkill, exclude=FALSE)

CrossTable(mydata$trans, mydata$injrkill, digits=round(3))
chisq.test(mydata$trans, mydata$injrkill)
table(mydata$trans, mydata$injrkill, exclude=FALSE)
table(mydata$sex_partner, exclude=FALSE)
table(mydata$trans, exclude=FALSE)
#---------------------------------------------
