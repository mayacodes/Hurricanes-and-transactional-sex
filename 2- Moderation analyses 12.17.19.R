#---------------------------------------------
#         CODE 2: MODERATION ANALYSES
#---------------------------------------------
# title: "Hurricanes and transactional sex study"
# author: "Maya Luetke"
# date: "April 1, 2019"
# last update: "December 17, 2019"
#---------------------------------------------



#---------------------------------------------
#         Install/load packages
#---------------------------------------------
#Load packages & libraries
#install.packages('logbin')
library(logbin)
#---------------------------------------------



#---------------------------------------------
#         Moderation 1: Food insecurity
#---------------------------------------------
#Test to see if there is significant moderation--Wald p-value for interation (int)
mydata$int<-mydata$injrkill*mydata$food_insecure
mod.1 <- logbin(trans ~ injrkill + food_insecure + int, data=mydata)
summary(mod.1)
#Make data subsets in order to run later analyses
food_insecure_1 <- subset(mydata, food_insecure==1)
food_insecure_0 <- subset(mydata, food_insecure==0)
#exp(confint(fit.logbin.food_insecure_1))
mod.food_insecure_1 <- logbin(trans ~ injrkill, data=food_insecure_1) 
mod.food_insecure_1.output <- cbind(round(exp(mod.food_insecure_1$coefficients),2), round(exp(confint(mod.food_insecure_1)),2), round(summary(mod.food_insecure_1)$coefficients[,4],6))
colnames(mod.food_insecure_1.output) <- c("PR", "Lower CI", "Upper CI", "P-value")
row.names(mod.food_insecure_1.output) <-c("Intercept","Food insecure=1")
#mod.food_insecure_1.output
#Among food insecure (food_insecure=0)
mod.food_insecure_0 <- logbin(trans ~ injrkill, data=food_insecure_0) 
mod.food_insecure_0.output <- cbind(round(exp(mod.food_insecure_0$coefficients),2), round(exp(confint(mod.food_insecure_0)),2) ,round(summary(mod.food_insecure_0)$coefficients[,4],6))
colnames(mod.food_insecure_0.output) <- c("PR", "Lower CI", "Upper CI", "P-value")
row.names(mod.food_insecure_0.output) <-c("Intercept","Food insecure=0")
#mod.food_insecure_0.output
#print results
mod.food.insecure <- rbind(mod.food_insecure_0.output[2,],mod.food_insecure_1.output[2,])
row.names(mod.food.insecure) <-c("Food insecure=0", "Food insecure=1") 
mod.food.insecure
#remove datasets
remove(food_insecure_1,food_insecure_0)
#---------------------------------------------




#---------------------------------------------
#         Moderation 2: Poverty/assets
#---------------------------------------------
#Test to see if there is significant moderation--Wald p-value for interation (int)
mydata$int<-mydata$injrkill*mydata$assets01
mod.2 <- logbin(trans ~ injrkill + assets01 + int, data=mydata)
summary(mod.2)
#Make data subsets in order to run later analyses
assets_1 <- subset(mydata, assets01==1)
assets_0 <- subset(mydata, assets01==0)
#exp(confint(fit.logbin.food_insecure_1))
mod.assets_1 <- logbin(trans ~ injrkill, data=assets_1) 
mod.assets_1.output <- cbind(round(exp(mod.assets_1$coefficients),2), round(exp(confint(mod.assets_1)),2), round(summary(mod.assets_1)$coefficients[,4],6))
colnames(mod.assets_1.output) <- c("PR", "Lower CI", "Upper CI", "P-value")
row.names(mod.assets_1.output) <-c("Intercept","Food insecure=1")
#mod.assets_1.output
#Among food insecure (food_insecure=0)
mod.assets_0 <- logbin(trans ~ injrkill, data=assets_0) 
mod.assets_0.output <- cbind(round(exp(mod.assets_0$coefficients),2), round(exp(confint(mod.assets_0)),2) ,round(summary(mod.assets_0)$coefficients[,4],6))
colnames(mod.assets_0.output) <- c("PR", "Lower CI", "Upper CI", "P-value")
row.names(mod.assets_0.output) <-c("Intercept","Food insecure=0")
#mod.assets_0.output
#print results
mod.assets01 <- rbind(mod.assets_0.output[2,],mod.assets_1.output[2,])
row.names(mod.assets01) <-c("Assets01=0", "Assets01=1") 
mod.assets01
#remove datasets
remove(assets_1,assets_0)
#---------------------------------------------



#---------------------------------------------
#         Moderation 3: Loss of income generating resources
#---------------------------------------------
#Test to see if there is significant moderation--Wald p-value for interation (int)
mydata$int<-mydata$injrkill*mydata$income_destroy
mod.3 <- logbin(trans ~ injrkill + income_destroy + int, data=mydata)
summary(mod.3)
#Make data subsets in order to run later analyses
income_destroy_1 <- subset(mydata, income_destroy==1)
income_destroy_0 <- subset(mydata, income_destroy==0)
#exp(confint(fit.logbin.food_insecure_1))
mod.incomedest_1 <- logbin(trans ~ injrkill, data=income_destroy_1) 
mod.incomedest_1.output <- cbind(round(exp(mod.incomedest_1$coefficients),2), round(exp(confint(mod.incomedest_1)),2), round(summary(mod.incomedest_1)$coefficients[,4],6))
colnames(mod.incomedest_1.output) <- c("PR", "Lower CI", "Upper CI", "P-value")
row.names(mod.incomedest_1.output) <-c("Intercept","Income_destroy=1")
#mod.incomedest_1.output
#Among food insecure (food_insecure=0)
mod.incomedest_0 <- logbin(trans ~ injrkill, data=income_destroy_0) 
mod.incomedest_0.output <- cbind(round(exp(mod.incomedest_0$coefficients),2), round(exp(confint(mod.incomedest_0)),2) ,round(summary(mod.incomedest_0)$coefficients[,4],6))
colnames(mod.incomedest_0.output) <- c("PR", "Lower CI", "Upper CI", "P-value")
row.names(mod.incomedest_0.output) <-c("Intercept","Income_destroy=0")
#mod.incomedest_0.output
#print results
mod.incomedest01 <- rbind(mod.incomedest_0.output[2,],mod.incomedest_1.output[2,])
row.names(mod.incomedest01) <-c("Income_destroy=0", "Income_destroy=1") 
mod.incomedest01
#remove datasets
remove(income_destroy_1,income_destroy_0)
#---------------------------------------------
