#---------------------------------------------
#         CODE 2: SEM MODELS
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
#install.packages('xtable')
#install.packages('lavaan')
#install.packages('semPlot')
#install.packages('MASS')
#install.packages('sjPlot')
library(xtable)
library(lavaan)     # Load the packages
library(semPlot)
library(MASS)
library(sjPlot)
#---------------------------------------------



#---------------------------------------------
#         Check that we have all model variables
#---------------------------------------------
table(mydata$Dist_miles)
table(mydata$income_destroy)
table(mydata$trans)
table(mydata$hhsize)
table(mydata$food_insecure)
table(mydata$no_meat)
#---------------------------------------------



#---------------------------------------------
#         Ordering variables
#---------------------------------------------
#Specifying the orderedness of binary variables
mydata[,c("trans",
          "income_destroy",
          "injrkill",
          "food_insecure",
          "stuff_damaged",
          "house_damaged",
          "wind_speed",
          "eat_meat",
          "no_meat"
)] <-
  lapply(mydata[,c("trans",
                   "income_destroy",
                   "injrkill",
                   "food_insecure",
                   "stuff_damaged",
                   "house_damaged",
                   "wind_speed",
                   "eat_meat",
                   "no_meat")], ordered)
#---------------------------------------------



#---------------------------------------------
#         Remove missing data
#---------------------------------------------
#data<- mydata[c('trans', 'income_destroy', 'injrkill', 'food_insecure', 'Dist_miles', 'stuff_damaged', 'house_damaged', 'log_income', 'log_assets_now')]
data<- mydata[c('trans', 'income_destroy', 'injrkill', 'food_insecure', 'Dist_miles', 'stuff_damaged', 'house_damaged', 'log_income', 'received_income', 'log_assets_now', 'currasset', 'wind_speed', 'speed_mph', 'eat_meat')]
data <- data[complete.cases(data), ] # unnecessary if no missing data
#---------------------------------------------



#---------------------------------------------
#         SEM FINAL MODEL: FIGURE 1
#---------------------------------------------
# Model 5 specification (vars= trans injrkill income_destroy food_insecure)
m1 <- 'econ_stress =~ NA*food_insecure + hhsize + neg_log_assets + no_meat 

#regressions
trans2 ~ h*econ_stress + g*income_destroy + i*injrkill
econ_stress ~ e*injrkill + f*income_destroy + c*neg_dist
income_destroy ~ a*neg_dist + d*injrkill
injrkill ~ b*neg_dist

econ_stress~~1*econ_stress

ind1:= d*g
ind2:= d*f*h
ind3:= e*h'

fit_m1 <- cfa(m1, data = mydata, std.lv=TRUE, ordered=c("trans2",
                                                        "income_destroy",
                                                        "injrkill",
                                                        "food_insecure",
                                                        "no_meat"))
summary(fit_m1, rsq=TRUE, standardized=TRUE, fit.measures=TRUE)
#---
#create a table with all parameter estimates
table5 <- parameterEstimates(fit_m1) 
library(sjPlot)
tab_df(table5)
#---
#print a plot of SEM model # rotation = 2 puts exogenous at left side
semPaths(fit_m1, style = "lisrel", rotation = 3) 
#---
#bootstrap the se for the indirect pathways
fit_ind1 <- cfa(m1, data= mydata, ordered=c("trans2",
                                            "income_destroy",
                                            "injrkill",
                                            "food_insecure",
                                            "eat_meat"), se= 'bootstrap', test="scaled.shifted", 
                estimator="DWLS", verbose=TRUE)
summary(fit_ind1, rsq=TRUE, standardized=TRUE, fit.measures=TRUE)
table5ind <- parameterEstimates(fit_ind1) 
#---
#check residuals
residuals(fit_m1, type = "cor")$cor
cor_table <- residuals(fit_m1, type = "cor")$cor

cor_table[upper.tri(cor_table)] <- NA # erase the upper triangle
diag(cor_table) <- NA # erase the diagonal 0's
#---------------------------------------------



#---------------------------------------------
#         TESTING: CONFIRMATORY FACTOR ANALYSIS
#---------------------------------------------
#confirmatory factor analysis 1
cfa.model1 <- 'hurr_impact =~ NA*Dist_miles + stuff_damaged + house_damaged + wind_speed

hurr_impact~~1*hurr_impact

econ_stress =~ NA*food_insecure + hhsize + neg_log_assets + no_meat 

econ_stress~~1*econ_stress'

fit.cfa.model1 <- cfa(cfa.model1, data = mydata, ordered=c("food_insecure",
                                                           "no_meat",
                                                           "stuff_damaged",
                                                           "house_damaged",
                                                           "wind_speed"))
summary(fit.cfa.model1, fit.measures=TRUE, standardized=TRUE)
fitted(fit.cfa.model1)$cov
corr <- cov2cor(fitted(fit.cfa.model1)$cov)
corr

fitMeasures(fit.cfa.model1, "cfi")
#---------------------------------------------



#---------------------------------------------
#         TESTING: CONFIRMATORY FACTOR ANALYSIS with just economic factors
#---------------------------------------------
#confirmatory factor analysis 1

cfa.model2 <- 'econ_stress =~ NA*food_insecure + hhsize + neg_log_assets + no_meat 
econ_stress~~1*econ_stress'

fit_ind2 <- cfa(cfa.model2, data= mydata, ordered=c("food_insecure","no_meat"), se= 'bootstrap', test="scaled.shifted", 
                estimator="DWLS", verbose=TRUE)
summary(fit_ind2, rsq=TRUE, standardized=TRUE, fit.measures=TRUE)


fitted(fit.cfa.model1)$cov
corr <- cov2cor(fitted(fit.cfa.model1)$cov)
corr

fitMeasures(fit.cfa.model1, "cfi")
#---------------------------------------------