setwd("C:\\Users\\venks\\Documents\\venky_DS\\multi linear regression")
getwd()

#1.toyota corolla car problem - solution
Corolla<-read.csv ("ToyotaCorolla.csv")
Corolla<-Corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]

 
 attach(Corolla)
summary(Corolla) # Explore the data
 
#qqnorm(Corolla)
#qqline(Corolla)

pairs(Corolla)   # Scatter plot for all pairs of variables
cor(Corolla) # correlation matrix

# The Linear Model of interest

model.car = lm(Price ~ . , data = Corolla)
# lm(Y ~ X)
summary(model.car)



###  Correlation matrix
#install.packages("corpcor")
library(corpcor)
cor(Corolla)


# Diagnostic Plots
install.packages(car)
library(car)
plot(model.car)# Residual Plots, QQ-Plos, Std. Residuals vs Fitted, Cook's distance
qqPlot(model.car, id.n=5) # QQ plots of studentized residuals, helps identify outliers

# Deletion Diagnostics for identifying influential variable
influence.measures(model.car)
influenceIndexPlot(model.car, id.n=3) # Index Plots of the influence measures
influencePlot(model.car, id.n=3) # A user friendly representation of the above

## Regression after deleting the 81th observation
model.car1<-lm(Price ~ ., data=Corolla[-81,])
summary(model.car1)


# ### Variance Inflation Factors
vif(model.car)  # VIF is > 10 => collinearity


library("MASS")
stepAIC(model.car) # backward
#doors, cc - not relavant data per AIC

plot(model.car)


model.final <- lm(Price~Age_08_04+KM+HP+Gears+Quarterly_Tax+Weight, data=Corolla)
summary(model.final)

#delete 81st (222nd, 961th )observation
model.final1 <- lm(Price~Age_08_04+KM+HP+Gears+Quarterly_Tax+Weight, data=Corolla[-81,])
summary(model.final1)


#par("mar") #I added to view avplots - margin
#par(mar=c(1,1,1,1)) #set margin

library(car)
avPlots(model.final1,id.n=2, id.cex=0.8, col="red")

vif(model.final1)


############################################

startups_50 <- read.csv ("50_Startups.csv")

View(startups_50)
attach (startups_50)
summary(startups_50)
pairs(startups_50)

model.startup = lm(Profit ~ ., data = startups_50)
summary (startups_50)

# install.packages(car)
# library(car)
plot(model.startups_50)# Residual Plots, QQ-Plos, Std. Residuals vs Fitted, Cook's distance
qqPlot(model.startups_50, id.n=2) 

library("MASS")
stepAIC(startups_50)

#read csv & find summary 
startups_50 <- read.csv ("50_Startups.csv")
startups_50<-startups_50[c("R.D.Spend", "Administration", "Marketing.Spend", "State", "Profit")]
View(startups_50)
attach (startups_50)
summary(startups_50)

#initial model 
model.startups_50 = lm (Profit ~ ., data = startups_50)
summary(model.startups_50)
# #results of initial model
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      5.013e+04  6.885e+03   7.281 4.44e-09 ***
#   R.D.Spend        8.060e-01  4.641e-02  17.369  < 2e-16 ***
#   Administration  -2.700e-02  5.223e-02  -0.517    0.608    
# Marketing.Spend  2.698e-02  1.714e-02   1.574    0.123    
# StateFlorida     1.988e+02  3.371e+03   0.059    0.953    
# StateNew York   -4.189e+01  3.256e+03  -0.013    0.990    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 9439 on 44 degrees of freedom
# Multiple R-squared:  0.9508,	Adjusted R-squared:  0.9452 
# F-statistic: 169.9 on 5 and 44 DF,  p-value: < 2.2e-16


#plot initial model
plot(model.startups_50)

#find noise / disturbance in data
influence.measures(model.startups_50)
influenceIndexPlot(model.startups_50, id.n=5 )
influencePlot(model.startups_50, id.n=5)
#by above method, 50,49,47, 46th data row are outliers

#now we build data model by removing outliers
#regression after deleting outliers

model.startups_50_A = lm (Profit ~ ., data = startups_50[-c(50,49,47,46),])
summary (model.startups_50_A)

# #results
# # Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      5.726e+04  5.944e+03   9.634  5.6e-12 ***
#   R.D.Spend        7.730e-01  4.077e-02  18.962  < 2e-16 ***
#   Administration  -5.204e-02  4.395e-02  -1.184   0.2435    
# Marketing.Spend  2.831e-02  1.481e-02   1.911   0.0631 .  
# StateFlorida    -9.124e+02  2.649e+03  -0.344   0.7323    
# StateNew York   -1.745e+03  2.644e+03  -0.660   0.5131    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 7225 on 40 degrees of freedom
# Multiple R-squared:  0.963,	Adjusted R-squared:  0.9584 
# F-statistic: 208.2 on 5 and 40 DF,  p-value: < 2.2e-16

#stepAIC

library("MASS")
stepAIC(model.startups_50)#initial model
stepAIC(model.startups_50_A) #initial model with out outliers
# 
# Start:  AIC=920.87
# Profit ~ R.D.Spend + Administration + Marketing.Spend + State
# 
# Df  Sum of Sq        RSS     AIC
# - State            2 5.1666e+05 3.9209e+09  916.88
# - Administration   1 2.3816e+07 3.9442e+09  919.17
# <none>                          3.9203e+09  920.87
# - Marketing.Spend  1 2.2071e+08 4.1410e+09  921.61
# - R.D.Spend        1 2.6878e+10 3.0799e+10 1021.94
# 
# Step:  AIC=916.88
# Profit ~ R.D.Spend + Administration + Marketing.Spend
# 
# Df  Sum of Sq        RSS     AIC
# - Administration   1 2.3539e+07 3.9444e+09  915.18
# <none>                          3.9209e+09  916.88
# - Marketing.Spend  1 2.3349e+08 4.1543e+09  917.77
# - R.D.Spend        1 2.7147e+10 3.1068e+10 1018.37
# 
# Step:  AIC=915.18
# Profit ~ R.D.Spend + Marketing.Spend
# 
# Df  Sum of Sq        RSS     AIC
# <none>                          3.9444e+09  915.18
# - Marketing.Spend  1 3.1165e+08 4.2560e+09  916.98
# - R.D.Spend        1 3.1149e+10 3.5094e+10 1022.46
# 
# Call:
#   lm(formula = Profit ~ R.D.Spend + Marketing.Spend, data = startups_50)
# 
# Coefficients:
#   (Intercept)        R.D.Spend  Marketing.Spend  
# 4.698e+04        7.966e-01        2.991e-02  
# 
#############################################################################
# > stepAIC(model.startups_50_A)
# Start:  AIC=884.88
# Profit ~ R.D.Spend + Administration + Marketing.Spend + State
# 
# Df  Sum of Sq        RSS    AIC
# - State            2 3.4413e+07 2.7055e+09 881.51
# - Administration   1 1.5829e+07 2.6869e+09 883.17
# <none>                          2.6711e+09 884.88
# - Marketing.Spend  1 2.0209e+08 2.8731e+09 886.46
# - R.D.Spend        1 2.4986e+10 2.7657e+10 997.42
# 
# Step:  AIC=881.51
# Profit ~ R.D.Spend + Administration + Marketing.Spend
# 
# Df  Sum of Sq        RSS    AIC
# - Administration   1 1.6125e+07 2.7216e+09 879.80
# <none>                          2.7055e+09 881.51
# - Marketing.Spend  1 2.0033e+08 2.9058e+09 883.01
# - R.D.Spend        1 2.5191e+10 2.7896e+10 993.84
# 
# Step:  AIC=879.8
# Profit ~ R.D.Spend + Marketing.Spend
# 
# Df  Sum of Sq        RSS    AIC
# <none>                          2.7216e+09 879.80
# - Marketing.Spend  1 2.6199e+08 2.9836e+09 882.30
# - R.D.Spend        1 2.8990e+10 3.1712e+10 998.12
# 
# Call:
#   lm(formula = Profit ~ R.D.Spend + Marketing.Spend, data = startups_50[-50, 
#                                                                         -49, ])
# 
# Coefficients:
#   (Intercept)        R.D.Spend  Marketing.Spend  
# 4.979e+04        7.754e-01        2.745e-02 

# removing outliers & 2 parameters - state , Admin---> model.startups_50_B is FINAL model
model.startups_50_B = lm (Profit ~ R.D.Spend + Marketing.Spend, data = startups_50[-c(50,49,47,46),])
summary (model.startups_50_B)

avPlots(model.startups_50_B, id.n=2, id.cex=0.8, col="red")

vif(model.startups_50_B)

# #Results for final model
# # Residuals:
# Min       1Q   Median       3Q      Max 
# -16713.8  -4219.8   -529.1   4383.3  12081.1 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     5.029e+04  2.425e+03  20.743   <2e-16 ***
#   R.D.Spend       7.507e-01  3.660e-02  20.511   <2e-16 ***
#   Marketing.Spend 3.501e-02  1.332e-02   2.627   0.0119 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 7137 on 43 degrees of freedom
# Multiple R-squared:  0.9612,	Adjusted R-squared:  0.9594 
# F-statistic: 532.5 on 2 and 43 DF,  p-value: < 2.2e-16
# 
# > avPlots(model.startups_50_B, id.n=2, id.cex=0.8, col="red")
# There were 28 warnings (use warnings() to see them)
# > vif(model.startups_50_B)
# R.D.Spend Marketing.Spend 
# 2.106045        2.106045


####################################################################

#3. problem_Statement(computer_data)#######

setwd("C:\\Users\\venks\\Documents\\venky_DS\\multi linear regression")
getwd()

Compdata <- read.csv("Computer_Data.csv")
#View(Compdata)
Compdata <- Compdata [c("price","speed","hd","ram","screen","cd","multi","premium",	"ads",	"trend")]

attach(Compdata)
summary(Compdata)

pairs(Compdata)
cor(Compdata)
str(Compdata)
Compdata$cd = as.numeric(Compdata$cd)
Compdata$multi = as.numeric(Compdata$multi)
Compdata$premium = as.numeric(Compdata$premium)
str(Compdata)

library(corpcor)
cor(Compdata)
#initial model

model.Compdata <- lm(price ~ ., data=Compdata)
summary(model.Compdata)

#initial model is R-squared:  0.7756,	Adjusted R-squared:  0.7752 
#need to check for disturbances in data

plot(model.Compdata)
qqplot(model.Compdata)
install.packages(car)
library(car)
influence.measures(model.Compdata)
#influenceIndexPlot(model.Compdata, id.n=3)
influencePlot(model.Compdata, id.n=3)

