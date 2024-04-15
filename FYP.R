library(pscl)
library(tidyverse)
library(performance)
library(OpenML)
library(farff)
library(zoo)
library(sp)
library(xts)
library(devtools)
library(datasets)
library(AER)
library(statmod)
library(tweedie)
library(GGally)
library(keras)
library(reticulate)
library(tensorflow)

initdata = read.csv("C://Users//tadhg//OneDrive//Desktop//4th Year//Final Year Project//Claims.csv", header = TRUE, stringsAsFactors =T)





#Initial Data Manipulation

Clean_data <- initdata
Clean_data <- Clean_data[Clean_data$Exposure <= 1,]
Clean_data <- Clean_data[Clean_data$ClaimNb <= 5,]
##Some Investigation 
scatter.smooth(Clean_data$VehPower, Clean_data$ClaimAmount)
boxplot(Clean_data$ClaimNb ~ Clean_data$VehPower, data = Clean_data)
boxplot(Clean_data$ClaimNb ~ Clean_data$VehGas, data = Clean_data)
boxplot(Clean_data$ClaimNb ~ Clean_data$Area, data = Clean_data)
boxplot(Clean_data$ClaimNb ~ Clean_data$Employment, data = Clean_data)
boxplot(Clean_data$ClaimNb ~ Clean_data$VehBrand, data = Clean_data)
ggplot(data = data.frame(Clean_data$VehBrand, Clean_data$ClaimNb), aes(x = Clean_data$VehBrand, y = Clean_data$ClaimNb)) +
  geom_violin() +
  labs(title = "Violin Plot of Claim Number by Vehicle Brand")
library(ggplot2)
average_claim_severity <- aggregate(Clean_data$ClaimNb ~ Clean_data$VehBrand, data = Clean_data, FUN = mean)

#Investigating Correlation between data

unclassed_data <- Clean_data 
unclassed_data$IDpol <- NULL

unclass(unclassed_data$Area)
unclassed_data$Area <- as.numeric(unclassed_data$Area)
unclass(unclassed_data$VehGas)
unclassed_data$VehGas <- as.numeric(unclassed_data$VehGas)
unclass(unclassed_data$Gender)
unclassed_data$Gender <- as.numeric(unclassed_data$Gender)
unclass(unclassed_data$Transmission)
unclassed_data$Transmission <- as.numeric(unclassed_data$Transmission)
unclass(unclassed_data$Employment)
unclassed_data$Employment <- as.numeric(unclassed_data$Employment)
unclass(unclassed_data$Region)
unclassed_data$Region <- as.numeric(unclassed_data$Region)
unclass(unclassed_data$VehBrand)
unclassed_data$VehBrand <- as.numeric(unclassed_data$VehBrand)







# Print the results
print(average_claim_severity)
average_claim_quantiles <- aggregate(Clean_data$ClaimNb ~ Clean_data$VehBrand, data = Clean_data, FUN = quantile)
print(average_claim_quantiles)
#Vehicl Brand
colors <- c("blue", "red")
barplot(average_claim_severity$`Clean_data$ClaimNb`, names.arg = average_claim_severity$`Clean_data$VehBrand`, xlab = "Vehicle Brand", ylab = "Average Number of Claims", main = "Average Number of Claims Per Vehicle Brand", col = colors)
## Vehicle Gas
average_claim_severity1 <- aggregate(Clean_data$ClaimNb ~ Clean_data$Area, data = Clean_data, FUN = mean)
# Print the results
print(average_claim_severity1)
barplot(average_claim_severity1$`Clean_data$ClaimNb`, names.arg = average_claim_severity1$`Clean_data$Area`, xlab = "Vehicle Gas", ylab = "Average Number of Claims", main = "Average Number of Claims With Respect To Vehicle Gas", col = colors)

#First Poisson GLM
first_glm <- glm(ClaimNb ~ offset(log(Exposure)) + VehPower + VehAge + factor(VehBrand) + factor(VehGas) + DrivAge + BonusMalus + factor(Area) + Density + factor(Region) + factor(Gender) + factor(Transmission) + factor(Employment) + Fines,family = poisson(link="log"), data = Clean_data)


# Load necessary library
library(ggplot2)

# Example generic lambda for the initial Poisson distribution
generic_lambda <- 3

# Simulated data for fitting Poisson GLM (assuming 'counts' is your response variable)
set.seed(123)
data <- data.frame(counts = rpois(100, lambda = generic_lambda))

# Extract estimated lambda from GLM
estimated_lambda <- exp(coef(first_glm)[1])

# Range of count values for plotting
x_values <- Clean_data$ClaimNb

# Create a data frame for plotting
plot_data <- data.frame(
  Counts = rep(x_values, 2),
  Probability = c(dpois(x_values, generic_lambda), dpois(x_values, estimated_lambda)),
  Distribution = rep(c("Generic", "Fitted via MLE"), each = length(x_values))
)

# Plotting with ggplot2
ggplot(plot_data, aes(x = Counts, y = Probability, group = Distribution, color = Distribution)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("Generic" = "blue", "Fitted via MLE" = "red")) +
  labs(title = "Comparison of Poisson Distributions Before and After MLE",
       subtitle = paste("Generic Lambda:", generic_lambda, "| Estimated Lambda:", round(estimated_lambda, 2)),
       x = "Counts",
       y = "Probability") +
  theme_minimal()
quantile(initdata$Exposure)
#Region
Clean_data2 <- Clean_data
levels(Clean_data$Region)
Clean_data2$grouped_region <- as.factor(ifelse(Clean_data$Region %in% c( 'Aquitaine', 'Auvergne', 'Bourgogne', 'Bretagne', 'Centre', 'Champagne-Ardenne','Corse', 'Franche-Comte', 'Haute-Normandie', 'Languedoc-Roussillon', 'Limousin', 'Midi-Pyrenees', 'Nord-Pas-de-Calais', 'Pays-de-la-Loire', 'Picardie', 'Poitou-Charentes', "Provence-Alpes-Cotes-D'Azure"), 'new_level', as.character(Clean_data$Region)))
grouped_region_glm <- glm(ClaimNb ~ offset(log(Exposure)) + VehPower + VehAge + factor(VehBrand) + factor(VehGas) + DrivAge + BonusMalus + factor(Area) + Density + factor(grouped_region) + factor(Gender) + factor(Transmission) + factor(Employment) + Fines,data = Clean_data2, family = poisson(link="log"))
summary(grouped_region_glm)
summary(first_glm)
no_region <- glm(ClaimNb ~ offset(log(Exposure)) + VehPower + VehAge + factor(VehBrand) + factor(VehGas) + DrivAge + BonusMalus + factor(Area) + Density + factor(Gender) + factor(Transmission) + factor(Employment) + Fines,data = Clean_data2, family = poisson(link="log"))
#Vehicle Brand

plot_average_response_categorical(data = Clean_data2, response_var = "ClaimNb", categorical_var = "VehBrand")
summary(no_region)
#Vehicle Brands B5, B14, B12, were all significant particularly B12
#Grouping of the remaining levels was investigated 
Clean_data2$grouped_vehiclebrand <- as.factor(ifelse(Clean_data$VehBrand %in% c('B1','B6', 'B4', 'B3', 'B2', 'B13', 'B11', 'B10'), 'low', as.character(Clean_data$VehBrand)))
glm_grouped_vehbrand <- glm(ClaimNb ~ offset(log(Exposure)) + VehPower + VehAge + factor(grouped_vehiclebrand) + factor(VehGas) + DrivAge + BonusMalus + factor(Area) + Density + factor(Gender) + factor(Transmission) + factor(Employment) + Fines,data = Clean_data2, family = poisson(link="log"))

summary(glm_grouped_vehbrand)
Clean_data2$grouped_vehiclebrand <- relevel(Clean_data2$grouped_vehiclebrand, ref = "low" )

#Grouping of Vehicle Age
Clean_data2$grouped_vehage = cut(Clean_data$VehAge, c(0,1,2,5,10,20,Inf), include.lowest = TRUE)
Clean_data2$capped_vehage = pmin(Clean_data$VehAge, 20)
glm_grouped_vehage<- glm(ClaimNb ~ offset(log(Exposure)) + VehPower + factor(grouped_vehage) + factor(grouped_vehiclebrand) + factor(VehGas) + DrivAge + BonusMalus + factor(Area) + Density + factor(Gender) + factor(Transmission) + factor(Employment) + Fines,data = Clean_data2, family = poisson(link="log"))
glm_capped_vehage<- glm(ClaimNb ~ offset(log(Exposure)) + VehPower + log(VehAge) + factor(grouped_vehiclebrand) + factor(VehGas) + DrivAge + BonusMalus + factor(Area) + Density + factor(Gender) + factor(Transmission) + factor(Employment) + Fines,data = Clean_data2, family = poisson(link="log"))

plot_average_response(data = Clean_data2, response_var = "ClaimNb", predictor_var = "VehAge", bin_width = 5)



#Driver Age
plot_average_response(data = Clean_data2, response_var = "ClaimNb", predictor_var = "DrivAge", bin_width = 5)
Clean_data2$grouped_drivage = cut(Clean_data$DrivAge, c(18,23,28,40,70,Inf), include.lowest = TRUE)
Clean_data2$capped_drivage = pmin(Clean_data$DrivAge, 25)
glm_capped_drivage<- glm(ClaimNb ~ offset(log(Exposure)) + VehPower + factor(grouped_vehage) + factor(grouped_vehiclebrand) + factor(VehGas) + log(DrivAge) + BonusMalus + factor(Area) + Density + factor(Gender) + factor(Transmission) + factor(Employment) + Fines,data = Clean_data2, family = poisson(link="log"))
glm_grouped_drivage<- glm(ClaimNb ~ offset(log(Exposure)) + VehPower + factor(grouped_vehage) + factor(grouped_vehiclebrand) + factor(VehGas) + factor(grouped_drivage) + BonusMalus + factor(Area) + Density + factor(Gender) + factor(Transmission) + factor(Employment) + Fines,data = Clean_data2, family = poisson(link="log"))
summary(glm_capped_drivage)

#Bonus Malus
plot_average_response(data = Clean_data2, response_var = "ClaimNb", predictor_var = "BonusMalus", bin_width = 10)
Clean_data2$grouped_bonusmalus = cut(Clean_data2$capped_bonuslmalus, c(50,60,70,80,100,Inf), include.lowest = TRUE)
glm_grouped_bonusmalus<- glm(ClaimNb ~ offset(log(Exposure)) + VehPower + factor(grouped_vehage) + factor(grouped_vehiclebrand) + factor(VehGas) + factor(grouped_drivage) + factor(grouped_bonusmalsu) + factor(Area) + Density + factor(Gender) + factor(Transmission) + factor(Employment) + Fines,data = Clean_data2, family = poisson(link="log"))
summary(glm_grouped_bonusmalus)
Clean_data2$capped_bonuslmalus = pmin(Clean_data$BonusMalus, 125)
glm_grouped_bonusmalus<- glm(ClaimNb ~ offset(log(Exposure)) + VehPower + factor(grouped_vehage) + factor(grouped_vehiclebrand) + factor(VehGas) + factor(grouped_drivage) + capped_bonuslmalus + factor(Area) + Density + factor(Gender) + factor(Transmission) + factor(Employment) + Fines,data = Clean_data2, family = poisson(link="log"))
summary(glm_grouped_bonusmalus)
table(Clean_data$BonusMalus)
glm_log_bonusmalus<- glm(ClaimNb ~ offset(log(Exposure)) + VehPower + factor(grouped_vehage) + factor(grouped_vehiclebrand) + factor(VehGas) + factor(grouped_drivage) + log(BonusMalus) + factor(Area) + Density + factor(Gender) + factor(Transmission) + factor(Employment) + Fines,data = Clean_data2, family = poisson(link="log"))
summary(glm_log_bonusmalus)

#Fines
plot_average_response(data = Clean_data2, response_var = "ClaimNb", predictor_var = "Fines", bin_width = 100)
Clean_data2$grouped_fines = cut(Clean_data$Fines, c(0,80,150,300,500,Inf), include.lowest = TRUE)
glm_grouped_fines<- glm(ClaimNb ~ offset(log(Exposure)) + VehPower + factor(grouped_vehage) + factor(grouped_vehiclebrand) + factor(VehGas) + factor(grouped_drivage) + log(BonusMalus) + factor(Area) + Density + factor(Gender) + factor(Transmission) + factor(Employment) + factor(grouped_fines),data = Clean_data2, family = poisson(link="log"))

summary(glm_grouped_fines)
Clean_data2$capped_fines = pmin(Clean_data$Fines, 200)
glm_capped_fines<- glm(ClaimNb ~ offset(log(Exposure)) + VehPower + factor(grouped_vehage) + factor(grouped_vehiclebrand) + factor(VehGas) + factor(grouped_drivage) + log(BonusMalus) + factor(Area) + Density + factor(Gender) + factor(Transmission) + factor(Employment) + log(Fines),data = Clean_data2, family = poisson(link="log"))
head(Clean_data2)

#Density
plot_average_response(data = Clean_data, response_var = "ClaimNb", predictor_var = "Density", bin_width = 1000)
table(Clean_data$Density)

Clean_data2$Density = Clean_data$Density
Clean_data2$grouped_density = cut(Clean_data$Density, c(0,1000, 4000,Inf), include.lowest = TRUE)
Clean_data2$capped_density = pmin(Clean_data$Density, 25000)
Clean_data2$capped_density = pmax(Clean_data2$capped_density, 10000)
glm_capped_density <- glm(ClaimNb ~ offset(log(Exposure)) + VehPower + factor(grouped_vehage) + factor(grouped_vehiclebrand) + factor(VehGas) + factor(grouped_drivage) + log(BonusMalus) + factor(Area) + capped_density + factor(Gender) + factor(Transmission) + factor(Employment) + factor(grouped_fines),data = Clean_data2, family = poisson(link="log"))

Clean_data3 = Clean_data2
glm_grouped_density <- glm(ClaimNb ~ offset(log(Exposure)) + VehPower + factor(grouped_vehage) + factor(grouped_vehiclebrand) + factor(VehGas) + factor(grouped_drivage) + log(BonusMalus) + factor(Area) + log(Density) + factor(Gender) + factor(Transmission) + factor(Employment) + factor(grouped_fines),data = Clean_data2, family = poisson(link="log"))
summary(glm_grouped_density)
##Grouping of Vehicle Power
glm_final <- glm(ClaimNb ~ offset(log(Exposure)) + capped_vehpower + factor(grouped_vehage) + factor(grouped_vehiclebrand) + factor(VehGas) + factor(grouped_drivage) + log(BonusMalus) + log(Density) + factor(grouped_fines),data = Clean_data3, family = poisson(link="log"))
summary(glm_final)


coefs <- summary(glm_final)$coefficients
head(summary(glm_final)$coefficients)

# Convert to data frame, exclude the intercept
importance_df <- data.frame(
  Variable = rownames(coefs),  # Exclude the intercept by removing the first element
  Coefficient = select(coefs, -c)      # Exclude the intercept coefficient
)
library(dplyr)
head(coefs)
new = coefs[-1]
view(coefs)
view(new)
rownames(coefs[-1,])
# Optionally, sort by absolute value of coefficients for better visualization
importance_df <- importance_df[order(-abs(importance_df$Coefficient.Estimate)), ]

view(importance_df)

importance_df <- data.frame(
  Variable = rownames(coefs),
  Coefficient = coefs[, "Estimate"],
  StdError = coefs[, "Std. Error"],
  zValue = coefs[, "Estimate"] / coefs[, "Std. Error"],
  PValue = coefs[, "Pr(>|z|)"]
)

importance_df = importance_df[-c(1),]
view(importance_df)
importance_df <- importance_df[order(abs(importance_df$zValue), decreasing = TRUE),]  # Sort by importance

library(ggplot2)
ggplot(importance_df, aes(x = reorder(Variable, -abs(zValue)), y = zValue, fill = zValue)) +
  geom_col() +
  coord_flip() +  # Flip the coordinates to make labels readable
  labs(title = "Variable Importance", x = "Variable", y = "Standardized Coefficient (z-value)") +
  theme_minimal() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, 
                       limit = c(min(importance_df$zValue), max(importance_df$zValue)), 
                       name = "Z-Value")  # Color scale to enhance visualization





summary(glm_final)
Clean_data3$capped_vehpower = pmin(Clean_data$VehPower, 9)
glm_capped_vehpower <- glm(ClaimNb ~ offset(log(Exposure)) + capped_vehpower + factor(grouped_vehage) + factor(grouped_vehiclebrand) + factor(VehGas) + factor(grouped_drivage) + log(BonusMalus) + log(Density) + factor(grouped_fines),data = Clean_data2, family = poisson(link="log"))
glm_log_vehpower <- glm(ClaimNb ~ offset(log(Exposure)) + log(VehPower) + factor(grouped_vehage) + factor(grouped_vehiclebrand) + factor(VehGas) + factor(grouped_drivage) + log(BonusMalus) + log(Density) + factor(grouped_fines),data = Clean_data2, family = poisson(link="log"))
summary(glm_capped_vehpower)
summary(glm_log_vehpower)
plot_average_response(data = Clean_data, response_var = "ClaimNb" ,predictor_var = "VehPower", bin_width = 1)
quantile(Clean_data$VehPower, probs = seq(0,1,0.1))
step(glm_final, direction = "both", k = 2)


###########################################
#Final GLM

glm_final <- glm(ClaimNb ~ offset(log(Exposure)) + capped_vehpower + factor(grouped_vehage) + factor(grouped_vehiclebrand) + factor(VehGas) + factor(grouped_drivage) + log(BonusMalus) + log(Density) + factor(grouped_fines),data = Clean_data2, family = poisson(link="log"))
summary(glm_final)
library("performance")
glm_test <- glm(ClaimNb ~ offset(log(Exposure)) + VehPower + VehAge + factor(VehBrand) + factor(VehGas) + DrivAge + BonusMalus + factor(Region) + factor(Area), data = Clean_data, family = poisson(link = "log"))
summary(glm_test)
###########################################




#######################################################
#### MODELS
########################################################


# Quasi Poisson

glm_quasi <- glm(ClaimNb ~ offset(log(Exposure)) + capped_vehpower + factor(grouped_vehage) + factor(grouped_vehiclebrand) + factor(VehGas) + factor(grouped_drivage) + log(BonusMalus) + log(Density) + factor(grouped_fines),data = Clean_data2, family = quasipoisson(link = "log"))

summary(glm_quasi)
#Negative Binomial 
library(MASS)
glm_negbin <- glm.nb(ClaimNb ~ offset(log(Exposure)) + capped_vehpower + factor(grouped_vehage) + factor(grouped_vehiclebrand) + factor(VehGas) + factor(grouped_drivage) + log(BonusMalus) + log(Density) + factor(grouped_fines),data = Clean_data2)
summary(glm_negbin)

#Zero-Inflated Poisson
zeroinfl_poisson <- zeroinfl(ClaimNb ~ offset(log(Exposure)) + capped_vehpower + factor(grouped_vehage) + factor(grouped_vehiclebrand) + factor(VehGas) + factor(grouped_drivage) + log(BonusMalus) + log(Density) + factor(grouped_fines),data = Clean_data2)
#Zero-Inflated Negative Binomial
zeroinfl_negbin <- zeroinfl(ClaimNb ~ offset(log(Exposure)) + capped_vehpower + factor(grouped_vehage) + factor(grouped_vehiclebrand) + factor(VehGas) + factor(grouped_drivage) + log(BonusMalus) + log(Density) + factor(grouped_fines),data = Clean_data2, dist = "negbin")
# Hurdle Model
hurdle_model <- hurdle(ClaimNb ~ offset(log(Exposure)) + capped_vehpower + factor(grouped_vehage) + factor(grouped_vehiclebrand) + factor(VehGas) + factor(grouped_drivage) + log(BonusMalus) + log(Density) + factor(grouped_fines),data = Clean_data2, dist = "negbin")
summary(hurdle_model)


Clean_data3 = dplyr::select(Clean_data2, -c("VehAge", "DrivAge", "VehBrand","VehPower", "Area", "Region", "Gender", "Transmission", "Employment", "capped_vehage", "capped_drivage", "capped_bonuslmalus", "grouped_bonusmalus", "capped_fines"))
Clean_data3 = dplyr::select(Clean_data3, -c("capped_density", "grouped_density", "grouped_region", "Fines"))

head(Clean_data3)

K = 5
train_err_poisson = train_error_nb = train_error_qp = train_error_zp = train_error_znb = train_error_hurdle = numeric(K)
test_err_poisson = test_error_nb = test_error_qp = test_error_zp = test_error_znb = test_error_hurdle = numeric(K)
abs_train_err_poisson = abs_train_error_nb = abs_train_error_qp = abs_train_error_zp = abs_train_error_znb = abs_train_error_hurdle = numeric(K)
abs_test_err_poisson = abs_test_error_nb = abs_test_error_qp = abs_test_error_zp = abs_test_error_znb = abs_test_error_hurdle = numeric(K)

head(Clean_data3)
K = 5
n = nrow(Clean_data3)
dat = Clean_data3[sample(1:n,n),]
folds = cut(1:n,K,labels=FALSE)
y = Clean_data3$ClaimNb

x = Clean_data3[,c('Exposure','BonusMalus','VehGas','Density','grouped_vehiclebrand','grouped_vehage','grouped_drivage','grouped_fines','capped_vehpower')]


for(k in 1:K){
  
  i.train = which(folds != k)
  
  #Regular Poisson
  
  poisson_1 <- glm(ClaimNb ~ capped_vehpower + grouped_vehage + grouped_vehiclebrand + VehGas + grouped_drivage + log(BonusMalus) + log(Density) + grouped_fines + offset(log(Exposure)) ,data = Clean_data3, subset = i.train, family = poisson(link = "log"))
  yh.1 = poisson_1$fitted.values
  yp.1 = predict(poisson_1, newdata = x[-i.train,], type = "response")
  
  #Poisson Errors
  train_err_poisson[k] = mean((yh.1-y[i.train])^2)
  test_err_poisson[k] = mean((yp.1 - y[-i.train])^2)
  abs_train_err_poisson[k] = mean(abs(yh.1-y[i.train]))
  abs_test_err_poisson[k] = mean(abs(yp.1 - y[-i.train]))
  
  #Negative Binomial
  negbin_1 <- glm.nb(ClaimNb ~ capped_vehpower + grouped_vehage + grouped_vehiclebrand + VehGas + grouped_drivage + log(BonusMalus) + log(Density) + grouped_fines + offset(log(Exposure)) ,data = Clean_data3, subset = i.train)
  yh.2 = negbin_1$fitted.values
  yp.2 = predict(negbin_1, newdata = x[-i.train,], type = "response")
  
  #Negative Binomial Error
  train_error_nb[k] = mean((yh.2 - y[i.train])^2)
  test_error_nb[k] = mean((y[-i.train] - yp.2)^2)
  abs_train_error_nb[k] = mean(abs(yh.2-y[i.train]))
  abs_test_error_nb[k] = mean(abs(yp.2 - y[-i.train]))
  #Quasi-Poisson
  
  qp_1 = glm(ClaimNb ~ capped_vehpower + grouped_vehage + grouped_vehiclebrand + VehGas + grouped_drivage + log(BonusMalus) + log(Density) + grouped_fines + offset(log(Exposure)) ,data = Clean_data3, subset = i.train, family = quasipoisson(link = "log"))
  yh.3 = qp_1$fitted.values
  yp.3 = predict(qp_1, newdata = x[-i.train,], type = "response")
  
  #Quasi-Poisson Errors
  train_error_qp[k] = mean((yh.3 - y[i.train])^2)
  test_error_qp[k] = mean((y[-i.train] - yp.3)^2)
  abs_train_error_qp[k] = mean(abs(yh.3-y[i.train]))
  abs_test_error_qp[k] = mean(abs(yp.3 - y[-i.train]))
  
  # Zero-Inflated Poisson 
  zip_poisson= zeroinfl(ClaimNb ~ capped_vehpower + grouped_vehage + grouped_vehiclebrand + VehGas + grouped_drivage + log(BonusMalus) + log(Density) + grouped_fines + offset(log(Exposure)) ,data = Clean_data3, subset = i.train)
  yh.4 = zip_poisson$fitted.values
  yp.4 = predict(zip_poisson, newdata = x[-i.train,], type = "response")
  
  #Zero-Inflated Poisson Errors
  train_error_zp[k] = mean((yh.4 - y[i.train])^2)
  test_error_zp[k] = mean((y[-i.train] - yp.4)^2)
  abs_train_error_zp[k] = mean(abs(yh.4-y[i.train]))
  abs_test_error_zp[k] = mean(abs(yp.4 - y[-i.train]))
  
  #Zero-Inflated Negative Binomial 
  zip_negbin = zeroinfl(ClaimNb ~ capped_vehpower + grouped_vehage + grouped_vehiclebrand + VehGas + grouped_drivage + log(BonusMalus) + log(Density) + grouped_fines + offset(log(Exposure)),data = Clean_data3, subset = i.train, dist = "negbin")
  yh.5 = zip_negbin$fitted.values
  yp.5 = predict(zip_negbin, newdata = x[-i.train,], type = "response")
  
  #Zero-Inflated Negative Binomial Errors
  train_error_znb[k] = mean((yh.5 - y[i.train])^2)
  test_error_znb[k] = mean((y[-i.train] - yp.5)^2)
  abs_train_error_znb[k] = mean(abs(yh.5-y[i.train]))
  abs_test_error_znb[k] = mean(abs(yp.5 - y[-i.train]))
  
  #Hurdle Model
  hurdle_1 = hurdle(ClaimNb ~ capped_vehpower + grouped_vehage + grouped_vehiclebrand + VehGas + grouped_drivage + log(BonusMalus) + log(Density) + grouped_fines + offset(log(Exposure)) ,data = Clean_data3, subset = i.train, dist = "negbin")
  yh.6 = hurdle_1$fitted.values
  yp.6 = predict(hurdle_1, newdata = x[-i.train,], type = "response")
  
  #Hurdle Errors
  train_error_hurdle[k] = mean((yh.6 - y[i.train])^2)
  test_error_hurdle[k] = mean((y[-i.train] - yp.6)^2)
  abs_train_error_hurdle[k] = mean(abs(yh.6-y[i.train]))
  abs_test_error_hurdle[k] = mean(abs(yp.6 - y[-i.train]))
  
}
mean(train_error_hurdle)


library(dplyr)
levels(Clean_data$V)
Clean_dataBR$VehBrand <- Clean_data$VehBrand %>%
  recode("B10" = "B10-14",
         "B11" = "B10-14",
         "B13" = "B10-14",
         "B14" = "B10-14",
         "B1"  = "B1",
         "B3"  = "B3-6",
         "B4" = "B3-6",
         "B5" = "B3-6",
         "B6" = "B3-6",
         "B12"= "B12",
         "B2" = "B2")

# Print the result
levels(Clean_dataBR$VehBrand)

test_modelgroupBR <- glm(Clean_dataBR$ClaimNb ~ Clean_dataBR$VehBrand + offset(log(Clean_dataBR$Exposure)), family = poisson(link = "log"))
test_modelregularBR <- glm(Clean_data$ClaimNb ~ Clean_data$VehBrand + offset(log(Clean_data$Exposure)), family = poisson(link = "log"))
#reg.splinesBR <- gam(Clean_data$ClaimNb~ s(Clean_data$VehBrand)+offset(log(Clean_data$Exposure)), family=poisson(link = "log"))
summary(test_modelgroupBR)
AIC(test_modelgroupBR)
AIC(test_modelregularBR)
# Print the result
print(grouped_variable)

##
quantile(Clean_dataVA$Density)
hist(Clean_dataVA$Density)
Clean_dataVA$Density = cut(Clean_data$Density, c(0, 50, 100, 200, 350, 500, 1000, 1500, 2000, Inf), include.lowest = TRUE)
#Grouping of Fines
quantile(Clean_dataVA$Fines)
hist(Clean_dataVA$Fines)
Clean_dataVA$Fines = cut(Clean_data$Fines, c(0,50,100,200,500,Inf), include.lowest = TRUE)
Clean_dataVA$BonusMalus = cut(Clean_data$BonusMalus, c(50,75,100,150,Inf), include.lowest = TRUE)
hist(Clean_dataVA$BonusMalus)


#Neural Network
library(keras)
library(tensorflow)
install_keras()
install_tensorflow()
view(Contracts)
mynet = keras_model()
n = nrow(Contracts)
dat = Contracts[sample(1:n,n),]
x = dat[,c("Exposure", "VehPower", "VehAge", "DrivAge", "BonusMalus", "VehBrand", "VehGas", "Area", "Density", "Region", "Gender", "Transmission", "Employment", "Fines")]
y = dat$ClaimNb
test.set = 1:(n/5)
train.set = y[-test.set]
?layer_input
view(Contracts)
#model = keras_model_sequential(layer_input(shape = c(14)), layer_dense(128, activation = 'relu'),layer_dense(10, activation = 'softmax', units = 1))
model <- keras_model_sequential()
model %>%
  layer_dense(units = 256, activation = 'relu',input_shape = c(14))%>%
  layer_dropout(rate = 0.2)%>%
  layer_dense(units = 128, activation = 'relu')%>%
  layer_dense(units = 10, activation = 'softmax')
length(Contracts$ClaimNb)

model %>%compile(
  loss = 'poisson',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
  )

