library(keras)
library(reticulate)
library(tensorflow)
library(keras)
library(caret)

head(Clean_data)
ml_data <- Clean_data
######### Tidying Up Region

ml_data$Region <- gsub("-", "_", ml_data$Region)
ml_data$Region <- gsub("'", "_", ml_data$Region)

##########One hot encoding
VehBrand_one_hot <- model.matrix(~VehBrand-1, data = ml_data)
VehGas_one_hot <- model.matrix(~VehGas-1, data = ml_data)
Region_one_hot <- model.matrix(~Region-1, data = ml_data)
Area_one_hot <- model.matrix(~Area-1, data = ml_data)
Gender_one_hot <- model.matrix(~Gender-1, data = ml_data)
Transmission_one_hot <- model.matrix(~Transmission-1, data = ml_data)
Employment_one_hot <- model.matrix(~Employment-1, data = ml_data)


ml_data_2 <- dplyr::select(ml_data, -IDpol)
ml_data_3 <- ml_data_2
ml_data_3 <- cbind(ml_data_2, VehBrand_one_hot, VehGas_one_hot, Region_one_hot, Area_one_hot, Gender_one_hot, Transmission_one_hot, Employment_one_hot)

ml_data_4 <- ml_data_3 %>% 
  dplyr::select(-where(function(x) is.character(x) | is.factor(x)))

view(ml_data_4)

columns_to_scale <- setdiff(names(ml_data_4), "ClaimNb")

preprocess_model <- preProcess(ml_data_4[, columns_to_scale], method = c("range"))
ml_data_5 <- predict(preprocess_model, ml_data_3[,columns_to_scale])
ml_data_5$ClaimNb <- ml_data_3$ClaimNb

ml_data_6 <- ml_data_5
######Framework for holdout data
random_numbers <- sample(1:4, nrow(ml_data_6), replace = TRUE)
ml_data_6$RandomNumber <- random_numbers
ml_training_data <- ml_data_6[ml_data_6$RandomNumber %in% c(1, 2, 3), ]
ml_holdout_data <- ml_data_6[ml_data_6$RandomNumber == 4, ]
ml_training_data2 <- dplyr::select(ml_training_data, -c(RandomNumber))
ml_holdout_data2 <- dplyr::select(ml_holdout_data, -c(RandomNumber))
ml_target_training <- ml_training_data$ClaimNb
ml_target_holdout <- ml_holdout_data$ClaimNb

reticulate::virtualenv_create('r-reticulate')
reticulate::install_python()
reticulate::use_python("C://Users//tadhg//Documents//.virtualenvs//r-reticulate//Script//python")
reticulate::use_python()
head(ml_training_data2)

devtools::install_github("rstudio/keras", dependencies = TRUE)
devtools::install_github("rstudio/tensorflow", dependencies = TRUE)
keras::install_keras()
tensorflow::install_tensorflow()

library(devtools)
library(keras)

# Define your model
model <- keras_model_sequential() %>%
  layer_dense(units = 32, activation = 'relu', input_shape = 55) %>%
  layer_dense(units = 16, activation = 'relu') %>%
  layer_dense(units = 1, activation = 'linear')

# Compile the model
model %>% compile(
  optimizer = 'adam',
  loss = 'mse'
)

# Fit the model
history <- model %>% fit(
  train_data, train_labels,
  epochs = 100,
  batch_size = 32,
  validation_split = 0.2
)

# `num_features` is the number of predictor variables you have.
# `train_data` is your training dataset excluding the response variable.
# `train_labels` is your count response variable.
# You may need to adjust `units` in each layer and `epochs` based on your specific data.
