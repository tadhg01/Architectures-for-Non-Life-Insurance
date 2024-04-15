view(Clean_data)
graph_data <- Clean_data
length(graph_data$IDpol)
length(Claims$IDpol)
(head(Contracts))
print(Contracts[1,])
#Next Step Histogram and Frequency Table for all factors in the data set
my_colors <- c("skyblue", "lightgreen", "lightcoral")
#Claim Number
hist(graph_data$ClaimNb, breaks = 10, col = my_colors, xlab = "Number of Claims", ylab = "Frequency", main = "Histogram of Claim Number")
print(table(graph_data$ClaimNb))
#Vehicle Power
hist(graph_data$VehPower, breaks = 15, col = my_colors, xlab = "Vehicle Power", ylab = "Frequency", main = "Histogram of Vehicle Power")
print(table(graph_data$VehPower))
#Vehicle Age
hist(graph_data$VehAge, col = my_colors, xlab = "Vehicle Age", ylab = "Frequency", main = "Histogram of Vehicle Age")
print(table(graph_data$VehAge))
#Driver Age
hist(graph_data$DrivAge, col = my_colors, xlab = "Driver Age", ylab = "Frequency", main = "Histogram of Driver Age")
print(table(graph_data$DrivAge))
#BonusMalus
hist(graph_data$BonusMalus, col = my_colors, xlab = "Bonus Malus", ylab = "Frequency", main = "Histogram of Bonus Malus")
print(table(graph_data$BonusMalus))
#Vehicle Brand
barplot(table(graph_data$VehBrand), col = my_colors, xlab = "Vehicle Brand", ylab = "Frequency", main = "Barplot of Vehicle Brand")
print(table(graph_data$VehBrand))
#VehGas
barplot(table(graph_data$VehGas), col = my_colors, xlab = "Vehicle Gas", ylab = "Frequency", main = "Barplot of Vehicle Gas")
print(table(graph_data$VehGas))
pie(table(graph_data$VehGas), main = "Pie Chart of Vehicle Gas", col = c("skyblue", "lightcoral"))
#Area
barplot(table(graph_data$Area), col = my_colors, xlab = "Area", ylab = "Frequency", main = "Barplot of Area")
print(table(graph_data$Area))
#Density
hist(graph_data$Density, col = my_colors, xlab = "Density", ylab = "Frequency", main = "Histogram of Density")
print(table(graph_data$Density))
view(graph_data)
#Region
barplot(table(graph_data$Region), main = "Barplot of Region", col = my_colors)
barplot(table(graph_data$Region), 
        beside = TRUE,
        main = "Grouped Bar Plot of Region",
        xlab = "Region",
        ylab = "Frequency",
        col = rainbow(length(unique(graph_data$Region))))

#Gender
pie(table(graph_data$Gender), col = c("skyblue", "coral"), main = "Pie Chart of Gender")
#Transmission
pie(table(graph_data$Transmission), col = c("skyblue", "coral"), main = "Pie Chart of Transmission")
#Employment
barplot(table(graph_data$Employment), col = my_colors, main = "Barplot of Employment")
pie(table(graph_data$Employment), col = c("skyblue", "coral", "lightgreen"), main = "Pie Chart of Transmission")
#Fines
hist(FullData$Fines, main = "Histogram of Fines", xlab = "Fines", ylab = "Frequency", col = rainbow(length(unique(FullData$Region))))
#Claim Amount

scatter.smooth(graph_data$Fines, graph_data$ClaimNb)
proportion_zero <- sum(Contracts$ClaimNb == 0) / nrow(Contracts)
print(proportion_zero)

boxplot(Clean_data$Density ~ Clean_data$Region, col = rainbow(n = length(Clean_data$Region)))
ggplot(data = Clean_data, aes(x = Region, y = Density, color = Region)) + geom_point()


plot_average_response_categorical(data = Clean_data, response_var = "Density", categorical_var = "Region" )

plot_average_response_categorical(data = Clean_data, response_var = "ClaimNb", categorical_var = "Transmission")

plot_average_response(data = Clean_data,response_var = "ClaimNb", predictor_var = "BonusMalus", bin_width = 10 )

plot_average_response(data = Clean_data, response_var = "ClaimNb", predictor_var = "Fines", bin_width = 100)
quantile(Clean_data$Fines)


########
#Bar Chart for training and test error
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Assuming you have these vectors already
train_error <- c(mean(train_err_poisson), mean(train_error_nb), mean(train_error_qp), mean(train_error_zp), mean(train_error_znb), mean(train_error_hurdle))  # Example training errors
test_error <- c(mean(test_err_poisson), mean(test_error_nb), mean(test_error_qp), mean(test_error_zp), mean(test_error_znb), mean(test_error_hurdle))  # Example test errors
model_names <- c("Poisson", "Negative Binomial", "Quasi-Poisson", "Zero-Infl Poisson", "Zero-Infl NegBin", "Hurdle")  # Model names

# Create a dataframe
df <- data.frame(Model = model_names,
                 TrainingError = train_error,
                 TestError = test_error)

# Convert from wide to long format for easier plotting with ggplot
df_long <- pivot_longer(df, cols = c("TrainingError", "TestError"), names_to = "ErrorType", values_to = "Error")

# Plot
ggplot(df_long, aes(x = Model, y = Error, fill = ErrorType)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Training and Test Error for Different Models",
       y = "Error",
       x = "") +
  scale_fill_manual(values = c("TrainingError" = "blue", "TestError" = "red"),
                    labels = c("Test Error", "Training Error")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

abs_train_error <- c(mean(abs_train_err_poisson), mean(abs_train_error_nb), mean(abs_train_error_qp), mean(abs_train_error_zp), mean(abs_train_error_znb), mean(abs_train_error_hurdle))  # Example training errors
abs_test_error <- c(mean(abs_test_err_poisson), mean(abs_test_error_nb), mean(abs_test_error_qp), mean(abs_test_error_zp), mean(abs_test_error_znb), mean(abs_test_error_hurdle))  # Example test errors
abs_model_names <- c("Poisson", "Negative Binomial", "Quasi-Poisson", "Zero-Infl Poisson", "Zero-Infl NegBin", "Hurdle")  # Model names

# Create a dataframe
df <- data.frame(Model = abs_model_names,
                 TrainingError = abs_train_error,
                 TestError = abs_test_error)

# Convert from wide to long format for easier plotting with ggplot
df_long <- pivot_longer(df, cols = c("TrainingError", "TestError"), names_to = "ErrorType", values_to = "Error")

# Plot
ggplot(df_long, aes(x = Model, y = Error, fill = ErrorType)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Training and Test Error for Different Models",
       y = "Error",
       x = "") +
  scale_fill_manual(values = c("TrainingError" = "blue", "TestError" = "red"),
                    labels = c("Test Error", "Training Error")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  


# Assuming you have these vectors


error_matrix <- matrix(c(train_error, test_error), ncol = 2)
colnames(error_matrix) <- c("Train Error", "Test Error")
rownames(error_matrix) <- model_names

# Convert the matrix to a table
error_table <- as.table(error_matrix)

# Print the table
print(error_table)

library(knitr)

# Create a data frame from your vectors
error_df <- data.frame(Model = model_names, TrainError = train_error, TestError = test_error)

# Use kable to create a more advanced table
kable_table <- kable(error_df, caption = "Training and Test Errors for Models", align = c('l', 'c', 'c'))

# Print the kable table
print(kable_table)





#############
#Machine Learning Section
mse_loss = history$metrics$loss
mse_validation = history$metrics$val_loss 

# Create a data frame that combines the vectors
data <- data.frame(Index = rep(1:length(mse_loss), 2),
                   Values = c(mse_loss, mse_validation),
                   Vector = factor(rep(c("Training Loss", "Validation Loss"), each=length(mse_loss))))

# Plot using ggplot
ggplot(data, aes(x = Index, y = Values, group = Vector, color = Vector)) +
  geom_line() +  # Draw lines connecting the points
  geom_point() + # Add the data points
  theme_minimal() +  # Use a minimal theme
  labs(title = "Plot of Training and Validation Loss", x = "Epochs", y = "Loss") +  # Add labels
  scale_color_manual(values = c("Training Loss" = "blue", "Validation Loss" = "red"))  # Set custom colors

shallow_training_mae = history_shallow$metrics$mae
shallow_val_mae = history_shallow$metrics$val_mae



data <- data.frame(Index = rep(1:length(shallow_training_mae), 2),
                   Values = c(shallow_training_mae, shallow_val_mae),
                   Vector = factor(rep(c("Training MAE", "Validation MAE"), each=length(shallow_training_mae))))

# Plot using ggplot
ggplot(data, aes(x = Index, y = Values, group = Vector, color = Vector)) +
  geom_line() +  # Draw lines connecting the points
  geom_point() + # Add the data points
  theme_minimal() +  # Use a minimal theme
  labs(title = "Plot of Training and Validation MAE", x = "Epochs", y = "MAE") +  # Add labels
  scale_color_manual(values = c("Training MAE" = "blue", "Validation MAE" = "red"))  # Set custom colors

plot(shallow_training_mse, pch = 16, type = 'o', col = 'red')
plot(shallow_val_mse, pch = 16, type = 'o', col = 'red')

shallow_training_loss = history_shallow$metrics$loss
shallow_val_loss = history_shallow$metrics$val_loss
view(history_shallow)

data_loss <- data.frame(Index = rep(1:length(shallow_training_loss), 2),
                   Values = c(shallow_training_loss, shallow_val_loss),
                   Vector = factor(rep(c("Training Loss", "Validation Loss"), each=length(shallow_training_loss))))

# Plot using ggplot
ggplot(data, aes(x = Index, y = Values, group = Vector, color = Vector)) +
  geom_line() +  # Draw lines connecting the points
  geom_point() + # Add the data points
  theme_minimal() +  # Use a minimal theme
  labs(title = "Plot of Training and Validation Loss", x = "Epochs", y = "Loss") +  # Add labels
  scale_color_manual(values = c("Training Loss" = "blue", "Validation Loss" = "red"))  # Set custom colors

plot(shallow_val_loss, pch = 16, type = 'o', col = 'red')

data <- data.frame(Index = rep(1:length(shallow_training_mse), 2),
                   Values = c(shallow_training_mse, shallow_val_mse),
                   Vector = factor(rep(c("Training MSE", "Validation MSE"), each=length(shallow_training_mse))))

# Plot using ggplot
ggplot(data, aes(x = Index, y = Values, group = Vector, color = Vector)) +
  geom_line() +  # Draw lines connecting the points
  geom_point() + # Add the data points
  theme_minimal() +  # Use a minimal theme
  labs(title = "Plot of Training and Validation MSE", x = "Epochs", y = "MSE") +  # Add labels
  