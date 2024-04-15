plot_claim_number <- function(categorical_var) {
  # Ensure the categorical variable is a factor
  Clean_data[[categorical_var]] <- as.factor(Clean_data[[categorical_var]])
  
  # Prepare the formula for aggregation
  formula_actual <- as.formula(paste("ClaimNb ~", categorical_var))
  formula_predicted <- as.formula(paste("Predicted_Claim_Number ~", categorical_var))
  
  # Calculate mean actual Claim Amounts
  actual_means <- aggregate(formula_actual, data = Clean_data, FUN = mean)
  names(actual_means) <- c(categorical_var, "Mean_Claim_Number")  # Rename columns
  
  # Calculate mean predicted Claim Amounts
  Clean_data$Predicted_Claim_Amount <- predict(glm_final, newdata = Clean_data, type = "response")
  predicted_means <- aggregate(formula_predicted, data = Clean_data, FUN = mean)
  names(predicted_means) <- c(categorical_var, "Mean_Predicted_Claim_Number")  
  
  #Plot
  ggplot() +
    geom_point(data = actual_means, aes_string(x = categorical_var, y = "Mean_Claim_Amount"), color = "green") +
    geom_line(data = actual_means, aes_string(x = categorical_var, y = "Mean_Claim_Amount", group = 1), color = "green") +
    geom_point(data = predicted_means, aes_string(x = categorical_var, y = "Mean_Predicted_Claim_Amount"), color = "blue") +
    geom_line(data = predicted_means, aes_string(x = categorical_var, y = "Mean_Predicted_Claim_Amount", group = 1), color = "blue") +
    labs(x = categorical_var, y = "Claim Amount", title = paste("Actual vs Predicted Claim Amounts by", categorical_var))
}

plot_average_response_categorical <- function(data, response_var, categorical_var) {
  # Ensure the categorical variable is a factor
  data[[categorical_var]] <- as.factor(data[[categorical_var]])
  
  # Calculate average response for each level of the categorical variable
  average_response <- data %>%
    group_by(.data[[categorical_var]]) %>%
    summarise(Avg_Response = mean(.data[[response_var]], na.rm = TRUE), .groups = 'drop')
  
  # Visualization
  p <- ggplot(average_response, aes(x = .data[[categorical_var]], y = Avg_Response, fill = .data[[categorical_var]])) +
    geom_col() +
    theme_minimal() +
    labs(x = categorical_var, y = paste0("Average ", response_var), 
         title = paste0("Average ", response_var, " by ", categorical_var)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed
  
  print(p)
}

plot_average_response <- function(data, response_var, predictor_var, bin_width = 1) {
  # Bin the predictor variable
  n = max(data[[predictor_var]])/bin_width
  bin_breaks <- seq(floor(min(data[[predictor_var]], na.rm = TRUE)), 
                    ceiling(max(data[[predictor_var]], na.rm = TRUE)), 
                    by = bin_width)
  predictor_bin_var <- paste0(predictor_var, "_bin")
  data[[predictor_bin_var]] <- cut(data[[predictor_var]], 
                                   breaks = bin_breaks, 
                                   include.lowest = TRUE, 
                                   right = FALSE)
  
  # Calculate average response for each bin
  average_response <- data %>%
    group_by(.data[[predictor_bin_var]]) %>%
    summarise(Avg_Response = mean(.data[[response_var]], na.rm = TRUE), .groups = 'drop')
  
  # Visualization
  p <- ggplot(average_response, aes(x = .data[[predictor_bin_var]], y = Avg_Response)) +
    geom_col(aes(fill = factor(.data[[predictor_bin_var]]))) + scale_fill_manual(values = rainbow(n), guide = FALSE) +
    theme_minimal() +
    labs(x = paste0(predictor_var, " Bin"), y = paste0("Average ", response_var), 
         title = paste0("Average ", response_var, " by ", predictor_var, " Bin")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed
  
  print(p)
}
