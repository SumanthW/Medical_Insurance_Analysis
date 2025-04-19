## Install packages if not present
install_if_missing <- function(pkg_name) {
  if (!pkg_name %in% installed.packages()) {
    message(paste("Installing package:", pkg_name))
    install.packages(pkg_name)
  } else {
    message(paste(pkg_name, "is already installed"))
  }
  # Load the package
  library(pkg_name, character.only = TRUE)
}

# library imports
install_if_missing("dplyr")
install_if_missing("ggplot2")
install_if_missing("reshape2")
install_if_missing("GGally")
install_if_missing("car")
install_if_missing("tidyr")
install_if_missing("DataExplorer")
## data cleaning and preparation

# load data
raw_data = read.csv("C:/Users/suman/Downloads/insurance.csv",header=T)

# remove four-cat variable
ins_data <- subset(raw_data,select=-c(region))

# check for missing and duplicated data
sum(is.na(ins_data)) # returns 0
sum(duplicated(ins_data)) # returns 1
ins_data <- unique(ins_data) # remove duplicate

# transform cat into numerical
ins_data$sex <- gsub("female","1",ins_data$sex)
ins_data$sex <- gsub("male","0",ins_data$sex) 
ins_data$smoker <- gsub("yes","1",ins_data$smoker)
ins_data$smoker <- gsub("no","0",ins_data$smoker)

ins_data$sex <- as.numeric(ins_data$sex) 
ins_data$smoker <- as.numeric(ins_data$smoker) 

## descriptive statistics - numeric

# remove cat variables
num_data <- subset(ins_data,select=-c(sex,smoker))
# new df for statistics
stats_data <- data.frame(Variable = character(), Mean = numeric(), Median = numeric(),
                         STDEV = numeric(), Min = numeric(), Max = numeric())
for (col_name in colnames(num_data)) {
  col_data <- num_data[[col_name]]
  stats_data <- rbind(stats_data, data.frame(Column = col_name,
                                             Mean = round(mean(col_data),2),
                                             Median = median(col_data),
                                             STDEV = round(sd(col_data),2),
                                             Min = min(col_data),
                                             Max = max(col_data)))
}

# histograms
hist(ins_data$age,xlab="Age (Years)",main="Histogram of Age")
hist(ins_data$bmi,xlab="Body Mass Index",main="Histogram of BMI")
hist(ins_data$children,xlab="Number of Children",main="Histogram of Children")
hist(ins_data$charges,xlab="Medical Insurance Charges (USD)",main="Histogram of Charges")

## descriptive statistics - categorical

# mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Mode(ins_data$sex)
Mode(ins_data$smoker)

# box plots
charges <- as.factor(ins_data$charges)
sex <- as.factor(ins_data$sex)
boxplot(charges ~ sex, 
        xlab="Sex", 
        ylab="Medical Insurance Charges (USD)",
        main="Charges by Sex")
smoker <- as.factor(ins_data$smoker)
boxplot(charges ~ smoker, 
        xlab="Smoking Status", 
        ylab="Medical Insurance Charges (USD)",
        main="Charges by Smoking Status")

# Utility functions

scatter_matrix <- function(data, response_var = "y") {
  # Check if response_var exists
  if (!(response_var %in% names(data))) {
    stop("Response variable '", response_var, "' not found in the dataset.")
  }
  
  # Select numeric predictors (excluding response_var)
  numeric_vars <- data %>% 
    select(where(is.numeric)) %>% 
    names()
  
  predictors <- setdiff(numeric_vars, response_var)
  
  # Check if there are predictors
  if (length(predictors) == 0) {
    stop("No numeric predictors found (excluding the response variable).")
  }
  
  # Reshape data for faceting
  plot_data <- data %>%
    select(all_of(c(response_var, predictors))) %>%
    pivot_longer(
      cols = all_of(predictors),
      names_to = "predictor",
      values_to = "value"
    )
  
  # Generate scatterplots
  ggplot(plot_data, aes(x = value, y = .data[[response_var]])) +
    geom_point(alpha = 0.3, color = "steelblue") +
    geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add regression line
    facet_wrap(~ predictor, scales = "free_x") +  # One panel per predictor
    labs(
      title = paste("Scatterplots of", response_var, "vs. Predictors"),
      x = "Predictor Value",
      y = response_var
    ) +
    theme_minimal() +
    theme(
      strip.background = element_rect(fill = "lightgray"),
      panel.spacing = unit(1, "lines")
    )
}

refined_data <- ins_data

# Scale data
scaled_data <- scale(refined_data[, sapply(refined_data, is.numeric)])%>% 
  as.data.frame()
head(scaled_data)

#Split Data
# num_groups <- 3
group_labels <- rep(1:3, length.out = nrow(refined_data))
group_labels <- sample(group_labels) # shuffle labels
data_groups <- data.frame(Value = refined_data, Group = group_labels)

train_data <- subset(data_groups , Group != 1)
test_data <- subset(data_groups, Group == 1)

train_data <- na.omit(train_data[, !(names(train_data) %in% c("Group"))])
test_data <- na.omit(test_data[, !(names(test_data) %in% c("Group"))])
train_data <- train_data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.) | is.nan(.), NA, .))) %>%  # Convert Inf/NaN to NA
  na.omit()

test_data <- test_data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.) | is.nan(.), NA, .))) %>%  # Convert Inf/NaN to NA
  na.omit()

# Analysis 1 - Correlation matrix & Scatter Plots
# Select only numeric columns and compute correlation
print_correlation <- function(refined_data) {
  numeric_data <- refined_data[sapply(refined_data, is.numeric)]
  cor_matrix <- cor(numeric_data, use = "complete.obs")  
  
  melted_cor <- melt(cor_matrix)
  
  # Create heatmap
  ggplot(melted_cor, aes(Var1, Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Correlation") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    coord_fixed() +
    geom_text(aes(label = round(value, 2)), color = "black", size = 3)
}

target <- "Value.charges"

# Analysis 1 results
print_correlation(refined_data)
print_correlation(scaled_data) #no change in correlation due to scaling

#scatter_matrix(train_data,target)

# Analysis 2: Fit model and check for null hypothesis that all the parameters are insignificant

# Fit the linear regression model
formula <- as.formula(paste(target, "~ ."))
model <- lm(formula, data = train_data)
# Summarize the model
summary(model)
anova(model)

# Calculate 95% confidence intervals for the coefficients
conf_intervals <- confint(model, level = 0.95)
print(conf_intervals)

step_model <- step(model, 
                   direction = "backward")

summary(step_model)
anova(step_model)

# Analysis 3: Build an interaction model and step down
train_data_higher_order <- train_data[ , !(names(train_data) %in% "Value.sex")]
formula <- as.formula(paste(target, "~ .^2")) 
full_model_interaction <- lm(formula, data = train_data_higher_order)  
# Summarize the model
summary(full_model_interaction)

step_model2 <- step(full_model_interaction,direction = "backward")
summary(step_model2)

# Computationally expensive!

# Analysis 4: Get Residual Plots
check_residuals<-function(model){
  # Residuals vs Fitted with smoother
  residualPlot(model)
  
  # Q-Q Plot with confidence bands
  qqPlot(model$residuals)
}

check_residuals(model)
check_residuals(step_model)
check_residuals(full_model_interaction)
check_residuals(step_model2)

# Analysis 5: Multicollinearity Check: VIF
check_VIF <- function(model, model_name) {
  vif_values <- car::vif(model)
  #vif_values <- car::vif(model, type = 'predictor') #Use this instead for interaction model?
  print(model_name)
  print(vif_values) 
  
  # Get names of predictors with VIF >= 5
  high_vif <- names(vif_values)[vif_values >= 5]  # Directly use names
  
  if(length(high_vif) > 0) {
    print(paste("High VIF predictors:", paste(high_vif, collapse = ", ")))
  } else {
    print("No predictors with VIF ≥ 5")
  }
  
  # Create barplot with names
  barplot(vif_values,
          horiz = TRUE,
          col = ifelse(vif_values >= 5, "salmon", "lightblue"),  # Color high VIF
          main = paste("VIF Values:", model_name), 
          xlab = "VIF",
          las = 1,
          names.arg = names(vif_values),  # Ensure names appear
          cex.names = 0.8  # Adjust text size if needed
  )
  abline(v = 5, col = "red", lty = 2)  # Threshold line
}

check_VIF(model,deparse(substitute(model)))
check_VIF(step_model,deparse(substitute(step_model)))
check_VIF(full_model_interaction,deparse(substitute(full_model_interaction)))
check_VIF(step_model2,deparse(substitute(step_model2)))

# fit new model to reduce multicollinearity of Model D
# center the input variables
age <- train_data_higher_order$Value.age - mean(train_data_higher_order$Value.age)
bmi <- train_data_higher_order$Value.bmi - mean(train_data_higher_order$Value.bmi)
smoker <- train_data_higher_order$Value.smoker - mean(train_data_higher_order$Value.smoker)
children <- train_data_higher_order$Value.children - mean(train_data_higher_order$Value.children)
# create new interaction terms
train_data_higher_order$Value.age.bmi <- age*bmi
train_data_higher_order$Value.bmi.smoker <- bmi*smoker
train_data_higher_order$Value.children.smoker <- children*smoker
# new model
formula <- as.formula(paste(target, "~ ."))
model_e <- lm(formula, data = train_data_higher_order)
# check VIF
vif_e <- vif(model_e)
check_VIF(model_e,deparse(substitute(model_e)))
check_residuals(model_e)

# Analysis 6: Check Hii aka Influence Diagnostics
analyze_influence <- function(model, data) {
  library(DataExplorer)
  
  # Compute influence statistics
  influence_stats <- influence.measures(model)
  
  # Get logical matrix of influential flags
  influence_matrix <- influence_stats$is.inf
  
  # Find rows flagged as influential by ANY diagnostic
  influential_points <- which(apply(influence_matrix, 1, any))
  
  # Optionally, Cook's distance threshold
  cooksD <- cooks.distance(model)
  cooks_threshold <- 4 / nrow(data)
  influential_cooks <- which(cooksD > cooks_threshold)
  
  # Combine both sets of influential points
  combined_influentials <- union(influential_points, influential_cooks)
  
  # Subset the influential data
  influential_data <- data[combined_influentials, ]
  
  # Generate a report on the influential observations
  create_report(influential_data)
  
  # Optionally return the data for further use
  return(list(
    influential_data = influential_data,
    influential_indices = combined_influentials
  ))
}
analyze_influence(model,train_data)
analyze_influence(step_model,train_data)
analyze_influence(full_model_interaction,train_data_higher_order)
analyze_influence(step_model2,train_data_higher_order)
analyze_influence(model_e,train_data_higher_order)
nrow(train_data)

# Retraining Model step_model2
retrain_without_influential <- function(model, data, threshold = NULL) {
  # Compute Cook's distance
  cooks_d <- cooks.distance(model)
  
  # Default threshold if not provided: 4 / n
  if (is.null(threshold)) {
    threshold <- 4 / nrow(data)
  }
  
  # Identify influential rows
  influential_rows <- which(cooks_d > threshold)
  
  # Remove influential rows
  cleaned_data <- data[-influential_rows, ]
  
  # Refit model with original formula
  updated_model <- lm(formula(model), data = cleaned_data)
  
  return(updated_model)
}
retrained_model <- retrain_without_influential(model_e,train_data_higher_order)
summary(retrained_model)

check_residuals(retrained_model)

formula <- as.formula(paste("log(",target,")", "~ .^2")) 
logarithmic_model <- lm(formula, data = train_data) 
summary(logarithmic_model)

check_residuals(logarithmic_model)

step_model3 <- step(logarithmic_model,direction = "backward")
summary(step_model3)

check_residuals(step_model3)

display_model_summary<- function(model) {
  print(summary(model))
  anova(model)
}

# Summarize analyses

display_model_summary(model)
display_model_summary(step_model)
display_model_summary(full_model_interaction)
display_model_summary(step_model2)
display_model_summary(retrained_model)

evaluate_model <- function(model, test_data, target) {
  predictions <- predict(model, newdata = test_data)
  actuals <- test_data[[target]]
  residuals <- actuals - predictions
  
  mae <- mean(abs(residuals))
  rmse <- sqrt(mean(residuals^2))
  r2 <- 1 - sum(residuals^2) / sum((actuals - mean(actuals))^2)
  
  cat("MAE:", mae, "\n")
  cat("RMSE:", rmse, "\n")
  cat("R-squared:", r2, "\n")
  
  plot(actuals, predictions,
       xlab = "Actual", ylab = "Predicted",
       main = "Predicted vs. Actual - Test Data",
       pch = 19, col = "steelblue")
  abline(0, 1, col = "red", lwd = 2)
}

# add interaction terms to test data
age <- test_data$Value.age - mean(test_data$Value.age)
bmi <- test_data$Value.bmi - mean(test_data$Value.bmi)
smoker <- test_data$Value.smoker - mean(test_data$Value.smoker)
children <- test_data$Value.children - mean(test_data$Value.children)
test_data$Value.age.bmi <- age*bmi
test_data$Value.bmi.smoker <- bmi*smoker
test_data$Value.children.smoker <- children*smoker


# add interaction terms to refined data
age <- refined_data$age - mean(refined_data$age)
bmi <- refined_data$bmi - mean(refined_data$bmi)
smoker <- refined_data$smoker - mean(refined_data$smoker)
children <- refined_data$children - mean(refined_data$children)
refined_data$age.bmi <- age*bmi
refined_data$bmi.smoker <- bmi*smoker
refined_data$children.smoker <- children*smoker

evaluate_model(retrained_model,test_data,target)
evaluate_model(model,test_data,target)

Cross_Validate <- function(data,formula) { #Takes data and Returns SSE of full and reduced models
  # partition data into 10 groups
  num_groups <- 10
  print(target)
  group_labels <- rep(1:10, c(rep(133, 9), 140))
  group_labels <- sample(group_labels) # shuffle labels
  data_groups <- data.frame(Value = data, Group = group_labels)
  
  train <- list()
  test <- list()
  sse_full <- list()
  sse_reduced <- list()
  Total_sse_full <- 0
  Total_sse_reduced <- 0
  
  for (i in 1:num_groups) {
    train[[i]] <- subset(data_groups, Group != i)
    test[[i]] <- subset(data_groups, Group == i)
  }
  
  for (i in 1:num_groups) {
    # fit full model
    model_full <- lm(formula, train[[i]])
    
    # predict response values
    test[[i]]$y_hat_full <- predict(model_full, newdata = test[[i]])
    
    # calculate test error sum of squares
    test[[i]]$err_full <- (test[[i]][[target]] - test[[i]]$y_hat_full)^2
    sse_full[[i]] <- sum(test[[i]]$err_full)
    
    # fit model without influence 
    model_reduced <- retrain_without_influential(model_full,train[[i]])
    
    # predict response values
    test[[i]]$y_hat_reduced <- predict(model_reduced, newdata = test[[i]])
    
    # calculate test error sum of squares
    test[[i]]$err_reduced <- (test[[i]][[target]] - test[[i]]$y_hat_reduced)^2
    sse_reduced[[i]] <- sum(test[[i]]$err_reduced)
    
  }
  
  Total_sse_full <- sum(unlist(sse_full))
  Total_sse_reduced <- sum(unlist(sse_reduced))
  
  return(list(SSE_full = Total_sse_full, SSE_reduced = Total_sse_reduced))
  
}


print(Cross_Validate(refined_data,formula(retrained_model)))
print(Cross_Validate(refined_data,formula(model)))
