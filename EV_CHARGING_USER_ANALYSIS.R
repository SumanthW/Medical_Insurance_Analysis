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

## data cleaning and preparation

# load data
raw_data = read.csv("C:/Users/suman/Downloads/archive (2)/detailed_ev_charging_stations.csv",header=T)

# remove columns that won't be used in the project
data <- subset(raw_data,select=-c(Station.ID,Address,Connector.Types))

# rename columns
new_names <- c("lat","long","charger","cost","availability","distance","users","operator",
               "chargecapacity","year","renewable","rating","parkspots","maintenance")
names(data) <- new_names

# check for missing and duplicated data
sum(is.na(data)) # returns 0
sum(duplicated(data)) # returns 0

# transform available hours into two numerical data types
data$availability <- gsub("24/7", "24", data$availability) 
data$availability <- gsub("6:00-22:00", "16", data$availability) 
data$availability <- gsub("9:00-18:00", "9", data$availability) 
data$availability <- as.numeric(data$availability) 

# transform maintenance frequency into a numerical data type
data$maintenance <- gsub("Annually", "1", data$maintenance) 
data$maintenance <- gsub("Quarterly", "4", data$maintenance) 
data$maintenance <- gsub("Monthly", "12", data$maintenance) 
data$maintenance <- as.numeric(data$maintenance)

# transform charger type into a numerical data type
data$charger <- gsub("AC Level 1", "1", data$charger) 
data$charger <- gsub("AC Level 2", "2", data$charger) 
data$charger <- gsub("DC Fast Charger", "3", data$charger) 
data$charger <- as.numeric(data$charger) 

# transform renewable energy source into a numerical data type
data$renewable <- gsub("Yes", "1", data$renewable) 
data$renewable <- gsub("No", "0", data$renewable) 
data$renewable <- as.numeric(data$renewable) 

## handle station operator categories

# function for replacing characters with binary
replace_word_with_binary <- function(word, vector) {
  # Convert vector to logical values: 1 if the word is found, 0 otherwise
  binary_vector <- ifelse(vector == word, 1, 0)
  return(binary_vector)
}

# c-1 = 4 binary predictor variables
data$evgo <- replace_word_with_binary("EVgo",data$operator)
data$chargepoint <- replace_word_with_binary("ChargePoint",data$operator)
data$greenlots <- replace_word_with_binary("Greenlots",data$operator)
data$ionity <- replace_word_with_binary("Ionity",data$operator)

# remove operator variable from data set
data <- subset(data,select=-c(operator))

## descriptive statistics - numeric
# remove cat variables
num_data <- subset(data,select=-c(charger,renewable,evgo,chargepoint,greenlots,ionity))
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
hist(data$lat,xlab="Latitude (°)",main="Histogram of Latitude")
hist(data$long,xlab="Longitude (°)",main="Histogram of Longitude")
hist(data$availability,xlab="Hours of Availability",main="Histogram of Available Hours")
hist(data$distance,xlab="Distance to City (km)",main="Histogram of Distance to City")
hist(data$users,xlab="Users Per Day",main="Histogram of Daily Users")
hist(data$chargecapacity,xlab="Charging Capacity (kW)",main="Histogram of Charging Capacity")
hist(data$year,xlab="Installation Year",main="Histogram of Installation Year")
hist(data$rating,xlab="Customer Rating",main="Histogram of Ratings")
hist(data$parkspots,xlab="Parking Spots",main="Histogram of Parking Spots")
hist(data$maintenance,xlab="Maintenance Per Year",main="Histogram of Maintenance Frequency")

## descriptive statistics - categorical

# see categories
unique(data$operator)
unique(data$renewable)
unique(data$charger)

# mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Mode(data$operator)
Mode(data$renewable)
Mode(data$charger)

# box plots
operator <- as.factor(raw_data$Station.Operator)
boxplot(raw_data$Usage.Stats..avg.users.day. ~ operator, 
        xlab="Station Operator", 
        ylab="Avg. Number of Daily Users",
        main="Daily Users by Station Operator")
energy <- as.factor(raw_data$Renewable.Energy.Source)
boxplot(raw_data$Usage.Stats..avg.users.day. ~ energy, 
        xlab="Renewable Energy Source?", 
        ylab="Avg. Number of Daily Users",
        main="Daily Users by Energy Source")
charger <- as.factor(raw_data$Charger.Type)
boxplot(raw_data$Usage.Stats..avg.users.day. ~ charger, 
        xlab="Charger Type", 
        ylab="Avg. Number of Daily Users",
        main="Daily Users by Charger Type")

#Utility functions

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

refined_data <- data
#
#Scale data
scaled_data <- scale(refined_data[, sapply(refined_data, is.numeric)])%>% 
  as.data.frame()
head(scaled_data)

#Split Data
#num_groups <- 3
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
  
target <- "Value.users"


#Analysis 1 - Correlation matrix & Scatter Plots
# Select only numeric columns and compute correlation
print_correlation <- function(refined_data) {
  cor_matrix <- refined_data %>%
    select(where(is.numeric)) %>%
    cor(use = "complete.obs")  
  
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


#Analysis 1 results
print_correlation(refined_data)
print_correlation(scaled_data)
#No change in correlation due to scaling

scatter_matrix(train_data,target)

#Analysis 2: Fit model and check for Null hypothesis that all the parameters are insignificant


head(train_data)

# Fit the linear regression model
model <- lm(Value.users ~ ., data = train_data)
View(train_data)

print(refined_data)
# Summarize the model
summary(model)
anova(model)
# Calculate 95% confidence intervals for the coefficients
conf_intervals <- confint(model, level = 0.95)
print(conf_intervals)

test_data$y_hat <- predict(model, newdata = test_data)

step_model <- step(model, 
                   direction = "backward")

summary(step_model)
anova(step_model)
test_data$y_hat_step <- predict(step_model, newdata = test_data)
head(test_data[, c(target, "y_hat","y_hat_step")])

#Analysis 3: Build an interaction model and step down
full_model_interaction <- lm(Value.users ~ .^2, data = train_data)  
# Summarize the model
summary(full_model_interaction)

step_model2 <- step(full_model_interaction,direction = "backward")
summary(step_model2)
#Computationally expensive!
test_data$y_hat_full_model_interaction <- predict(full_model_interaction, newdata = test_data)
test_data$y_hat_full_model_interaction_step <- predict(step_model2, newdata = test_data)
head(test_data[, c(target, "y_hat","y_hat_step","y_hat_full_model_interaction","y_hat_full_model_interaction_step")])

#Analysis 4: Get Residual Plots
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
check_residuals(final_model)

#Analysis 5: Check Hii aka Influence Diagnostics
analyze_influence <- function(model){
  influence_stats <- influence.measures(model)
  summary(influence_stats)
}
analyze_influence(model)
analyze_influence(step_model)
analyze_influence(full_model_interaction)
analyze_influence(step_model2)
analyze_influence(final_model)

#Analysis 6: Multicollinearity Check: VIF
check_VIF <- function(model, model_name) {
  vif_values <- car::vif(model)
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
#check_VIF(full_model_interaction,deparse(substitute(full_model_interaction)))
#check_VIF(step_model2,deparse(substitute(step_model2)))

#Refined model
final_model = lm(Value.users ~ Value.long + Value.charger + Value.availability + Value.maintenance +
                 Value.evgo + Value.lat:Value.rating + Value.charger:Value.cost + Value.availability:Value.distance 
                 + Value.year:Value.maintenance +Value.renewable:Value.rating, data = train_data)
summary(final_model)
test_data$y_hat_final_model <- predict(final_model, newdata = test_data)
head(test_data[, c(target, "y_hat","y_hat_step","y_hat_full_model_interaction","y_hat_full_model_interaction_step","y_hat_final_model")])

display_model_summary<- function(model) {
  print(summary(model))
  anova(model)
}

#Summarize analyses

display_model_summary(model)
display_model_summary(step_model)
display_model_summary(full_model_interaction)
display_model_summary(step_model2)
display_model_summary(final_model)

