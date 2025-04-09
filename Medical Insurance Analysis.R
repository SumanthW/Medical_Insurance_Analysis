### data set #1
# load data
ins_data = read.csv("insurance.csv",header=T)

# remove four-cat variable
ins_data <- subset(ins_data,select=-c(region))

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

plot(ins_data)
cor(ins_data)

model <- lm(charges~.,ins_data)
summary(model)