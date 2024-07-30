#BUAN 6356.005 - Business Analytics with R - F23
#Group 9
#Professor: Zhe Zhang
#Final Project Code Submission
#1.) Yash Khatavkar-ymk220003 
#2.) Kiran Magar-kxm230022
#3.) Aditi Modi-axm230041 
#4.) Aashay Bhujbal-asb220014

#  Clear the environment
rm(list = ls())

#  Assign dir1 to the folder on your computer where this excel file (HW1) is.
dir1 <- getwd();

#  set the working directory to dir1
setwd(dir1)

#Importing Libraries
library(ranger)
library("ggplot2")
library(caret)
install.packages("data.table")
library(data.table)

#Importing Data
creditcarddata <- read.csv("/Users/yashkhatavkar/Desktop_V2/UT Dallas MSBA/BA_With_R/creditcard.csv")

#Exploring Data
dim  <- dim(creditcarddata)
head <- head(creditcarddata,6)
tail <- tail(creditcarddata,6)
table <- table(creditcarddata$Class)
summary <- summary(creditcarddata$Amount)
names <- names(creditcarddata)
var <- var(creditcarddata$Amount)
standard_dev <- sd(creditcarddata$Amount)

# Distribution of 'Class' variable
ggplot(creditcardata, aes(x = factor(Class))) +
  geom_bar() +
  labs(title = "Distribution of Class",
       x = "Class",
       y = "Frequency")

# Amount distribution
ggplot(creditcarddata, aes(x = Amount)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Amount",
       x = "Amount",
       y = "Frequency")

# Class vs. Amount
ggplot(creditcardata, aes(x = factor(Class), y = Amount)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Class vs. Amount",
       x = "Class",
       y = "Amount")

# Correlation plot
correlation_matrix <- cor(creditcarddata[, sapply(df, is.numeric)])
ggplot(data = melt(correlation_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Correlation Plot")


# Boxplot comparing Amount distribution by Class
ggplot(creditcarddata, aes(x = factor(Class), y = Amount)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Amount Distribution by Class",
       x = "Class",
       y = "Amount")

# Density plots for numeric variables
library(dplyr)
library(tidyr)

df %>%
  gather(key = "variable", value = "value", -Class) %>%
  ggplot(aes(x = value, fill = factor(Class))) +
  geom_density(alpha = 0.5) +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Density Plots for Numeric Variables")

# Correlation heatmap
library(corrplot)

correlation_matrix <- cor(df[, sapply(df, is.numeric)])
corrplot(correlation_matrix, method = "color")

# Class distribution by Time (if 'Time' column exists)
if ("Time" %in% names(df)) {
  ggplot(df, aes(x = Time, fill = factor(Class))) +
    geom_bar() +
    labs(title = "Class Distribution by Time",
         x = "Time",
         y = "Frequency")
} else {
  print("Time variable not found in the dataset.")
}

#Lets check the missing values in the data
sum(is.na(df))

#lets visualize the missing values if any, using library naniar
install.packages("naniar")
library(naniar)
dim(df)
table(df$Class)
summary(df$Amount)
names(df)
var(df$Amount)
sd(df$Amount)

#Data Manipulation
head(creditcarddata)
creditcarddata$Amount=scale(creditcarddata$Amount)
NewData=creditcarddata[,-c(1)]
head(NewData)


#Data Modeling
library(caTools)
set.seed(123)
data_sample = sample.split(NewData$Class,SplitRatio=0.80)
train_data = subset(NewData,data_sample==TRUE)
test_data = subset(NewData,data_sample==FALSE)
dim(train_data)
dim(test_data)

#Applying Different Models Now

#1.) Fitting Logistic Regression Model
Logistic_Model=glm(Class~.,test_data,family=binomial())
summary(Logistic_Model)
plot(Logistic_Model)

#ROC Curve
library(pROC)

# Check the levels of test_data$Class
levels(test_data$Class)

# If the levels are not ordered as expected (0 and 1), set them explicitly
test_data$Class <- factor(test_data$Class, levels = c("0", "1"))

# Run the ROC analysis
auc.gbm <- roc(test_data$Class, lr.predict, plot = TRUE, col = "blue", direction = "<")


#2.) Decision Tree Model
install.packages("rpart")
library(rpart)
library(rpart.plot)
decisionTree_model <- rpart(Class ~ . , creditcarddata, method = 'class')
predicted_val <- predict(decisionTree_model, creditcarddata, type = 'class')
probability <- predict(decisionTree_model, creditcarddata, type = 'prob')
rpart.plot(decisionTree_model)

#3.) Artificial Neural Network
library(neuralnet)
ANN_model =neuralnet (Class~.,train_data,linear.output=FALSE)
plot(ANN_model)
predANN=compute(ANN_model,test_data)
resultANN=predANN$net.result
resultANN=ifelse(resultANN>0.5,1,0)


#4.) Gradient Boosting Model (GBM)
library(gbm, quietly=TRUE)
# Get the time to train the GBM model
system.time(
  model_gbm <- gbm(Class ~ .
                   , distribution = "bernoulli"
                   , data = rbind(train_data, test_data)
                   , n.trees = 500
                   , interaction.depth = 3
                   , n.minobsinnode = 100
                   , shrinkage = 0.01
                   , bag.fraction = 0.5
                   , train.fraction = nrow(train_data) / (nrow(train_data) + nrow(test_data))
  )
)
# Determine best iteration based on test data
model.influence = relative.influence(model_gbm, n.trees = gbm.iter, sort. = TRUE)
#Plot the gbm model
plot(model_gbm)

# Plot and calculate AUC on test data
gbm_test = predict(model_gbm, newdata = test_data, n.trees = gbm.iter)
gbm_auc = roc(test_data$Class, gbm_test, plot = TRUE, col = "red")
print(gbm_auc)