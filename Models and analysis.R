library(ggplot2)
library(forecast)
library(rpart)
library(rpart.plot)
library(caret)

# install.packages('dplyr')
library(dplyr)

loan.df <- read.csv('Loan_Data.csv', stringsAsFactors = TRUE)

str(loan.df)

# Cleaning the data
# Filter data: Remove rows that had missing values e.g: ("", N/A). Did not remove "" values within Gender column.
filtered.df <- loan.df %>% na.omit() %>% filter(Married != "") %>% filter(Dependents != "") %>% filter(Self_Employed != "")

# Add Total_Income column combining applicant and co-applicant income
total.income <- filtered.df$ApplicantIncome + filtered.df$CoapplicantIncome

# Remove ApplicantIncome and CoapplicantIncome columns as they will be replaced by TotalIncome from above
filtered.df <- filtered.df[-c(7,8)]

# Add TotalIncome column
filtered.df$Total_Income <- total.income

# Move TotalIncome column to the correct position within the data frame
filtered.df <- filtered.df %>% relocate(Total_Income, .before = LoanAmount)



# Plots
ggplot(data = filtered.df) + geom_point(aes(x = ApplicantIncome, y = LoanAmount, color = Loan_Status))

ggplot(data = filtered.df) + geom_point(aes(x = Loan_Amount_Term, y = LoanAmount))

ggplot(data = filtered.df) + geom_bar(aes(x = Property_Area))




# Classification Tree
filtered.df <- filtered.df[-1]

set.seed(2022)
train.index <- sample(1:nrow(filtered.df), nrow(filtered.df) * 0.6)

# Making Data Partition
train.df <- filtered.df[train.index, ]
valid.df <- filtered.df[-train.index, ]

# Plotting the tree
default.ct <- rpart(Loan_Status ~ ., data = train.df, method = "class", control = rpart.control(maxdepth = 4))

# Viewing the Tree
rpart.plot(default.ct, extra = 1)

default.ct.point.pred <- predict(default.ct, valid.df, type = "class")
default.ct.point.pred

# Confusion Matrix for validation data
confusionMatrix(default.ct.point.pred, valid.df$Loan_Status)





# KNN
selected.numeric.columns <- c(6:8, 11)

# Filter data to only have numeric columns
filtered.numeric.df <- filtered.df[, selected.numeric.columns]


# Same as Classification Tree
set.seed(2022)
train.index <- sample(1:nrow(filtered.numeric.df), nrow(filtered.numeric.df) * 0.6)

# Making Data Partition
train.df <- filtered.numeric.df[train.index, ]
valid.df <- filtered.numeric.df[-train.index, ]

# Making copies of the dataset
train.norm.df <- train.df
valid.norm.df <- valid.df

norm.values <- preProcess(train.df[, -4], method=c("center", "scale"))
train.norm.df[, -4] <- predict(norm.values, train.df[, -4])
valid.norm.df[, -4] <- predict(norm.values, valid.df[, -4])

# install.packages('FNN')
library(FNN)

knn.pred <- knn(train.norm.df[, -4], valid.norm.df[, -4], cl = train.norm.df[, 4], k = 20)

confusionMatrix(knn.pred, valid.norm.df[, 4])

# K values accuracy from 5 to 15.
k.values <- c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
confusion.accuracy <- c(0.6294, 0.6091, 0.665, 0.6497, 0.6751, 0.6447, 0.6701, 0.665, 0.665, 0.6599, 0.6751)
k.results.df <- data.frame(k.values, confusion.accuracy)


ggplot(data = k.results.df) + 
  geom_bar(aes(x = reorder(factor(k.values), -confusion.accuracy), y = confusion.accuracy), stat = 'identity', fill='steelblue') + 
  labs(x = "K-Values", y = " Confusion Matrix Accuracy", title = "Determining the best K-Value")

# Appears that K-values 9 and 15 are the most optimal with the highest confusion Matrix Accuracy of 0.6751
