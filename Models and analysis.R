library(ggplot2)
library(forecast)
library(rpart)
library(rpart.plot)
library(caret)

# install.packages('dplyr')
library(dplyr)

loan.df <- read.csv('Loan_Data.csv', stringsAsFactors = TRUE)

str(loan.df)

# Filter data: Remove rows that had missing values e.g: ("", N/A). Did not remove "" values within Gender column.
filtered.df <- loan.df %>% na.omit() %>% filter(Married != "") %>% filter(Dependents != "") %>% filter(Self_Employed != "")


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
default.ct <- rpart(Loan_Status ~ ., data = train.df, method = "class")

# Viewing the Tree
rpart.plot(default.ct, extra = 1)

default.ct.point.pred <- predict(default.ct, valid.df, type = "class")
default.ct.point.pred

# Confusion Matrix for validation data
confusionMatrix(default.ct.point.pred, valid.df$Loan_Status)
