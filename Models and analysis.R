library(ggplot2)

install.packages('dplyr')
library(dplyr)

loan.df <- read.csv('Loan_Data.csv', stringsAsFactors = TRUE)

str(loan.df)


ggplot(data = loan.df) + geom_point(aes(x = ApplicantIncome, y = LoanAmount, color = Loan_Status))

ggplot(data = loan.df) + geom_point(aes(x = Loan_Amount_Term, y = LoanAmount))

ggplot(data = loan.df) + geom_bar(aes(x = Property_Area))
