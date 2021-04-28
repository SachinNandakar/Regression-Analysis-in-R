# clear the workspace
rm(list = ls())

# Include the following packages:
library(dplyr)
library(ggplot2)

# We are using the inbuilt dataset "iris" of RStudio
# Brief on scatter plot, before proceeding to regression models
View(iris)
ggplot(data=iris,
       mapping = aes(x=Sepal.Length, y=Petal.Length,
                     color=Species))+
  geom_point()


model_SLPL <- Sepal.Length~Petal.Length
lin_fit_SLPL <- lm(model_SLPL,iris)
lin_fit_SLPL

# Run the plot function multiple times to visualise different plots
plot(lin_fit_SLPL)

# Predicting the values: using predict() function taking sample value
sample_PL <- data.frame(Petal.Length=4.5)
res_SL <- predict(lin_fit_SLPL, sample_PL)
print(res_SL)



# Another Approach of splitting data into Training & testing
# to train and & evaluate the model

#install.packages("caTools")  #Uncomment & run code if package is not installed prior
library(caTools)

# Use iris dataset from data file
data <- iris
# Divide 'data' into Training data & Testing data (70:30)

data_split <- sample.split(data, SplitRatio = 0.7)
train_data <- subset(data, data_split == TRUE)
test_data <- subset(data, data_split == FALSE)

# Now that our data has been split into training and test 
# set, we implement our linear modeling model as follows:

model <- lm(Sepal.Length~Petal.Length, data = train_data)
res_SL <- predict(model,test_data)
res_SL

# Glance on the values of Predicted, Estimated & Difference
print(res_SL)
print(test_data[,1])

diff=res_SL-test_data[,1]
print(diff)

# The summary of the Regression model 
summary(model)




# MULTIPLE LINEAR REGRESSION
# To model, we are refering the 'mtcars' dataset of Rstudio
View(mtcars)

# FIT MLR " mpg = disp + drat + wt "
model_mul <- mpg~disp+drat+wt
mul_lin_fit <- lm(model_mul, mtcars)

mul_lin_fit

# Additional Summary Information
summary(mul_lin_fit)
