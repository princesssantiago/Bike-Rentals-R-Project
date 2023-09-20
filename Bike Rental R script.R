#Bike Rental
bike<-read.csv('bikeshare.csv') #Read csv file
head(bike) 
#Install and load ggplot library for data visualization
install.packages('ggplot2')
library(ggplot2)
#Create scatterplot visual for Temperature
ggplot(bike,aes(temp,count))+geom_point(alpha=0.2,aes(color=temp))+theme_classic() +ggtitle("Scatterplot of Rental Count by Temperature")
#Convert datetime into POSIXct
bike$datetime <- as.POSIXct(bike$datetime)
#Create scatterplot visual for DateTime
ggplot(bike, aes(x = datetime, y = count, color = temp)) +
  geom_point() +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "datetime", y = "count", color = "temp") +ggtitle("Scatterplot of Rental Count by DateTime")
#Calculate correlation between temperature and count
cor(bike[,c('temp','count')])
#Create Boxplot visual for Season
ggplot(bike,aes(factor(season),count)) + geom_boxplot(aes(color=factor(season))) +theme_classic() + ggtitle("Boxplot of Rental Count by Season")
#Install and load lubridate package for date and time
install.packages("lubridate")
library(lubridate)
#Extract Hour format from the datetime column in the dataset and creates a new column for Hour format
bike$hour <- format(bike$datetime, "%H")
#Creates scatterplot of count and hour based on temp by working day
ggplot(bike[bike$workingday == 1, ], aes(x = hour, y = count, color = temp)) +
  geom_point(alpha=0.2,position = position_jitter(w = 1, h = 0)) +
  scale_color_gradientn(colors = c('blue', 'green', 'red')) + 
  labs(x = "hour", y = "count", color = "temp") +
  ggtitle("Scatterplot of Rental Count by Hour (Working Day)")
#Creates scatterplot of count and hour based on temp by non working day
ggplot(bike[bike$workingday == 0, ], aes(x = hour, y = count, color = temp)) +
  geom_point(alpha=0.2,position = position_jitter(w = 1, h = 0)) +
  scale_color_gradientn(colors = c('blue', 'green', 'red')) + 
  labs(x = "hour", y = "count", color = "temp") +
  ggtitle("Scatterplot of Rental Count by Hour (Non Working Day)")
#Creates and view a linear regression model yo predict count based on temp 
temp.model <- lm(count ~ temp, data = bike)
summary(temp.model)
#Create a new dataframe for the prediction of temperature with only 25 degree Celsius 
new_data <- data.frame(temp = 25)
#Predict count for the new data frame usinf linear regression model
predicted_count_predict <- predict(temp.model, newdata = new_data)
predicted_count_predict
#Convert Hour column to numeric value
bike$hour <- sapply(bike$hour, as.numeric)
#Check the data type of Hour column
str(bike$hour)
#Create a linear regression model  usinf all of the features
full_model <- lm(count ~ ., data = bike)
#Display a summary of the linear regression model
summary(full_model)

