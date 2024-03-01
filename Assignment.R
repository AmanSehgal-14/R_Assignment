#Loading Libraries
library(tidyverse)
library(scales)
library(plotly)
library(ggcorrplot)

#Setting the directory
setwd("C:/Users/78605/Documents/R")

#Loading the data from csv file
data_ret <- read.csv("walmart.csv",header = TRUE,stringsAsFactors = TRUE)

#Checking the dimensions of the data
dim(data_ret)

#Checking the structure of the data
str(data_ret)

#Taking a peak of the data
head(data_ret)

#checking the summary of the data
summary(data_ret)

#Checking for NA values
colSums(is.na(data_ret))

#handing NA values
#data_ret$Holiday_Flag[is.na(data_ret$Holiday_Flag)] <- round(mean(data_ret$Holiday_Flag,na.rm = TRUE))
filtered_data <- data_ret[data_ret$Date==data_ret[is.na(data_ret$Holiday_Flag),][1,2],]
data_ret$Holiday_Flag[is.na(data_ret$Holiday_Flag)] <- 
  filtered_data$Holiday_Flag[which.max(filtered_data$Holiday_Flag)]

#Checking for NA values after handling it for confirmation
colSums(is.na(data_ret))

#Handling datatype of column Date
typeof(data_ret$Date)
date1 <- as.Date(data_ret$Date,format = '%d-%m-%Y')
date2 <- as.Date(data_ret$Date,format = '%d/%m/%Y')
for(i in 1:length(date1)){
  if(is.na(date1[i])){
    date1[i] <- date2[i]
  }
}

#Checking for NA values in Date list for confirmation
sum(is.na(date1))

#Assigning the final date list to the column
data_ret$Date <- date1

#Checking the datatype of Date Column
str(data_ret)

#Making Store number as characters to easily categorise the same
data_ret$Store <- as.character(data_ret$Store)

#Checking for equality in rows of each store
table(data_ret$Store)

#Total number of stores
cat("Number of stores in the data = ",length(unique(data_ret$Store)),sep = '')

#Calculating average weekly sales of Walmart
cat("Average weekly sales = $",round(mean(data_ret$Weekly_Sales),2),sep = '')

#identifying correlation and plotting the same
corr <- round(cor(data_ret[,3:8]),3)
ggcorrplot(corr,title = 'Correlation chart')

#Summarizing data to calculate top stores who made the maximum sales
total_sales <- data_ret %>% 
  group_by(Store) %>% 
  summarise(sum_Sales=sum(Weekly_Sales))
top_total_sales <- head(total_sales %>% arrange(desc(sum_Sales)),n=15)
print(top_total_sales,n=15)

#Plotting the above data (Bar chart)
ggplot(data=top_total_sales, aes(x=Store,y=sum_Sales)) +
  geom_bar(stat="identity",fill='#69b3a2') +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  scale_x_discrete(limits = top_total_sales$Store)+
  labs(title="Top 15 performing stores", 
       x="Store ID", y = "Sales ($)")
  
#Calculating top stores who made the maximum sales during holidays
holiday_sales <- data_ret %>% 
  filter(Holiday_Flag==1)%>% 
  group_by(Store) %>% 
  summarise(sum_Sales=sum(Weekly_Sales))
top_holiday_sales <-head(holiday_sales %>% arrange(desc(sum_Sales)),n=15)
print(top_holiday_sales)

#Plotting the above data(Bar chart)
ggplot(data=top_holiday_sales, aes(x=Store,y=sum_Sales)) +
  geom_bar(stat="identity",fill='#69b3a2') +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  scale_x_discrete(limits = top_holiday_sales$Store)+
  labs(title="Top 15 performing stores (on holidays)", 
       x="Store ID", y = "Sales ($)")

#Calculating year wise total sales
#Adding year column
data_ret <- data_ret %>% 
  mutate(Year=format(data_ret$Date,format='%Y'))
year_wise_sale <- data_ret %>% group_by(Year) %>% summarise(sum_Sales= sum(Weekly_Sales))
year_wise_sale <- year_wise_sale %>% arrange(desc(sum_Sales))
print(year_wise_sale)

#Plotting the above data (pie chart)
ggplot(data=year_wise_sale, aes(x='',y=sum_Sales,fill=Year))+
  geom_bar(stat='identity')+coord_polar('y')+theme_void()+
  scale_fill_brewer(palette="Set1")+ 
  scale_y_discrete(limits = year_wise_sale$Year)+
  geom_text(aes(label = round(sum_Sales/sum(sum_Sales)*100,3)), position = position_stack(vjust = 0.5))+
  labs(title="Year wise sales contribution (Percentage)")

#Calculating weekly total sales
sales <- data_ret %>% select(Date,Weekly_Sales)
trend_sales <- sales %>% group_by(Date) %>% 
  summarise(sum_Sales=sum(Weekly_Sales)) %>% arrange(Date)
trend_sales

#Plotting the above data (line chart)
ggplot(data=trend_sales, aes(x=Date, y=sum_Sales)) +
  geom_line()+
  geom_point()+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  labs(title="Weekly sales of Walmart from 2010 to 2012", 
       x="Week Start Date", y = "Sales ($)")

#Plotting the above data using plotly (Line chart)
plot_ly(trend_sales,x=~Date,y=~sum_Sales,type = "scatter",
        mode = "lines") %>% 
      layout(title = "Weekly sales of Walmart from 2010 to 2012",
        xaxis = list(title = "Week Start Date"),
        yaxis = list (title = "Sales in $"))



  
