#load library
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

#upload data and create data set
Games <- read_csv("https://raw.githubusercontent.com/viguerrero/Games-Sales-Analysis/master/Video_Games_Sales_as_at_22_Dec_2016.csv")

#convert data.frame to tibble
Games <- as_tibble(Games)

#Confirm Class
class(Games)

#Numbre of rows and colums and column names
nrow(Games)
ncol(Games)
names(Games)

#Confirm unique elements in Name column, platform, Genre, Publisher and Developer
n_distinct(Games$Name)
n_distinct(Games$Platform)
n_distinct(Games$Publisher)
n_distinct(Games$Genre)
n_distinct(Games$Developer)

#Remove from data set incomplete data and N/As
Games <- Games[complete.cases(Games),]

#Confirm no N/As in data
any(is.na(Games))

#See changes in unique values
n_distinct(Games$Name)
n_distinct(Games$Platform)
n_distinct(Games$Publisher)
n_distinct(Games$Genre)
n_distinct(Games$Developer)

#Create Vector of duplicate names, in alpabetical order
duplicates.games <- sort(Games$Name[duplicated(Games$Name)])

#View reason why there are duplicates of a specific game
duplicates.view1 <- Games %>% filter(Name==duplicates.games[1])

#Calculate mean sales by Platform and plot the findings
Platform_sales <- Games %>% 
  group_by(Platform) %>% 
  summarize(Sales.p = mean(Global_Sales))

ggplot(Platform_sales, aes(x=Platform,y = Sales.p)) +geom_bar(stat = "identity")


#Calculate mean sales by Genre and plot the findings
Genre_sales <- Games %>% 
  group_by(Genre) %>% 
  summarize(Sales.p = mean(Global_Sales))
ggplot(Genre_sales, aes(x=Genre,y = Sales.p)) +geom_bar(stat = "identity")


#Calculate mean sales by Genre and plot the findings
Year_release_sales <- Games %>% 
  group_by(Year_of_Release) %>% 
  summarize(Sales.p = mean(Global_Sales))
ggplot(Year_release_sales, aes(x=Year_of_Release,y = Sales.p)) +geom_bar(stat = "identity")


#See games released per year
Games_per_year <- Games %>% 
  group_by(Year_of_Release) %>%
  count(Year_of_Release)

#Understand why N/As for year appear in data
Games_per_year.Na <- Games %>% 
  group_by(Year_of_Release) %>%
  filter(Year_of_Release== "N/A")

#Determine Total Sales per Year of release and plot findings
Sales_per_Year <- Games %>% 
  group_by(Year_of_Release) %>% 
  summarize(Sales = sum(Global_Sales))
ggplot(Sales_per_Year, aes(x=Year_of_Release,y = Sales)) +geom_bar(stat = "identity")

#See Genres per Year
Genres_per_year <- Games %>% 
  group_by(Year_of_Release,Genre) %>%
  count(Year_of_Release,Genre)

#See Misc Games per Year
Misc_per_year <- Genres_per_year %>% filter(Genre == "Misc")

ggplot(Misc_per_year, aes(x=Year_of_Release,y = n)) +geom_bar(stat = "identity")

#Calculate Mean per Game Rating
Rating_sales <- Games %>% 
  group_by(Rating) %>% 
  summarize(Sales.p = mean(Global_Sales))
ggplot(Rating_sales, aes(x=Rating,y = Sales.p)) + geom_bar(stat = "identity")

#Calculate Mean per publisher and see top 10 publishers
Publisher_sales <- Games %>% 
  group_by(Publisher) %>% 
  summarize(Sales.p = mean(Global_Sales))

Publisher_sales %>% 
  top_n(n=10) %>% 
  arrange(desc(Sales.p))

#Calculate mean per developer and see top 10 Developers
Developer_sales <- Games %>% 
  group_by(Developer) %>% 
  summarize(Sales.p = mean(Global_Sales))

Developer_sales %>% 
  top_n(n=10) %>% 
  arrange(desc(Sales.p))

#Determine what kind of games developers create
Developers.genres <- Games %>% 
  count(Developer, Genre) %>%
  group_by(Developer)

#Verifying top developer
Good.sc.studio.genres <- Developers.genres %>%
  filter(Developer=="Good Science Studio")

view(Good.sc.studio.genres)

#Verifying a well known creator
Developers.genres %>%
  filter(Developer=="Nintendo")

#Plot Crit_Score and Global_Sales
Games %>%
  ggplot(aes(Critic_Score, Global_Sales)) +
  geom_point(alpha = 0.5)


#Find Correlation between Crit_Score and Global_Sales
cor(Games$Global_Sales,Games$Critic_Score)

#Find correlation between Crit_count and Global Sales
cor(Games$Global_Sales,Games$Critic_Count)

#Convert User_score to numeric and find correlation with Global_Sales
Games <- Games %>% mutate(User_Score= as.numeric(User_Score))
class(Games$User_Score)
cor(Games$Global_Sales,Games$User_Score)

#Find correlation between User_count and Global Sales
cor(Games$Global_Sales,Games$User_Count)

#Find User and Crit Score per Genre
Score_Per_Genre<- Games %>% 
  group_by(Genre) %>%
  summarize(Critics= mean(Critic_Score), Users= mean(User_Score))

View(Score_Per_Genre)

#Get all Misc Genre data
All_Misc <- Games %>% 
  filter(Genre=="Misc")

#Correlation of critic_score and User Score in Misc Games
cor(All_Misc$Global_Sales,All_Misc$Critic_Score)
cor(All_Misc$Global_Sales,All_Misc$User_Score)
cor(All_Misc$Global_Sales,All_Misc$Critic_Count)
cor(All_Misc$Global_Sales,All_Misc$User_Count)

#Get all shooter Genre data and Correlation of critic_score and User Score
All_shooter <- Games %>% 
  filter(Genre=="Shooter")

cor(All_shooter$Global_Sales,All_shooter$Critic_Score)
cor(All_shooter$Global_Sales,All_shooter$User_Score)
cor(All_shooter$Global_Sales,All_shooter$Critic_Count)
cor(All_shooter$Global_Sales,All_shooter$User_Count)

#Get Role-Playing Genre data and Correlation of critic_score and User Score
All_Role_Playing <- Games %>% 
  filter(Genre=="Role-Playing")

cor(All_Role_Playing$Global_Sales,All_Role_Playing$Critic_Score)
cor(All_Role_Playing$Global_Sales,All_Role_Playing$User_Score)
cor(All_Role_Playing$Global_Sales,All_Role_Playing$Critic_Count)
cor(All_Role_Playing$Global_Sales,All_Role_Playing$User_Count)

#Get Strategy Genre data and Correlation of critic_score and User Score
All_Strategy <- Games %>% 
  filter(Genre=="Strategy")

cor(All_Strategy$Global_Sales,All_Strategy$Critic_Score)
cor(All_Strategy$Global_Sales,All_Strategy$User_Score)
cor(All_Strategy$Global_Sales,All_Strategy$Critic_Count)
cor(All_Strategy$Global_Sales,All_Strategy$User_Count)


#Divide data into Train and Test set after setting the seed
set.seed(1)
y <- Games$Global_Sales
test_index <- createDataPartition(y, times = 1, p = 0.1, list = FALSE)
test_set <- Games %>% slice(test_index)
train_set <- Games %>% slice(-test_index)

#Find train Set mean
mu <- mean(Games$Global_Sales)

#Create Lambda and RMSE check
lambdas <- seq(0, 10, 0.25)

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Determine best lambda value
rmses <- sapply(lambdas, function(l){
  b_i <- train_set %>%
    group_by(Platform) %>%
    summarize(b_i = sum(Global_Sales - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="Platform") %>%
    group_by(Genre) %>%
    summarize(b_u = sum(Global_Sales - b_i - mu)/(n()+l))
  predicted_Sales <- 
    test_set %>% 
    left_join(b_i, by = "Platform") %>%
    left_join(b_u, by = "Genre") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  return(RMSE(test_set$Global_Sales, predicted_Sales))
})

#Determine lowest lambda value
lambda <- lambdas[which.min(rmses)]
lambda

#visualize lambda values
qplot(lambdas, rmses) 

#New lambda Value
lambdas <- seq(10, 100, 10)

#Rerun of lambda Analysis
rmses <- sapply(lambdas, function(l){
  b_i <- train_set %>%
    group_by(Platform) %>%
    summarize(b_i = sum(Global_Sales - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="Platform") %>%
    group_by(Genre) %>%
    summarize(b_u = sum(Global_Sales - b_i - mu)/(n()+l))
  predicted_Sales <- 
    test_set %>% 
    left_join(b_i, by = "Platform") %>%
    left_join(b_u, by = "Genre") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  return(RMSE(test_set$Global_Sales, predicted_Sales))
})

#New check of lambda plot
qplot(lambdas, rmses) 

lambda <- lambdas[which.min(rmses)]
lambda

#New lambda Value
lambdas <- seq(100, 300, 10)

#Rerun of lambda Analysis a 2nd time
rmses <- sapply(lambdas, function(l){
  b_i <- train_set %>%
    group_by(Platform) %>%
    summarize(b_i = sum(Global_Sales - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="Platform") %>%
    group_by(Genre) %>%
    summarize(b_u = sum(Global_Sales - b_i - mu)/(n()+l))
  predicted_Sales <- 
    test_set %>% 
    left_join(b_i, by = "Platform") %>%
    left_join(b_u, by = "Genre") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  return(RMSE(test_set$Global_Sales, predicted_Sales))
})

#New check of lambda plot
qplot(lambdas, rmses) 

lambda <- lambdas[which.min(rmses)]
lambda

#Check with Rating and Genre combination keep lambda values
rmses <- sapply(lambdas, function(l){
  b_i <- train_set %>%
    group_by(Rating) %>%
    summarize(b_i = sum(Global_Sales - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="Rating") %>%
    group_by(Genre) %>%
    summarize(b_u = sum(Global_Sales - b_i - mu)/(n()+l))
  predicted_Sales <- 
    test_set %>% 
    left_join(b_i, by = "Rating") %>%
    left_join(b_u, by = "Genre") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  return(RMSE(test_set$Global_Sales, predicted_Sales))
})

#New check of lambda plot
qplot(lambdas, rmses) 


#Final predictive algorithm
b_i <- train_set %>%
  group_by(Platform) %>%
  summarize(b_i = sum(Global_Sales - mu)/(n()+lambda))
b_u <- train_set %>% 
  left_join(b_i, by="Platform") %>%
  group_by(Genre) %>%
  summarize(b_u = sum(Global_Sales - b_i - mu)/(n()+lambda))
predicted_Sales <- 
  test_set %>% 
  left_join(b_i, by = "Platform") %>%
  left_join(b_u, by = "Genre") %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred
RMSE(test_set$Global_Sales, predicted_Sales)

#Variance in sales
max(Games$Global_Sales)-min(Games$Global_Sales)


