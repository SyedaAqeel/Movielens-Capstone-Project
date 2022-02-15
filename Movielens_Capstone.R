####################################
#MOVIE RECOMMENDATION CAPSTONE PROJECT 
####################################


##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)


#################################
# Examining edx Data Set
#################################

# Number of rows and columns in edx data set 
dim(edx)

# Structure of the data set
str(edx)

#Number of distinct movies in edx data set
n_distinct(edx$movieId)

# Number of distinct users in edx data set
n_distinct(edx$userId)

# Number of distinct ratings given to movies in edx data set
n_distinct(edx$rating)

# Range of ratings in edx data set
range(edx$rating)

# Number of distinct genres
n_distinct(edx$genres)


###########################
# Loading required libraries 
###########################


library(dplyr)
library(lubridate)
library(ggthemes)
library(scales)



###########################
# Tidying edx data set 
###########################

# Extracting Dates 

edx <- mutate(edx, date = as_datetime(timestamp))


# Extracting year in which movie released 

edx <- edx %>% mutate(title = str_trim(title)) %>%
  extract(title, c("Tp", "release"),
  regex = "^(.*) \\(([0-9 \\-]*)\\)$",remove = F)%>%
  mutate(release = if_else(str_length(release) > 4,
  as.integer(str_split(release, "-", simplify = T)[1]), as.integer(release)))%>%
  mutate(title = if_else(is.na(Tp), title, Tp)) %>%
  select(-Tp)


#############################
# Exploring Movies data
#############################


# Plot histogram of distribution of movies

edx %>% count(movieId) %>% ggplot(aes(n)) +
   geom_histogram(bins = 30, color = "white") +
   scale_x_log10() + 
   ggtitle("Distribution of Movies") +
   xlab("Number of Ratings") +
   ylab("Number of Movies") + 
   theme_stata()

# Plot box plot of ratings for movies in release year

 edx %>% group_by(movieId) %>%
  summarize(Ratings = n(), Year = as.character(release)) %>%
  qplot(Year, Ratings, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  ggtitle("Ratings in Year of Release") +
  theme_stata(base_size = 4)


#############################
# Exploring Users Data
#############################

# Plot histogram of distribution of users

edx %>% count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bin = 30, color = "white") +
  scale_x_log10() + 
  ggtitle("Users") +  theme_stata()

# Plot heatmap of users and movies 

users <- sample(unique(edx$userId), 100)
edx %>% filter(userId %in% users) %>%
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% 
  select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")

###############################
# Exploring Ratings Data
###############################

# Distribution of ratings in edx data set 

edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line() +
  ggtitle("Rating Distribution") +
  scale_y_log10() +
  xlab("Rating") +
  ylab("Count") +
  theme_stata()

# Count of ratings received

edx %>% group_by(rating) %>% summarize(n=n())


# Total duration of ratings

tibble(`First Date of Record` = date(as_datetime(min(edx$timestamp), origin="1970-01-01")),
       `Last Date of Record` = date(as_datetime(max(edx$timestamp), origin="1970-01-01"))) %>%
        mutate(Duration = duration(max(edx$timestamp)-min(edx$timestamp)))

# Plot histogram of rating distribution per year

edx %>% mutate(year = year(date)) %>%
  ggplot(aes(year)) + 
  geom_histogram() +
  ggtitle("Rating Distribution Per Year") +
  xlab("Year") +
  ylab("Number of Ratings") +
  scale_y_continuous(labels = comma) +
  theme_stata()

# Trend in Ratings

edx %>% mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth() + ggtitle("Trend in Ratings") + theme_stata()


################################
# Exploring Genres Data
################################

# Top 10 movie genres
edx %>% group_by(genres) %>% 
  summarise(n=n()) %>%
  arrange(desc(n)) %>% top_n(10)

# Top 10 rated genres Error Bar
edx %>% group_by(genres) %>%
  summarize(n = n(), Avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  arrange(desc(n)) %>% slice(1:10) %>% 
  mutate(Genres = reorder(genres, Avg)) %>%
  ggplot(aes(x = Genres, y = Avg, ymin = Avg - 2*se, ymax = Avg + 2*se)) + 
  geom_point() + 
  geom_errorbar() + 
  ggtitle("Top 10 Rated Genres Error Bar ") +         
  theme_stata() 

# 10 Worst Genres
edx %>% group_by(genres) %>% 
  summarise(n=n()) %>%
  arrange(n) %>% top_n(-10)


# Worst 10 rated genres Error Bar
edx %>% group_by(genres) %>%
  summarize(n = n(), Avg = mean(rating),se = sd(rating)/sqrt(n())) %>%
  arrange(n) %>% slice(1:10) %>% 
  mutate(Genres = reorder(genres, Avg)) %>%
  ggplot(aes(x = Genres, y = Avg, ymin = Avg - 2*se,ymax = Avg + 2*se)) +
  geom_point() +
  geom_errorbar() +  
  ggtitle("Worst 10 Rated Genres Error Bar ") +
  theme_stata(base_size = 8)

#######################################
# Partition edx in to Train & Test Set
#######################################

set.seed(1, sample.kind="Rounding")

# Test set is 10% of edx 

test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

# Ensure userId and movieId in test set are also in train set

test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test set back into train set

removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

#######################################################




#####################################
# Define Root Mean Square Error (RMSE)
#####################################

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
##################################################




##################################################
# Method - Linear Model
##################################################

#Linear model is estimated with this equation 

# y_hat = mu + bi + bu + epsilon u,i

# where mu = average ratings, bi = movie effect, bu = user effect, epsilon = error term 



#Naive Bayes Approach
############################

# y_hat = mu + epsilon u,i

# model assumes same rating for all movies and users with differences explained by random variation in error term.

mu_hat <- mean(train_set$rating)

naive_rmse <- RMSE(test_set$rating, mu_hat)

# create table to store results of approaches applied & respective RMSE

result <- data.frame(Method = "Naive Bayes Approach", RMSE = naive_rmse)

result


# Movie Effect Bias
#################################

# y_hat <- mu + bi + epsilon u, i

# model weights in average rating for movie i since different movies are rated differently 

 
mu <- mean(train_set$rating)

# determining Movie Effect Bias

b_i <- train_set %>% 
  group_by(movieId) %>% 
  summarize(bi = mean(rating - mu))

qplot(bi, data = b_i , bins = 10, color = I("black")) +
  ggtitle("Movie Effect Distribution") +
  xlab("Movie Effect") +
  ylab("Count") +
  scale_y_continuous(labels = comma) + theme_stata()


predicted_ratings <- mu + test_set %>% 
  left_join(b_i, by='movieId') %>%
  pull(bi)

result <- bind_rows(result,
                    tibble(Method = "Movie Effect Model ",
                           RMSE = RMSE(test_set$rating, predicted_ratings)))
result                         

# User Effect Bias
################################

# y_hat = mu + bi + bu + epsilon u,i

# model weights in variation in ratings because different users rate differently
# determining user effect bias 

b_u <- train_set %>% 
  left_join(b_i, by='movieId') %>%
  group_by(userId) %>%
  summarize(bu = mean(rating - mu - bi))

qplot(bu, data = b_u , bins = 10, color = I("black")) +
  ggtitle("User Effect Distribution") +
  xlab("User Effect") +
  ylab("Count") +
  scale_y_continuous(labels = comma) + theme_stata()

predicted_ratings <- test_set %>% 
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  mutate(pred = mu + bi + bu) %>%
  pull(pred)

result <- bind_rows(result,
                    tibble(Method = "Movie & User Effect Model ",
                           RMSE = RMSE(test_set$rating, predicted_ratings)))
result

######################################
# Method - Regularized Movie & User Model
#######################################

# penalize large sets of estimates that come from small sample of ratings given to movies and ratings given by users
# use cross validation to pick lamda

lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  bi <- train_set %>% 
    group_by(movieId) %>%
    summarize(bi = sum(rating - mu)/(n()+l))
  
  bu <- train_set %>% 
    left_join(bi, by="movieId") %>%
    group_by(userId) %>%
    summarize(bu = sum(rating - bi - mu)/(n()+l))
  
  predicted_ratings <- 
    test_set %>% 
    left_join(bi, by = "movieId") %>%
    left_join(bu, by = "userId") %>%
    mutate(pred = mu + bi + bu) %>%
    pull(pred)
  
  return(RMSE(test_set$rating, predicted_ratings))
})

qplot(lambdas, rmses) + ggtitle("Regularization Parameter - Lamda") + theme_stata()

lambda <- lambdas[which.min(rmses)]

lambda

regularized_rmse <- min(rmses)

regularized_rmse

result <- bind_rows(result, data.frame(Method = "Regularized Movie & User Effect Model ", RMSE = regularized_rmse))

result

#######################################
# Method - Matrix Factorization 
#######################################

# model factorize the matrix of residuals r into a vector p and vector q 
# r approximately equals to pq
# this approach explains more of the variance in model we used earlier like this
# y_hat = mu + bi + bu + pq + epsilon 
# where p is user rating score matrix 
#       q is movie rating score matrix

#using Recommender System 

if(!require(recosystem))
  install.packages("recosystem", repos = "http://cran.us.r-project.org")


# filtering required inputs

#user_index - integer vector giving the user indices of rating scores
#item_index - integer vector giving the item indices of rating scores
#rating     - numeric vector of the observed entries in the rating matrix

train_data <- with(train_set, data_memory(user_index = userId,
                                          item_index = movieId,
                                          rating = rating))
test_data <- with(test_set, data_memory(user_index = userId,
                                        item_index = movieId,
                                        rating = rating))
# r is object returned by Reco()

r <- Reco()

# Using cross validation to tune the model parameters
# using default values for tuning 
opts <- r$tune(train_data, opts = list(dim = c(10, 20),
                                          costp_l1 = c(0, 0.1),
                                          costp_l2 = c(0.01, 0.1),
                                          costq_l1 = c(0, 0.1),
                                          costq_l2 = c(0.01, 0.1),
                                          lrate = c(0.01, 0.1)))

# training model

r$train(train_data, opts = c(opts$min, nthread = 1, niter = 20))


# predicting ratings 

predicted_ratings <- r$predict(test_data, out_memory())

result <- bind_rows(result,
                    tibble(Method = "Matrix Factorization Model ",
                           RMSE = RMSE(test_set$rating, predicted_ratings)))
result

###########################################################################

# result shows Matrix Factorization Method achieved Root Mean Square Error of 0.7903594
# this is lowest of all RMSE calculated 
# thus Matrix Factorization will be best approach to Train edx data set





#################################################################

# Final Model 

# Matrix Factorization - Edx & Validation Set  

#################################################################

set.seed(1, sample.kind = "Rounding")
library(recosystem)

# filtering required inputs from edx set & validation set

edx_rs <- with(edx, data_memory(user_index = userId,
                                  item_index = movieId,
                                  rating = rating))
validation_rs <- with(validation, data_memory(user_index = userId,
                                                item_index = movieId,
                                                rating = rating))
# model object r returned by Reco()

r <- Reco()

# Using cross validation to tune the model parameters
# using default values for tuning 

options <- r$tune(edx_rs , opts = list(dim = c(10, 20),
                                          costp_l1 = c(0, 0.1),
                                          costp_l2 = c(0.01, 0.1),
                                          costq_l1 = c(0, 0.1),
                                          costq_l2 = c(0.01, 0.1),
                                          lrate = c(0.01, 0.1)))

# training model 

r$train(edx_rs, opts = c(options$min, nthread = 1, niter = 20))


# predicting ratings 

predicted_ratings <- r$predict(validation_rs, out_memory())

result <- bind_rows(result,
                    tibble(Method = "Final Matrix Factorization Model ",
                           RMSE = RMSE(validation$rating, predicted_ratings)))

result


# RMSE of Final Matrix Factorization Model is 0.7885821 which is much lower than
# evaluation criteria set at 0.86490








