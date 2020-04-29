library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)

################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
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

rm(dl, ratings, movies, test_index, temp, movielens, removed)



# exploratory analysis ----------------------------------------


#general about edx
summary(edx)
dim(edx)
head(edx)

#number of each rating
table(edx$rating)
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()

#number of different movies and users
edx %>% summarize(users = n_distinct(userId),
                  movies = n_distinct(movieId))

#number of each rating in some genre
zanr <- c("Drama","Comedy","Thriller","Romance")
sapply(zanr, function(z)
  sum(str_detect(edx$genres, z)))

#the most rated movies
edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

#separating the year from the movie title
#title structure
head(edx$title)

#does each title have the same structure
sum(str_detect(edx$title,"\\(\\d{4}\\)$")) == length(edx$title)

year <- str_extract(edx$title,"\\(\\d{4}\\)$") %>% str_extract("\\d{4}")
title <- str_remove(edx$title,"\\(\\d{4}\\)$")

#new edx set with title and year separated
edx <- edx %>% select(-title) %>%  mutate(year=year, title=title)




# distribution of predictor -----------------------------------------------

#movie rating effect (some movies get rated more than others)  
edx %>% group_by(movieId) %>% summarise(n=n()) %>% ggplot(aes(n)) +
  geom_histogram(bins = 50) +
  scale_x_log10() +
  ggtitle("Movie")

#user rating effect (some users are more active than others at rating movies)
edx %>% group_by(userId) %>% summarise(n=n()) %>% ggplot(aes(n)) +
  geom_histogram(bins = 80)  +
  scale_x_log10() + 
  ggtitle("Users")  

# time rating effect (movies that came out after 1993 get more ratings and after 
# 1993 the number of ratings decreases over year)

edx %>%  group_by(movieId) %>% filter(year > "1970") %>% 
  summarize(n = n(), year = first(year))%>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#genres rating effect(some generes get rated more than others))
edx %>% group_by(genres) %>%
  summarize(n = n()) %>% 
  filter(n >= 40000) %>% 
  mutate(genres = reorder(genres, n)) %>%
  ggplot(aes(x = genres, y = n)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# predictive analysis  ----------------------------------------------------------


#creatin test set and train set from edx set
ind <- createDataPartition(edx$rating, 1, 0.2, list=FALSE)
temp <- edx[ind,]
train <- edx[-ind,]
# Make sure userId and movieId in test set are also in train set
test <- temp %>% 
  semi_join(train, by = "movieId") %>%
  semi_join(train, by = "userId")

# Add rows removed from test set back into train set
removed <- anti_join(temp, test)
nrow(removed)
train <- rbind(train, removed)

#function for calculation RMSE-Residual Mean Squared Error
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#basic model (just average rating predicted)
avg <- mean(train$rating)
error1 <- RMSE(test$rating,avg)

#model with movie effect
f <- train %>% group_by(movieId) %>% summarize(b_f = mean(rating-avg))

f %>% ggplot(aes(b_f)) + geom_histogram(bins = 20)

predict_movie <- test %>% left_join(f, by="movieId") %>% mutate(pred= avg+b_f) %>% pull(pred)  
error2 <- RMSE(test$rating, predict_movie)


#model with movie and user effect
k <- train %>% left_join(f, by = "movieId") %>% group_by(userId) %>%
  summarise(b_k = mean(rating-(avg + b_f)))

k %>% ggplot(aes(b_k)) + geom_histogram(bins = 30)

predict_fk <- test %>% left_join(f, by = "movieId") %>%
  left_join(k, by = "userId") %>% 
  mutate(b_fk = avg  + b_f + b_k) %>%  pull(b_fk)
error3 <- RMSE(test$rating, predict_fk)

tbl <- data_frame(model= c("basic model","movie effect", "movie-user effect"), 
                     RMSE=c(error1,error2,error3))
tbl %>% knitr::kable()


#model  regularization and tuning parameter with 5-fold cross validation

#create 5 new pairs test-train sets and choose the best penalty parameter

i <- createDataPartition(train$rating, 5, 0.2, list=FALSE)
N <- 1:5

best_lambde <- sapply(N,function(n){
  
  temp <- train[i[,n],]
  train1 <- train[-i[,n],]
  
  test1 <- temp %>% 
    semi_join(train1, by = "movieId") %>%
    semi_join(train1, by = "userId")
  
  # Add rows removed from test set back into train set
  removed <- anti_join(temp, test1)
  train1 <- rbind(train1, removed)
  
  #tuning parameter on train1 set and test1 set
  avg1 <- mean(train1$rating)
  lambda <- seq(2,8,0.25)
  
  errors <- sapply(lambda, function(l){
    f <- train1 %>% group_by(movieId) %>% summarise(b_f = sum(rating-avg1)/(n()+l))
    k <- train1 %>% left_join(f, by = "movieId") %>% group_by(userId) %>% 
      summarise(b_k=sum(rating-(avg1 + b_f))/(n()+l))
    pred_fk <- test1 %>% left_join(f, by="movieId") %>% left_join(k, by = "userId") %>% 
      mutate(pred = avg1 + b_f + b_k) %>% pull(pred)
    RMSE(pred_fk, test1$rating)
  })
  
  c(err=min(errors), lamb=lambda[which.min(errors)])
  
})

#the best parameter after 5 fold cross validation
best_lambdas <- data.frame(t(best_lambde))
l <- best_lambdas %>% arrange(err) %>% pull(lamb) %>% .[1]
l
#apply the best parameter on trainig set and test set
f <- train %>% group_by(movieId) %>% summarise(b_f = sum(rating-avg)/(n()+l))
k <- train %>% left_join(f, by = "movieId") %>% group_by(userId) %>% 
  summarise(b_k=sum(rating-(avg + b_f))/(n()+l))
pred_fk <- test %>% left_join(f, by="movieId") %>% left_join(k, by = "userId") %>% 
  mutate(pred = avg + b_f + b_k) %>% pull(pred)
error4 <- RMSE(pred_fk, test$rating)


tbl <- bind_rows(tbl, data.frame(model="regularization", RMSE = error4))
tbl %>% knitr::kable()


################################################################################

# applying final model regularization on validation set to obtain the final RMSE

################################################################################

avg <- mean(edx$rating)
f <- edx %>% group_by(movieId) %>% summarise(b_f = sum(rating-avg)/(n()+l))
k <- edx %>% left_join(f, by = "movieId") %>% group_by(userId) %>% 
  summarise(b_k=sum(rating-(avg + b_f))/(n()+l))
pred_fk <- validation %>% left_join(f, by="movieId") %>% left_join(k, by = "userId") %>% 
  mutate(pred = avg + b_f + b_k) %>% pull(pred)
final_error <-  RMSE(pred_fk, validation$rating)

tbl <- bind_rows(tbl, data.frame(model="regularization (edx/validation)", RMSE = final_error))
tbl %>% knitr::kable()

