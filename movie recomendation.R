



################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")

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
set.seed(1, sample.kind = "Rounding")
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


# -------------------------- Exploratory Analysis --------------------------------

# Data Exploratory------------------------------

 # general about edx
 dim(edx)
 head(edx)
 summary(edx)

 # number of different users and movies 
 edx %>% summarize(users = n_distinct(userId),
                   movies = n_distinct(movieId))

 # number of different genres conbinations
 edx %>% summarize(genres = n_distinct(genres))
 
 
 # which is the most rated genre
 g <- c("Drama","Comedy","Thriller","Romance", "Horror", "Action")
 sapply(g, function(z)
           sum(str_detect(edx$genres, z)))
 
 
 # the most rated movies
 edx %>% group_by(movieId, title) %>%
   summarize(count = n()) %>%
   arrange(desc(count))%>% head()


 # the best movie
 
 # the highest rate have movie with one or couple of rating
 edx %>% group_by(movieId, title) %>% 
   summarise(avg = mean(rating), n = n()) %>% 
   arrange(desc(avg)) %>% head()
   
 # The first 6 movies with the highest average rating and number of rating
 # (among those rated more than 100 times)
 edx %>% group_by(movieId, title) %>% 
   summarise(a = mean(rating), n = n()) %>% 
   filter(n > 100) %>% 
   arrange(desc(a)) %>% head()
 
 
 
# Data Preprocessing----------------------------
 
 # separating the year released from the movie title
 

 # title structure
 head(edx$title)
 
 # does each title have the same structure
 sum(str_detect(edx$title,"\\(\\d{4}\\)$")) == length(edx$title)
 
 # cutting off the year
 year <- str_extract(edx$title,"\\(\\d{4}\\)$") %>% str_extract("\\d{4}") 
 
 # removing year from the title
 title <- str_remove(edx$title,"\\(\\d{4}\\)$") # 
 
 # new edx set with separate title and year released
 new_edx <- edx %>% 
            select(-title) %>%  
            mutate(year = year, title = title)
 
 #timestamp into date and time form

 new_edx <- new_edx %>% 
              mutate(timestamp = as_datetime(timestamp))
 
 head(new_edx)
 


# Exploratory Visualization---------------------

 # distribution of movie rating
 
 new_edx %>%
   ggplot(aes(x = rating) ) +
   geom_bar(fill = "cadetblue", color = "gray40") +
   ggtitle("Movie ratings") 
 
 # some movies get rated more than others
 p1 <- new_edx %>% 
         group_by(movieId) %>% 
         summarise(n=n()) %>% 
         ggplot(aes(n)) +
         geom_histogram(bins = 40, color = "grey40", fill = "cadetblue") +
         scale_x_log10() +
         ggtitle("Movie ")
 
 # some users are more active than others at rating movies
 p2 <- new_edx %>% 
         group_by(userId) %>% 
         summarise(n=n()) %>% 
         ggplot(aes(n)) +
         geom_histogram(bins = 40, color = "grey40", fill = "cadetblue")  +
         scale_x_log10() + 
         ggtitle("Users")  
 
 
 grid.arrange(p1, p2, ncol = 2)
 
 # Average rating by genres
 new_edx %>% group_by(genres) %>%
   summarize(n = n(),avg = mean(rating)) %>% 
   filter(n >= 40000) %>% 
   mutate(genres = reorder(genres, avg)) %>%
   ggplot(aes(x = genres, y = avg)) +  
   geom_point() +
   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
   ggtitle("Average rating by genres")


 
 
 # movies that came out after 1993 get more ratings and after 
 # 1993 the number of ratings decreases over year
 new_edx %>%  group_by(movieId) %>% 
   filter(year > "1970") %>% 
   summarize(n = n(), year = first(year))%>%
   qplot(year, n, data = ., geom = "boxplot") +
   coord_trans(y = "sqrt") +
   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
   ggtitle("Count of rating over years")
 
 
  
# -------------------------- Results --------------------------------
 
# predictive analysis  ---------------------------


 # creating test set and train set from new_edx set  
 ind <- createDataPartition(new_edx$rating, 1, 0.2, list=FALSE)
 temp <- new_edx[ind,]
 train <- new_edx[-ind,]

 # make sure userId and movieId in test set are also in train set
 test <- temp %>% 
   semi_join(train, by = "movieId") %>%
   semi_join(train, by = "userId")

 # add rows removed from test set back into train set
 removed <- anti_join(temp, test)
 train <- rbind(train, removed)

 #function for calculation RMSE-Residual Mean Squared Error 
 RMSE <- function(true, predicted){
   sqrt(mean((true - predicted)^2))
 }

 # model building


  # basic model (just average rating predicted)
  mu_hat <- mean(train$rating)
  error1 <- RMSE(test$rating, mu_hat)

  tbl <- tibble(model = "basic model (train/test)", 
                RMSE = error1)
  tbl %>% kable()
  
  # model with movie effect (some movies are rated higher than others)
  f <- train %>% group_by(movieId) %>% 
                 summarize(b_i = mean(rating - mu_hat))

  # plot of movie effect
  f %>% ggplot(aes(b_i)) + 
        geom_histogram(bins = 40, fill = "cadetblue",color = "grey40") +
        ggtitle("Movie effect") 
      
  # constructing predictors
  predict_movie <- test %>% 
                    left_join(f, by="movieId") %>% 
                    mutate(pred= mu_hat + b_i) %>% pull(pred)  

  # RMSE for movie effecet
  error2 <- RMSE(test$rating, predict_movie)

  tbl <- bind_rows(tbl, tibble(model = "movie effect (train/test)", RMSE = error2))
  tbl %>% kable()
  
  
 #plot of user effect
  train %>% 
    group_by(userId) %>% 
    summarize(b_u = mean(rating)) %>% 
    filter(n()>=100) %>%
    ggplot(aes(b_u)) + 
    geom_histogram(bins = 40, color = "grey40", fill = "cadetblue") +
    ggtitle("User effect") 
  
   # model with movie and user effect
  k <- train %>% left_join(f, by = "movieId") %>% group_by(userId) %>%
     summarise(b_u = mean(rating-(mu_hat + b_i)))

  # constructing predictors
  predict_fk <- test %>% 
                left_join(f, by = "movieId") %>%
                left_join(k, by = "userId") %>% 
                mutate(pred = mu_hat  + b_i + b_u) %>%  pull(pred)

  # RMSE for movie-user effect
  error3 <- RMSE(test$rating, predict_fk)

  tbl <- bind_rows(tbl, tibble(model = "movie-user effect (train/test)", RMSE = error3))
  tbl %>% kable()
  
# model regularization and tuning parameter with 5-fold cross validation (using bootstrap version)

 # create 5 new pairs test-train sets and choose the best penalty parameter
 i <- createDataPartition(train$rating, 5, 0.2, list=FALSE)
 N <- 1:5

 # finding the best penalty parameter (using bootstramp)
 best_lambde <- sapply(N,function(n){
      
      # creatin test1 set and train1 set from train set  
      temp <- train[i[,n],]
      train1 <- train[-i[,n],]
  
      # Make sure userId and movieId in test1 set are also in train1 set
      test1 <- temp %>% 
        semi_join(train1, by = "movieId") %>%
        semi_join(train1, by = "userId")
  
      # Add rows removed from test1 set back into train1 set
      removed <- anti_join(temp, test1)
      train1 <- rbind(train1, removed)
  
      # tuning parameter on train1 set and test1 set
      avg1 <- mean(train1$rating)
      lambda <- seq(2,8,0.25)
      
      # errors for every parameter
      errors <- sapply(lambda, function(l){
            f <- train1 %>% 
                   group_by(movieId) %>% 
                   summarise(b_i = sum(rating - avg1)/(n()+l))
            
            k <- train1 %>% 
                   left_join(f, by = "movieId") %>% 
                   group_by(userId) %>% 
                   summarise(b_u = sum(rating-(avg1 + b_i))/(n()+l))
            
            pred_fk <- test1 %>% 
                         left_join(f, by="movieId") %>% 
                         left_join(k, by = "userId") %>% 
                         mutate(pred = avg1 + b_i + b_u) %>%
                         pull(pred)
    
            RMSE(pred_fk, test1$rating)
                                          })
      c(err=min(errors), lamb=lambda[which.min(errors)])
  
                              })

 # the best parameter after 5-fold cross validation
   best_lambdas <- data.frame(t(best_lambde))
   l <- best_lambdas %>% arrange(err) %>% pull(lamb) %>% .[1]
   l

  # apply the best parameter on trainig set and test set
  f <- train %>%
         group_by(movieId) %>% 
         summarise(b_i = sum(rating - mu_hat) / (n() + l))
  
  
  k <- train %>% 
         left_join(f, by = "movieId") %>% 
         group_by(userId) %>%
         summarise(b_u = sum(rating - (mu_hat + b_i)) / (n() + l))
  
  # constructing predictors
  pred_fk <- test %>% 
               left_join(f, by = "movieId") %>% 
               left_join(k, by = "userId") %>%
               mutate(pred = mu_hat + b_i + b_u) %>% 
               pull(pred)
  
  # RMSE for regularization model
  error4 <- RMSE(pred_fk, test$rating)

 #table of errors for comparation
   tbl <- bind_rows(tbl, tibble(model="regularization (train/test)", RMSE = error4))
   tbl %>% kable()


################################################################################

# applying final model regularization on new_edx set to obtain the final RMSE

################################################################################

  
  mu_hat <- mean(new_edx$rating)
 
  f <- new_edx %>% 
           group_by(movieId) %>% 
           summarise(b_i = sum(rating - mu_hat)/(n()+l))
 
  k <- new_edx %>%
           left_join(f, by = "movieId") %>% 
           group_by(userId) %>%
           summarise(b_u = sum(rating - (mu_hat + b_i)) / (n() + l))
 
  pred_fk <- validation %>% 
                left_join(f, by = "movieId") %>% 
                left_join(k, by = "userId") %>%
                mutate(pred = mu_hat + b_i + b_u) %>% 
                pull(pred)
 
  final_error <-  RMSE(pred_fk, validation$rating)

  tbl <- bind_rows(tbl, tibble(model = "regularization (edx/validation)", RMSE = final_error))
  tbl %>% kable()

  
