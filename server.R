## server.R

# load functions

## get_user_ratings provided by Prof. Liang

get_user_ratings = function(value_list) {
  dat = data.table( UserID = "99999",
                    MovieID = sapply(strsplit(names(value_list), "_"),
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                    Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

###############System1###################################################

# genre based recommendation


### Approach 1: Returns movies based on Genre. Movies are filtered based on Highest Average Rating among all movies tagged for the chosen genre.

# get_movie_genre  = function(InputGenre = "Action") {
#   recom = ratings %>%
#     group_by(MovieID) %>%
#     summarize(ratings_per_movie = n(),
#               ave_ratings = round(mean(Rating), dig = 3)) %>%
#     inner_join(movies, by = 'MovieID') %>%
#     filter(grepl(InputGenre, Genres)) %>%
#     top_n(10, ave_ratings) %>% # select top 10 highest average rated movies for the selected genre. ###### Approach 1
#     select('MovieID',
#            'Title',
#            'ave_ratings',
#            'ratings_per_movie',
#            'Genres') %>%
#     arrange(desc(ave_ratings))
# }


### Approach 2: Returns movies based on Genre. Movies are filtered based on Popularity. Popularity is measured by following criteria:- 
### i)filter users with more than 50 ratings in training data ii) filter movies rated by those users iii) filter movies which have more than 500 ratings overall
### iv) return the movies with highest average ratings.


get_movie_genre  = function(InputGenre = "Action") {

  mr.users = ratings %>%       ### users who have rated more than 500 movies
    group_by(UserID) %>%
    summarize(number_of_ratings_per_user = n()) %>%
    filter(number_of_ratings_per_user > 50)

  selected.movies = ratings %>%  ###
    group_by(UserID) %>%
    inner_join(mr.users, by = 'UserID')


  hr.genre = selected.movies %>%
    group_by(MovieID) %>%
    summarize(ratings_per_movie = n(),
              ave_ratings = round(mean(Rating), dig = 3)) %>%
    inner_join(movies, by = 'MovieID') %>%
    filter(ratings_per_movie > 500) %>%
    filter(grepl(InputGenre, Genres)) %>%
    top_n(10, ave_ratings) %>%
    select('MovieID',
           'Title',
           'ave_ratings',
           'ratings_per_movie',
           'Genres') %>%
    arrange(desc(ave_ratings))
}



#################System1################################################

# read in data

myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies,
                  split = "::",
                  fixed = TRUE,
                  useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID,
                          function(x)
                            paste0(small_image_url, x, '.jpg?raw=true'))



# Ratings data from 

ratings = read.csv(
  paste0(myurl, 'ratings.dat?raw=true'),
  sep = ':',
  colClasses = c('integer', 'NULL'),
  header = FALSE
)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
ratings$Timestamp = NULL
colnames(ratings) = c('UserID', 'MovieID', 'Rating')



## users can give ratings only to those movies which are highly rated.

##  TODO

shinyServer(function(input, output, session) {

  # show the movies to be rated based on user ratings
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(
          width = 2,
          div(style = "text-align:center", img(
            src = movies$image_url[(i - 1) * num_movies + j], height = 150
          )),
          div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
          div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(
            paste0("select_", movies$MovieID[(i - 1) * num_movies + j]),
            label = "",
            dataStop = 5
          ))
        )) #00c0ef
      })))
    })
  })
  # 
  # # Calculate recommendations when the submit button is clicked
  # df_genre <- eventReactive(input$btn1, {
  #   withBusyIndicatorServer("btn1", {
  #     # showing the busy indicator
  #     # hide the rating container
  #     useShinyjs()
  #     jsCode <-
  #       "document.querySelector('[data-widget=collapse]').click();"
  #     runjs(jsCode)
  #     
  #     # get the user's rating data
  #     value_list <- reactiveValuesToList(input)
  #     user_ratings <- get_user_ratings(value_list)
  #     
  #     user_results = (1:10) / 10
  #     user_predicted_ids = 1:10
  #     recom_results <- data.table(
  #       Rank = 1:10,
  #       MovieID = movies$MovieID[user_predicted_ids],
  #       Title = movies$Title[user_predicted_ids],
  #       Predicted_rating =  user_results
  #     )
  #     
  #   }) # still busy
  #   
  # }) # clicked on button
  
  
  # Calculate recommendations when the submit button is clicked
  df_ratings <- eventReactive(input$btn2, {
    withBusyIndicatorServer("btn2", {
      # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <-
        "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      new_user <- user_ratings$UserID

      ratings <- rbind(user_ratings, ratings)
      dimension_names <- list(UserID = sort(unique(ratings$UserID)), MovieID = sort(unique(ratings$MovieID)))
      ratingmat <- spread(select(ratings, MovieID, UserID, Rating), MovieID, Rating) %>% select(-UserID)
      ratingmat <- as.matrix(ratingmat)
      dimnames(ratingmat) <- dimension_names
      ratingmat[is.na(ratingmat)] <- 0 #### NA Handling before predictions
      sparse_ratings <- as(ratingmat, "sparseMatrix")
      rm(ratingmat)
      gc()
      real_ratings <- new("realRatingMatrix", data = sparse_ratings)


      # run the recommender alogrithm

      # rec = Recommender(real_ratings, method = 'IBCF', parameter = list(normalize = 'Z-score', method = 'Cosine', k= 25))
      # rec = Recommender(real_ratings, method = 'UBCF', parameter = list(normalize = 'Z-score', method = 'Cosine', nn = 25))
      # saved.recom = saveRDS(rec,"savedrecomIBCF.RDS")
      # saved.recom = saveRDS(rec,"savedrecomUBCF.RDS")
      rec = readRDS("savedrecomIBCF.RDS")
      # rec = readRDS("savedrecomUBCF.RDS")
      res = predict(rec, real_ratings[new_user,], type = 'ratings')

      rec_list = as(res, 'list')
      #print(sum(is.na(rec_list[[1]])))
      rec_list = ifelse(is.na(rec_list[[1]]),0, rec_list[[1]])
      
      print(rec_list)

      # sort, organize, and return the results
      user_results <- sort(rec_list, decreasing = TRUE)[1:10]
      #print(user_results)
      user_predicted_ids <- as.numeric(names(user_results))
      p.movies = movies[0,]
      for (i in 1:10) {
        
        p.movies[i,] = rbind(movies[movies$MovieID == user_predicted_ids[i] ,] )
        p.movies
      }
      print(p.movies)
      recom_result <- data.table(Rank = 1:10, 
                                  MovieID = p.movies$MovieID,
                                  Title = p.movies$Title,
                                  Predicted_rating =  user_results)
      print(recom_result)

    }) # still busy
    
  }) # clicked on button
  
  
  # display the recommendations
  output$genreresults <- renderUI({
    num_rows <- 2
    num_movies <- 5
    selGen = input$selectedGenre  ##new
    print(selGen) ##new
    #recom_result <- df_genre()
    recom_result <- get_movie_genre(selGen)  ##new
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(
          width = 2,
          status = "success",
          solidHeader = TRUE,
          title = paste0("Rank ", (i - 1) * num_movies + j),
          
          div(style = "text-align:center",
              a(
                img(src = movies$image_url[movies$MovieID == recom_result$MovieID[(i - 1) * num_movies + j]], height = 150)
              )),
          div(style = "text-align:center; font-size: 100%",
              strong(movies$Title[movies$MovieID == recom_result$MovieID[(i - 1) * num_movies + j]]))
          
        )
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
  
  
  # display the recommendations
  output$ratingresults <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df_ratings()

    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(
          width = 2,
          status = "success",
          solidHeader = TRUE,
          title = paste0("Rank ", (i - 1) * num_movies + j),
          
          div(style = "text-align:center",
              a(
                img(src = movies$image_url[movies$MovieID == recom_result$MovieID[(i - 1) * num_movies + j]], height = 150)
              )),
          div(style = "text-align:center; font-size: 100%",
              strong(movies$Title[movies$MovieID == recom_result$MovieID[(i - 1) * num_movies + j]]))
          
        )
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
}) # server function