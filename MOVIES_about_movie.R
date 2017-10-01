# MOVIE TITLE 
n_movies <- length(levels(factor(movies$movie_title)))

# COLOR: 19 películas no cuentan con este campo
color_type <- levels(as.factor(movies$color))
eda1 <- select(movies,color) %>%
  filter(!is.na(color)) %>%
  ggplot(aes(color)) +
  geom_bar()
summ_color <- movies %>%
  select(color) %>%
  count(color)
summ_color$perc <- summ_color$n / length(movies$color)

# NUMBER OF CRITICS FOR REVIEW: Dato tipo númerico. NA será interpretado como cero
movies$num_critic_for_reviews[is.na(movies$num_critic_for_reviews)] <- 0
summary(movies$num_critic_for_reviews)
eda2 <- ggplot(movies, aes(num_critic_for_reviews)) +
  geom_histogram(aes(y = ..density..), binwidth=20)+
  geom_density()

# DURATION
summary(movies$duration)
eda3 <- ggplot(movies, aes(duration))+
  geom_histogram(bins=200) 

#GROSS
summary(movies$gross)
eda4 <- boxplot(movies$gross, horizontal=TRUE,xlab="gross")

#GENRES
movie_genres <- transform(select(movies,movie_title,genres), genre = colsplit(genres, split = "\\|", names = c('1', '2','3','4','5','6','7','8')))
genre_1 <- levels(factor(movie_genres$genre.X1))
genre_2 <- levels(factor(movie_genres$genre.X2))
genre_3 <- levels(factor(movie_genres$genre.X3))
genre_4 <- levels(factor(movie_genres$genre.X4))
genre_5 <- levels(factor(movie_genres$genre.X5))
genre_6 <- levels(factor(movie_genres$genre.X6))
genre_7 <- levels(factor(movie_genres$genre.X7))
genre_8 <- levels(factor(movie_genres$genre.X8))
all_genres <- as.data.frame(levels(factor(union(genre_1,genre_2) %>% union(genre_3) %>% union(genre_4) %>% union(genre_5) %>%
                              union(genre_6) %>% union(genre_7) %>% union(genre_8))))
colnames(all_genres) <- "genre"
all_genres_nrow <- length(levels(all_genres$genre))
for(i in 1:all_genres_nrow){
all_genres$n_movies[i] <- nrow(movie_genres %>% filter(grepl(all_genres$genre[i],genres)))
}
eda5 <- wordcloud(all_genres$genre,all_genres$n_movies,colors =  brewer.pal(12, "Paired"))


