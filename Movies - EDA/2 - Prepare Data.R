# MOVIES 
# Prepare Data

library(tibble)

# NUMERO DE COLUMNAS Y REGISTROS
# Nuestro data set cuenta con 28 columnas y 5,043 observaciones. 
glimpse(rd_movies)

# TIPOS DE DATO
# Verificamos el tipo de dato de cada variable y hacemos algun ajuste en caso de ser necesario
# Todo se ve OK

# AGRUPAR
# Cada columna será agrupada dependiendo del tipo de información que contiene
# dejando a movie_title como nuestra variable llave
# mandaremos nuestra variable llave (movie_title) a minusculas
# Y caracteres especiales serán removidos

rd_movies$movie_title <- str_replace_all(tolower(rd_movies$movie_title), "[[:punct:]]", "")

# Acerca de la película: 11 variables
a_movies <- rd_movies %>%
  select (movie_title, color, duration, genres, content_rating, title_year, budget, gross,language, country, aspect_ratio)

glimpse(a_movies)

# Acerca de los reviews y comunicación con el usurio: 9 variables
a_reviews <- rd_movies %>%
  select(movie_title, imdb_score, movie_fb_likes = movie_facebook_likes, review_critics = num_critic_for_reviews, reviews_users = num_user_for_reviews, rate_users = num_voted_users, fb_posts = facenumber_in_poster, post_keywords = plot_keywords, imdb_link = movie_imdb_link)

glimpse(a_reviews)

#Acerca del director: 3 variables
a_director <- rd_movies %>%
  select(movie_title, director_name, director_fb_likes = director_facebook_likes)

glimpse(a_director)

# Acerca de los actores: 8 variables 
a_cast <- rd_movies %>%
  select(movie_title, a1_name = actor_1_name, a1_fb_likes = actor_1_facebook_likes,a2_name = actor_2_name, a2_fb_likes = actor_2_facebook_likes, a3_name = actor_3_name, a3_fb_likes = actor_3_facebook_likes, cast_fb_likes = cast_total_facebook_likes)

glimpse(a_cast)
  

