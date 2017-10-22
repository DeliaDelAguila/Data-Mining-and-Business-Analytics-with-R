# MOVIES 
# clean Data - About Movies

library(readr)
library(outliers)
library(dplyr)

#############
# FUNCIONES #
#############

fix_duplicate <- function(x) {
  print(paste(length(duplicated(x)[duplicated(x)==TRUE])," duplicados"))
  x[!duplicated(x),]
}

#############

# 123 duplicados
movies <- fix_duplicate(a_movies)
glimpse(movies)
problems(movies)
#######
# Color
####### 
# Blanco y Negro o a Color
levels(factor(movies$color))

#######
# Duration
#######
# 15 NA
summary(movies$duration)
movies_na_duration <- movies %>% select(movie_title, duration) %>% filter(is.na(duration))

# Outliers
# Podemos dividir en
# >195: Series completas o Versiones extendidas de películas
# <60: Un episodío de una serie o Cortometrajes
summary(movies$duration)

# 1. SERIES - TODOS LOS EPISODIOS
#series_name, episodes
series <- list('trapped',11,
               'carlos',3,
               'the company',6,
               'emma',4,
               'shaun the sheep',1,
               'robot chicken',1,
               '10000 bc',1,
               'anger management',1,
               'home movies',1,
               'its always sunny in philadelphi',1,
               'rules of engagement',1,
               'sabrina the teenage witch',1,
               'strangers with candy',1,
               'sonny with a chance',1,
               'star wars the clone wars',1)

# 2. OTRA VERSION: Edición del Director, versión extendida..
#movie_title,special_duration, reason, real_duration
duration_version <- list('blood in blood out',330,'original cut',180,
                       'heavens gate',325,'rough cut',149,
                       'the legend of suriyothai',300,'extended',185,
                       'das boot',293,'original uncut',149,
                       'apocalypse now',289,NA,147,
                       'gods and generals',280,'director cut',219,
                       'arn the knight templar',270,'tv',139,
                       'cleopatra',251,'50th anniversary',192,
                       'once upon a time in america',251,'director cut',229,
                       'the wolf of wall street',240,'rough cut',180,
                       'dances with wolves',236,'director cut',181,
                       'lawrence of arabia',227,'restored roadshow',216,
                       'gone with the wind',226,'copyright length',238,
                       'the greatest story ever told',225,'premiere',141,
                       'the godfather part ii',220,'special edition',202,
                       'the last emperor',219,'tv',163,
                       'the thin red line',215,'rough cut',170,
                       'watchmen',215,'ultimate cut',162,
                       'woodstock',215,'dvd',184,
                       'nixon',212,'director cut',192,
                       'wyatt earp',212,'extended edition',191,
                       'alexander',206,'ultimate cut',175,
                       'jfk',206,'director cut',189,
                       'king kong',201,'extended version',187,
                       'its a mad mad mad mad world',197,'extended',192,
                       'troy',196,'director cut',163,
                       'iron man 3',195,'assembly cut',130,
                       'kingdom of heaven',194,'director cut',144)

# 3. CORTOMETRAJES
cortometrajes <- list('the touch',
                      'vessel',
                      'walmart the high cost of low price')

boxplot(movies$duration, outcol="red")
boxplot(movies$duration, outline=FALSE)

outliers_duration <- as.data.frame(boxplot.stats(movies$duration)$out)
colnames(outliers_duration) <- c("value")

arrange(outliers_duration,value)

# Series
series <- movies %>% filter(grepl("[TV]",content_rating))
boxplot(series$duration, outcol="red")
outliers_series_duration <- as.data.frame(boxplot.stats(series$duration)$out)
colnames(outliers_series_duration) <- c("duration")
merge(series,outliers_series_duration,by="duration")

#######
# Genres
#######
# 26 generos, siendo Drama el más popular con 2536 películas
# y Game-Show el menos pupular con solo una 1 película
movie_genres <- transform(select(movies,movie_title,genres), genre = reshape::colsplit(genres, split = "\\|", names = c('1', '2','3','4','5','6','7','8')))
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

#######
# Content Rating
#######

#From: https://contribute.imdb.com/updates/guide/certificates

levels(as.factor(movies$content_rating))

rating_groups <- list('Approved'='General Audiences',
                      'G'='General Audiences',
                      'GP'='Parental Guidance',
                      'M'='General Audiences',
                      'NC-17'='No Under 17',
                      'Not Rated'='No certificate',
                      'Passed',NA,
                      'PG'='Parental Guidance',
                      'PG-13'='Parental Guidance-13',
                      'R'='No Under 17',
                      'TV-14'='TV-No UNder 14',
                      'TV-G'='TV-General Audiences',
                      'TV-MA'='TV-No Under 17',
                      'TV-PG'='TV-Parental Guidance',
                      'TV-Y'='TV-General Audiences',
                      'TV-Y7'='TV-General Audiences',
                      'Unrated'='No certificate',
                      'X'='TV-No Under 17'
)

#######
# Title Year
#######

#La mayoría de las películas son del 2000 en adelante
title_year_n <- movies %>% 
  select(title_year) %>%
  filter(!is.na(title_year)) %>%
  dplyr::count(title_year)

ggplot(title_year_n, aes(title_year,n)) +
  geom_line()

#######
# Budget
#######

# Notemos que el Budget no está en la misma moneda,
# por ejemplo Steamboy y Akira está en Yenes Japoneses 
# o Kabhi Alvida Naa Kehna que está en Rupia India
summary(movies$budget)

boxplot(movies$budget,outcol="red")
outliers_budget <- as.data.frame(boxplot.stats(movies$budget)$out)
colnames(outliers_budget) <- c("budget")
outliers_budget %>% 
  dplyr::count(budget) %>%
  arrange(desc(budget))

#######
# Gross
#######

# Por el contrario a budget, el valor de gross está en USD
# y los outliers se refieren a películas muy taquilleras como Avatar y Titanic
boxplot(movies$gross,outcol="red")
outliers_gross <- as.data.frame(boxplot.stats(movies$gross)$out)
colnames(outliers_gross) <- c("gross")
outliers_gross %>% 
  dplyr::count(gross) %>%
  arrange(desc(gross))

#######
# Language
#######


#######
# Country
#######











