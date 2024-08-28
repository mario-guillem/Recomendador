library(tidyverse)
library(ggplot2)
library(highcharter)
library(plotly)
library(viridisLite)
library(cowplot)
library(treemap)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(maps)
library(countrycode)
library(purrr)
library(htmltools)

primaryColor <- '#9f94ff'

minColor = "#ffffff"
maxColor = "#3737cc"

red_minColor = "#ffd6d6"
red_maxColor = "#ff0000"

# Loading data

df2 <- read.csv("C:/Users/Mario/Desktop/wines.csv")

summary(df)

## Missing Values

#Rows and Columns ->
dim(df2)
cat('Complete cases -> ', sum(complete.cases(df)))
cat('Total null values -> ' , sum(is.na(df)))
#Columnas con NA's
list_na <- colnames(df)[apply(df, 2, anyNA)]
cat('Columns with null values -> ', list_na)



nulls <- sapply(df, function(x) sum(is.na(x)))
cat('null count for each column -> ')
nulls


cat('Using imputeTS to fill NA values with mean ... ')

#Como existe una gran cantidad de missing values en los precios, eliminar casi el 7% de las observaciones nos parece excesivo
# es por ello que sustituiremos los na por la media de los precios.
library(imputeTS)
mean_fill <- df%>%
  na_mean()
cat('Missing values after filling with mean -> ', sum(is.na(mean_fill)))
df <- mean_fill   

df <- df[!duplicated(df[,c("taster_name", "title")]),]




# Exploratory Data Analysis

### High Quality Wines (100 points)


value_data = df %>%
  select(title, price, points) %>%
  filter(title != '' & points == 100) %>%
  arrange(price) %>%
  head(15)

(value_data)

### Bad Quality Wines (< 85 points)


value_data = df %>%
  select(title, price, points) %>%
  filter(title != '' & points < 85) %>%
  arrange(points) %>%
  head(15)

value_data




### Affordable and Good Quality Wines


value_data = df %>%
  select(title, price, points) %>%
  filter(title != '' & points > 95) %>%
  mutate(price = as.integer(price)) %>%
  arrange(price) %>%
  head(30)

value_data

### Economic and Fine Quality Wines (< $10)


value_data = df %>%
  select(title, price, points) %>%
  filter(title != '' & price < 10 & points >= 90) %>%
  mutate(price = as.integer(price)) %>%
  arrange(price) %>%
  head(30)

value_data


# Data Preprocessing               
#Nos aseguramos que no haya ningun nombre del catador vacio ni el titulo o nombre del vino vacio
rating_df <- df %>% select(taster_name, title, points) %>% filter(taster_name != '' & title != '') %>% mutate(points = as.numeric(points)) %>% drop_na()    
#Eliminamos los duplicados como hemos hecho anteriormente.
rating_df <- rating_df[!duplicated(rating_df[,c("taster_name", "title")]),]
cat('removing duplicates')

library(recommenderlab)
rating_matrix <- as(rating_df, "realRatingMatrix")                
print(paste(dim(rating_matrix)[1], 'users rated', dim(rating_matrix)[2], 'wines'))

ratings <- as.vector(rating_matrix@data)           
ratings <- round(ratings)
ratings <- factor(ratings)
ratings <- ratings[ratings != 0]

table(ratings)  

qplot(ratings, 
      fill=ratings
     ) + ggtitle("Distribution of All ratings") + theme_minimal() + 
     theme(legend.position = "none")

library(ggridges) 

temp_df <- df %>% filter(points >= 90)
temp_df$points = as.character(temp_df$points)        

n_user_ratings = rowCounts(rating_matrix)
cat('user ratings')
n_user_ratings                

#nos quedaremos con los que tengan mas de 100 porque consideramos que son mas expertos y experimentados en la evaluacion de los vinos.
rating_matrix <- rating_matrix[n_user_ratings > 100]


# Item Based Collaborative Filtering (Finding Similar Wines)



cos_similarity = function(A,B){
  A = as.numeric(A)
  B = as.numeric(B)  
  num = sum(A * B, na.rm = T)
  den = sqrt(sum(A^2, na.rm = T)) * sqrt(sum(B^2, na.rm = T)) 
  result = num/den
  
  return(result)
} 


matrix_item = rating_df %>%
  group_by(title) %>%
  pivot_wider(names_from = title, values_from = points) %>%
  as.data.frame()

row.names(matrix_item) = matrix_item$taster_name
matrix_item$taster_name = NULL

#n_user_ratings = rowCounts(matrix_item)                
#matrix_item <- matrix_item[n_user_ratings >= 300]
matrix_item = as.matrix(matrix_item)

#null_percent <- sum(is.na(user_item)) / ( ncol(user_item) * nrow(user_item))                
#print(paste('very sparse matrix, since',null_percent, 'of the cells lack data'))         

item_recommendation = function(wine_name, rating_matrix = matrix_item, n_recommendations = 10){
  print(paste('Wines Similar to -> ', wine_name))
  
  index_ = which(colnames(rating_matrix) == wine_name)
  similarity = apply(rating_matrix, 2, FUN = function(y) cos_similarity(rating_matrix[,index_], y))
  
  similar_wines = tibble(Wine = names(similarity), 
                         similarity = similarity                           
  ) %>%
    filter(Wine != wine_name & similarity >= 0.5) %>% 
    top_n(n_recommendations, similarity) %>%
    arrange(desc(similarity)) %>%
    head(n_recommendations)
  
  recommendations = df %>%
    filter(title %in% similar_wines$Wine) %>%
    mutate(similarity = similar_wines$similarity) %>%
    group_by(title) %>% select(title, price, points, country)
  
  return(recommendations)
  
}                

item_recommendation("Prospect 772 2014 The Brat Grenache (Calaveras County)")
kable(item_recommendation("Torbreck 2006 The Struie Shiraz (Barossa)"))
kable(item_recommendation("Stemmari 2013 Dalila White (Terre Siciliane)"))
                                   
