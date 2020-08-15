#' ---
#' title: "Data preparation of Titanic passengers"
#' subtitle: Add data from Wikipedia (but not the survival information)
#' author: Statistical Mouse
#' output: html_notebook
#' ---

#' This notebook reads in all Kaggle data for the Titanic challenge,
#' does feature cleaning, adds information, and writes it to CSV files.  
#' This notebook does not add survival information to the data.

#' Libraries  
#' * patchwork is used to compose charts  
#' * visdat is used to find missing data  

library(tidyverse)
library(patchwork)
library(visdat)

#' # Read Kaggle data

df_train <- read_csv('../data/titanic/train.csv') %>%
  add_column(DataSource = 'train')

df_test <- read_csv('../data/titanic/test.csv') %>%
  add_column(Survived = 0, .before = 'Pclass') %>%
  add_column(DataSource = 'test')

df_kaggle <- bind_rows(df_train, df_test)
kaggle_first_class <- df_kaggle %>% filter(Pclass==1)
kaggle_second_class <- df_kaggle %>% filter(Pclass==2)
kaggle_third_class <- df_kaggle %>% filter(Pclass==3)

#' # Read additional data from Wikipedia

library(stringr)
library(xml2)
library(rvest)
library(stringi)

passengers_list_on_wikipedia <- 'https://en.wikipedia.org/wiki/Passengers_of_the_RMS_Titanic'
passengers_list_html <- read_html(passengers_list_on_wikipedia)

read_as_tibble <- function(n) {
  xpath <-str_c(c('//*[@id="mw-content-text"]/div[1]/table[', n, ']'), 
                collapse = '')
  
  t <- passengers_list_html %>% 
    rvest::html_nodes('body') %>% 
    xml2::xml_find_all(xpath) %>%
    html_table(fill = T)
  t <- t[[1]]
  
  tibble(name = t$Name,
         age = t$Age,
         hometown = t$Hometown,
         destination = t$Destination)
}

fetch_from_wiki_and_store_locally <- function() {
  wiki_first_class <- read_as_tibble(2) 
  wiki_second_class <- read_as_tibble(3)
  wiki_third_class <- read_as_tibble(4)
  save(wiki_first_class,
       wiki_second_class,
       wiki_third_class,
       file = 'passenger-tables-from-wikipedia.RData')  
}
fetch_from_wiki_and_store_locally()
load('passenger-tables-from-wikipedia.RData')

cleanup_wiki_names <- function(df) {
  df %>%
    mutate(Name = str_replace(Name, 'and \\w*, ', '')) %>%
    mutate(Name = str_replace(Name, '\\[\\d\\d\\]', '')) %>%
    mutate(Name = str_replace(Name, '\\[\\d\\d\\]', '')) %>%
    mutate(Name = str_replace(Name, 'née|alias|nee', ''))
}

wiki_first_class <- wiki_first_class %>%
  add_column(wiki_id = 1:nrow(wiki_first_class)+11000, .before = 'name') %>%
  rename(Name=name) %>%
  cleanup_wiki_names()

wiki_second_class <- wiki_second_class %>%
  add_column(wiki_id = 1:nrow(wiki_second_class)+12000, .before = 'name') %>%
  rename(Name=name) %>%
  cleanup_wiki_names()

wiki_third_class <- wiki_third_class %>%
  add_column(wiki_id = 1:nrow(wiki_third_class)+13000, .before = 'name') %>%
  rename(Name=name) %>%
  cleanup_wiki_names()

#' # Combine age from the two datasets

convert_wiki_age <- function(df) {
  age_months <- filter(df, grepl('mo', df$age)) %>% 
    mutate(age = case_when(
      age=='1 mo.' ~ 1/12,
      age=='2 mo.' ~ 2/12,
      age=='3 mo.' ~ 3/12,
      age=='4 mo.' ~ 4/12,
      age=='5 mo.' ~ 5/12,
      age=='6 mo.' ~ 6/12,
      age=='7 mo.' ~ 7/12,
      age=='8 mo.' ~ 8/12,
      age=='9 mo.' ~ 9/12,
      age=='10 mo.' ~ 10/12,
      age=='11 mo.' ~ 11/12,
      age=='12 mo.' ~ 12/12)) %>%
    mutate(age = round(age, digits=1))
  age_years <- filter(df, !grepl('mo', df$age))
  age_years$age <- as.double(age_years$age)
  bind_rows(age_months, age_years)
}

to_ascii <- function(x) {
  stri_trans_general(str = x, id = "Latin-ASCII")
}

data_first_class <- bind_rows(
  select(kaggle_first_class, 
         PassengerId, Name, Age),
  convert_wiki_age(wiki_first_class) %>%
    select(PassengerId=wiki_id, Name, Age=age))
data_first_class$Name <- sapply(data_first_class$Name, to_ascii)

data_second_class <- bind_rows(
  select(kaggle_second_class, 
         PassengerId, Name, Age),
  convert_wiki_age(wiki_second_class) %>%
    select(PassengerId=wiki_id, Name, Age=age))
data_second_class$Name <- sapply(data_second_class$Name, to_ascii)

data_third_class <- bind_rows(
  select(kaggle_third_class, 
         PassengerId, Name, Age),
  convert_wiki_age(wiki_third_class) %>%
    select(PassengerId=wiki_id, Name, Age=age))
data_third_class$Name <- sapply(data_third_class$Name, to_ascii)

#' ## Combine data for first class passengers

library(quanteda)

c1 <- corpus(data_first_class, docid_field = 'PassengerId', text_field = 'Name') 
docvars(c1, "Class") <- 1
c2 <- corpus(data_second_class, docid_field = 'PassengerId', text_field = 'Name') 
docvars(c1, "Class") <- 2
c3 <-corpus(data_third_class, docid_field = 'PassengerId', text_field = 'Name') 
docvars(c1, "Class") <- 3
passenger_corpus <- c1+c2+c3

passenger_dfm <- dfm(passenger_corpus, remove_punct = TRUE)
topfeatures(passenger_dfm)

tstat_sim <- textstat_simil(passenger_dfm, passenger_dfm,
                            method = "cosine", margin = "features")



summary(passenger_corpus)
texts(c1)
