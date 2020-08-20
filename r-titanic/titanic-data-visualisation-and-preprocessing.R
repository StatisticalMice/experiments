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
library(stringr)
library(xml2)
library(rvest)
library(stringi)
library(purrr)

df_kaggle <- bind_rows(
  read_csv('../data/titanic/train.csv') %>%
    add_column(DataSource = 'train'),
  read_csv('../data/titanic/test.csv') %>%
    add_column(Survived = NaN, .before = 'Pclass') %>%
    add_column(DataSource = 'test')
)

read_as_tibble <- function(n) {
  xpath <-str_c(c('//*[@id="mw-content-text"]/div[1]/table[', n, ']'), 
                collapse = '')
  
  t <- read_html('https://en.wikipedia.org/wiki/Passengers_of_the_RMS_Titanic') %>% 
    rvest::html_nodes('body') %>% 
    xml2::xml_find_all(xpath) %>%
    html_table(fill = T)
  t <- t[[1]]
  
  tibble(Name = t$Name,
         Age = t$Age,
         Hometown = t$Hometown,
         Destination = t$Destination) %>%
    add_column(DataSource = 'wiki')
}

fetch_data_and_store_locally <- function() {
  df_wiki <- bind_rows(
    read_as_tibble(1) %>%
      add_column(Pclass = 1),
    read_as_tibble(2) %>%
      add_column(Pclass = 2),
    read_as_tibble(3) %>%
      add_column(Pclass = 3)
  )
  save(df_wiki, file = 'passenger-tables-from-wikipedia.RData')  
}
fetch_data_and_store_locally()
load('passenger-tables.RData')

df_wiki <- df_wiki %>%
  # [12] ->
  mutate(Name = str_replace_all(Name, '\\[\\d\\d\\]', '')) %>%
  # the following messes up separate() if not fixed
  mutate(Name = str_replace(Name, 'Beane.', 'Beane,')) %>%
  separate(Name, into=c('Name_Last', 'Name_Rest'), sep = ',\ ', extra = 'merge')

df_kaggle <- df_kaggle %>%
  # ")" -> )
  mutate(Name = str_replace(Name, '"\\)"', '\\)')) %>%
  # "" ->
  mutate(Name = str_replace(Name, '\\"\\"', '')) %>%
  # )" -> ")
  mutate(Name = str_replace(Name, '\\)\\"', '\\"\\)')) %>%
  separate(Name, into=c('Name_Last', 'Name_Rest'), sep = ',\ ')

convert_wiki_age <- function(df) {
  age_months <- filter(df, grepl('mo', df$Age)) %>% 
    mutate(Age = case_when(
      Age=='1 mo.' ~ 1/12,
      Age=='2 mo.' ~ 2/12,
      Age=='3 mo.' ~ 3/12,
      Age=='4 mo.' ~ 4/12,
      Age=='5 mo.' ~ 5/12,
      Age=='6 mo.' ~ 6/12,
      Age=='7 mo.' ~ 7/12,
      Age=='8 mo.' ~ 8/12,
      Age=='9 mo.' ~ 9/12,
      Age=='10 mo.' ~ 10/12,
      Age=='11 mo.' ~ 11/12,
      Age=='12 mo.' ~ 12/12)) %>%
    mutate(Age = round(Age, digits=1))
  age_years <- filter(df, !grepl('mo', df$Age))
  age_years$Age <- as.double(age_years$Age)
  bind_rows(age_months, age_years)
}

df_wiki <- convert_wiki_age(df_wiki) %>%
  add_column(PassengerId = 1:nrow(df_wiki)+2000, .before = 1)

df_all <- bind_rows(
  df_wiki,
  df_kaggle
) %>%
  # "Annie" -> Name_Nick
  mutate(Name_Nick = str_extract(Name_Rest, pattern = '"(.*?)"')) %>%
  mutate(Name_Rest = str_remove(Name_Rest, pattern = '"(.*?)"')) %>%
  # (Marie Eugenie) -> Name_In_Brackets
  mutate(Name_In_Brackets = str_extract(Name_Rest, pattern = '\\((.*?)\\)')) %>%
  mutate(Name_Rest = str_remove(Name_Rest, pattern = '\\((.*?)\\)'))
  
df_all <- df_all %>%
  mutate(marker = str_detect(Name_Last, 'and ')) %>%
  mutate(Name_Last = case_when(
    marker ~ str_extract(Name_Rest, '\\w*$'),
    TRUE ~ Name_Last
  )) %>%
  mutate(Name_Rest = case_when(
    marker ~ str_remove(Name_Rest, '\\w*$'),
    TRUE ~ Name_Rest
  )) %>%
  mutate(Name_Title = str_extract(Name_Rest, '^[^\\ ]+')) %>%
  mutate(Name_Rest = str_remove(Name_Rest, '^[^\\ ]+')) %>%
  mutate(Name_Title = str_remove(Name_Title, pattern = '\\.'))

df_all <- df_all %>%
  select(-marker)
  
df_all <- df_all %>%
  relocate(Name_Title, Name_Last, Name_Rest, Name_Nick, Name_In_Brackets, .after=PassengerId)

df_all <- df_all %>%
  mutate(Name_In_Brackets = str_remove_all(Name_In_Brackets, pattern = '\\)|\\(') %>% str_squish()) %>%
  mutate(Name_Nick = str_remove_all(Name_Nick, pattern = '\\"') %>% str_squish()) %>%
  mutate(Name_Last = str_squish(Name_Last) %>% stri_trans_general(id = "Latin-ASCII")) %>%
  mutate(Name_Rest = str_squish(Name_Rest) %>% stri_trans_general(id = "Latin-ASCII")) %>%
  mutate(Name_Title = str_squish(Name_Title)) %>%
  arrange(Name_Last, Name_Title, Name_Rest)
  
df_tmp <- df_all %>%
  select(PassengerId, Name_Title, Name_Last, Name_Rest, DataSource, Age, Pclass, Name_Nick, Name_In_Brackets)

df_tmp %>%
  arrange(Name_Last, Name_Title, Name_Rest) %>%
  View()

df_kaggle_first_class <- df_all %>% filter(Pclass==1, DataSource!='wiki')
df_wiki_first_class <- df_all %>% filter(Pclass==1, DataSource=='wiki')

df_kaggle_second_class <- df_all %>% filter(Pclass==2, DataSource!='wiki')
df_wiki_second_class <- df_all %>% filter(Pclass==2, DataSource=='wiki')

df_kaggle_third_class <- df_all %>% filter(Pclass==2, DataSource!='wiki')
df_wiki_third_class <- df_all %>% filter(Pclass==3, DataSource=='wiki')

left <- df_kaggle_first_class %>% 
  filter(is.na(Age)) %>%
  select(PassengerId, Name_Title, Name_Last, DataSource, Name_Rest, Name_Nick, Name_In_Brackets, Age)
right <- df_wiki_first_class %>%
  select(Name_Title, Name_Last, DataSource, Name_Rest, Name_Nick, Name_In_Brackets, Age)
left_join(left, right, by = c('Name_Title', 'Name_Last')) %>%
  relocate(Name_Rest.x, Name_Rest.y, DataSource.y, .after = Name_Last) %>%
  View()

left <- df_kaggle_second_class %>% 
  filter(is.na(Age)) %>%
  select(PassengerId, Name_Title, Name_Last, DataSource, Name_Rest, Name_Nick, Name_In_Brackets, Age)
right <- df_wiki_second_class %>%
  select(Name_Title, Name_Last, DataSource, Name_Rest, Name_Nick, Name_In_Brackets, Age)
left_join(left, right, by = c('Name_Title', 'Name_Last')) %>%
  relocate(Name_Rest.x, Name_Rest.y, DataSource.y, .after = Name_Last) %>%
  View()

left <- df_kaggle_third_class %>% 
  filter(is.na(Age)) %>%
  select(PassengerId, Name_Title, Name_Last, DataSource, Name_Rest, Name_Nick, Name_In_Brackets, Age)
right <- df_wiki_third_class %>%
  select(Name_Title, Name_Last, DataSource, Name_Rest, Name_Nick, Name_In_Brackets, Age)
left_join(left, right, by = c('Name_Title', 'Name_Last')) %>%
  relocate(Name_Rest.x, Name_Rest.y, DataSource.y, .after = Name_Last) %>%
  View()






