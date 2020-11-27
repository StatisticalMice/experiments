#' ---
#' title: "House Prices: Advanced Regression Techniques"
#' subtitle: Data pre-processing and visualization
#' author: Statistical Mouse
#' output: html_notebook
#' ---

library(tidyverse)
library(magrittr)
library(patchwork)
library(missForest)

#' Read the data
house_features <- read_csv('../data/house_prices/train.csv') %>%
  column_to_rownames(var = 'Id')
house_prices <- house_features %>% select(SalePrice)
house_features %<>% select(-SalePrice)

summarise_df <- function(df) {
  a <- summarise(df, across(everything(), n_distinct)) %>%
    add_column(summary_type = 'n_distinct', .before = 1)
  b <- summarise(df, across(everything(), ~ sum(is.na(.)))) %>%
    add_column(summary_type = 'count_na', .before = 1)
  c <- summarise(df, across(everything(), ~ sum(!is.na(.)))) %>%
    add_column(summary_type = 'count_not_na', .before = 1)
  d <- summarise(df, across(everything(), class)) %>%
    add_column(summary_type = 'class', .before = 1)
  r1 <- bind_rows(a, b, c) 
  r2 <- pivot_longer(r1,
                     cols = -summary_type, 
                     names_to = 'variable', 
                     values_to = 'count')
  r3 <- pivot_wider(r2,
                    names_from = 'summary_type', 
                    values_from = 'count')
  r3 %>% arrange(variable)
}
summary_house_features <- summarise_df(house_features)

feature_classes <- house_features  %>% 
  summarise_all(class)  %>% 
  pivot_longer(everything(), names_to = 'variable', values_to = 'class') 
summary_house_features %<>% inner_join(feature_classes, by = 'variable') 
summary_house_features %<>% relocate('class', .before = 'n_distinct')






#' Features that have more than this amount of unique values have 'many values'
many_values_cutoff <- 30


value_counts  %>%
  View()

ggplot(data = target_price, aes(x = SalePrice)) + 
  geom_histogram() + ggtitle('Target price')

ggplot(mapping = aes(unlist(s[s<100])))+ geom_bar()

table(house_features$Alley, useNA = 'ifany')

house_features_chr

training_data_factors <- training_data %>% mutate_if(is.character,as.factor)

train_df <- missForest(training_data_factors, verbose = TRUE)

data <- train_df[1]$ximp

# remove columns with two many missing values (iteratively based on later code)

#df_train <- df_train %>% select(-c('PoolQC', 'MiscFeature', 'Alley',
#                                   'Fence', 'FireplaceQu'))

# split character columns

df_train_fct_columns <- data %>% select(where(is.factor))

summarise_df <- function(df) {
  a <- summarise(df, across(everything(), n_distinct)) %>%
    add_column(summary_type = 'n_distinct', .before = 1)
  b <- summarise(df, across(everything(), ~ sum(is.na(.)))) %>%
    add_column(summary_type = 'count_na', .before = 1)
  c <- summarise(df, across(everything(), ~ sum(!is.na(.)))) %>%
    add_column(summary_type = 'count_not_na', .before = 1)
  r1 <- bind_rows(a, b, c) 
  r2 <- pivot_longer(r1,
                     cols = -summary_type, 
                     names_to = 'variable', 
                     values_to = 'count')
  r3 <- pivot_wider(r2,
                    names_from = 'summary_type', 
                    values_from = 'count')
  r3 %>% arrange(variable)
}

df_train_fct_columns_info <- summarise_df(df_train_fct_columns)
knitr::kable(df_train_fct_columns_info, 'simple')

# split numeric columns
df_train_numeric_columns <- data %>% select(where(is.numeric))

df_train_numeric_columns_info <- summarise_df(df_train_numeric_columns)
knitr::kable(df_train_numeric_columns_info, 'simple')

# any other column types we missed above?
# df_train %>% select(where(~ !is.character(.) && !is.numeric(.) ))

ggplot(data = target_price, aes(x = SalePrice)) + 
  geom_histogram() + ggtitle('Target price')

plot_them <- function(df) {
  ggplot(data = df, aes(x = value)) + 
    stat_count() + 
    facet_wrap(~variable, scales = "free")
}

df_train_fct_columns.melt <- df_train_fct_columns %>% 
  pivot_longer(everything(), names_to = 'variable', values_to = 'value')
plot_them(df_train_fct_columns.melt)

df_train_numeric_columns.melt <- df_train_numeric_columns %>% 
  pivot_longer(everything(), names_to = 'variable', values_to = 'value')
plot_them(df_train_numeric_columns.melt)

mybarplot <- function(df, variable, l) {
  ggplot(df, aes_string(x=variable)) +
    geom_bar() +
    xlab(label=l)+
    theme_classic() +
    theme(axis.title.y = element_blank())
}

for (col in colnames(df_train_chr_columns)) {
  print(mybarplot(df_train_chr_columns, col, col))
}


