#' ---
#' title: "House Prices: Advanced Regression Techniques"
#' subtitle: Data pre-processing and visualization
#' author: Statistical Mouse
#' output: html_notebook
#' ---

library(tidyverse)
library(patchwork)

# read the data
df_train = read_csv('../data/house_prices/train.csv') %>%
  column_to_rownames(var = 'Id')

# split the target column
df_train_target_price <- df_train %>% select(SalePrice)
df_train <- df_train %>% select(-SalePrice)

# remove columns with two many missing values (iteratively based on later code)

#df_train <- df_train %>% select(-c('PoolQC', 'MiscFeature', 'Alley',
#                                   'Fence', 'FireplaceQu'))

# split character columns
df_train_chr_columns <- df_train %>% select(where(is.character))

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

df_train_chr_columns_info <- summarise_df(df_train_chr_columns)
knitr::kable(df_train_chr_columns_info, 'simple')

# split numeric columns
df_train_numeric_columns <- df_train %>% select(where(is.numeric))

df_train_numeric_columns_info <- summarise_df(df_train_numeric_columns)
knitr::kable(df_train_numeric_columns_info, 'simple')

# any other column types we missed above?
# df_train %>% select(where(~ !is.character(.) && !is.numeric(.) ))

ggplot(data = df_train_target_price, aes(x = SalePrice)) + 
  geom_histogram() + ggtitle('Target price')

plot_them <- function(df) {
  ggplot(data = df, aes(x = value)) + 
    stat_count() + 
    facet_wrap(~variable, scales = "free")
}

df_train_chr_columns.melt <- df_train_chr_columns %>% 
  pivot_longer(everything(), names_to = 'variable', values_to = 'value')
plot_them(df_train_chr_columns.melt)

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


