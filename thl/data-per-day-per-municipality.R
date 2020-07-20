library(tidyverse)
library(rjstat)
library(lubridate)

base_url <- 'https://sampo.thl.fi/pivot/prod/fi/epirapo/covid19case/'

#
# Case count per age group
# 

data <- fromJSONstat(str_c(base_url, 
                          'fact_epirapo_covid19case.json?column=ttr10yage-444309'))
count_per_age_group <- 
  data$`Tartuntatautirekisterin COVID-19-tapaukset` %>%
  as_tibble() %>%
  rename(age=ttr10yage, case_count=value) %>%
  mutate(case_count = as.numeric(case_count))
colnames(count_per_age_group)

count_per_age_group %>%
  filter(age != 'Kaikki ikäryhmät') %>%
  ggplot(aes(x=age, y=case_count)) +
  geom_col() +
  theme_light()

#
# Case count per date per municipality
# 

data2 <- fromJSONstat(str_c(base_url, 
                          'fact_epirapo_covid19case.json?',
                          'row=dateweek2020010120201231-443702L&column=hcdmunicipality2020-445222'))
count_per_date_per_mun <-
  data2$`Tartuntatautirekisterin COVID-19-tapaukset` %>%
  as_tibble() %>%
  rename(date=dateweek2020010120201231, 
         municipality=hcdmunicipality2020, 
         case_count=value) %>%
  mutate(date = as.Date(date), case_count = as.numeric(case_count)) %>%
  filter(municipality != "Kaikki Alueet")

colnames(count_per_date_per_mun)

count_per_date_per_mun %>%
  filter(date>'2020-06-01',
         date<today()) %>%
ggplot(aes(x=date, y=case_count, group=municipality)) +
  geom_col() +
  theme_light() +
  facet_wrap(~ municipality)
