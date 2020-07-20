library(tidyverse)

base_url <- 'https://sampo.thl.fi/pivot/prod/fi/epirapo/covid19case/'

# try reading the data dimensions, and print them (not used)
dimensions_url <- str_c(base_url, 'fact_epirapo_covid19case.dimensions.json')
dimensions_text <- content(GET(dimensions_url), 'text')
dimensions_text <- str_replace(dimensions_text, 'thl\\.pivot\\.loadDimensions\\(', '')
dimensions_text <- str_replace(dimensions_text, '\\);', '')
dimensions <- fromJSON(dimensions_text)
dimensions <- as_tibble(dimensions)
print(dimensions)
municipalities <- as_tibble((dimensions[[3]][[1]])[[8]][[1]])
print(municipalities)
hus_municipalities <- as_tibble(municipalities[[8]][[21]])
print(hus_municipalities)
# ^ not used

#
# Case count per age group
# 

aineisto <- fromJSON(str_c(base_url, 
                           'fact_epirapo_covid19case.json?column=ttr10yage-444309'))

#puretaan kategoriat paloiksi
label <- as.data.frame(unlist(aineisto$dataset$dimension$ttr10yage$category$label))
index <- as.data.frame(unlist(aineisto$dataset$dimension$ttr10yage$category$index))

#Nimetään palaset
names(label)<-"label"
names(index)<-"index"

#Laitetana vielä rivinumerot sarakkeiksi, jotta nämä saadaan yhteen.
label<-rownames_to_column(label)
index<-rownames_to_column(index)

#Yhdistetään rivinimeä käyttäen
kategoriat <- index %>% left_join(label,by="rowname")

#otetaan data
data <- as.data.frame(unlist(aineisto$dataset$value))

#Nimetään
names(data)<-"case_count"
data<-rownames_to_column(data)
data$rowname<-as.numeric(data$rowname)

#Yhdistetään muuhun aineistoon
dataset <- kategoriat %>% left_join(data,by=c("index"="rowname"))
dataset <- as_tibble(dataset)
dataset$case_count = as.numeric(dataset$case_count)
View(dataset)
dataset %>%
  filter(label != 'Kaikki ikäryhmät') %>%
  ggplot(aes(x=label, y=case_count)) +
  geom_col() +
  theme_light()


