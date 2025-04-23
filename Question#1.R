library(tidyverse)
library(scales)
library(dplyr)
library(stringr)


movies_initial <- read_csv("TMDB_movie_dataset_v11.csv")

month_translation <- c(
  Jan = "Січ", Feb = "Лют", Mar = "Бер", Apr = "Кві", May = "Тра", Jun = "Чер",
  Jul = "Лип", Aug = "Сер", Sep = "Вер", Oct = "Жов", Nov = "Лис", Dec = "Гру")

untitled_translations <- c("untitled", "без назви", "بدون عنوان", "Без названия", 
                           "Senza Titolo", "Bez názvu", "Ohne Titel", "無題",
                           "Sin título", "Isimsiz", "Uten tittel")

revenue_to_na <- c(1407985, 1270893, 1224207, 1326885, 1294302, 1236552)
budget_to_na <- c(1381066, 1235037, 1224207, 1057999, 1022208, 
                  1201764, 1399448, 1426913, 1449031, 1320160, 
                  1453767, 1453985, 1414861, 1398923, 1365277,
                  1417006, 1441191, 1450893, 1450893, 1301115, 1228885,
                  1272552, 1229118, 1294480, 622311, 1369796, 1108211,
                  1159786, 1136385, 1274485, 791799, 1407307, 1168339,
                  1367551, 615895, 800954, 1436352)
runtime_to_na <- c(206026, 368247, 717019, 206026, 392372, 454409, 500980,
                   544686, 732330, 66871, 633832, 671214, 531640, 535892,
                   523167, 631038, 698754, 685310, 651033, 125120, 298752)

movies_clean <- movies_initial %>%
  
  #Numeric columns
  mutate(
    vote_average = if_else(vote_average <= 0, NA_real_, vote_average),
    vote_count = if_else(vote_count < 0, NA_integer_, vote_count),
    revenue = if_else(revenue < 0, NA_real_, revenue),
    runtime = if_else(runtime <= 0, NA_real_, runtime),
    budget = if_else(budget < 10, NA_real_, budget)) %>%
  
  #Categorical variables
  mutate(  
    status = factor(status),
    original_language = factor(original_language)) %>%
  
  #Text columns
  mutate(
    production_companies = na_if(str_trim(production_companies), ""),
    keywords = na_if(str_trim(keywords), ""),
    genres = na_if(str_trim(genres), "")) %>%
  
  #Untitled rows
  mutate(
    title = if_else(str_to_lower(title) %in% untitled_translations, NA_character_, title),
    original_title = if_else(str_to_lower(original_title) %in% untitled_translations, NA_character_, original_title)) %>%
  
  #Date  
  mutate(  
    release_date = gsub('"', '', release_date),
    release_date = gsub("\\s+", "", release_date),
    release_date = as.Date(release_date),
    release_year = year(release_date),
    release_month = month(release_date),
    release_day = day(release_date),
    release_month_ua = month_translation[as.character(month(release_date, 
                                                            label = TRUE, abbr = TRUE))]) %>%
  
  #Date labels
  mutate(
    season = case_when(
      release_month_ua %in% c("Січ", "Лют", "Бер") ~ "Зима",
      release_month_ua %in% c("Кві", "Тра", "Чер") ~ "Весна",
      release_month_ua %in% c("Лип", "Сер", "Вер") ~ "Літо",
      release_month_ua %in% c("Жов", "Лис", "Гру") ~ "Осінь",
      TRUE ~ NA_character_)) %>%
  
  #Manual error correction
  mutate(
    revenue = if_else(release_date > Sys.Date(), NA_real_, revenue)) %>%
  
  mutate(
    revenue = if_else(id %in% revenue_to_na, NA_real_, revenue),
    budget  = if_else(id %in% budget_to_na, NA_real_, budget),
    runtime = if_else(id %in% runtime_to_na, NA_real_, runtime)) %>%
  
  #Unnecessary columns removal
  select(-backdrop_path, -homepage, -popularity, -poster_path, -imdb_id)







movies <- movies_clean

# q-q-графік для бюджету
ggplot(movies, aes(sample = budget)) +
  stat_qq(alpha = 0.3) +
  stat_qq_line() +  
  labs(x = "Теоретичні квантилі", y = "Квантилі бюджету") +
  theme_minimal()+
  theme(text = element_text(size = 15))

# log q-q-графік для бюджету
ggplot(movies, aes(sample = log(budget))) +
  stat_qq(alpha = 0.3) +
  stat_qq_line() +  
  labs(x = "Теоретичні квантилі", y = "Квантилі логарифмованого бюджету") +
  theme_minimal()+
  theme(text = element_text(size = 15))


# q-q-графік для тривалості
ggplot(movies, aes(sample = runtime)) +
  stat_qq(alpha = 0.3) +
  stat_qq_line() + 
  labs(x = "Теоретичні квантилі", y = "Квантилі тривалості фільму") +
  theme_minimal()+
  theme(text = element_text(size = 15))

# q-q-графік для тривалості
ggplot(movies, aes(sample = log(runtime))) +
  stat_qq(alpha = 0.3) +
  stat_qq_line() + 
  labs(x = "Теоретичні квантилі", y = "Квантилі логарифмованої тривалості") +
  theme_minimal()+
  theme(text = element_text(size = 15))







"Які компанії мають найвищий сумарний бюджет?"

# робимо окремий рядок для кожної компаній зі списку для одного фільму
separate_companies <- movies %>%
  separate_longer_delim(production_companies, delim = ", ")


# зберемо в одну назву деякі відомі компанії з великою кількістю менших підрозділів
separate_companies <- separate_companies %>%
  mutate(production_companies = case_when(
    str_detect(production_companies, "20th Century|Fox Searchlight Pictures|Fox 2000 Pictures") ~ "20th Century Studios",
    str_detect(production_companies, "Disney") ~ "The Walt Disney Company",
    str_detect(production_companies, "Sony Pictures") ~ "Sony Pictures",
    str_detect(production_companies, "DreamWorks") ~ "DreamWorks",
    str_detect(production_companies, "Paramount") ~ "Paramount Pictures",
    str_detect(production_companies, "Columbia") ~ "Columbia Pictures",
    str_detect(production_companies, "Universal") ~ "Universal Pictures",
    str_detect(production_companies, "Warner Bros") ~ "Warner Bros.",
    str_detect(production_companies, "Bout Me") ~ "Bout Me Healing Production",
    str_detect(production_companies, "Lionsgate") ~ "Lionsgate",
    str_detect(production_companies, "Miramax") ~ "Miramax Films",
    str_detect(production_companies, "Marvel") ~ "Marvel Studios",
    TRUE ~ production_companies
  ))


# сумарні бюджети для компаній
company_budget_sum <- separate_companies %>%
  group_by(production_companies) %>%
  filter(!production_companies %in% NA)%>%
  summarize(sum_budget = sum(budget, na.rm = TRUE), 
            movie_count = n() ) %>%
  arrange(desc(sum_budget))%>%
  slice_max(sum_budget, n = 20)

# сумарні бюджети та кількість фільмів для компаній
company_budget_sum %>%  arrange(desc(company_budget_sum$sum_budget)) %>% view()

# побудова графіка
ggplot(company_budget_sum, aes(x = reorder(production_companies, sum_budget), y = sum_budget, size = movie_count)) +
  geom_segment( aes(x=reorder(production_companies, sum_budget), 
                    xend=reorder(production_companies, sum_budget),
                    y=0, yend=sum_budget), color="grey") +
  geom_point( color="orange", size=4)+
  coord_flip()+
  labs(x = "Компанії", y = "Сумарний бюджет", size = "Кількість фільмів")+
  theme(text = element_text(size = 15))+
  theme_bw()+
  scale_y_continuous(labels = dollar_format(scale = 1e-6, suffix = "M"))







"Чи впливає тривалість фільму на бюджет?"

# позбудемося рядків що не попадуть на графік
new_movies <- movies %>% filter(!is.na(runtime)) 
new_movies <- new_movies %>%filter(!is.na(budget))


# view найбільший бюджет
new_movies %>% select(id, title,budget, runtime) %>%
  arrange(desc(new_movies$budget)) %>% view()

# view найменший бюджет
new_movies %>% select(id, title,budget) %>%
  arrange(new_movies$budget) %>% view()

# view найбільша тривалість
new_movies %>% select(id, title,budget, runtime) %>%
  arrange(desc(new_movies$runtime)) %>% view()

# view найменшиа тривалість
new_movies %>% select(id, title,runtime) %>%
  arrange(new_movies$runtime) %>% view()

runtime_to_na <- c(1221350, 1407985, 1407979, 1329543, 1297501, 1149393, 1297501,
                   1422046, 1305464, 1305448, 1389081, 1411972, 604529, 1280165,
                   1280118, 1309787, 1443026, 1391492, 1305527, 1304078, 1219508, 
                   1198660, 1446249, 1252015, 1394286, 1375597, 1251255, 1251251,
                   1392831, 1123268, 1388897, 898555, 1225733, 1455260, 375569, 
                   1174831, 1261349, 1233925, 64505, 1337824, 1210368, 1231022,
                   1194488, 1291825, 1243479, 1261317, 1297562, 1454675, 916116,
                   787838, 1372631, 1454247,  1291168, 1245274, 1244858, 1229874,
                   1232028, 1244454, 1428634, 998162, 1236010, 1026095, 292374,
                   1234955, 29266, 1369012, 1400976, 1425003, 1338706, 1228387,
                   914213, 1412549, 1425498, 1370730, 1200123, 1237183)

new_movies$runtime[new_movies$id %in% runtime_to_na] <- NA

# побудувати без нулів
ggplot(new_movies, aes(x = runtime, y = budget))+
  geom_point(alpha = 0.3, color='darkblue')+
  labs(x = "Тривалість фільму", y = "Бюджет")+
  theme(text = element_text(size = 125))+
  theme_bw()+
  scale_x_continuous(labels = function(x) paste(x, "min"))+
  scale_y_continuous(labels = dollar_format(scale = 1e-6, suffix = "M"))



# обмежити найбільші значення
new_movies = subset(new_movies, new_movies$runtime < 500)

# побудувати обмежений графік
ggplot(new_movies, aes(x = runtime, y = budget))+
  geom_point(alpha = 0.3, color='darkblue')+
  labs(x = "Тривалість фільму", y = "Бюджет")+
  theme(text = element_text(size = 125))+
  theme_bw()+
  scale_x_continuous(labels = function(x) paste(x, "min"))+
  scale_y_continuous(labels = dollar_format(scale = 1e-6, suffix = "M"))






"Чи впливає жанр фільму на бюджет?"


# робимо окремий рядок для кожного жанру зі списку для одного фільму
separate_genres <- movies %>%
  separate_longer_delim(genres, delim = ", ")%>%filter(!is.na(genres))

# перетворюємо стовпець з жанрами на факторний (необхідно для побудови графіка)
separate_genres$genres <- as.factor(separate_genres$genres)

# викремимо потрібні стовпці
genres_budget <- separate_genres %>% select(id, genres, budget)


# побудова графіка
ggplot(genres_budget, aes(x=genres, y=budget, fill=genres)) + 
  geom_violin(color = "black", alpha = 0.6)+
  geom_boxplot(fill = "lightgreen", alpha = 0.5)+
  scale_y_log10()+
  labs(x = "Жанри", y = "Бюджет")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")