library(tidyverse)
library(stringr)


data <- read.csv("D:\\Навчання\\3 Курс\\2 Семестр\\AD\\TMDB_movie_dataset_v11.csv", 
                 stringsAsFactors = FALSE)

# dictionary of month translations
month_translation <- c(
  Jan = "Січ", Feb = "Лют", Mar = "Бер",
  Apr = "Кві", May = "Тра", Jun = "Чер",
  Jul = "Лип", Aug = "Сер", Sep = "Вер",
  Oct = "Жов", Nov = "Лис", Dec = "Гру"
)

cleaned_data <- data %>%
#Numeric columns
mutate(
  vote_average = if_else(vote_average <= 0, NA_real_, vote_average),
  vote_count = if_else(vote_count < 0, NA_integer_, vote_count),
  revenue = if_else(revenue < 0, NA_real_, revenue),
  runtime = if_else(runtime <= 0, NA_real_, runtime),
  genres = na_if(genres, ""),
  budget = if_else(budget <= 0, NA_real_, budget)) %>%

#Factor variables
mutate(  
  status = factor(status),
  original_language = factor(original_language),
) %>%
  
mutate(
  genres_split = str_split(genres, ", ")
) %>%

#Text columns
mutate(
 production_companies = na_if(str_trim(production_companies), ""),
 keywords = na_if(str_trim(keywords), "")) %>%

#Date

mutate(  
  release_date = gsub('"', '', release_date),
  release_date = gsub("\\s+", "", release_date),
  release_date = as.Date(release_date),
  release_year = year(release_date),
  release_month = month(release_date),
  release_month_ua = month_translation[as.character(month(release_date, label = TRUE, abbr = TRUE))]
) %>%

#Date labels
mutate(
  season = case_when(
    release_month_ua %in% c("Січ", "Лют", "Бер") ~ "Зима",
    release_month_ua %in% c("Кві", "Тра", "Чер") ~ "Весна",
    release_month_ua %in% c("Лип", "Сер", "Вер") ~ "Літо",
    release_month_ua %in% c("Жов", "Лис", "Гру") ~ "Осінь",
    TRUE ~ "Unknown"
  )
) %>%

#Remove unnecessary columns
select(-backdrop_path, -homepage, -popularity, -poster_path, -imdb_id)

music_movies <- cleaned_data %>%
  filter(str_detect(genres, "\\bMusic\\b"))
sum(cleaned_data$vote_count >= 10, na.rm = TRUE)

# Отримуємо мінімальне та максимальне значення рейтингу
min_rating <- min(cleaned_data$vote_average, na.rm = TRUE)
max_rating <- max(cleaned_data$vote_average, na.rm = TRUE)

# Виведення результатів
cat("Мінімальний рейтинг:", min_rating, "\n")
cat("Максимальний рейтинг:", max_rating, "\n")

# Підрахунок кількості фільмів з рейтингом 0, з обробкою NA
zero_rating_count_check <- sum(cleaned_data$vote_average == 0, na.rm = TRUE)
cat("Кількість значень рейтингу, рівних 0:", zero_rating_count_check)

genre_votes <- cleaned_data %>%
  select(title, genres, vote_average, vote_count, genres_split) %>%
  filter(vote_count >= 10)


##кількість фільмів увигляді таблиці


## Підрахунок кількості фільмів для кожного жанру
genre_count <- cleaned_data %>%
  unnest(genres_split) %>%  # Розгортаємо жанри на окремі рядки
  group_by(genres_split) %>%  # Групуємо за жанром
  summarise(count = n(), .groups = "drop") %>%  # Підраховуємо кількість фільмів для кожного жанру
  arrange(desc(count))  # Сортуємо за кількістю

# Виведення результату
print(genre_count, n = Inf)


# Підрахунок кількості фільмів у кожному жанрі (використовуючи перший жанр)
genre_count_first_genre <- cleaned_data %>%
  mutate(first_genre = sapply(genres_split, function(x) x[1])) %>%  # Отримуємо перший жанр
  group_by(first_genre) %>%
  summarise(count = n()) %>%
  arrange(desc(count))  # Сортуємо за кількістю

# Виведення результату у вигляді таблиці
print(genre_count_first_genre, n = Inf)

## Графік середні оцінки за жанрами 

# Обчислення середньої оцінки для кожного жанру
avg_ratings <- cleaned_data %>%
  select(genres_split, vote_average) %>%
  filter(!is.na(vote_average), vote_average > 0) %>%
  unnest(genres_split) %>%  # Розгортаємо список
  mutate(genres_split = str_trim(genres_split)) %>%
  mutate(genres_split = na_if(genres_split, "")) %>%
  filter(!is.na(genres_split)) %>%
  group_by(genres_split) %>%
  summarise(mean_rating_genre = mean(vote_average), .groups = "drop") %>%
  arrange(desc(mean_rating_genre))

# Побудова графіка середніх оцінок за жанрами
ggplot(avg_ratings, aes(x = reorder(genres_split, mean_rating_genre), y = mean_rating_genre, fill = mean_rating_genre)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Середні оцінки за жанрами", 
       x = "Жанр", 
       y = "Середня оцінка") +
  scale_fill_gradient(low = "blue", high = "red")


## Графік середні оцінки за 1 жанром
cleaned_data_first_genre <- cleaned_data %>%
  select(genres, vote_average) %>%
  filter(!is.na(genres) & !is.na(vote_average) & vote_average != 0) %>%
  mutate(genres = str_split(genres, ",\\s*")) %>%  # Розбиваємо за комою і пробілом
  mutate(genres = map_chr(genres, 1))  # Беремо лише перший жанр

# Обчислення середньої оцінки для кожного першого жанру
avg_ratings_first_genre <- cleaned_data_first_genre %>% 
  group_by(genres) %>% 
  summarise(mean_rating_first_genre = mean(vote_average, na.rm = TRUE)) %>%
  arrange(desc(mean_rating_first_genre))

# Побудова графіка середніх оцінок за першими жанрами
ggplot(avg_ratings_first_genre, aes(x = reorder(genres, mean_rating_first_genre), y = mean_rating_first_genre, fill = mean_rating_first_genre)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Середні оцінки за першими жанрами", 
       x = "Перший жанр", 
       y = "Середня оцінка") +
  scale_fill_gradient(low = "blue", high = "red")


# Побудова графіка модуля різниці між середніми оцінками
merged_ratings <- avg_ratings %>%
  left_join(avg_ratings_first_genre, by = c("genres_split" = "genres")) %>%
  mutate(difference = abs(mean_rating_genre - mean_rating_first_genre))  # Використовуємо правильні назви змінних

# Побудова графіка модуля різниці між середніми оцінками
ggplot(merged_ratings, aes(x = reorder(genres_split, difference), y = difference, fill = difference)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Модуль різниці між середніми оцінками за жанрами", 
       x = "Жанр", 
       y = "Модуль різниці") +
  scale_fill_gradient(low = "blue", high = "red")


##двовимірний графік зв'язок між популярністю жанру та середнім рейтингом

# 1. Розбиваємо колонку genres в окремі жанри
genre_stats <- cleaned_data %>%
  filter(!is.na(genres), genres != "") %>%
  mutate(genres = str_split(genres, ",\\s*")) %>%  # розділяємо по комі
  unnest(genres) %>%  # "розгортаємо" жанри у нові рядки
  mutate(genres = str_trim(genres))  # видаляємо зайві пробіли

# 2. Рахуємо середній рейтинг і кількість шоу на кожен жанр
genre_stats_summary <- genre_stats %>%
  group_by(genres) %>%
  summarise(avg_rating = mean(vote_average, na.rm = TRUE),
            count = n(), .groups = "drop") %>%
  arrange(desc(count)) %>%
  mutate(genre_id = row_number(),
         genre_label = paste0(genre_id, ". ", genres))

# 3. Побудова графіку
ggplot(genre_stats_summary, aes(x = count, y = avg_rating, size = count, color = genre_label)) +
  geom_point(alpha = 0.7) +
  scale_size(range = c(3, 10)) +
  geom_text(aes(label = genre_id), vjust = -1, size = 4, color = "black") +
  labs(title = "Зв’язок між популярністю жанру та середнім рейтингом",
       x = "Кількість шоу у жанрі",
       y = "Середній рейтинг",
       size = "Кількість шоу",
       color = "Жанр (номер)") +
  theme_minimal() +
  theme(legend.position = "right",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))


##Середня оцінка 50% найвищих оцінок
#Розділення жанрів
genre_stats <- cleaned_data %>% 
select(genres, vote_average) %>% 
filter(!is.na(genres) & !is.na(vote_average) & vote_average != 0) %>%
separate_rows(genres, sep = ", ")  # Розділити жанри, якщо вони записані через кому

# Фільтруємо для кожного жанру тільки 50% найвищих оцінок
top_10_percent_ratings <- genre_stats %>%
  group_by(genres) %>%
  filter(vote_average >= quantile(vote_average, 0.5, na.rm = TRUE)) %>%
  summarise(mean_rating = mean(vote_average, na.rm = TRUE)) %>%
  arrange(desc(mean_rating))

# Побудова графіка
ggplot(top_10_percent_ratings, aes(x = reorder(genres, mean_rating), y = mean_rating, fill = mean_rating)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Середні оцінки 50% найвищих оцінок за жанрами", 
       x = "Жанр", 
       y = "Середня оцінка 50% найвищих оцінок") +
  scale_fill_gradient(low = "blue", high = "red")


## Медіанна оцінка за жанрами
# 1. Розділення жанрів і фільтрація 0 значень рейтингу
genre_stats <- cleaned_data %>% 
  select(genres, vote_average) %>%
  filter(!is.na(genres) & !is.na(vote_average) & vote_average != 0) %>%
  separate_rows(genres, sep = ",") %>%           # розділяємо, навіть якщо нема пробілу
  mutate(genres = str_trim(genres)) %>%          # видаляємо пробіли
  filter(genres != "")                           # прибираємо порожні жанри

# 2. Обчислення медіанної оцінки для кожного жанру
median_ratings <- genre_stats %>% 
  group_by(genres) %>% 
  summarise(median_rating = median(vote_average, na.rm = TRUE)) %>%  # Використовуємо median
  arrange(desc(median_rating))  # Сортуємо по медіанній оцінці

# 3. Побудова графіка медіанних оцінок за жанрами
ggplot(median_ratings, aes(x = reorder(genres, median_rating), y = median_rating, fill = median_rating)) +
  geom_bar(stat = "identity") +  # Бар графік
  coord_flip() +  # Перевертаємо графік для зручності
  theme_minimal() +  # Простий стиль
  labs(
    title = "Медіанна оцінка за жанрами", 
    x = "Жанр", 
    y = "Медіанна оцінка"
  ) +
  scale_fill_gradient(low = "blue", high = "red")  # Градиєнт кольорів для оцінки


##"Різниця між середньою і медіанною оцінкою за жанрами", 
# 1. Розділення жанрів
genre_stats <- cleaned_data %>% 
  select(genres, vote_average) %>% 
  filter(!is.na(genres) & !is.na(vote_average) & vote_average != 0) %>% 
  separate_rows(genres, sep = ", ")  # Розбиваємо жанри по комі та пробілу

# 2. Обчислення середньої і медіанної оцінки для кожного жанру
ratings_by_genre <- genre_stats %>% 
  group_by(genres) %>% 
  summarise(
    mean_rating = mean(vote_average, na.rm = TRUE),
    median_rating = median(vote_average, na.rm = TRUE)
  ) %>% 
  mutate(rating_difference = mean_rating - median_rating)  # Різниця між середнім і медіанним рейтингом

# 3. Побудова графіка різниці середньої і медіанної оцінки за жанрами
ggplot(ratings_by_genre, aes(x = reorder(genres, rating_difference), y = rating_difference, fill = rating_difference)) +
  geom_bar(stat = "identity") +  # Бар графік
  coord_flip() +  # Перевертаємо графік для зручності
  theme_minimal() +  # Простий стиль
  labs(
    title = "Різниця між середньою і медіанною оцінкою за жанрами", 
    x = "Жанр", 
    y = "Різниця оцінок (середнє - медіанне)"
  ) +
  scale_fill_gradient(low = "blue", high = "red")  # Градиєнт кольорів для різниці


#Графіки з врахуванням, що genre_vote >= 10


## Графік середні оцінки за жанрами

# Обчислення середньої оцінки для кожного жанру
avg_ratings <- genre_votes %>%
  select(genres_split, vote_average) %>%
  filter(!is.na(vote_average), vote_average > 0) %>%
  unnest(genres_split) %>%  # Розгортаємо список
  mutate(genres_split = str_trim(genres_split)) %>%
  mutate(genres_split = na_if(genres_split, "")) %>%
  filter(!is.na(genres_split)) %>%
  group_by(genres_split) %>%
  summarise(mean_rating_genre = mean(vote_average), .groups = "drop") %>%
  arrange(desc(mean_rating_genre))

# Побудова графіка середніх оцінок за жанрами
ggplot(avg_ratings, aes(x = reorder(genres_split, mean_rating_genre), y = mean_rating_genre, fill = mean_rating_genre)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Середні оцінки за жанрами", 
       x = "Жанр", 
       y = "Середня оцінка") +
  scale_fill_gradient(low = "blue", high = "red")

##Графк різниці середньї оцінки за жанрами для всіх фільмів і для фільмів де кількість голосів >10


genre_avg_filtered <- genre_votes %>%
  separate_rows(genres, sep = ",\\s*") %>%
  filter(!is.na(genres) & genres != "") %>%
  group_by(genres) %>%
  summarise(avg_filtered = mean(vote_average, na.rm = TRUE), .groups = "drop")

genre_avg_full <- cleaned_data %>%
  select(genres, vote_average) %>%
  separate_rows(genres, sep = ",\\s*") %>%
  filter(!is.na(genres) & genres != "") %>%
  group_by(genres) %>%
  summarise(avg_full = mean(vote_average, na.rm = TRUE), .groups = "drop")

# 2. Об’єднуємо обидва датафрейми по жанру
genre_diff <- genre_avg_filtered %>%
  inner_join(genre_avg_full, by = "genres") %>%
  mutate(diff = avg_filtered - avg_full) %>%
  arrange(desc(diff))

# 3. Графік
ggplot(genre_diff, aes(x = reorder(genres, diff), y = diff)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(
    title = "Різниця середньої оцінки жанру (filtered vs full dataset)",
    x = "Жанр",
    y = "Різниця оцінки"
  ) +
  theme_minimal()


##двовимірний графік зв'язок між популярністю жанру та середнім рейтингом

# 1. Розбиваємо колонку genres в окремі жанри
genre_stats <- genre_votes %>%
  filter(!is.na(genres), genres != "") %>%
  mutate(genres = str_split(genres, ",\\s*")) %>%  # розділяємо по комі
  unnest(genres) %>%  # "розгортаємо" жанри у нові рядки
  mutate(genres = str_trim(genres))  # видаляємо зайві пробіли

# 2. Рахуємо середній рейтинг і кількість шоу на кожен жанр
genre_stats_summary <- genre_stats %>%
  group_by(genres) %>%
  summarise(avg_rating = mean(vote_average, na.rm = TRUE),
            count = n(), .groups = "drop") %>%
  arrange(desc(count)) %>%
  mutate(genre_id = row_number(),
         genre_label = paste0(genre_id, ". ", genres))

# 3. Побудова графіку
ggplot(genre_stats_summary, aes(x = count, y = avg_rating, size = count, color = genre_label)) +
  geom_point(alpha = 0.7) +
  scale_size(range = c(3, 10)) +
  geom_text(aes(label = genre_id), vjust = -1, size = 4, color = "black") +
  labs(title = "Зв’язок між популярністю жанру та середнім рейтингом",
       x = "Кількість шоу у жанрі",
       y = "Середній рейтинг",
       size = "Кількість шоу",
       color = "Жанр (номер)") +
  theme_minimal() +
  theme(legend.position = "right",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))
