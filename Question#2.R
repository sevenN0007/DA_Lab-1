library(tidyverse)


df <- read_csv("TMDB_movie_dataset_v11.csv")


df_clean <- cleaned_data %>%
  mutate(
    release_month = month(release_date, label = TRUE, abbr = TRUE, locale = "uk_UA.UTF-8"),
    release_month_ua = as.character(release_month),
    season = case_when(
      release_month %in% c("Січ", "Лют", "Бер") ~ "Зима",
      release_month %in% c("Кві", "Тра", "Чер") ~ "Весна",
      release_month %in% c("Лип", "Сер", "Вер") ~ "Літо",
      release_month %in% c("Жов", "Лис", "Гру") ~ "Осінь",
      TRUE ~ "Unknown"
    )
  )


# 1. ЖАНРИ ЗА КІЛЬКІСТЮ ФІЛЬМІВ
df_clean <- df %>%
  mutate(
    genres = ifelse(is.na(genres), "Unknown", genres),  
    genres_split = str_split(genres, ",")  
  )
head(df_clean)
unique_genres <- df_clean %>%
  unnest(genres_split) %>%
  mutate(
    genres_split = str_trim(genres_split),          
    genres_split = str_to_title(genres_split)       
  ) %>%
  filter(genres_split != "Unknown") %>%
  distinct(genres_split)


# топ-20 жанрів без Unknown
top_genres_all <- df_clean %>%
  unnest(genres_split) %>%
  mutate(
    genres_split = str_trim(genres_split),           
    genres_split = str_to_title(genres_split)        
  ) %>%
  filter(genres_split != "Unknown") %>%
  group_by(genres_split) %>%
  summarise(total_count = n(), .groups = 'drop') %>%
  arrange(desc(total_count)) %>%
  slice_max(order_by = total_count, n = 20, with_ties = TRUE)

# приберемо жанри які повторюються
top_genres_all <- top_genres_all %>%
  distinct(genres_split, .keep_all = TRUE)


ggplot(top_genres_all, aes(x = reorder(genres_split, total_count), y = total_count, fill = genres_split)) + 
  geom_bar(stat = "identity") +  
  labs(
    title = "Жанри за кількістю фільмів",
    x = "Жанр",
    y = "Кількість фільмів"
  ) +
  coord_flip() +  
  scale_y_continuous(labels = scales::comma) +  
  theme_minimal() +  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    legend.position = "none"  
  )



# №2 ЯКІ ЖАНРИ ВИХОДЯТЬ НАЙЧАСТІШЕ У ПЕВНІ СЕЗОНИ
genre_season <- df_clean %>%
  filter(season != "Unknown") %>%  
  unnest(genres_split) %>%
  mutate(
    genres_split = str_trim(genres_split),  
    genres_split = str_to_title(genres_split)  
  ) %>%
  filter(genres_split != "Unknown") %>%  
  group_by(season, genres_split) %>%
  summarise(count = n(), .groups = 'drop')
print(genre_season)




ggplot(genre_season, aes(x = genres_split, y = count, fill = genres_split)) +
  geom_col() +  # Побудова стовпчикової діаграми
  facet_wrap(~season, scales = "fixed", ncol = 2) +  # Фасетування за сезонами
  labs(
    x = "Жанр",
    y = "Кількість фільмів"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),  
    legend.position = "none",  
    strip.text = element_text(size = 12)  
  )




# ІНТЕНСИВНІСТЬ ПОЯВИ ЖАНРІВ У СЕЗОНАХ
ggplot(genre_season, aes(x = season, y = fct_reorder(genres_split, desc(count)), fill = count)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "plasma", direction = -1) +  
  labs(
    title = "Інтенсивність появи жанрів у сезонах",
    x = "Сезон",
    y = "Жанр",
    fill = "Кількість"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )


# ПРИБУТОК ЗА СЕЗОНАМИ
season_revenue <- df_clean %>%
  filter(
    revenue > 0,
    season != "Unknown",
    !id %in% c(1270893, 1224207)          
  ) %>%                            
  group_by(season) %>%
  summarise(avg_revenue = mean(revenue, na.rm = TRUE)) %>%
  arrange(desc(avg_revenue))

ggplot(season_revenue, aes(x = season, y = avg_revenue, fill = season)) +
  geom_col() +
  labs(x = "Сезон", y = "Середній дохід (USD)", title = "Середній касовий збір фільмів за сезонами") +
  scale_y_continuous(labels = scales::comma) +
  theme(legend.position = "none")  

