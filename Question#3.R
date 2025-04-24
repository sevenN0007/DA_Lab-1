library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(tidyr)


#setwd("C:/Users/c2sma/OneDrive/Документи/R-TT/R-Studio/Lab1withR")

source("General_cleaning.R")  # підключаємо файл з функцією очистки даних

# 1.Очищаємо дані
movies_cleaned <- movies_cleaned %>%
  filter(runtime > 80 & runtime < 500, vote_count > 3000)


# Об'єднання компаній за ключовими словами
movies_cleaned <- movies_cleaned %>%
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
    TRUE ~ production_companies
  ))

# Загальний середній рейтинг по всім фільмам (m)
global_avg_rating <- mean(movies_cleaned$vote_average, na.rm = TRUE)

#  Сепарація
movies_separated <- movies_cleaned %>%
  filter(!is.na(production_companies)) %>%
  separate_rows(production_companies, sep = ", ")

# 3. Підрахунок
company_counts <- movies_separated %>%
  count(production_companies, name = "num_movies")

# 4. Визначаємо C як 25-й процентиль кількості фільмів
C <- quantile(company_counts$num_movies, 0.25, na.rm = TRUE)

# 4. Визначаємо C як медіану
#C <- median(company_counts$num_movies, na.rm = TRUE)

print(C)

# 5. Рахуємо рейтинг компаній з урахуванням кількості фільмів
company_ratings <- movies_cleaned %>%
  filter(!is.na(production_companies), !is.na(vote_average)) %>%
  separate_rows(production_companies, sep = ", ") %>%
  group_by(production_companies) %>%
  summarise(
    num_movies = n(),
    sum_rating = sum(vote_average, na.rm = TRUE)
  ) %>%
  filter(num_movies > 5) %>%  # Фільтруємо компанії з більше ніж 5 фільмами
  mutate(weighted_rating = (sum_rating + C * global_avg_rating) / (num_movies + C)) %>%
  arrange(desc(weighted_rating))


# Візуалізація топ-40 компаній
ggplot(company_ratings %>% slice_max(n = 40, order_by = weighted_rating, with_ties = FALSE),
       aes(x = reorder(production_companies, weighted_rating), y = weighted_rating, fill = weighted_rating)) +
  geom_col() +
  geom_text(aes(label = round(weighted_rating, 2)), hjust = -0.2, size = 5) +  
  coord_flip() +
  scale_fill_gradient(low = "skyblue", high = "darkblue") +
  labs(title = "Топ-40 компаній за зваженим рейтингом",
       x = "Кінокомпанія",
       y = "Зважений рейтинг") +
  theme_minimal()




# 3. Пузирковий графік (Bubble Chart)
ggplot(company_ratings %>% slice_max(n = 40, order_by = weighted_rating, with_ties = FALSE), 
       aes(x = num_movies, y = weighted_rating, size = num_movies, color = weighted_rating)) +
  geom_point(alpha = 0.7) +
  scale_color_gradient(low = "green", high = "blue") +
  labs(title = "Топ-40 компаній: рейтинг vs кількість фільмів",
       x = "Кількість фільмів",
       y = "Зважений рейтинг",
       size = "Кількість фільмів") +
  theme_minimal()

company_counts %>%
  slice_max(num_movies, n = 1)

ggplot(company_counts, aes(x = num_movies)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black") +
  labs(title = "Розподіл кількості фільмів на компанію",
       x = "Кількість фільмів",
       y = "Кількість компаній") +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray", size = 0.5),
    panel.grid.minor = element_line(color = "gray", size = 0.25),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )




# Вибираємо топ-6 компаній за зваженим рейтингом
top6_companies <- company_ratings %>%
  slice_max(n = 6, order_by = weighted_rating)

# Фасетна гістограма для топ-6 компаній
movies_cleaned %>%
  filter(!is.na(production_companies), !is.na(vote_average)) %>%
  separate_rows(production_companies, sep = ", ") %>%
  filter(production_companies %in% top6_companies$production_companies) %>%
  ggplot(aes(x = vote_average)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "black") +
  facet_wrap(~ production_companies) +
  labs(
    title = "Розподіл рейтингів фільмів по топ-6 компаніях",
    x = "Середній рейтинг",
    y = "Кількість фільмів"
  ) +
  theme_minimal()

# Обираємо топ-10 компаній за зваженим рейтингом
top10_companies <- company_ratings %>%
  slice_max(order_by = weighted_rating, n = 10)

# Створюємо дані для графіка
mekko_data <- top10_companies %>%
  mutate(share = num_movies / sum(num_movies))  # частка фільмів

# Побудова графіка
ggplot(mekko_data, aes(x = reorder(production_companies, -share), y = weighted_rating, fill = weighted_rating)) +
  geom_col(width = mekko_data$share, color = "black") +  
  scale_fill_gradient(low = "lightyellow", high = "red") +
  labs(
    title = "частка фільмів і зважений рейтинг компаній",
    x = "Кінокомпанія (ширина — частка за кількістю фільмів)",
    y = "Зважений рейтинг"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


