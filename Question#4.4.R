#STEP 1: LOADING DATA AND INITIAL INSPECTION
library(scales)
library(tidyverse)
library(viridis)
library(wordcloud2)

movies_initial <- read_csv("TMDB_movie_dataset_v11.csv")

head(movies_initial)
str(movies_initial)
summary(movies_initial)

#STEP 2: DATA CHECKING AND INITIAL TRANSFORMATIONS
na_counts <- colSums(is.na(movies_initial))
print(na_counts)

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
                  1272552, 1229118, 1294480, 622311, 1369796, 1108211)
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



#STEP 3: FILTERING DATA AND LOOKING FOR ERRONEOUS ENTRIES
movies_released <- movies_clean %>%
  filter(status == "Released", release_date <= Sys.Date())

#Extreme revenue cases in terms of revenue:
top_revenue <- movies_released %>%  arrange(desc(revenue)) %>% slice_head(n = 50)
bottom_revenue <- movies_released %>% arrange(revenue) %>% slice_head(n = 50)

#Extreme revenue cases in terms of recency:
latest_release <- movies_released %>% arrange(desc(release_date)) %>% slice_head(n = 50)
earliest_release <- movies_released %>% arrange(release_date) %>% slice_head(n = 50)



#STEP 4: VISUALIZING REVENUE DISTRIBUTIONS
movies_released_nonzero <- movies_released %>%
  filter(revenue > 0)

#Histogram of revenue (log-transformation)
ggplot(movies_released_nonzero, aes(x = revenue)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x), labels = scales::comma) +
  labs(title = "Розподіл доходів", x = "Дохід", y = "Частота") +
  theme_minimal() +
  theme(
  plot.title = element_text(size = 20),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text = element_text(size = 14))

#Histogram of release years
ggplot(movies_released_nonzero, aes(x = release_year)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  labs(title = "Розподіл років", x = "Рік виходу", y = "Частота") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14))



#STEP 5: REVENUE BY RELEASE YEAR
revenue_yearly <- movies_released %>%
  group_by(release_year) %>%
  summarise(
    avg_revenue = mean(revenue, na.rm = TRUE),
    med_revenue = median(revenue, na.rm = TRUE),
    std_revenue = sd(revenue, na.rm = TRUE),
    count = count()
  ) %>%
  mutate(sem_revenue = std_revenue / sqrt(count))

#Data from 1950 onward
ggplot(revenue_yearly, aes(x = release_year, y = avg_revenue)) +
  geom_line(color = "purple4", size = 1.1) +
  geom_point(size = 4, color = "orange") +
  geom_errorbar(aes(ymin = avg_revenue - sem_revenue, ymax = avg_revenue + sem_revenue),
                width = 0.7, color = "gray34") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(
    limits = c(1950, max(revenue_yearly$release_year, na.rm = TRUE)),
    breaks = seq(1950, max(revenue_yearly$release_year, na.rm = TRUE), by = 5)) +
  labs(title = "Зміна середнього доходу з роками", x = "Рік виходу", y = "Середній дохід") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14))

movies_1997 <- movies_clean %>%
  filter(release_year == 1997) %>%
  arrange(desc(revenue)) %>%
  select(title, revenue)
print(movies_1997)



#STEP 6: REVENUE DISTRIBUTIONS BY DECADE
movies_aft_1910 <- movies_released_nonzero %>%
  filter(release_year >= 1910)

#Group of density graphs
movies_aft_1910 %>%
  mutate(decade = paste0(floor(release_year / 10) * 10, "s")) %>%
  ggplot(aes(x = revenue)) +
  geom_density(fill = "lightblue", alpha = 0.7) +
  scale_x_log10() +
  facet_wrap(~ decade, scales = "free_x") +
  labs(title = "Графік щільності за десятиріччями після логарифмічної трансформації",
       x = "Дохід", y = "Щільність") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 14))

movies_1930s <- movies_clean %>%
  filter(release_year >= 1930, release_year < 1940) %>%
  arrange(desc(revenue)) %>%
  select(title, revenue)
print(movies_1930s)

movies_1940s <- movies_clean %>%
  filter(release_year >= 1940, release_year < 1950) %>%
  arrange(desc(revenue)) %>%
  select(title, revenue)
print(movies_1940s)

movies_2010s <- movies_clean %>%
  filter(release_year >= 2010, release_year < 2020) %>%
  arrange(desc(revenue)) %>%
  select(title, revenue)
print(movies_2010s)



#STEP 7: REVENUE HEATMAP BY YEAR AND MONTH
heatmap_data <- movies_released_nonzero %>%
  mutate(year_bin = if_else(release_year < 1970, "<1970", as.character(release_year))) %>%
  group_by(year_bin, release_month) %>%
  summarise(total_revenue = sum(revenue, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    release_month = factor(release_month, levels = 1:12, labels = unname(month_translation), ordered = TRUE))

past_years <- c("<1970", sort(unique(heatmap_data$year_bin[heatmap_data$year_bin != "<1970"])))
heatmap_data$year_bin <- factor(heatmap_data$year_bin, levels = past_years)

ggplot(heatmap_data, aes(x = year_bin, y = release_month, fill = total_revenue)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_viridis_c(option = "C", labels = comma) +
  labs(
    title = "Теплова карта за датою виходу", subtitle = "Дані до 1970 року було згруповано",
    x = "Рік виходу", y = "Місяць виходу", fill = "Загальний дохід") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14))

movies_jun_2016 <- movies_clean %>%
  filter(release_year == 2016, release_month == 6) %>%
  select(title, revenue)
print(movies_jun_2016)

movies_dec_2009 <- movies_clean %>%
  filter(release_year == 2009, release_month == 12) %>%
  select(title, revenue)
print(movies_dec_2009)



#STEP 8: REVENUE BY DAYS
release_counts <- movies_released_nonzero %>%
  group_by(release_day) %>%
  summarise(movie_count = n())

movies_x_counts <- movies_released_nonzero %>%
  left_join(release_counts, by = "release_day")

ggplot(movies_x_counts, aes(x = factor(release_day), y = revenue, fill = movie_count)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_log10(labels = scales::comma) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Кількість фільмів") +
  labs(title = "Розподіл доходів за календарними днями",
       x = "Календарний день", y = "Дохід") +
  theme_minimal() +
  theme(
    plot.title   = element_text(size = 20),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text    = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 14))



#STEP 9: REVENUE DISTRIBUTIONS BY SELECTED TIME PERIODS
movies_aft_2000 <- movies_released_nonzero %>% 
  filter(release_year >= 2000)
movies_aft_2000$log_revenue <- log10(movies_aft_2000$revenue)

ggplot(movies_aft_2000, aes(x = log_revenue, fill = release_year > 2010)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("blue", "orange"), labels = c("2000-2010", "Після 2010")) +
  labs(title = "Графік щільності для порівняння часових періодів на логарифмічній шкалі",
       x = "Дохід", y = "Щільність", fill = "Часовий проміжок") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 14))