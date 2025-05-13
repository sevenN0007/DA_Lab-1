setwd("D://UNI//da//lab 2")
library(tidyverse)
library(scales)
library(dplyr)
library(stringr)
library(GGally)
library(boot)

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



# кореляційні матриці
movies_numeric <- movies[, c("vote_average", "vote_count", "revenue", "runtime",
                                   "budget", "release_year", "release_month", "release_day")]
# пірсона
ggcorr(movies_numeric, label = TRUE)
# спірмана
spearman_corr <- cor(movies_numeric, method = "spearman")
ggcorr(movies_numeric, method = c("pairwise", "spe"), label = TRUE)






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



# ДІ для середнього бюджету в топ компаніях

# відберемо потрібні стовпці для обчислень
x_cb <- separate_companies[, c("production_companies", "budget")] %>% 
  filter(!is.na(budget))

# функція для розрахунку ДІ
ci_avg_budget <- x_cb %>% 
  group_by(production_companies) %>%
  filter(!is.na(production_companies))%>%
  filter(!is.na(budget))%>%
  summarize(sum_budget = sum(budget, na.rm = TRUE),
            mean = mean(budget),
            sd = sd(budget),
            n = n(),
            se = sd/sqrt(n()),
            a = mean(budget) + qnorm(0.025) * se,
            b = mean(budget) + qnorm(0.975) * se)%>%
  arrange(desc(sum_budget))%>%
  slice_max(sum_budget, n = 20)

# вивід таблиці
ci_avg_budget%>% 
  arrange(desc(mean))%>%
  select(-sum_budget)%>%
  view()

# побудова графіка
ggplot(ci_avg_budget, aes(x = reorder(production_companies, mean), y = mean)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = a, ymax = b), width = 0.3) +
  coord_flip() +
  labs(
    title = "ДІ для середнього бюджету в компаніях з найбійльшим сумарним бюджетом",
    x = "Компанії",
    y = "ДІ для середнього бюджету"
  ) +
  theme_minimal()









"Чи впливає тривалість фільму на бюджет?"

set.seed(100)

# ДІ для кореляції між бюджетом та тривалістю

# відберемо потрібні стовпці для обчислень
x_br <- movies[, c("budget", "runtime")] %>% 
  filter(!is.na(budget))%>%
  filter(!is.na(runtime))

# статистика для бутстрепу
boot_cor <- function(x, indices){
  cor_bar <- cor(x[indices, ])[1, 2]
  return(cor_bar)
}

# функція для розрахунку ДІ
boot_cor_ci <- function(data, R = 5000, conf = 0.95) {

  boot_out <- tryCatch({
    boot(data = data, statistic = boot_cor, R = R)
  }, error = function(e) return(NULL))
  
  ci <- boot.ci(boot_out, type = c("norm", "basic", "perc", "bca"), conf = conf)
  
  ci_low_norm <- ci$norm[2]
  ci_high_norm <- ci$norm[3]
  
  ci_low_basic <- ci$basic[4]
  ci_high_basic <- ci$basic[5]
  
  ci_low_perc <- ci$percent[4]
  ci_high_perc <- ci$percent[5]
  
  ci_low_bca <- ci$bca[4]
  ci_high_bca <- ci$bca[5]
  
  tibble(
    cor = cor(data)[1, 2],
    se = sd(boot_out$t),
    ci_norm_low = ci_low_norm,
    ci_norm_high = ci_high_norm,
    ci_basic_low = ci_low_basic,
    ci_basic_high = ci_high_basic,
    ci_perc_low = ci_low_perc,
    ci_perc_high = ci_high_perc,
    ci_bca_low = ci_low_bca,
    ci_bca_high = ci_high_bca
  )
}

# застосування функції пошуку ДІ
result <- boot_cor_ci(x_br)

# вивід таблиці
result%>%view()










"Чи впливає жанр фільму на бюджет?"


# робимо окремий рядок для кожного жанру зі списку для одного фільму
separate_genres <- movies %>%
  separate_longer_delim(genres, delim = ", ")%>%filter(!is.na(genres))

# перетворюємо стовпець з жанрами на факторний (необхідно для побудови графіка)
separate_genres$genres <- as.factor(separate_genres$genres)




# ДІ для медіан бюджету по жанрах

set.seed(42)

# відберемо потрібні стовпці для обчислень
x <- separate_genres[, c("genres", "budget")] %>% 
  filter(!is.na(budget))

# статистика для бутстрепу
boot_median <- function(x, indices){
  n <- length(x)
  median_bar <- median(x[indices])
  return(median_bar)
}

# функція для розрахунку ДІ
boot_median_ci <- function(budgets, R = 5000, conf = 0.95) {
  
  boot_out <- tryCatch({
    boot(data = budgets, statistic = boot_median, R = R)
  }, error = function(e) return(NULL))
  
  ci <- boot.ci(boot_out, type = c("norm", "basic", "perc"), conf = conf)
  
  ci_low_norm <- ci$norm[2]
  ci_high_norm <- ci$norm[3]
  
  ci_low_basic <- ci$basic[4]
  ci_high_basic <- ci$basic[5]
  
  ci_low_perc <- ci$percent[4]
  ci_high_perc <- ci$percent[5]
  
  tibble(
    median = median(budgets),
    se = sd(boot_out$t),
    n = n(),
    ci_norm_low = ci_low_norm,
    ci_norm_high = ci_high_norm,
    ci_basic_low = ci_low_basic,
    ci_basic_high = ci_high_basic,
    ci_perc_low = ci_low_perc,
    ci_perc_high = ci_high_perc
  )
}

# застосування функції пошуку ДІ
result <- x %>%
  group_by(genres) %>%
  summarise(
    result = list(boot_median_ci(budget)),
    .groups = "drop"
  )%>%
  unnest(result)

# виведення таблиці
result %>%
  arrange(desc(median))%>%
  view()

# приведення таблиці до вигляду, зручного для побудови графіку
medians <- separate_genres %>% 
  select(id, genres, budget) %>%
  group_by(genres) %>%
  summarise(median_budget = median(budget, na.rm = TRUE))%>%
  arrange(median_budget)

result_wide <- result %>%
  pivot_longer(
    cols = starts_with("ci_"), 
    names_to = c("method", ".value"),
    names_pattern = "ci_(.*)_(low|high)"
  ) %>%
  select(genres, method, median, se, low, high)

result_wide <- result_wide%>%
  mutate(genres = factor(genres, levels = medians$genres[order(medians$median_budget)]))%>%
  arrange(desc(median))

# Побудова графіку
ggplot(result_wide, aes(x = genres, ymin = low, ymax = high, color = method)) +
  geom_errorbar(position = position_dodge(width = 0.5), width = 0.6, size = 1) +
  geom_point(aes(y = median), color = "black", size = 1.8, shape = 16) +
  coord_flip() +
  labs(
    y = "ДІ",
    x = "Жанри",
    title = "Бутстреп-ДІ для медіани бюджету по жанрах",
    color = "Тип інтервалу"
  ) +
  theme_minimal()







# ДІ для дисперсії тривалості по жанрах

# відберемо потрібні стовпці для обчислень
x <- separate_genres[, c("genres", "runtime")] %>% 
  filter(!is.na(runtime))

# розрахунки ДІ
ci_var_runtime <- x %>% 
  group_by(genres) %>%
  filter(!is.na(genres))%>%
  filter(!is.na(runtime))%>%
  summarize(var = var(runtime),
            n = n(),
            sd = sd(runtime),
            se = sd/sqrt(n),
            a = var(runtime) + qnorm(0.025) * se,
            b = var(runtime) + qnorm(0.975) * se
  )%>%
  arrange(desc(var))

# виведення таблиці
ci_var_runtime%>% 
  view()

# побудова графіка
ggplot(ci_var_runtime, aes(x = reorder(genres, var), y = var)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = a, ymax = b), width = 1) +
  coord_flip() +
  labs(
    title = "ДІ для дисперсії тривалості по жанрах",
    x = "Жанри",
    y = "ДІ для дисперсії тривалості"
  ) +
  theme_minimal()





# ДІ для середньої тривалості по жанрах

# розрахунки ДІ
ci_mean_runtime <- x %>% 
  group_by(genres) %>%
  filter(!is.na(genres))%>%
  filter(!is.na(runtime))%>%
  summarize(mean = mean(runtime),
            n = n(),
            sd = sd(runtime),
            se = sd/sqrt(n),
            a = mean(runtime) + qnorm(0.025) * se,
            b = mean(runtime) + qnorm(0.975) * se
  )%>%
  arrange(desc(mean))

# виведення таблиці
ci_mean_runtime%>% 
  view()

# побудова графіка
ggplot(ci_mean_runtime, aes(x = reorder(genres, mean), y = mean)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = a, ymax = b), width = 0.4) +
  coord_flip() +
  labs(
    title = "ДІ для середньоЇ тривалості по жанрах",
    x = "Жанри",
    y = "ДІ для середньоЇ тривалості"
  ) +
  theme_minimal()













# ТЕСТУВАННЯ ГІПОТЕЗ

# порівняння дисперсії бюджету для фільмів з тривалістю 80-150 хв і для інших фільмів

# відберемо потрібні стовпці для обчислень
runtime_split <- movies[, c("runtime", "budget")] %>% 
  filter(!is.na(runtime))%>%
  filter(!is.na(budget))

# функція для розрахунку тесту Волда
compare_budget <-function (runtime_split) {
  
  group_80_150 <- runtime_split %>% filter(runtime >= 80 & runtime <= 150)
  group_other <- runtime_split %>% filter(runtime < 80 | runtime > 150)
  
  n_80_150 <- nrow(group_80_150)
  n_other <- nrow(group_other)
  
  mean_hat_80_150 <- mean(group_80_150$budget, na.rm = TRUE)
  mean_hat_other <- mean(group_other$budget, na.rm = TRUE)
  
  mean_hat_diff <- mean_hat_80_150 - mean_hat_other
  
  var_hat_80_150 <- var(group_80_150$budget, na.rm = TRUE)
  var_hat_other <- var(group_other$budget, na.rm = TRUE)
  
  se_80_150 <- sqrt(var_hat_80_150 / n_80_150)
  se_other <- sqrt(var_hat_other / n_other)
  
  se_diff <- sqrt(se_80_150^2 + se_other^2)
  
  T_stat <- mean_hat_diff / se_diff
  p_value <- pnorm(T_stat, lower.tail = FALSE)
  
  conf_int <- c(mean_hat_diff - qnorm(0.95)*se_diff, Inf)
  
  reject <- ifelse(p_value < 0.05, "TRUE", "FALSE")
  
  tibble(
    mean_hat_diff = mean_hat_diff,
    se_diff = se_diff,
    T_stat = T_stat,
    p_value = p_value,
    conf_int_lower = conf_int[1],
    conf_int_upper = conf_int[2],
    reject = reject
  )
}

# застосування тесту Волда
hypothesis <- compare_budget(runtime_split)

# виведення таблиці
hypothesis%>%view()
