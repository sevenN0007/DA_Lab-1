library(dplyr)
library(ggplot2)
library(boot)
library(knitr)
library(tidyr)


#setwd("C:/Users/c2sma/OneDrive/Документи/R-TT/R-Studio/DA_Lab-2")

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
# Побудова таблиці з дисперсією та довірчими інтервалами
company_ratings <- movies_cleaned %>%
  filter(!is.na(production_companies), !is.na(vote_average)) %>%
  separate_rows(production_companies, sep = ", ") %>%
  group_by(production_companies) %>%
  summarise(
    num_movies = n(),
    sum_rating = sum(vote_average, na.rm = TRUE),
    mean_rating = mean(vote_average, na.rm = TRUE),
    variance = var(vote_average, na.rm = TRUE),
    sd = sd(vote_average, na.rm = TRUE)
  ) %>%
  filter(num_movies > 5) %>%
  mutate(
    weighted_rating = (sum_rating + C * global_avg_rating) / (num_movies + C),
    # t-квантиль для 95% довіри(Стюдента)
    a_t = mean_rating + qt(0.025, df= num_movies - 1) * sd / sqrt(num_movies),
    b_t = mean_rating + qt(0.975, df= num_movies - 1) * sd / sqrt(num_movies)
  ) %>%
  arrange(desc(weighted_rating))


# Візьмемо перші 40 компаній
top_40_companies <- company_ratings %>% 
  slice_max(order_by = weighted_rating, n = 40)  

# Побудова графіка
ggplot(top_40_companies, aes(x = reorder(production_companies, mean_rating), y = mean_rating)) +
  geom_point() +  # крапка для середнього рейтингу
  geom_errorbar(aes(ymin = a_t, ymax = b_t), width = 0.2) +  # довірчий інтервал
  coord_flip() +  # перевертаня графіка
  labs(
    title = "Середній рейтинг фільмів за компаніями",
    x = "Компанія",
    y = "Середній рейтинг з довірчим інтервалом (95%)"
  ) +
  theme_minimal()



# Побудова таблиці
company_table <- top_40_companies %>%
  select(
    Компанія = production_companies,
    `mean` = mean_rating,
    `Sd` = sd,
    `n` = num_movies,
    `a` = a_t,
    `b` = b_t
  )

# Виведення таблиці
print(company_table)



#------Перевірка на нормальність-------------

ggplot(movies_cleaned, aes(x = vote_average)) +
  geom_histogram(binwidth = 0.2, fill = "skyblue", color = "black") +
  labs(title = "Гістограма рейтингів", x = "Рейтинг", y = "Кількість фільмів") +
  theme_minimal()

ggplot(movies_cleaned, aes(sample = vote_average)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ-графік для vote_average", x = "Теоретичні квантили", y = "Фактичні квантили") +
  theme_minimal()






# --- чи перевищує середній рейтинг TOHO значення 7.34? ---

# Формулювання:
#H₀: μ_TOHO <  7.34
#H₁: μ_TOHO >=   7.34


# Вибірка TOHO
toho_movies <- movies_cleaned %>%
  filter(str_detect(production_companies, "TOHO")) %>%
  select(vote_average)

# Основні статистики
n <- nrow(toho_movies)
mean_TOHO <- mean(toho_movies$vote_average, na.rm = TRUE)
s <- sd(toho_movies$vote_average, na.rm = TRUE)
mu_0 <- 7.34

# Стандартна похибка
SE <- s / sqrt(n)

# Тест Волда 
z_stat <- (mean_TOHO - mu_0) / SE
p_value_wald <- 1 - pnorm(z_stat)  # Односторонній правий тест

# t-тест (точний при нормальному розподілі)
t_stat <- (mean_TOHO - mu_0) / SE
p_value_t <- 1 - pt(t_stat, df = n - 1)  # Односторонній правий тест

# Висновки
conclusion_wald <- ifelse(p_value_wald < 0.05, "Відхиляємо H₀ (тест Волда)", "Не відхиляємо H₀ (тест Волда)")
conclusion_t <- ifelse(p_value_t < 0.05, "Відхиляємо H₀ (t-тест)", "Не відхиляємо H₀ (t-тест)")

# Таблиця результатів
results <- data.frame(
  Параметр = c("mean", "n", "SD", "SE", "Z-статистика (Волда)", "P-значення (Волда)", "Висновок (Волда)",
               "T-статистика", "P-значення (t-тест)", "Висновок (t-тест)"),
  Значення = c(round(mean_TOHO, 3), n, round(s, 3), round(SE, 3), 
               round(z_stat, 3), round(p_value_wald, 4), conclusion_wald,
               round(t_stat, 3), round(p_value_t, 4), conclusion_t)
)



# Виведення таблиці
kable(results, align = "c" )




hist(toho_movies$vote_average, breaks = 15, main = "Histogram TOHO ratings")
qqnorm(toho_movies$vote_average); qqline(toho_movies$vote_average)





#------Довірчі інтервали для дисперсії за хі-квадат----------

# Додамо довірчі інтервали дисперсії до таблиці
top_40_companies_disp_ci <- top_40_companies %>%
  mutate(
    df = num_movies - 1,
    a_var = df * sd^2 / qchisq(0.975, df = df),
    b_var = df * sd^2 / qchisq(0.025, df = df)
  ) %>%
  select(
    Компанія = production_companies,
    `var` = variance,
    n = num_movies,
    `a_var` = a_var,
    `b_var` = b_var
  )

# Виведення таблиці
kable(top_40_companies_disp_ci, digits = 3, align = "c")


# Побудова графіка довірчих інтервалів для дисперсії
ggplot(top_40_companies_disp_ci, aes(x = reorder(Компанія, `var`), y = `var`)) +
  geom_point() +  # Точка — сама дисперсія
  geom_errorbar(aes(ymin = `a_var`, ymax = `b_var`),  width = 0.2) +  # Довірчий інтервал
  coord_flip() +
  labs(
    title = "Довірчі інтервали для дисперсії рейтингу фільмів за компаніями",
    x = "Компанія",
    y = "Оцінка дисперсії з 95% довірчим інтервалом"
  ) +
  theme_minimal()




#----з бутстрепом-----

# Функція бутстреп-оцінки дисперсії з варіацією
boot_var <- function(x, indices, estimate_var = TRUE, R_for_sd = 200) {
  n <- length(x)
  var_bar <- (n - 1)/n * var(x[indices])  # несмещена дисперсія
  
  if (estimate_var) {
    boot_out <- boot(x[indices], statistic = boot_var, R = R_for_sd, estimate_var = FALSE)
    return(c(var_bar, var(boot_out$t[, 1])))
  } else {
    return(c(var_bar))
  }
}


# Обчислити бутстреп-інтервали для дисперсії
boot_disp_ci <- movies_separated %>%
  filter(production_companies %in% top_40_companies$production_companies,
         !is.na(vote_average)) %>%
  group_by(production_companies) %>%
  summarise(
    n = n(),
    boot_obj = list(boot(vote_average, statistic = boot_var, R = 2000))
  ) %>%
  mutate(
    boot_ci = purrr::map(boot_obj, ~boot.ci(.x, index = c(3, 4), type = c("basic", "perc", "bca", "stud"))),
    var_est = purrr::map_dbl(boot_obj, ~mean(.x$t[,3])),
    a_bca = purrr::map_dbl(boot_ci, ~.x$bca[4]),
    b_bca = purrr::map_dbl(boot_ci, ~.x$bca[5])
  ) %>%
  select(Компанія = production_companies, var = var_est, n, a_bca, b_bca)




# Вивести таблицю з бутстреп-довірчими інтервалами
kable(boot_disp_ci, digits = 3, align = "c")

# Побудова графіка бутстреп-довірчих інтервалів
ggplot(boot_disp_ci, aes(x = reorder(Компанія, var), y = var)) +
  geom_point(color = "darkgreen") +
  geom_errorbar(aes(ymin = a_bca, ymax = b_bca), width = 0.2) +
  coord_flip() +
  labs(
    title = "Бутстреп-довірчі інтервали для дисперсії",
    x = "Компанія",
    y = "Оцінка дисперсії з 95% довірчим інтервалом "
  ) +
  theme_minimal()






# --- Перевірка гіпотез щодо рівності дисперсій Marvel та Pixar ---

# Формулювання гіпотез:
# H₀: σ²_Marvel = σ²_Pixar
# H₁: σ²_Marvel ≠ σ²_Pixar


x_marvel <- movies_separated %>% filter(production_companies == "Marvel Studios") %>% pull(vote_average)
x_pixar  <- movies_separated %>% filter(production_companies == "Pixar") %>% pull(vote_average)

observed_diff <- var(x_marvel) - var(x_pixar)

# Bootstrap 
set.seed(123)
R <- 5000
combined <- c(x_marvel, x_pixar)
n1 <- length(x_marvel)

boot_diffs <- replicate(R, {
  shuffled <- sample(combined)
  var(shuffled[1:n1]) - var(shuffled[(n1 + 1):length(combined)])
})

# p-value як частка |перестановочних різниць| >= |спостереженої|
p_value_boot <- mean(abs(boot_diffs) >= abs(observed_diff))


# Висновок
conclusion_f <- ifelse(p_value_boot < 0.05,
                       "Відхиляємо H₀",
                       "Не відхиляємо H₀ ")

# Таблиця результатів
results_f <- data.frame(
  Параметр = c("n_Marvel", "n_Pixar", "Var_Marvel", "Var_Pixar",
               "Δ Дисперсій", "P-значення (Bootstrap)", "Висновок"),
  Значення = c(n_marvel,
               n_pixar,
               round(var(x_marvel), 4),
               round(var(x_pixar), 4),
               round(observed_diff, 4),
               round(p_value_boot, 4),
               conclusion_f)
)


# Вивід таблиці
kable(results_f, align = "c")


