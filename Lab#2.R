#STEP 1: LOADING AND CLEANING DATA
library(knitr)
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

movies_released <- movies_clean %>%
  filter(status == "Released", release_date <= Sys.Date())

movies_released_nonzero <- movies_released %>%
  filter(revenue > 0)



#STEP 2: Revenue analysis by decades/months/days
movies_released_nonzero$decade <- floor(movies_released_nonzero$release_year / 10) * 10

decade_summary <- movies_released_nonzero %>%
  group_by(decade) %>%
  summarise(
    mean_revenue = mean(revenue, na.rm = TRUE),
    sd_revenue = sd(revenue, na.rm = TRUE),
    n = sum(!is.na(revenue)),
    se_revenue = sd_revenue / sqrt(n)
  ) %>%
  mutate(
    critical_value = case_when(
      n <= 1 ~ NA_real_,
      n < 30 ~ qt(0.975, df = n - 1),
      TRUE ~ qnorm(0.975)
    ),
    ci_lower = mean_revenue - critical_value * se_revenue,
    ci_upper = mean_revenue + critical_value * se_revenue
  )

kable(decade_summary, 
      col.names = c("Month", "Mean Revenue", "SD", "n", "SE", "Critical Value", "CI Lower", "CI Upper"))

ggplot(decade_summary, aes(x = mean_revenue, y = as.factor(decade))) +
  geom_point(size = 3, color = "black") +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2, linewidth = 0.7, color = "black") +
  labs(
    x = "Середній дохід",
    y = "Десятиліття"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray55", size = 0.1),
    panel.grid.minor = element_line(color = "gray55", size = 0.1),
    plot.title = element_text(size = 22, face = "bold"),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text = element_text(size = 14))


month_summary <- movies_released_nonzero %>%
  group_by(release_month) %>%
  summarise(
    mean_revenue = mean(revenue, na.rm = TRUE),
    sd_revenue = sd(revenue, na.rm = TRUE),
    n = sum(!is.na(revenue)),
    se_revenue = sd_revenue / sqrt(n)
  ) %>%
  mutate(
    critical_value = case_when(
      n <= 1 ~ NA_real_,
      n < 30 ~ qt(0.975, df = n - 1),
      TRUE ~ qnorm(0.975)
    ),
    ci_lower = mean_revenue - critical_value * se_revenue,
    ci_upper = mean_revenue + critical_value * se_revenue
  )

kable(month_summary, 
      col.names = c("Month", "Mean Revenue", "SD", "n", "SE", "Critical Value", "CI Lower", "CI Upper"))

ggplot(month_summary, aes(x = mean_revenue, y = as.factor(release_month))) +
  geom_point(size = 3, color = "black") +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2, size = 1.2, color = "black") +
  labs(
    x = "Середній дохід",
    y = "Місяць"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray55", size = 0.1),
    panel.grid.minor = element_line(color = "gray55", size = 0.1),
    plot.title = element_text(size = 22, face = "bold"),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text = element_text(size = 14))


day_summary <- movies_released_nonzero %>%
  group_by(release_day) %>%
  summarise(
    mean_revenue = mean(revenue, na.rm = TRUE),
    sd_revenue = sd(revenue, na.rm = TRUE),
    n = sum(!is.na(revenue)),
    se_revenue = sd_revenue / sqrt(n)
  ) %>%
  mutate(
    critical_value = case_when(
      n <= 1 ~ NA_real_,
      n < 30 ~ qt(0.975, df = n - 1),
      TRUE ~ qnorm(0.975)
    ),
    ci_lower = mean_revenue - critical_value * se_revenue,
    ci_upper = mean_revenue + critical_value * se_revenue
  )

kable(day_summary, 
      col.names = c("Day", "Mean Revenue", "SD", "n", "SE", "Critical Value", "CI Lower", "CI Upper"))

ggplot(day_summary, aes(x = mean_revenue, y = as.factor(release_day))) +
  geom_point(size = 3, color = "black") +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2, size = 1.2, color = "black") +
  labs(
    x = "Середній дохід",
    y = "Календарний день"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray55", size = 0.1),
    panel.grid.minor = element_line(color = "gray55", size = 0.1),
    plot.title = element_text(size = 22, face = "bold"),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text = element_text(size = 14))



#STEP 3: Correlation between revenue and budget
cor_boot <- function(data, indices) {
  d <- data[indices, ]
  return(cor(d$budget, d$revenue, method = "pearson", use = "complete.obs"))
}

set.seed(123)
boot_corr <- boot(data = movies_released_nonzero, statistic = cor_boot, R = 1000)

boot_corr_ci <- boot.ci(boot_corr, type = c("norm", "basic", "perc", "bca"))
se_corr <- sd(boot_corr$t)

correlation_detailed <- tibble::tibble(
  Correlation = c(boot_corr$t0),
  SE = c(sd(boot_corr$t)),
  CI_Normal = c(
    sprintf("%.6f - %.6f", boot_corr_ci$normal[2], boot_corr_ci$normal[3])
  ),
  CI_Basic = c(
    sprintf("%.6f - %.6f", boot_corr_ci$basic[4], boot_corr_ci$basic[5])
  ),
  CI_Percentile = c(
    sprintf("%.6f - %.6f", boot_corr_ci$perc[4], boot_corr_ci$perc[5])
  ),
  CI_BCa = c(
    sprintf("%.6f - %.6f", boot_corr_ci$bca[4], boot_corr_ci$bca[5])
  )
)

print(correlation_detailed)



#STEP 4: Recent decades analysis
movies_released_nonzero <- movies_released_nonzero %>%
  mutate(
    release_year = lubridate::year(release_date),
    release_decade = case_when(
      release_year >= 2000 & release_year < 2010 ~ "2000s",
      release_year >= 2010 & release_year < 2020 ~ "2010s",
      TRUE ~ NA_character_
    )
  )

movies_2000 <- movies_released_nonzero %>% filter(release_decade == "2000s", !is.na(revenue))
movies_2010 <- movies_released_nonzero %>% filter(release_decade == "2010s", !is.na(revenue))

n_2000 <- nrow(movies_2000)
n_2010 <- nrow(movies_2010)

mean_2000 <- mean(movies_2000$revenue)
mean_2010 <- mean(movies_2010$revenue)

sd_2000 <- sd(movies_2000$revenue)
sd_2010 <- sd(movies_2010$revenue)

se_2000 <- sd_2000 / sqrt(n_2000)
se_2010 <- sd_2010 / sqrt(n_2010)

se_diff <- sqrt(se_2000^2 + se_2010^2)

wald_statistic <- (mean_2000 - mean_2010) / se_diff

p_value <- 2 * (1 - pnorm(abs(wald_statistic)))

ci_lower <- (mean_2000 - mean_2010) - 1.96 * se_diff
ci_upper <- (mean_2000 - mean_2010) + 1.96 * se_diff

se_diff <- sqrt((sd_2000^2 / n_2000) + (sd_2010^2 / n_2010))

wald_statistic <- (mean_2000 - mean_2010) / se_diff

p_value <- 2 * (1 - pnorm(abs(wald_statistic)))

alpha <- 0.05
z_crit <- qnorm(1 - alpha / 2)
ci_lower <- (mean_2000 - mean_2010) - z_crit * se_diff
ci_upper <- (mean_2000 - mean_2010) + z_crit * se_diff

cat("Wald test for difference in means (2000s vs. 2010s):\n")
cat(sprintf("  N (2000s): %d | N (2010s): %d\n", n_2000, n_2010))
cat(sprintf("  Mean 2000s: %.2f | Mean 2010s: %.2f\n", mean_2000, mean_2010))
cat(sprintf("  SE 2000s: %.2f | SE 2010s: %.2f\n", se_2000, se_2010))
cat(sprintf("  SD 2000s: %.2f | SD 2010s: %.2f\n", sd_2000, sd_2010))
cat(sprintf("  Combined SE of difference: %.2f\n", se_diff))
cat(sprintf("  Difference: %.2f\n", mean_2000 - mean_2010))
cat(sprintf("  Wald Statistic (Z): %.3f\n", wald_statistic))
cat(sprintf("  P-value: %.4f\n", p_value))
cat(sprintf("  95%% CI for difference: [%.2f, %.2f]\n", ci_lower, ci_upper))