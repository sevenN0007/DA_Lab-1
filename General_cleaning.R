movies_initial <- read_csv("TMDB_movie_dataset_v11.csv")

month_translation <- c(
  Jan = "Січ", Feb = "Лют", Mar = "Бер", Apr = "Кві", May = "Тра", Jun = "Чер",
  Jul = "Лип", Aug = "Сер", Sep = "Вер", Oct = "Жов", Nov = "Лис", Dec = "Гру"
)

revenue_to_na <- c(1407985, 1270893, 1224207, 1326885, 1294302, 1236552)
budget_to_na <- c(1381066, 1235037, 1057999, 1022208, 
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
  
  #Date  
  mutate(  
    release_date = gsub('"', '', release_date),
    release_date = gsub("\\s+", "", release_date),
    release_date = as.Date(release_date),
    release_year = year(release_date),
    release_month = month(release_date),
    release_day = day(release_date),
    release_month_ua = month_translation[as.character(month(release_date, label = TRUE, abbr = TRUE))]
  ) %>%
  
  #Date labels
  mutate(
    season = case_when(
      release_month_ua %in% c("Січ", "Лют", "Бер") ~ "Зима",
      release_month_ua %in% c("Кві", "Тра", "Чер") ~ "Весна",
      release_month_ua %in% c("Лип", "Сер", "Вер") ~ "Літо",
      release_month_ua %in% c("Жов", "Лис", "Гру") ~ "Осінь",
      TRUE ~ NA_character_
    )
  ) %>%
  
  #Manual error correction
  mutate(
    revenue = if_else(release_date > Sys.Date(), NA_real_, revenue)
  ) %>%
  
  mutate(
    revenue = if_else(id %in% revenue_to_na, NA_real_, revenue),
    budget  = if_else(id %in% budget_to_na, NA_real_, budget),
    runtime = if_else(id %in% runtime_to_na, NA_real_, runtime)
  ) %>%
  
  #Unnecessary columns removal
  select(-backdrop_path, -homepage, -popularity, -poster_path, -imdb_id)
