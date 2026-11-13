# Глава учебника: https://locusclassicus.github.io/text_analysis/tabular.html

library(tidyverse)

##################################
# ЧАСТЬ 1: ШКОЛЬНАЯ ЛИТЕРАТУРА
##################################


url <- "https://dataverse.pushdom.ru/api/access/datafile/4229"
readme <- "https://dataverse.pushdom.ru/api/access/datafile/4233"

# скачивание в папку files в родительской директории
dir.create("files")
download.file(url, destfile = "./files/curricula.tsv") 
download.file(readme, destfile = "./files/readme.tsv")

# читаем данные из файла
curricula <- read_tsv("./files/curricula.tsv")

# проверяем тип данных
class(curricula)

# имена столбцов и сводка
glimpse(curricula)
colnames(curricula)

# отобрать столбцы
curricula_nc <- curricula |> 
  select(-comment)

# посмотреть уникальные значения
curricula_nc |> 
  distinct(year) |> 
  pull(year)

# отобрать ряды
curricula_nc |> 
  filter(year > 1950 & year < 1960)

# обобщить значения 
curricula_nc |> 
  group_by(year) |> 
  summarise(total = n())

curricula_nc |> 
  count(year)

# упорядочить
curricula_nc |> 
  count(year) |> 
  arrange(n)

# рассказать про оператор %in% !!!

# преобразовать (новый столбец)
title_length <- curricula_nc |> 
  distinct(author, title) |> 
  mutate(n_char = nchar(title)) |> 
  arrange(-n_char)  

# преобразовать (по условию)
curricula_nc |> 
  mutate(year = case_when(year == "1937-38" ~ "1937",
                          .default = year)) |> 
  distinct(year) |> 
  pull(year)

curricula_priority <- curricula_nc |> 
  mutate(priority = case_when(is.na(priority) ~ 1,
                              .default = 0))

# соотношение приоритет / нет по годам
curricula_priority_summary <- curricula_priority |> 
  group_by(year) |> 
  summarise(total = n(),
            is_priority = sum(priority),
            other = total - is_priority)

curricula_priority_summary

# посчитать na 
curricula |>
  summarise(
    na_author = sum(is.na(author))
  )

curricula |>
  summarise(
    across(
      everything(),
      \(x) sum(is.na(x))
    )
  )

# исправление ошибок

table2 |> 
  pivot_wider(names_from = type, values_from = count)

table3 |> 
  separate(rate, sep = "/", into = c("cases", "population"))
  
# Подумайте и посчитайте на досуге:
# У каких авторов больше всего произведений (во всех программах)?
# Какие произведения упоминаются в программах чаще всего?
# На принятые в каких годах программы приходится больше всего произведений?

