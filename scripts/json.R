# Глава учебника: https://locusclassicus.github.io/text_analysis/json.html

# ФУНКЦИОНАЛЫ И JSON ----------------------------------------------------------
#
# Функционал — функция, которая принимает другую функцию как аргумент.
# Он помогает повторить одно действие для нескольких объектов.
# Сначала прочитаем таблицы HP, затем разберём JSON на TBBT и masterpieces.
#
# Материалы к занятию:
# https://locusclassicus.github.io/text_analysis/json.html

library(tidyverse)
library(jsonlite)


# 1. Одна функция — несколько файлов ------------------------------------------


# Получаем пути ко всем CSV в распакованной папке.
paths <- list.files("files/HP", pattern = "csv$", full.names = TRUE)
paths

# map() вызывает read_csv для каждого пути и собирает таблицы в список.
# set_names(basename) даёт элементам списка имена файлов: records.csv и т. д.
hp <- paths |>
  set_names(basename) |>
  map(read_csv)

names(hp)

# Список может хранить объекты разного размера: например, таблицы с разным числом строк.
# pluck() извлекает элемент по имени или позиции.
records <- hp |> 
  pluck("records.csv")

# 2. Какой результат возвращает map()? ----------------------------------------

# nrow() принимает таблицу и возвращает число строк.
hp |>
  map(nrow)

# map() всегда возвращает список. map_int() собирает результаты в integer-вектор.
hp |> 
  map_int(nrow)

# Основные варианты:
# map()     — список, в котором могут быть любые объекты;
# map_int() — вектор целых чисел;
# map_dbl() — числовой вектор;
# map_chr() — вектор строк;
# map_lgl() — логический вектор.
# Для map_int/chr/dbl/lgl каждый вызов должен вернуть ровно одно значение.

# Напишем функцию, которая описывает одну таблицу.
count_records <- function(data) {
  data |>
    summarise(rows = n(), resources = n_distinct(`BL record ID`))
}

count_records(records)

# Применим её ко всем таблицам, затем объединим результаты по строкам.
# names_to сохраняет имена элементов списка в отдельном столбце file.
hp |>
  map(count_records) |>
  list_rbind(names_to = "file")

# В records одна строка соответствует записи; в titles — заглавию;
# в names — связанному имени; в topics — рубрике; в classification — индексу.

# Если функция нужна только здесь, можно не давать ей отдельного имени.
# Это анонимная функция: map сам передаст ей очередную таблицу как data.
hp |>
  map(function(data) {
    data |>
      drop_na(Languages) |> 
      count(Languages, sort = TRUE) |>
      slice_head(n = 3)
  }) |>
  list_rbind(names_to = "file")

# Здесь составные значения Languages пока считаются одной строкой.
# Как изменить функцию, чтобы English ; German попало в две группы?


# 3. JSON: данные с вложенной структурой --------------------------------------

# В таблице данные организованы в строки и столбцы.
# JSON позволяет вложить один набор данных в другой:
# сериал -> эпизоды -> заметки об эпизоде.
#
# { } — объект с именованными полями: {"name": "Sheldon", "season": 1}
# [ ] — массив значений: ["Sheldon", "Leonard", "Penny"]
#
# В JSON есть строки, числа, логические значения true/false и null.
# Строки и имена полей записываются в двойных кавычках.
# Объекты и массивы могут находиться друг внутри друга.
#
# После чтения JSON обычно получаем список вложенных списков.
# read_json() читает файл. fromJSON() также удобно использовать для JSON-строки.

# TBBT.json — учебные данные о «Теории большого взрыва».
tbbt <- read_json("files/TBBT.json")

# Сначала смотрим имена полей и первый уровень вложенности.
names(tbbt)
str(tbbt, max.level = 1)

# pluck() проходит по списку: каждый следующий аргумент — следующий шаг внутрь.
tbbt |> 
  pluck("name")

tbbt |> 
  pluck("casting", "Sheldon")

tbbt |> 
  pluck("casting", "Sheldon", "actor_name")

# read_json() по умолчанию сохраняет списки.

# 4. TBBT: из списка в таблицу -------------------------------------------------

# Количества эпизодов хранятся в именованном списке: S01, S02 и т. д.
season_data <- tbbt |> 
  pluck("episodes_count_per_season")

seasons <- tibble(
  season = names(season_data),
  episodes = map_int(season_data, as.integer)
)
seasons

# Теперь достанем список персонажей.
cast_data <- tbbt |> 
  pluck("casting")

cast_data |> 
  pluck("Sheldon")

# map_chr() получает список и функцию pluck.
# Следующий аргумент — имя поля, которое pluck достаёт из КАЖДОЙ карточки.

cast <- tibble(
  character = map_chr(cast_data, pluck, "character_name"),
  actor = map_chr(cast_data, pluck, "actor_name", .default = NA_character_),
  first_episode = map_chr(cast_data, pluck, "first_appearance", .default = NA_character_)
)
cast

# Некоторые поля отсутствуют. .default подставляет вместо них NA_character_.
# Это пропуск в строковом столбце. NULL, возникающий при отсутствии поля,
# не подошёл бы: у него длина 0, а map_chr() ждёт одну строку на персонажа.

cast |>
  filter(is.na(first_episode))

# По такому же принципу собираем таблицу эпизодов.
episode_data <- tbbt |> 
  pluck("episode_list")

episodes <- tibble(
  episode_id = map_chr(episode_data, pluck, "episode_id"),
  title = map_chr(episode_data, pluck, "title")
)
episodes

# В ID первые три символа обозначают сезон: S01E01 -> S01.
episodes |>
  mutate(season = str_sub(episode_id, 1, 3)) |>
  count(season)

# Сравните результат с seasons. В учебном файле 280 карточек эпизодов.
# Количество карточек описывает именно этот файл.


# 5. Когда значений несколько: list-столбец ------------------------------------

# У эпизода может быть несколько заметок important_notes.
# Поэтому здесь нужен map(), а не map_chr(): результат одного вызова — список.
notes <- tibble(
  episode_id = map_chr(episode_data, pluck, "episode_id"),
  notes = map(episode_data, pluck, "important_notes", .default = list())
)
notes

# В list-столбце одна ячейка хранит целый набор значений.
# unnest_longer() размещает эти значения в отдельных строках.
# keep_empty = TRUE оставляет и эпизоды без заметок.
notes_long <- notes |>
  unnest_longer(notes, values_to = "note", keep_empty = TRUE)

notes_long

# Теперь один эпизод может занимать несколько строк — по числу заметок.
# Чтобы узнать число эпизодов, считаем уникальные ID, а не строки.
notes_long |>
  summarise(rows = n(), episodes = n_distinct(episode_id))

