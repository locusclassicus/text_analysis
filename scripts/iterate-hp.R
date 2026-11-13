# Глава учебника: https://locusclassicus.github.io/text_analysis/iterate.html

# ФУНКЦИИ. ЧТО МОЖНО НАЙТИ В КАТАЛОГЕ ГАРРИ ПОТТЕРА? -------------------------
#
# Функция объединяет несколько действий под одним именем.
# Написав её один раз, можно повторять действия с другими данными и настройками.
#
# В HP.zip — описания ресурсов Британской библиотеки, связанных с Гарри Поттером:
# издания романов, переводы, исследования, путеводители и даже кулинарные книги.
# Попробуем собрать из них небольшую выставку «За пределами Хогвартса».
#
# Материал к занятию:
# https://locusclassicus.github.io/text_analysis/iterate.html


# 1. Открываем данные ---------------------------------------------------------

library(tidyverse)

# Рабочая папка должна содержать этот скрипт и папку files.
# Распаковываем архив в папку files/HP.
unzip("files/HP.zip", exdir = "files/HP")

# Читаем все столбцы как текст: так сохранятся начальные нули в ID записей.
records <- read_csv("files/HP/records.csv", col_types = cols(.default = col_character()))
titles <- read_csv("files/HP/titles.csv", col_types = cols(.default = col_character()))

# records: одна строка — одна запись каталога.
# titles: одна строка — одно заглавие, в том числе вариант названия той же записи.
# Поэтому количество строк в двух таблицах различается.
records |>
  select(`BL record ID`, Title, Languages) |>
  slice_head(n = 5)

records |>
  summarise(rows = n(), resources = n_distinct(`BL record ID`))

titles |>
  summarise(rows = n(), resources = n_distinct(`BL record ID`))

# В records 755 строк, в titles 962, но в обеих таблицах 755 разных ID.
# Дальше работаем с records, чтобы варианты заглавий не увеличивали наши подсчёты.


# 2. Первая функция -----------------------------------------------------------

# Начнём с простого действия: уберём лишние пробелы и приведём текст к нижнему регистру.
"  Harry   Potter  " |>
  str_squish() |>
  str_to_lower()

# Запишем те же действия в функцию.
clean_title <- function(text) {
  text |>
    str_squish() |>
    str_to_lower()
}

# clean_title — имя функции.
# function(text) — создаём функцию с одним аргументом text.
# Внутри { } записаны действия над этим аргументом.
# Результат последнего действия возвращается из функции.

clean_title("  Harry   Potter  ")
clean_title(c("HARRY POTTER", "  Harrius Potter  "))

# Функция работает и с одной строкой, и с вектором строк.
# Поэтому её можно использовать внутри mutate() для целого столбца.
records |>
  mutate(title_clean = clean_title(Title)) |>
  select(Title, title_clean) |>
  slice_head(n = 5)

# Внутри функции text обозначает переданные данные.
# Функция не меняет records сама по себе: результат нужно сохранить через <-.


# 3. От готового кода к функции ------------------------------------------------

# найдём книги, в названии которых встречается cook.
# fixed() ищет буквальный текст, ignore_case = TRUE позволяет не учитывать регистр.
records |>
  filter(str_detect(Title, fixed("cook", ignore_case = TRUE))) |>
  select(Title, Languages, Publisher)

# Для другого поиска пришлось бы копировать код и менять слово.
# Сделаем изменяемые части аргументами: data — таблица, word — искомый текст.
find_titles <- function(data, word) {
  data |>
    filter(str_detect(Title, fixed(word, ignore_case = TRUE))) |>
    select(Title, Languages, Publisher)
}

# Теперь меняем только аргументы.
find_titles(records, "cook")
find_titles(records, "Bible")
find_titles(records, "design")

# Функция получает таблицу и возвращает таблицу.
# Возвращённый результат можно сохранить или передать дальше по цепочке.
cookbooks <- find_titles(records, "cook")

cookbooks |>
  count(Publisher, sort = TRUE)

# Через |> таблица попадает в первый аргумент функции.
records |>
  find_titles("cook")

# Функция подходит и для другой таблицы с теми же нужными столбцами.
titles |>
  find_titles("Harrius")


# 4. Аргументы по умолчанию ---------------------------------------------------

# Добавим ограничение: сколько названий показывать.
# Новое определение заменяет предыдущую версию find_titles.
find_titles <- function(data, word, n = 5) {
  data |>
    filter(str_detect(Title, fixed(word, ignore_case = TRUE))) |>
    select(Title, Languages, Publisher) |>
    slice_head(n = n)
}

# data и word нужно указать; n можно пропустить — тогда будет использовано 5.
find_titles(records, "magic")
find_titles(records, "magic", n = 10)

# Аргументы можно передавать по позиции или по имени.
# Имена особенно полезны, когда у функции несколько настроек.
find_titles(data = records, word = "magic", n = 3)

# Имя без скобок обозначает саму функцию; со скобками — её вызов.
find_titles
find_titles(records, "magic")

# Вопрос: что возвращает функция, если подходящих названий нет?
find_titles(records, "несуществующее название")


# 5. Условие внутри функции ---------------------------------------------------

# Теперь найдём издателей, у которых больше всего записей в нашем каталоге.
# Добавим настройку: считать всё или только ресурсы с английским языком.
top_publishers <- function(data, n = 5, english_only = FALSE) {

  # if выполняет блок, только если english_only равно TRUE.
  # str_detect() находит English и в составных значениях вроде English ; German.
  if (english_only) {
    data <- data |>
      filter(str_detect(Languages, "English"))
  }

  # Этот код выполняется в обоих случаях.
  result <- data |>
    filter(!is.na(Publisher)) |>
    count(Publisher, sort = TRUE) |>
    slice_head(n = n)

  # return() явно указывает, что функция возвращает.
  return(result)
}

top_publishers(records)
top_publishers(records, n = 10, english_only = TRUE)

# data и result внутри функции — локальные переменные.
# Изменяя data внутри вызова, мы не переписываем исходную таблицу records.
#
# if выбирает действие для функции целиком: фильтровать или не фильтровать.
# filter() выбирает строки таблицы. Это разные задачи.
#
# return(result) можно заменить последней строкой result — результат будет тем же.


# 6. Как из результатов получается история -----------------------------------

# Один и тот же герой объединяет очень разные ресурсы.
# Посмотрим на религиозную дискуссию, кулинарию и обучение программированию.
records |> 
  find_titles("Christian")
records |> 
  find_titles("cook")
records |> 
  find_titles("design patterns")

# А что найдётся по слову philosoph?
records |> 
  find_titles("philosoph", n = 10)

# Среди результатов есть Philosopher's Stone — название романа.


# Языки и переводы.
# Несколько языков в одной ячейке разделены точкой с запятой.
languages <- records |>
  select(`BL record ID`, Languages) |>
  filter(!is.na(Languages)) |>
  separate_longer_delim(Languages, delim = ";") |>
  mutate(Languages = str_trim(Languages)) |>
  distinct(`BL record ID`, Languages)

languages |>
  count(Languages, sort = TRUE)

# Посмотрим, какие ресурсы описаны как латинские и древнегреческие.
languages |>
  filter(Languages %in% c("Latin", "Greek, Ancient (to 1453)")) |>
  left_join(select(records, `BL record ID`, Title), by = "BL record ID")


# 7. Попробуйте самостоятельно ------------------------------------------------

# Напишите top_languages(data, n = 5).
# Возьмите за основу цепочку из раздела 6: разделите языки, посчитайте записи,
# оставьте n самых частых языков. Функция должна возвращать таблицу.

