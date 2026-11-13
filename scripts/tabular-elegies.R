# Глава учебника: https://locusclassicus.github.io/text_analysis/tabular.html

library(tidyverse)

elegies_meta <- read_tsv("./files/elegies/elegies_metadata.tab")

# ============================================================
# TIDYVERSE: КАК МЕНЯЛАСЬ РУССКАЯ ЭЛЕГИЯ В 1815–1835 ГОДАХ
#
# Исследовательский вопрос:
# действительно ли поздняя элегия становится формально
# однообразнее и чаще пишется четырехстопным ямбом?
# ============================================================


# 1. ЧТЕНИЕ И ПОДГОТОВКА МЕТАДАННЫХ ---------------------------

elegies_meta <- elegies_meta |>
 
   rename(
    year        = Year1,
    meter       = Meter,
    metre_form  = Razmer,
    source_type = Type,
    canon       = canon_ou_non,
    reprints    = Перепечатки
  ) |>
  mutate(
    # В исходных данных одно значение записано двумя способами:
    # per и periodicals. Объединяем эти категории.
    source_type = recode(
      source_type,
      "periodicals" = "per"
    ),
    
    # Заменяем технические сокращения понятными названиями.
    source_type = recode(
      source_type,
      "per" = "периодика",
      "alm" = "альманах",
      "col" = "сборник"
    ),
    
    # Такая периодизация не содержится в исходной таблице.
    # Мы создаем ее специально для сравнения этапов эволюции жанра.
    period = case_when(
      year <= 1820 ~ "1815–1820",
      year <= 1827 ~ "1821–1827",
      TRUE         ~ "1828–1835"
    ),
    
    # Явно задаем порядок периодов.
    # Иначе R может расположить их по алфавиту.
    period = factor(
      period,
      levels = c("1815–1820", "1821–1827", "1828–1835")
    ),
    
    # Логические переменные удобны для подсчетов:
    # TRUE считается единицей, FALSE — нулем.
    canonical    = canon == "canon",
    has_subtitle = !is.na(`Genre subtitle`),
    has_reprint  = !is.na(reprints)
  )

glimpse(elegies_meta)


# 2. ГДЕ В ТАБЛИЦЕ НАХОДЯТСЯ ПРОПУСКИ? ------------------------

missing_data <- elegies_meta |>
  # Для каждого столбца считаем количество NA.
  # Результатом пока будет одна очень широкая строка.
  summarise(
    across(
      everything(),
      \(x) sum(is.na(x))
    )
  ) |>
  # Превращаем широкую строку в длинную таблицу:
  # название переменной — количество пропусков.
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing"
  ) |>
  arrange(desc(missing))

missing_data

# Пропуск не всегда означает ошибку.
# Например, Place заполнено преимущественно для отдельных изданий,
# а сведения о перепечатках имеются лишь у некоторых текстов.


# 3. СОЗДАНИЕ ИССЛЕДОВАТЕЛЬСКОЙ ВЫБОРКИ -----------------------

# В диссертации для анализа изменений во времени используются
# публикации из периодики и альманахов.
#
# Авторские сборники исключаются, поскольку год выхода сборника
# не обязательно совпадает с годом создания или первой публикации
# вошедшего в него стихотворения.

elegies_meta |> 
  count(source_type)

research_corpus <- elegies_meta |>
  filter(source_type %in% c("периодика", "альманах"))

nrow(research_corpus)

# В этой выборке должно быть 390 текстов.


# 4. ОБЩАЯ ХАРАКТЕРИСТИКА ТРЕХ ПЕРИОДОВ -----------------------

period_summary <- research_corpus |>
  group_by(period) |>
  summarise(
    # Общее количество текстов.
    texts = n(),
    
    # Количество различных метрических формул.
    metre_forms = n_distinct(metre_form),
    
    # Количество текстов, написанных четырехстопным ямбом.
    iamb4_texts = sum(metre_form == "Я4"),
    
    # Среднее логической переменной дает долю TRUE.
    iamb4_share = mean(metre_form == "Я4"),
    
    canonical_share = mean(canonical),
    
    .groups = "drop"
  ) |>
  # Переводим доли в проценты.
  mutate(
    across(
      ends_with("_share"),
      \(x) round(100 * x, 1)
    )
  )

period_summary

# На приложенных данных получается:
#
# 1815–1820: Я4 — 15,8% текстов;
# 1821–1827: Я4 — 30,1%;
# 1828–1835: Я4 — 57,1%.
#
# Гипотеза о распространении четырехстопного ямба подтверждается.
#
# Однако количество метрических форм меняется не линейно:
# 15 → 24 → 11.
#
# Следовательно, сначала метрическое разнообразие увеличивается,
# а его резкое сокращение происходит только в поздний период.


# 5. ПЯТЬ ГЛАВНЫХ РАЗМЕРОВ КАЖДОГО ПЕРИОДА --------------------

top_metres <- research_corpus |>
  # Считаем тексты для каждой комбинации периода и размера.
  count(
    period,
    metre_form,
    name = "texts"
  ) |>
  group_by(period) |>
  # Знаменатель здесь — все тексты соответствующего периода.
  mutate(
    share = texts / sum(texts)
  ) |>
  # Внутри каждого периода оставляем пять наиболее частых размеров.
  slice_max(
    order_by = texts,
    n = 5,
    with_ties = FALSE
  ) |>
  ungroup() |>
  arrange(period, desc(texts))

top_metres

# В ранний период первое место занимает Я6,
# тогда как в поздний период Я4 заметно опережает остальные размеры.


# 6. ПЕРЕХОД ОТ ДЛИННОЙ ТАБЛИЦЫ К ШИРОКОЙ ---------------------

metre_table <- research_corpus |>
  count(
    metre_form,
    period,
    name = "texts"
  ) |>
  pivot_wider(
    names_from = period,
    values_from = texts,
    values_fill = 0
  ) |>
  arrange(desc(`1828–1835`))

metre_table

# Длинный формат удобен для группировки и вычислений:
# одна строка соответствует одной комбинации размера и периода.
#
# Широкий формат удобнее для чтения:
# периоды превращаются в отдельные столбцы.


# 7. КАК МЕНЯЕТСЯ ДОЛЯ Я4 ПО ОТДЕЛЬНЫМ ГОДАМ? -----------------

iamb4_by_year <- research_corpus |>
  group_by(year) |>
  summarise(
    texts = n(),
    iamb4_texts = sum(metre_form == "Я4"),
    iamb4_share = mean(metre_form == "Я4"),
    metre_forms = n_distinct(metre_form),
    .groups = "drop"
  ) |>
  mutate(
    iamb4_share = round(100 * iamb4_share, 1)
  )

iamb4_by_year

# Группировка по крупным периодам показывает общий процесс,
# но скрывает колебания между отдельными годами.
# Особенно осторожно следует интерпретировать годы,
# представленные небольшим количеством текстов.


# 8. ПРОСТАЯ ВИЗУАЛИЗАЦИЯ МЕТРИЧЕСКОГО СДВИГА -----------------

ggplot(
  iamb4_by_year,
  aes(x = year, y = iamb4_share)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Распространение четырехстопного ямба",
    x = NULL,
    y = "Доля текстов, %"
  )


# 9. MAP: ЧТЕНИЕ ПАПКИ С ТЕКСТАМИ -----------------------------

# list.files() находит все текстовые файлы в папке corpus.
# full.names = TRUE сохраняет полный путь к каждому файлу.

text_files <- tibble(
  path = list.files(
    "./files/elegies/elegies_corpus/corpus",
    pattern = "\\.txt$",
    full.names = TRUE
  )
)

texts <- text_files |>
  mutate(
    # В оригинальном корпусе имя файла содержит id текста.
    # parse_number() извлекает из имени числовой идентификатор.
    id = parse_number(basename(path)),
    
    # map() применяет read_lines() отдельно к каждому файлу.
    # В результате возникает списочный столбец:
    # каждая его ячейка содержит строки одного стихотворения.
    lines = map(
      path,
      read_lines
    )
  )

texts_long <- texts |>
  unnest_longer(
    lines,
    values_to = "line"
  ) |>
  filter(str_squish(line) != "") |>
  group_by(id) |>
  mutate(line_number = row_number()) |>
  ungroup()
    
# 10. СТАНОВЯТСЯ ЛИ ЭЛЕГИИ КОРОЧЕ? ----------------------------

poem_stats <- texts_long |>
  count(
    id,
    name = "n_lines"
  )


# Присоединяем метаданные по идентификатору стихотворения.

poem_stats <- poem_stats |>
  left_join(
    elegies_meta,
    by = "id"
  )

poem_stats


# Проверяем, для всех ли файлов нашлись метаданные.
# Если таблица пустая, соединение прошло успешно.

poem_stats |>
  filter(is.na(year))


# Для исследования динамики оставляем тексты
# из периодики и альманахов.

poem_stats_periodicals <- poem_stats |>
  filter(source_type %in% c("периодика", "альманах"))


# Рассчитываем статистику длины элегий по годам.

year_stats <- poem_stats_periodicals |>
  group_by(year) |>
  summarise(
    # Количество элегий, опубликованных в этом году.
    texts = n(),
    
    # Среднее количество строк.
    mean_lines = mean(n_lines),
    
    # Медианное количество строк.
    median_lines = median(n_lines),
    
    # Самая короткая и самая длинная элегии.
    shortest = min(n_lines),
    longest = max(n_lines),
    
    .groups = "drop"
  ) |>
  mutate(
    across(
      c(mean_lines, median_lines),
      \(x) round(x, 1)
    )
  ) |>
  arrange(year)

year_stats


# Простой график изменения медианной длины.

ggplot(
  year_stats,
  aes(x = year, y = median_lines)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Как менялась длина русской элегии",
    x = NULL,
    y = "Медианная длина, строк"
  )
