# Глава учебника: https://locusclassicus.github.io/text_analysis/embed.html

# ============================================================
# ШПАРГАЛКА 4. ОТ СРАВНЕНИЯ ФРАЗ К ПОИСКУ В КОРПУСЕ
# ============================================================
# Цель: найти фрагменты о браке и материальной обеспеченности.
# Продолжение 01–03: нужны embed_text(), cosine_similarity(), model_name.
# Пакеты: tidyverse, ollamar. Ollama запущена, модель скачана.
# Данные: подготовленные ТЕКСТЫ двух романов Остин.
# Векторы ниже участники получают сами.

library(tidyverse)
# ollamar:: вызывается внутри embed_text(); отдельно пакет не подключаем.

# 1. ЧИТАЕМ ТАБЛИЦУ ТЕКСТОВ -----------------------------------
# RDS — формат хранения объектов R; read_rds() читает объект.
# Здесь хранится таблица фрагментов, а не модель или её ответы.
# Разбиение подготовлено в embed-00-pilot-word2vec.R.
# Фрагмент — до 12 непустых строк внутри главы.
corpus <- read_rds("files/embeddings/austen_chunks.rds") |>
  filter(book %in% c("Pride & Prejudice", "Sense & Sensibility")) |>
  select(chunk_id, book, chapter, block,
         source_line_start, source_line_end, text)

corpus |>
  count(book)

corpus |>
  slice_head(n = 2) |>
  select(chunk_id, chapter, text)

# Перед первой отправкой просмотрите несколько строк.
# Пунктуация, имена, отрицания и порядок слов сохранены.
# Если пока хотите попробовать на одной главе:
# corpus <- corpus |> filter(book == "Pride & Prejudice", chapter == 22)
# Для сравнения романов потом вернитесь к полной таблице.

# 2. ПОИСК ПО СЛОВУ -------------------------------------------
corpus |>
  filter(str_detect(text, regex("\\bmoney\\b", ignore_case = TRUE))) |>
  count(book)

# Наш сюжет — обеспеченность женщины в связи с браком.
# Эту ситуацию можно описать без слова money.
# Расширенный словарь полезно сравнить со смысловым поиском:
corpus |>
  filter(str_detect(
    text, regex("\\b(money|fortune|income|property|pounds)\\b",
                ignore_case = TRUE)
  )) |>
  count(book)

# 3. ПОЛУЧАЕМ ВЕКТОРЫ ФРАГМЕНТОВ ------------------------------
# Для документа EmbeddingGemma использует префикс title: ... | text: ...
# title: none означает, что отдельного заголовка мы не передаём.
# Функция embed_text() та же, что в предыдущей шпаргалке.
#
# map() последовательно отправит каждый фрагмент в Ollama.
# .progress = TRUE показывает ход расчёта.
# Этот блок выполняем один раз, а не при каждой смене запроса.
# В корпусе 1823 коротких фрагмента; на компьютерах время различается.
corpus <- corpus |>
  mutate(
    model_input = str_c("title: none | text: ", text),
    vector = map(model_input, embed_text, .progress = TRUE),
    model = model_name
  )

corpus |>
  select(chunk_id, text, vector) |>
  slice_head(n = 3)

# Сохраняем полученные векторы вместе с текстами.
write_rds(corpus, "files/embeddings/austen_live_vectors.rds")

# При повторном занятии можно прочитать именно этот результат:
# corpus <- read_rds("files/embeddings/austen_live_vectors.rds")
# Используйте ту же модель для нового запроса.

# 4. ФОРМУЛИРУЕМ НОВЫЙ ЗАПРОС --------------------------------
# Можно написать СВОЙ запрос; для него сейчас будет рассчитан вектор.
# Смена текста запроса требует повторить блоки 4–6.
# Повторять расчёт всех документов не требуется.
query <- "A woman marries to secure a home and a reliable income."

# Для запроса используется другой префикс.
# Эта пара форматов обучена для задачи «запрос — подходящий документ».
# В предыдущей шпаргалке мы решали задачу «сходство двух фраз».
query_vector <- str_c("task: search result | query: ", query) |>
  embed_text()

# 5. СЧИТАЕМ СХОДСТВО ЗНАКОМОЙ ФОРМУЛОЙ -----------------------
# К каждому вектору документа применяем ту же функцию косинуса.
# Меняется представление текста, но не определение меры.
results <- corpus |>
  mutate(
    similarity = map_dbl(vector, cosine_similarity, y = query_vector),
    query = query
  ) |>
  select(-vector, -model_input) |>
  arrange(desc(similarity))

# Берём по десять наиболее близких фрагментов каждого романа.
# with_ties = FALSE ограничивает число строк при равенстве баллов.
top_results <- results |>
  group_by(book) |>
  slice_max(similarity, n = 10, with_ties = FALSE) |>
  mutate(rank = row_number()) |>
  ungroup()

top_results |>
  select(book, rank, chunk_id, chapter, similarity)

# 6. ЧИТАЕМ И ПРОВЕРЯЕМ ---------------------------------------
# Сначала прочитайте первый результат «Гордости и предубеждения».
chosen <- top_results |>
  filter(book == "Pride & Prejudice", rank == 1)

chosen |>
  pull(text)

# Подходит ли он именно к вопросу о ЖЕНСКОЙ обеспеченности?
# Кто говорит? О ком? Как оценивает такой брак?
# Высокий балл означает близость представлений, не верный ответ.

# Фрагмент мог разрезать предложение: откроем соседние блоки.
corpus |>
  filter(
    book == pull(chosen, book),
    chapter == pull(chosen, chapter),
    abs(block - pull(chosen, block)) <= 1
  ) |>
  arrange(block) |>
  pull(text)

# УПРАЖНЕНИЯ --------------------------------------------------
# 1. Найдите в десятке подходящий и неподходящий фрагмент.
# 2. Напишите запрос о давлении родственников и повторите блоки 4–6.
# 3. Попробуйте русскую формулировку того же запроса.
# 4. Сопоставьте по два проверенных эпизода из двух романов.
#
# Нельзя по верхней десятке заключить, где тема встречается чаще:
# отбор построен по сходству с конкретным запросом.

# Сохраняем тексты и запрос для ручной разметки.
top_results |>
  mutate(relevant = NA_integer_, reason = "") |>
  write_csv("files/embeddings/live_search_top10.csv")

# relevant: 1 — соответствует критерию, 0 — не соответствует,
# NA — решение ещё не принято. reason — основание решения.
# Итог: проверенная выборка для чтения и анализа своего материала.
