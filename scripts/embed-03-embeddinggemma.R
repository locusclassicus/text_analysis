# Глава учебника: https://locusclassicus.github.io/text_analysis/embed.html

# ============================================================
# ШПАРГАЛКА 3. ПОЛУЧАЕМ ЭМБЕДДИНГИ ЧЕРЕЗ OLLAMA
# ============================================================
# Цель: получить векторы самим и сравнить результат с частотами.
# Продолжение 01 и 02: нужны texts, lexical_scores, cosine_similarity.
# Пакеты: tidyverse, ollamar. Ollama уже установлена и запущена.
# Учебник: глава 11. Здесь используем первую EmbeddingGemma.
# Документация: https://hauselin.github.io/ollama-r/reference/embed.html

library(tidyverse)
# Функции Ollama вызываем через ollamar::, без library(ollamar).
# В ollamar тоже есть pull(), но она скачивает модель.
# Нам важно сохранить pull() из dplyr для извлечения столбцов.

# 1. ПРОВЕРЯЕМ ПОДКЛЮЧЕНИЕ И МОДЕЛЬ ---------------------------
# Установка Ollama и наличие конкретной модели — разные вещи.
ollamar::test_connection()

ollamar::list_models() |>
  filter(str_detect(name, "embeddinggemma"))

# Если модели нет, заранее скачайте её один раз:
# ollamar::pull("embeddinggemma")
# На занятии повторно скачивать её не нужно.

model_name <- "embeddinggemma:latest"

# 2. ОДИН ТЕКСТ -> ОДИН ВЕКТОР --------------------------------
sentence <- texts |>
  filter(id == "A") |>
  pull(text)

# Префикс сообщает модели задачу: сравнение смысла двух фраз.
# Это рекомендованный формат EmbeddingGemma.
# Для поиска документов в следующей шпаргалке формат будет другим.
model_input <- str_c("task: sentence similarity | query: ", sentence)
model_input

embedding <- ollamar::embed(
  model = model_name,
  input = model_input,
  host = "http://localhost:11434"
)

# ollamar возвращает матрицу: один столбец на входной текст.
# Передали одну фразу -> получили один столбец из 768 чисел.
dim(embedding)

# Превращаем его в обычную таблицу tidyverse.
# .name_repair задаёт имя единственному столбцу.
coordinates <- embedding |>
  as_tibble(.name_repair = ~ "value") |>
  mutate(coordinate = row_number(), .before = 1)

coordinates |>
  slice_head(n = 8)

# Эти числа получены обученной моделью.
# 17-я координата не называется «любовь» или «деньги».
# Значение отдельных координат обычно нельзя так истолковать.
# Модель вернула вектор; оценку сходства сейчас вычислим сами.

# 3. ПОВТОРЯЕМ ДЛЯ НЕСКОЛЬКИХ ТЕКСТОВ -------------------------
# Оборачиваем уже знакомые действия в небольшую функцию.
# На входе — строка с нужным префиксом.
# На выходе — числовой вектор (pull() извлекает столбец таблицы).
embed_text <- function(model_input) {
  ollamar::embed(
    model = model_name,
    input = model_input,
    host = "http://localhost:11434"
  ) |>
    as_tibble(.name_repair = ~ "value") |>
    pull(value)
}

# map() возвращает список векторов, по одному на каждую строку.
sentence_vectors <- texts |>
  mutate(
    model_input = str_c("task: sentence similarity | query: ", text),
    vector = map(model_input, embed_text)
  )

sentence_vectors |>
  select(id, text, vector)

reference_embedding <- sentence_vectors |>
  filter(id == "A") |>
  pull(vector) |>
  pluck(1)

embedding_scores <- sentence_vectors |>
  mutate(
    embedding_cosine = map_dbl(
      vector, cosine_similarity, y = reference_embedding
    )
  ) |>
  filter(id != "A") |>
  select(id, embedding_cosine)

# 4. СРАВНИВАЕМ ТРИ ПРЕДСТАВЛЕНИЯ -----------------------------
comparison <- lexical_scores |>
  left_join(embedding_scores, by = "id")

comparison |>
  select(id, text, count_cosine, tfidf_cosine, embedding_cosine)

# Сравниваем ПОРЯДОК соседей и читаем сами фразы.
# Значение 0.6 в TF-IDF и 0.6 в EmbeddingGemma
# не означает одинаковой степени смыслового сходства.
comparison |>
  arrange(desc(embedding_cosine)) |>
  select(id, text, embedding_cosine)

# Упражнение:
# 1. Поднялась ли перефразированная фраза B относительно банковской C?
# 2. Где оказался отказ D? Почему близость темы не равна согласию?
# 3. Измените одну фразу и заново получите её вектор.
# Порядок результатов проверяем, не задаём заранее.
#
# Здесь мы НЕ обучали EmbeddingGemma на четырёх фразах.
# Мы применили уже обученную модель к новым текстам.
# Установленная Ollama выполняет расчёт локально.
