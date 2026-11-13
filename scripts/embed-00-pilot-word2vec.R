# Глава учебника: https://locusclassicus.github.io/text_analysis/embed.html

# ПИЛОТ К МАСТЕР-КЛАССУ О ВЕКТОРАХ --------------------------------------
# Для преподавателя: подготовить материал и проверить соседей слов.
# Учебник: https://locusclassicus.github.io/text_analysis/embed.html
# Данные: шесть романов из janeaustenr 1.0.0; сеть не нужна.
# Пакеты устанавливаются заранее: janeaustenr, dplyr, stringr, word2vec.
# Запускать из корня проекта в чистой сессии R.

library(janeaustenr)
library(dplyr)
library(stringr)
library(word2vec)

# 1. Готовим фрагменты с адресами в исходном тексте ------------------------
# Строка в пакете — типографская строка около 70 знаков, не предложение.
# Сохраняем её номер, чтобы можно было прочитать окружающий контекст.
# chapter — сквозной номер главы внутри романа, включая многотомную Emma.
lines <- austen_books() |>
  group_by(book) |>
  mutate(
    source_line = row_number(),
    heading = str_detect(text, regex("^chapter [0-9ivxlc]+$", ignore_case = TRUE)),
    chapter = cumsum(heading)
  ) |>
  ungroup()

# Убираем титульные страницы, пустые строки и заголовки.
# Не удаляем имена, служебные слова и отрицания.
# Соседние 12 непустых строк одной главы образуют поисковый фрагмент.
# Это простое фиксированное разбиение может разрезать предложение.
# При интерпретации обязательно читаем соседние фрагменты.
chunks <- lines |>
  filter(chapter > 0, !heading, str_trim(text) != "") |>
  filter(!str_detect(text, regex("^volume [ivxlc]+$", ignore_case = TRUE))) |>
  group_by(book, chapter) |>
  mutate(block = (row_number() - 1L) %/% 12L + 1L) |>
  group_by(book, chapter, block) |>
  summarise(
    source_line_start = min(source_line),
    source_line_end = max(source_line),
    text = str_squish(paste(text, collapse = " ")),
    .groups = "drop"
  ) |>
  mutate(
    chunk_id = sprintf("austen_%04d", row_number()),
    n_words = str_count(text, "[A-Za-z]+(?:'[A-Za-z]+)?")
  ) |>
  select(chunk_id, everything())

dir.create("files/embeddings", recursive = TRUE, showWarnings = FALSE)
dir.create("files/embeddings", recursive = TRUE, showWarnings = FALSE)
saveRDS(chunks, "files/embeddings/austen_chunks.rds")

corpus_summary <- chunks |>
  group_by(book) |>
  summarise(chapters = n_distinct(chapter), chunks = n(), words = sum(n_words))
print(corpus_summary)
write.csv(corpus_summary, "files/embeddings/corpus_summary.csv", row.names = FALSE)

# 2. Обучаем одну небольшую модель на всех шести романах -------------------
# Каждому слову соответствует один вектор; контексты смешиваются.
# Список tokens содержит отдельный вектор слов для каждого фрагмента.
# Словоформы money и moneys остаются разными; лемматизацию здесь не делаем.
tokens <- str_extract_all(str_to_lower(chunks$text), "[a-z]+(?:'[a-z]+)?")
set.seed(20261107)
elapsed <- system.time({
  model <- word2vec(
    x = tokens, type = "skip-gram", dim = 50,
    window = 5, iter = 20, min_count = 5, threads = 1
  )
})

# Сохраняем обычную матрицу чисел: её можно открыть без пакета word2vec.
# Для воспроизведения конкретной демонстрации используем сохранённую матрицу:
# один seed не гарантирует совпадение обучения на всех версиях и платформах.
vectors <- as.matrix(model)
saveRDS(vectors, "files/embeddings/austen_word_vectors.rds")

# 3. Получаем соседей слов ------------------------------------------------
normalized <- vectors / sqrt(rowSums(vectors^2))
neighbors <- list()
for (term in c("money", "marriage", "love", "fortune")) {
  scores <- drop(normalized %*% normalized[term, ])
  scores[rownames(normalized) == term] <- -Inf
  best <- head(order(scores, decreasing = TRUE), 8)
  neighbors[[term]] <- data.frame(
    term1 = term, term2 = rownames(normalized)[best],
    cosine = scores[best], rank = seq_along(best)
  )
}
neighbors <- bind_rows(neighbors)
print(neighbors)
write.csv(neighbors, "files/embeddings/word_neighbors.csv", row.names = FALSE)

# 4. Проверяем контекст для начала занятия --------------------------------
# Это поиск по словам, а не результат EmbeddingGemma.
anchors <- chunks |>
  filter(
    book == "Pride & Prejudice",
    str_detect(text, fixed("comfortable home")) |
      str_detect(text, fixed("preservative from want"))
  )
print(anchors |> select(chunk_id, chapter, source_line_start, text), width = 100)
write.csv(anchors, "files/embeddings/lexical_anchors.csv", row.names = FALSE)

# 5. Фиксируем условия пилота --------------------------------------------
writeLines(
  c(
    "Дата проверки: 2026-11-07",
    "Параметры word2vec: skip-gram; dim=50; window=5; iter=20; min_count=5; threads=1",
    "Seed: 20261107; токены: [a-z]+(?:'[a-z]+)?; без лемматизации",
    paste("Обучение, секунд:", unname(elapsed["elapsed"])),
    paste("Слов в словаре:", nrow(vectors)),
    capture.output(sessionInfo())
  ),
  "files/embeddings/word2vec_session.txt"
)

# Упражнение для будущей студенческой версии:
# выберите одного соседа marriage и проверьте его в исходных фрагментах.
# Объясните, является ли он синонимом или только связан с похожей ситуацией.
