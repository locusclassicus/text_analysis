# Глава учебника: https://locusclassicus.github.io/text_analysis/embed.html

# ПИЛОТ СМЫСЛОВОГО ПОИСКА -------------------------------------------------
# Для преподавателя; сначала выполните embed-00-pilot-word2vec.R.
# Данные: фрагменты двух романов Остин. Пакет: httr2.
# Требуется запущенная локальная Ollama и модель embeddinggemma:latest.
# Модель здесь не скачивается. Ключи и облачные сервисы не используются.
# Участники могут работать с сохранёнными векторами без Ollama.
# Документация: https://docs.ollama.com/api/embed
# Префиксы: https://ai.google.dev/gemma/docs/embeddinggemma/model_card

library(httr2)

# 1. Выбираем два полных романа -------------------------------------------
chunks <- readRDS("files/embeddings/austen_chunks.rds")
documents <- droplevels(as.data.frame(
  chunks[chunks$book %in% c("Pride & Prejudice", "Sense & Sensibility"), ]
))

queries <- data.frame(
  query_id = c("security_en", "love_en", "pressure_en", "refusal_en", "security_ru"),
  text = c(
    "A woman marries to secure a home and a reliable income.",
    "Marriage for love despite poverty.",
    "Relatives put pressure on a woman to accept a wealthy suitor.",
    "A woman refuses a financially advantageous marriage.",
    "Женщина выходит замуж, чтобы обеспечить себе дом и средства к существованию."
  )
)

# 2. Одна небольшая функция общения с локальной моделью -------------------
# R отправляет строки и получает строки матрицы: один текст — один вектор.
# truncate = FALSE запрещает молча отрезать конец слишком длинного текста.
model_name <- "embeddinggemma:latest"
embed <- function(texts) {
  response <- request("http://localhost:11434/api/embed") |>
    req_body_json(list(model = model_name, input = as.list(texts), truncate = FALSE)) |>
    req_timeout(120) |>
    req_perform() |>
    resp_body_json()
  matrix <- do.call(rbind, lapply(response$embeddings, unlist))
  stopifnot(nrow(matrix) == length(texts), all(is.finite(matrix)))
  # Нормируем явно, чтобы скалярное произведение равнялось косинусу.
  lengths <- sqrt(rowSums(matrix^2))
  stopifnot(all(lengths > 0))
  matrix / lengths
}

tags <- request("http://localhost:11434/api/tags") |>
  req_perform() |>
  resp_body_json()
model_info <- Filter(function(x) identical(x$name, model_name), tags$models)
stopifnot(length(model_info) == 1L)
ollama_version <- request("http://localhost:11434/api/version") |>
  req_perform() |>
  resp_body_json()

# 3. Считаем векторы небольшими порциями ----------------------------------
# Названия романов не добавляем в текст: модель сравнивает сами фрагменты.
# И запросы, и документы обрабатывает одна версия модели.
document_prefix <- "title: none | text: "
query_prefix <- "task: search result | query: "
parts <- split(seq_len(nrow(documents)), ceiling(seq_len(nrow(documents)) / 32))
vectors <- vector("list", length(parts))
elapsed <- system.time({
  for (i in seq_along(parts)) {
    vectors[[i]] <- embed(paste0(document_prefix, documents$text[parts[[i]]]))
    message("Готово порций: ", i, " / ", length(parts))
  }
})
document_vectors <- do.call(rbind, vectors)
query_vectors <- embed(paste0(query_prefix, queries$text))
rownames(document_vectors) <- documents$chunk_id
rownames(query_vectors) <- queries$query_id
stopifnot(ncol(document_vectors) == 768L, ncol(query_vectors) == 768L)

# 4. Сохраняем единый файл, чтобы не потерять порядок текстов и векторов ----
cache <- list(
  documents = documents, document_vectors = document_vectors,
  queries = queries, query_vectors = query_vectors,
  metadata = list(
    checked_on = "2026-11-07", model = model_name,
    digest = model_info[[1]]$digest, details = model_info[[1]]$details,
    ollama_version = ollama_version$version,
    document_prefix = document_prefix, query_prefix = query_prefix,
    truncate = FALSE, normalized = TRUE,
    document_seconds = unname(elapsed["elapsed"])
  )
)
saveRDS(cache, "files/embeddings/austen_search_cache.rds")

# 5. Сравниваем результаты внутри каждого романа --------------------------
# Эти баллы показывают близость к запросу, а не вероятность верного ответа.
results <- list()
for (q in seq_len(nrow(queries))) {
  scores <- drop(document_vectors %*% query_vectors[q, ])
  for (book_name in unique(as.character(documents$book))) {
    candidates <- which(documents$book == book_name)
    best <- head(candidates[order(scores[candidates], decreasing = TRUE)], 10)
    results[[length(results) + 1L]] <- data.frame(
      query_id = queries$query_id[q], rank = seq_along(best),
      score = scores[best], documents[best, ],
      relevant = NA, reason = NA_character_
    )
  }
}
results <- do.call(rbind, results)
write.csv(results, "files/embeddings/search_top10.csv", row.names = FALSE)

# 6. Готовим материал для ручной проверки ---------------------------------
# Для security_en: выдача модели, случайные совпадения словаря и случайный фон.
# Словарь выбран до просмотра результатов; это лексический ориентир, не эталон.
pattern <- "\\b(money|fortune|income|property|pounds)\\b"
keyword_hit <- grepl(pattern, documents$text, ignore.case = TRUE, perl = TRUE)
set.seed(20261107)
review <- list()
for (book_name in unique(as.character(documents$book))) {
  candidates <- which(documents$book == book_name)
  keyword <- candidates[keyword_hit[candidates]]
  keyword_sample <- sample(keyword, min(10L, length(keyword)))
  background <- sample(candidates, min(10L, length(candidates)))
  semantic <- results$chunk_id[
    results$query_id == "security_en" & results$book == book_name
  ]
  ids <- unique(c(semantic, documents$chunk_id[keyword_sample], documents$chunk_id[background]))
  rows <- match(ids, documents$chunk_id)
  review[[book_name]] <- data.frame(
    documents[rows, ],
    from_semantic_top10 = ids %in% semantic,
    from_keyword_sample = ids %in% documents$chunk_id[keyword_sample],
    from_random_sample = ids %in% documents$chunk_id[background],
    keyword_hit = keyword_hit[rows],
    relevant = NA, reason = NA_character_
  )
}
review <- do.call(rbind, review)
review <- review[sample(seq_len(nrow(review))), ]
write.csv(review, "files/embeddings/review_pool_teacher.csv", row.names = FALSE)
# В студенческой таблице скрываем способ отбора, чтобы он не влиял на оценку.
blind <- review[c("chunk_id", "book", "chapter", "source_line_start", "source_line_end", "text", "relevant", "reason")]
write.csv(blind, "files/embeddings/review_pool_student.csv", row.names = FALSE)

writeLines(
  c(capture.output(str(cache$metadata)), capture.output(sessionInfo())),
  "files/embeddings/embeddinggemma_session.txt"
)
print(results[results$query_id == "security_en" & results$rank <= 2,
              c("book", "chapter", "chunk_id", "score")])

# Упражнение: оцените строки relevant и reason вручную.
# Наличие ключевого слова само по себе не доказывает релевантность.
