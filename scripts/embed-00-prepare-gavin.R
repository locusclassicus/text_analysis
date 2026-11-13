# Глава учебника: https://locusclassicus.github.io/text_analysis/embed.html

# ПОДГОТОВКА АВТОРСКОЙ ТАБЛИЦЫ ГЭВИНА ДЛЯ QMD -----------------
# Для преподавателя. Источник:
# https://github.com/michaelgavin/empson/blob/master/ci/eebo_old.rda
# Скачанный файл не изменяем; переводим матрицу в tibble.
# Векторы не пересчитываем и модель не обучаем.
library(tidyverse)

# .rda содержит объект eebo; загружаем только в отдельное окружение.
source_data <- new.env()
load("files/embeddings/gavin2018/eebo_old.rda", envir = source_data)

# Одна строка — слово; один элемент списка — все его 1751 координаты.
# Индексация здесь нужна только для импорта авторской матрицы.
word_vectors <- tibble(word = rownames(source_data$eebo)) |>
  mutate(vector = map(word, ~ source_data$eebo[.x, ]))

write_rds(word_vectors, "files/embeddings/gavin_word_vectors.rds")

# В скачанном файле суммы СТОЛБЦОВ равны 1, суммы строк — разные.
# Это расходится с фразой про суммы строк в авторском walkthrough.
# Сохраняем фактическую структуру файла, не исправляем её молча.
inspection <- tibble(
  statistic = c("rows", "columns", "min_column_sum", "max_column_sum",
                "min_row_sum", "max_row_sum"),
  value = c(
    nrow(source_data$eebo), ncol(source_data$eebo),
    min(colSums(source_data$eebo)), max(colSums(source_data$eebo)),
    min(rowSums(source_data$eebo)), max(rowSums(source_data$eebo))
  )
)
write_csv(inspection, "files/embeddings/gavin_data_check.csv")
inspection
