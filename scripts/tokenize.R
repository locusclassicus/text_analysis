# ============================================================
# ТОКЕНИЗАЦИЯ И ЛЕММАТИЗАЦИЯ — КОД К ГЛАВЕ
# https://locusclassicus.github.io/text_analysis/tokenize.html
# «Гарри Поттер и философский камень»
# Пояснения — в главе учебника; здесь только код.
# ============================================================

# Установка (один раз):
# install.packages(c("tidyverse", "tidytext", "udpipe", "igraph", "ggraph", "scales"))
# remotes::install_github("bradleyboehmke/harrypotter")

library(tidyverse)
library(tidytext)
library(udpipe)
library(harrypotter)


# ------------------------------------------------------------
# 1. ДАННЫЕ
# ------------------------------------------------------------

class(philosophers_stone)
length(philosophers_stone)
str_trunc(philosophers_stone[1], 80)

# Название главы отделено от текста символом 　.
chapters <- tibble(
  chapter = seq_along(philosophers_stone),
  text = philosophers_stone
) |>
  mutate(
    title = str_extract(text, "^[^　]+"),
    text = str_remove(text, "^[^　]+　+"),
    text = str_squish(text)
  ) |>
  select(chapter, title, text)

chapters


# ------------------------------------------------------------
# 2. ТОКЕНИЗАЦИЯ
# ------------------------------------------------------------

example <- tibble(text = "Owls see owls. An owl!")

example_tokens <- example |>
  unnest_tokens(word, text)

example_tokens

example_tokens |>
  summarise(tokens = n(), types = n_distinct(word))

# Другие виды токенов
small <- tibble(text = "An owl arrived. Harry opened the letter.")

small |>
  unnest_tokens(sentence, text, token = "sentences")

small |>
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

# Правила токенизатора
tricky <- tibble(text = "Harry's owl can't read a well-known book.")

tricky |>
  unnest_tokens(word, text)

tibble(text = "It cost 17 Sickles in 1991.") |>
  unnest_tokens(word, text, strip_numeric = TRUE)

# Предложения: "Mr. and Mrs." режется неправильно
chapters |>
  unnest_tokens(sentence, text, token = "sentences", to_lower = FALSE) |>
  select(sentence) |>
  slice_head(n = 2)

# ЗАДАНИЕ: сколько типов в "Harry, HARRY and Harry's owls."?

# Весь роман
words <- chapters |>
  select(chapter, text) |>
  unnest_tokens(word, text)

words |>
  summarise(tokens = n(), types = n_distinct(word))


# ------------------------------------------------------------
# 3. СТОП-СЛОВА
# ------------------------------------------------------------

words |>
  count(word, sort = TRUE)

stop_list <- stop_words |>
  distinct(word)

content_counts <- words |>
  anti_join(stop_list, by = "word") |>
  count(word, sort = TRUE)

content_counts |>
  slice_head(n = 15) |>
  ggplot(aes(n, reorder(word, n))) +
  geom_col(fill = "#326A8B") +
  labs(x = NULL, y = NULL) +
  theme_light()


# ------------------------------------------------------------
# 4. МОДЕЛЬ UDPIPE
# ------------------------------------------------------------

# Скачать один раз (файл появится в рабочей папке):
# udpipe_download_model(language = "english-ewt")

english_ewt <- udpipe_load_model(file = "english-ewt-ud-2.5-191206.udpipe")


# ------------------------------------------------------------
# 5. ФОРМАТ CONLL-U
# ------------------------------------------------------------

demo <- tibble(
  doc_id = c("verb", "noun", "adj"),
  text = c("She saw two children and went home.",
           "The saw was sharp.",
           "They read an interesting book.")
)

demo_annotate <- udpipe_annotate(english_ewt, x = demo$text, doc_id = demo$doc_id)

cat(demo_annotate$conllu)

demo_pos <- demo_annotate |>
  as.data.frame() |>
  as_tibble()

demo_pos |>
  select(doc_id, token, lemma, upos)


# ------------------------------------------------------------
# 6. ПОЛЯ UPOS, FEATS, DEP_REL
# ------------------------------------------------------------

demo_pos |>
  filter(upos %in% c("NOUN", "PROPN")) |>
  select(token, lemma, upos, xpos)

demo_pos |>
  filter(str_detect(feats, "Number=Plur")) |>
  select(token, lemma, feats)

demo_pos |>
  filter(doc_id == "adj") |>
  select(token_id, token, head_token_id, dep_rel)

# ЗАДАНИЕ: разметьте "The books are old." и "She books a room.".
# Какая часть речи у books во втором предложении?


# ------------------------------------------------------------
# 7. РАЗМЕТКА РОМАНА
# ------------------------------------------------------------

# Около полуминуты
annotation <- udpipe_annotate(
  english_ewt,
  x = chapters$text,
  doc_id = chapters$chapter,
  parser = "none"
) |>
  as.data.frame() |>
  as_tibble()

# Или готовый результат из репозитория учебника:
# load("data/hp_annotation.Rdata")

annotation

annotation |>
  filter(doc_id == "1", sentence_id == 1) |>
  distinct(sentence) |>   # текст предложения повторяется в строке каждого токена
  pull(sentence)

annotated_words <- annotation |>
  filter(!upos %in% c("PUNCT", "SYM")) |>
  mutate(form = str_to_lower(token),
         lemma = str_to_lower(lemma))

annotated_words |>
  summarise(tokens = n(),
            forms = n_distinct(form),
            lemmas = n_distinct(lemma))

annotated_words |>
  filter(lemma == "say") |>
  count(form, upos, sort = TRUE)

annotated_words |>
  filter(upos == "NOUN") |>
  count(lemma, sort = TRUE) |>
  slice_head(n = 10)


# ------------------------------------------------------------
# 8. ПРОВЕРКА МОДЕЛИ
# ------------------------------------------------------------

annotated_words |>
  filter(form %in% c("crept", "leapt", "shook")) |>
  count(form, lemma, upos)

annotated_words |>
  filter(form == "hagrid") |>
  count(form, upos, xpos)

annotated_words |>
  filter(form == "hagrid", upos == "AUX") |>
  distinct(sentence) |>
  slice_head(n = 3) |>
  pull(sentence)


# ------------------------------------------------------------
# 9. ЧАСТОТНОСТЬ И ЗАКОН ЦИПФА
# ------------------------------------------------------------

counts <- words |>
  count(word, sort = TRUE)

N <- sum(counts$n)   # число токенов
V <- nrow(counts)    # число типов

c(N = N, V = V)

counts <- counts |>
  mutate(tf = n / N,
         rank = row_number())

counts

counts |>
  ggplot(aes(rank, tf)) +
  geom_line(linewidth = 1.1, alpha = 0.7, color = "navyblue") +
  theme_light()

# Прогноз при α = 1
counts |>
  filter(rank %in% c(1, 2, 10, 100)) |>
  mutate(zipf_guess = round(counts$n[1] / rank)) |>
  select(rank, word, n, zipf_guess)

counts |>
  ggplot(aes(rank, tf)) +
  geom_line(linewidth = 1.1, alpha = 0.7, color = "navyblue") +
  geom_smooth(method = "lm", formula = y ~ x,
              linetype = 2, color = "tomato") +
  scale_x_log10() +
  scale_y_log10() +
  theme_light()

lm(log10(tf) ~ log10(rank), data = counts) |>
  coef()


# ------------------------------------------------------------
# 10. СПЕКТР ЧАСТОТ
# ------------------------------------------------------------

counts |>
  filter(n == 1)

spectrum <- counts |>
  count(n, name = "V_m") |>
  rename(m = n)

spectrum

spectrum |>
  summarise(types = sum(V_m), tokens = sum(m * V_m))

spectrum |>
  filter(m <= 10) |>
  ggplot(aes(m, V_m)) +
  geom_col(fill = "#B15E47") +
  scale_x_continuous(breaks = 1:10) +
  labs(x = "m", y = "V_m") +
  theme_light()

V1 <- spectrum |>
  filter(m == 1) |>
  pull(V_m)

c(share_of_types = V1 / V, share_of_tokens = V1 / N)

# ЗАДАНИЕ: какую долю словаря составляют слова, встретившиеся дважды?


# ------------------------------------------------------------
# 11. РОСТ СЛОВАРЯ
# ------------------------------------------------------------

count_vocabulary <- function(size) {
  words |>
    slice_head(n = size) |>
    summarise(N = n(), V = n_distinct(word))
}

checkpoints <- c(1000, 5000, seq(10000, 70000, by = 10000), nrow(words))

growth <- map_dfr(checkpoints, count_vocabulary) |>
  mutate(TTR = V / N)

growth

growth |>
  ggplot(aes(N, V)) +
  geom_line(color = "navyblue") +
  geom_point(color = "navyblue") +
  labs(x = "Прочитано токенов", y = "Встречено типов") +
  theme_light()


# ------------------------------------------------------------
# 12. TF-IDF
# ------------------------------------------------------------

chapters_tfidf <- words |>
  count(chapter, word) |>
  bind_tf_idf(word, chapter, n)

chapters_tfidf |>
  filter(chapter == 1) |>
  arrange(-tf_idf) |>
  slice_head(n = 5)

chapters_tfidf |>
  filter(chapter <= 6) |>
  group_by(chapter) |>
  slice_max(tf_idf, n = 8, with_ties = FALSE) |>
  ungroup() |>
  ggplot(aes(reorder_within(word, tf_idf, chapter), tf_idf,
             fill = as.factor(chapter))) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~chapter, scales = "free", nrow = 2) +
  scale_x_reordered() +
  coord_flip() +
  theme_light()


# ------------------------------------------------------------
# 13. ДИАГРАММА РАССЕЯНИЯ: ГЛАВЫ 5 И 6
# ------------------------------------------------------------

spread_freq <- words |>
  filter(chapter %in% c(5, 6)) |>
  count(chapter, word) |>
  group_by(chapter) |>
  mutate(tf = n / sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = chapter, values_from = tf,
              values_fill = 0, names_prefix = "chapter_")

spread_freq

library(scales)

set.seed(2026)

spread_freq |>
  filter(chapter_5 > 0, chapter_6 > 0) |>
  ggplot(aes(x = chapter_5, y = chapter_6)) +
  geom_abline(color = "grey40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.05,
              height = 0.05, color = "darkblue") +
  geom_text(aes(label = word), check_overlap = TRUE,
            vjust = 1.5, color = "grey30") +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  theme_minimal()


# ------------------------------------------------------------
# 14. КОЛЛОКАЦИИ: ЛЕММЫ В НИЖНЕМ РЕГИСТРЕ
# ------------------------------------------------------------

lemmas <- annotation |>
  mutate(lemma = str_to_lower(lemma))


# ------------------------------------------------------------
# 15. В ОДНОМ ПРЕДЛОЖЕНИИ: КТО С КЕМ УПОМИНАЕТСЯ
# ------------------------------------------------------------

characters <- lemmas |>
  filter(upos == "PROPN") |>
  cooccurrence(group = c("doc_id", "sentence_id"), term = "lemma") |>
  as_tibble()

characters

library(igraph)
library(ggraph)

characters_graph <- characters |>
  slice_head(n = 40) |>
  graph_from_data_frame(directed = FALSE)

set.seed(2026)

ggraph(characters_graph, layout = "fr") +
  geom_edge_link(aes(edge_width = cooc), colour = "#9DB7C9",
                 show.legend = FALSE) +
  scale_edge_width(range = c(0.3, 3)) +
  geom_node_point(colour = "#B15E47", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3.5) +
  theme_void()


# ------------------------------------------------------------
# 16. ПО СОСЕДСТВУ: СЛОВА РЯДОМ ИЛИ ЧЕРЕЗ ОДНО
# ------------------------------------------------------------

neighbours <- cooccurrence(
  lemmas$lemma,
  relevant = lemmas$upos %in% c("NOUN", "ADJ"),
  skipgram = 1
) |>
  as_tibble()

neighbours

neighbours_graph <- neighbours |>
  arrange(desc(cooc), term1, term2) |>
  slice_head(n = 60) |>
  graph_from_data_frame()

set.seed(2026)

ggraph(neighbours_graph, layout = "fr") +
  geom_edge_link(aes(edge_width = cooc), colour = "#9DB7C9",
                 arrow = arrow(length = unit(2, "mm"), type = "closed"),
                 end_cap = circle(2.5, "mm"), show.legend = FALSE) +
  scale_edge_width(range = c(0.3, 2)) +
  geom_node_point(colour = "#B15E47", size = 2.5) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3.3) +
  theme_void()


# ------------------------------------------------------------
# 17. КОЛЛОКАЦИИ И МЕРЫ СВЯЗИ: PMI, MD, LFMD
# ------------------------------------------------------------

# Пунктуацию и частицы не удаляем, а заменяем на NA:
# так они разрывают соседство.
collocations <- lemmas |>
  mutate(lemma = if_else(upos %in% c("PUNCT", "PART"), NA, lemma)) |>
  keywords_collocation(term = "lemma", group = c("doc_id", "sentence_id"))

collocations_clean <- collocations |>
  filter(!left %in% stop_list$word, !right %in% stop_list$word)

collocations_clean |>
  arrange(desc(freq)) |>
  select(keyword, freq, freq_left, freq_right, pmi, md, lfmd) |>
  slice_head(n = 10)

# ЗАДАНИЕ: чему равна MD для пары слов, которые встречаются только вместе?

top10 <- function(measure) {
  collocations_clean |>
    arrange(desc(.data[[measure]])) |>
    slice_head(n = 10) |>
    pull(keyword)
}

tibble(
  freq = top10("freq"),
  pmi = top10("pmi"),
  md = top10("md"),
  lfmd = top10("lfmd")
)


# ------------------------------------------------------------
# 18. ПРОВЕРКА ПО ТЕКСТУ
# ------------------------------------------------------------

restricted <- annotation |>
  filter(str_detect(sentence, "Restricted Section")) |>
  distinct(doc_id, sentence_id, sentence)

restricted |>
  count(doc_id)

restricted |>
  pull(sentence)
