# Глава учебника: https://locusclassicus.github.io/text_analysis/plot.html

# ВИЗУАЛИЗАЦИЯ ДАННЫХ В GGPLOT2 -------------------------------
#
# Шпаргалка по выбору графика и интерпретации результата.
#
# Данные:
# Underwood, Ted, Patrick Kimutis, and Jessica Witte. 2020.
# "NovelTM Datasets for English-Language Fiction, 1700–2009".
# Journal of Cultural Analytics 5 (2).
# https://doi.org/10.22148/001c.13147
#
# Репозиторий с данными и кодом к статье:
# https://github.com/tedunderwood/noveltmmeta
#
# Мы НЕ загружаем полный набор из 210 тысяч томов.
# Используется manual_title_subset.tsv — вручную проверенная выборка:
# 2730 строк, 19 столбцов, около 0.5 МБ на диске.
# Для обычного ноутбука это очень маленький датасет.
#
# Главный сюжет урока:
# цифровая библиотека показывает не просто "историю литературы".
# Она одновременно отражает:
#   1) время первой публикации произведения;
#   2) автоматически предполагаемую дату публикации;
#   3) историю каталогизации;
#   4) собирательские предпочтения академических библиотек.


# 0. КАК ВЫБРАТЬ ГРАФИК ---------------------------------------
#
# Вопрос                                             График
# ------------------------------------------------------------
# Сколько наблюдений в каждой категории?             geom_bar()
# Как распределена числовая переменная?              geom_histogram()
# Как сравнить числовые распределения по группам?    geom_boxplot()
# Связаны ли две числовые переменные?                geom_point()
# Как показатель меняется во времени?               geom_line()
# Как меняется состав целого?                        position = "fill"
# У нас уже посчитаны значения столбиков?            geom_col()
# Нужно сравнить одинаковый сюжет в подгруппах?      facet_wrap()
#
# Сначала формулируем вопрос — затем выбираем геом.
# Один и тот же набор данных не требует одного "правильного" графика:
# разные графики отвечают на разные вопросы.


# 1. ПАКЕТЫ И ДАННЫЕ ------------------------------------------

library(tidyverse)

# Если файл уже лежит в папке data, читаем локальную копию.
# Иначе read_tsv() загрузит маленькую таблицу прямо из GitHub.

data_url <- paste0(
  "https://raw.githubusercontent.com/tedunderwood/",
  "noveltmmeta/master/metadata/manual_title_subset.tsv"
)

noveltm <-  read_tsv(data_url, show_col_types = FALSE)

# Проверяем размер таблицы и памяти.
# nrow() и ncol() должны вернуть 2730 и 19.

nrow(noveltm)
ncol(noveltm)
format(object.size(noveltm), units = "auto")

# Быстрый обзор переменных.

glimpse(noveltm)


# 2. ЧТО ОЗНАЧАЮТ ДАТЫ ----------------------------------------
#
# inferreddate — предполагаемая дата публикации заглавия, автоматически
#                выведенная из MARC-полей выбранного тома. В title list
#                старались выбирать самый ранний доступный том, поэтому
#                эта переменная служит приближением к первой публикации.
#                Но это приближение: дата может относиться к переизданию.
#
# latestcomp   — позднейшая возможная дата создания, вычисленная
#                автоматически с учетом известных изданий и дат автора.
#
# firstpub     — вручную уточненная дата первой публикации произведения.
#
# Поэтому inferreddate разумно использовать как предполагаемую дату первой
# публикации в больших автоматически составленных списках, где firstpub нет.
# В ручной выборке firstpub позволяет проверить и уточнить это предположение.

noveltm <- noveltm |>
  mutate(
    # Насколько автоматическая оценка по выбранному тому позднее firstpub?
    edition_lag = inferreddate - firstpub
  )


# 3. СТОЛБИКОВАЯ ДИАГРАММА: СКОЛЬКО ОБЪЕКТОВ В ГРУППАХ --------
#
# geom_bar() отвечает на вопрос:
# "Сколько строк относится к каждой категории?"
#
# Он сам считает наблюдения, поэтому в aes() указываем только x.
# Переменная y пока не нужна.

noveltm |>
  ggplot(aes(category)) +
  geom_bar(fill = "steelblue") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Число произведений",
    title = "Состав вручную проверенной выборки NovelTM"
  ) +
  theme_minimal()

# Содержательный вывод:
# большинство строк отнесено к longfiction, но в выборке остаются
# shortfiction, juvenile, nonfiction, poetry и drama.
#
# Это не просто "шум". Граница между художественной прозой,
# очерком, биографией, фольклором и детской литературой исторически
# подвижна. В статье разногласия разметчиков особенно заметны на границе
# longfiction / shortfiction.
#
# ВАЖНО: выборка распределена по времени почти равномерно — обычно
# по 13 строк на каждый inferreddate. Поэтому число строк по десятилетиям
# нельзя принимать за число книг, реально опубликованных в это десятилетие.


# 4. ГИСТОГРАММА: КАК РАСПРЕДЕЛЕНА ЧИСЛОВАЯ ПЕРЕМЕННАЯ ---------
#
# Вопрос:
# "Насколько inferreddate расходится с уточненной firstpub?"
#
# Гистограмма делит числовую ось на интервалы — bins — и показывает,
# сколько наблюдений попало в каждый интервал.
#
# geom_histogram() подходит для ОДНОЙ числовой переменной.
# Столбиковая диаграмма, напротив, обычно показывает категории.

lag_data <- noveltm |>
  filter(
    edition_lag >= 0
  )

lag_data |>
  ggplot(aes(edition_lag)) +
  geom_histogram(
    binwidth = 5,
    boundary = 0,
    fill = "steelblue",
    color = "grey30"
  ) +
  labs(
    x = "Разница между inferreddate и firstpub, лет",
    y = "Число произведений",
    title = "Обычно inferreddate близка к уточненной firstpub"
  ) +
  theme_grey()

# На полном графике длинный правый хвост почти не виден:
# большинство значений равно нулю, но встречаются задержки в десятки
# и даже сотни лет.
#
# coord_cartesian() приближает нужную часть графика, НЕ удаляя данные
# перед вычислением гистограммы.

lag_data |>
  ggplot(aes(edition_lag)) +
  geom_histogram(
    binwidth = 5,
    boundary = 0,
    fill = "steelblue",
    color = "grey30"
  ) +
  coord_cartesian(xlim = c(0, 100)) +
  labs(
    x = "Разница между inferreddate и firstpub, лет",
    y = "Число произведений",
    title = "Первые сто лет библиографического запаздывания"
  ) +
  theme_minimal()

# Проверим вывод числами.

lag_data |>
  summarise(
    same_year = mean(edition_lag == 0),
    later_than_10_years = mean(edition_lag > 10),
    later_than_25_years = mean(edition_lag > 25),
    later_than_50_years = mean(edition_lag > 50)
  )

# В данных примерно четыре пятых томов совпадают по дате с firstpub.
# Но около 8% отстоят от первой публикации более чем на десять лет.
# Поэтому inferreddate нельзя автоматически называть датой создания.


# 5. БОКСПЛОТ: СРАВНЕНИЕ РАСПРЕДЕЛЕНИЙ -------------------------
#
# geom_boxplot() отвечает на вопрос:
# "Как различаются типичные значения и разброс в нескольких группах?"
#

noveltm |>
  ggplot(aes(hathiadvent, edition_lag, fill = hathiadvent)) +
  geom_boxplot(show.legend = FALSE) +
  coord_flip() +
  labs(
    x = NULL
  ) +
  theme_minimal()

# КАК ЧИТАТЬ БОКСПЛОТ?
#
# Линия внутри коробки — медиана: половина заглавий короче,
# половина длиннее этого значения.
#
# Сама коробка содержит среднюю половину наблюдений.
# Ее нижняя и верхняя границы называются квартилями.
#
# "Усы" показывают основной разброс значений.
# Отдельные точки — необычно короткие или длинные заглавия.
# Это не обязательно ошибки: они могут быть содержательно интересны.
#
# Содержательный вывод:
# медианная длина заглавия у длинной прозы меньше, чем у короткой прозы
# и детской литературы. Однако коробки заметно перекрываются:
# категория не позволяет надежно предсказать длину отдельного заглавия.


# 6. ДИАГРАММА РАССЕЯНИЯ: СВЯЗЬ ДВУХ ЧИСЛОВЫХ ПЕРЕМЕННЫХ ------
#
# geom_point() отвечает на вопрос:
# "Как связаны две числовые переменные?"
#
# Здесь по оси X — уточненная firstpub, по оси Y — автоматически
# предполагаемая inferreddate.
# Если даты совпадают, точка лежит на диагонали y = x.

date_scatter <- noveltm |>
  filter(
    firstpub >= 1700,
    inferreddate >= firstpub
  )

# Несколько самых больших запаздываний подпишем отдельно.

date_labels <- date_scatter |>
  slice_max(edition_lag, n = 15, with_ties = FALSE) |>
  mutate(label = str_trunc(shorttitle, 20))

date_scatter |>
  ggplot(aes(firstpub, inferreddate, color = hathiadvent)) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "grey50"
  ) +
  geom_point(alpha = 0.55) +
  geom_text(
    data = date_labels,
    aes(label = label),
    color = "black",
    size = 3,
    nudge_x = 8,
    check_overlap = TRUE,
    show.legend = FALSE
  ) +
  labs(
    x = "Первая публикация произведения",
    y = "Предполагаемая дата: inferreddate",
    color = NULL,
    title = "История произведения и история библиотечного тома — не одно и то же"
  ) +
  theme_minimal()

# что придумали на паре: градиент 

noveltm |> 
  filter(firstpub > 1700, inferreddate > firstpub) |> 
  ggplot(aes(firstpub, inferreddate)) +
  geom_point(show.legend = FALSE, 
             aes(color = abs(edition_lag))
             ) + 
  geom_text(data = label_data, aes(label = plot_label), check_overlap = TRUE) +
  scale_color_gradient(
    low = "lightblue",
    high = "darkblue"
  )

# Содержательный вывод:
# основное облако прижато к диагонали, но далеко над ней находятся
# случаи, где дата выбранного тома дала слишком позднюю оценку:
# например, Robinson Crusoe и разные версии Arabian Nights.
#
# Такой график показывает не только корреляцию. Диагональ задает
# содержательное ожидание: "даты должны совпасть". Нас интересуют
# систематические отклонения от этого ожидания.


# 7. РИСУНОК 10: КАК МЕНЯЛСЯ НАЦИОНАЛЬНЫЙ СОСТАВ? -------------
#
# Вопрос:
# "Как менялись доли британских, американских и других авторов?"
#
# Сначала считаем доли внутри каждого пятнадцатилетнего периода,
# затем передаем уже готовые значения в geom_line().
#
# Ограничиваем выборку adult fiction, как в статье:
# longfiction + shortfiction.

nationality_data <- noveltm |>
  filter(
    firstpub >= 1800,
    firstpub <= 2009,
    category %in% c("longfiction", "shortfiction")
  ) |>
  mutate(
    period = (firstpub %/% 10) * 10,
    nationality_group = case_when(
      nationality == "uk" ~ "UK",
      nationality == "us" ~ "US",
      is.na(nationality)   ~ "Unknown",
      TRUE                 ~ "Other"
    ),
    nationality_group = factor(
      nationality_group,
      levels = c("UK", "US", "Other", "Unknown")
    )
  ) |>
  count(period, nationality_group, name = "n") |>
  group_by(period) |>
  mutate(share = n / sum(n)) |>
  ungroup()

nationality_data |>
  ggplot(aes(period, share, color = nationality_group)) +
  geom_line(linewidth = 0.9) +
  geom_point() +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    x = NULL,
    y = "Доля произведений",
    color = NULL,
    title = "Британское преобладание сменяется американским",
    subtitle = "Adult fiction, пятнадцатилетние периоды; по мотивам Figure 10"
  ) +
  theme_minimal()

# Содержательный вывод:
# в начале XIX века выборка явно британская.
# Доля американских авторов быстро растет и примерно к концу XIX века
# становится выше британской. В середине XX века заметнее становятся
# авторы за пределами UK и US.
#
# Но это не карта мирового литературного производства.
# Большинство библиотек HathiTrust находятся в США, поэтому коллекция
# особенно хорошо отражает американские собирательские практики.
#
# В опубликованном Figure 10 категория Unknown не проведена отдельной
# линией, хотя входит в знаменатель. Здесь мы показываем ее явно:
# пропуски — тоже свойство данных, а не пустое место вне анализа.


# 8. СОСТАВ ЦЕЛОГО: ЛИНИИ ИЛИ ЗАПОЛНЕННЫЕ СТОЛБИКИ? -----------
#
# Те же данные можно показать как 100%-е столбики.
# Такой график отвечает на вопрос о СОСТАВЕ каждого периода.
#
# Линии лучше показывают траекторию отдельной группы.
# Заполненные столбики лучше показывают, из каких частей состоит целое.

nationality_data |>
  ggplot(aes(period, share, fill = nationality_group)) +
  geom_col(width = 13) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "Национальный состав взрослой прозы в выборке NovelTM"
  ) +
  theme_minimal()

# Почему geom_col(), а не geom_bar()?
#
# geom_bar() сам считает строки.
# geom_col() берет уже посчитанное значение y — здесь переменную share.


# 9. РАНЖИРОВАНИЕ: КАКИЕ ТЕКСТЫ ЧАЩЕ ВСЕГО ВСТРЕЧАЮТСЯ? -------
#
# allcopiesofwork — оценка числа копий полного произведения в HathiTrust.
# Это НЕ продажи, НЕ число читателей и не точное число изданий.
# Многотомные произведения могут давать дробные значения.
#
# Горизонтальный geom_col() удобен для ранжированного списка,
# потому что подписи произведений длиннее чисел.

top_copies <- noveltm |>
  filter(category %in% c("longfiction", "shortfiction", "juvenile")) |>
  slice_max(allcopiesofwork, n = 12, with_ties = FALSE) |>
  mutate(
    title_label = str_c(
      str_trunc(shorttitle, 20),
      " — ",
      coalesce(author, "author unknown")
    ),
    title_label = fct_reorder(title_label, allcopiesofwork)
  )

top_copies |>
  ggplot(aes(allcopiesofwork, title_label)) +
  geom_col(fill = "steelblue") +
  labs(
    x = "Оцененное число копий произведения в HathiTrust",
    y = NULL,
    title = "Библиотечная заметность — не то же самое, что число названий"
  ) +
  theme_minimal()

# Содержательный вывод:
# список поднимает вопрос о литературной "посмертной жизни".
# В HathiTrust особенно много копий некоторых произведений Киплинга,
# Скотта, Остен, Твена и других авторов, переиздававшихся десятилетиями.
#
# Для оценки ранней заметности авторы статьи используют copiesin25yrs —
# число копий, найденных в первые 25 лет. Оно меньше зависит от поздней
# канонизации, хотя по-прежнему отражает состав академических библиотек.


# 10. FACET_WRAP: ОДИН ВОПРОС В НЕСКОЛЬКИХ ПОДГРУППАХ ---------
#
# Фасеты нужны, когда цветных линий становится слишком много
# или когда формы распределений важнее прямого сравнения значений.
#
# Здесь отдельно покажем распределение ненулевого библиографического
# запаздывания для разных категорий прозы.

noveltm |>
  filter(
    category %in% c("longfiction", "shortfiction", "juvenile"),
    firstpub > 0,
    edition_lag > 0
  ) |>
  ggplot(aes(edition_lag)) +
  geom_histogram(
    binwidth = 5,
    boundary = 0,
    fill = "steelblue",
    color = "white"
  ) +
  coord_cartesian(xlim = c(0, 100)) +
  facet_wrap(~ category, ncol = 1) +
  labs(
    x = "Разница между inferreddate и firstpub, лет",
    y = "Число произведений",
    title = "Одинаковый показатель в трех категориях прозы"
  ) +
  theme_minimal()

# У фасетов должны быть одинаковые шкалы, если мы хотим сравнивать
# высоты и положение столбиков непосредственно.
# free_y или free_x допустимы, но меняют исследовательский вопрос:
# тогда легче рассмотреть форму каждой группы, но труднее сравнить группы.



# 13. ЛИДЕРЫ  
#
# Сравниваем:
# allcopiesofwork — все найденные библиотечные экземпляры произведения;
# copiesin25yrs — экземпляры изданий, вышедших в первые 25 лет
# после первой публикации.

top_12 <- noveltm |>
  slice_max(allcopiesofwork, n = 12) |>
  mutate(
    author = coalesce(author, "author unknown"),
    book = paste0(str_trunc(shorttitle, 15), " — ", str_trunc(author, 15)),
    
    # Располагаем книги по общему числу экземпляров.
    book = fct_reorder(book, allcopiesofwork)
  ) |>
  select(book, allcopiesofwork, copiesin25yrs) |>
  pivot_longer(
    cols = c(allcopiesofwork, copiesin25yrs),
    names_to = "period",
    values_to = "copies"
  ) |>
  mutate(
    period = recode(
      period,
      allcopiesofwork = "Все экземпляры",
      copiesin25yrs = "Экземпляры первых 25 лет"
    )
  )

top_12 |>
  ggplot(aes(book, copies, fill = period)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Количество библиотечных экземпляров",
    fill = NULL,
    title = "Двенадцать наиболее распространённых произведений",
    subtitle = "Все экземпляры и экземпляры изданий первых 25 лет"
  ) +
  theme_minimal()
