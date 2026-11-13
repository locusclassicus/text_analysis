# Компьютерный анализ текста

Учебник к курсу «Компьютерный анализ текста» для магистерской программы [«Цифровые методы в гуманитарных науках»](https://www.hse.ru/ma/dh/) НИУ ВШЭ. Автор курса — Ольга Валерьевна Алиева. Учебные годы: 2023/24 — 2026/27.

Сайт учебника: <https://locusclassicus.github.io/text_analysis/>

## Что где лежит

- `book/` — исходники учебника (Quarto book): главы `*.qmd`, оглавление `_quarto.yml`, библиография `references.bib`, картинки `images/`.
- `docs/` — собранный сайт. GitHub Pages публикует его из этой папки. Вручную не правится: только через рендер.
- `data/` — готовые объекты R (`.Rdata`, `.rds`): результаты долгих вычислений, которые главы загружают через `load("../data/…")`.
- `files/` — исходные датасеты для глав, занятий и домашних заданий.
- `scripts/` — шпаргалки к занятиям: код главы с короткими пояснениями.
- `helper_scripts/` — как готовились некоторые датасеты и иллюстрации.

## Как собрать сайт

Главы выполняются из папки `book/`, поэтому пути к данным начинаются с `../`.

```bash
cd book
quarto render tokenize.qmd   # одна глава
quarto render                # вся книга
```

Результат попадает в `docs/`. После коммита и `git push` GitHub Pages обновляет сайт.

## Главы и шпаргалки

| Глава | Шпаргалка |
|---|---|
| [Начало работы](https://locusclassicus.github.io/text_analysis/start.html) | — |
| [Таблицы. Опрятные данные](https://locusclassicus.github.io/text_analysis/tabular.html) | [tabular.R](scripts/tabular.R), [tabular-elegies.R](scripts/tabular-elegies.R) |
| [Визуализации](https://locusclassicus.github.io/text_analysis/plot.html) | [plot.R](scripts/plot.R), [plot-noveltm.R](scripts/plot-noveltm.R) |
| [Циклы, условия, функции](https://locusclassicus.github.io/text_analysis/iterate.html) | [iterate.R](scripts/iterate.R), [iterate-hp.R](scripts/iterate-hp.R) |
| [Импорт: JSON](https://locusclassicus.github.io/text_analysis/json.html) | [json.R](scripts/json.R) |
| [Разметка TEI XML](https://locusclassicus.github.io/text_analysis/xml.html) | [xml.R](scripts/xml.R) |
| [Публикационная система Quarto](https://locusclassicus.github.io/text_analysis/share.html) | [share.qmd](scripts/share.qmd) |
| [Регулярные выражения](https://locusclassicus.github.io/text_analysis/regex.html) | [regex.R](scripts/regex.R) |
| [Веб-скрапинг](https://locusclassicus.github.io/text_analysis/scrape.html) | [scrape.R](scripts/scrape.R) |
| [Токенизация и лемматизация](https://locusclassicus.github.io/text_analysis/tokenize.html) | [tokenize.R](scripts/tokenize.R) |
| [Векторные представления слов](https://locusclassicus.github.io/text_analysis/embed.html) | мастер-класс: [embed-workshop.qmd](scripts/embed-workshop.qmd), [embed-concepts.md](scripts/embed-concepts.md), `scripts/embed-*.R` |
| [Эмоциональная тональность](https://locusclassicus.github.io/text_analysis/sentiment.html) | — |
| [Тематическое моделирование c LDA](https://locusclassicus.github.io/text_analysis/lda.html) | — |
| [Контроль версий с Git](https://locusclassicus.github.io/text_analysis/git.html) | [git.txt](scripts/git.txt) |
| [Кластеризация и метод главных компонент](https://locusclassicus.github.io/text_analysis/cluster.html) | — |
| [Стилометрический анализ с пакетом {stylo}](https://locusclassicus.github.io/text_analysis/stylo.html) | — |
| [Консенсусные деревья и сети](https://locusclassicus.github.io/text_analysis/consensus.html) | — |
| [Сетевые данные в {igraph}](https://locusclassicus.github.io/text_analysis/igraph.html) | — |
| [Графический дизайн сетей с {ggraph} и {visNetwork}](https://locusclassicus.github.io/text_analysis/ggraph.html) | — |
| [Анализ сетей и обнаружение сообществ](https://locusclassicus.github.io/text_analysis/dracor.html) | — |
| [Пространственные данные в R](https://locusclassicus.github.io/text_analysis/maps.html) | — |
| [Интерактивные карты с {leaflet}](https://locusclassicus.github.io/text_analysis/leaflet.html) | — |
| [Регрессионный анализ](https://locusclassicus.github.io/text_analysis/regression.html) | — |
| [Регрессионные модели: SVR](https://locusclassicus.github.io/text_analysis/multivar.html) | — |
| [Бинарная классификация](https://locusclassicus.github.io/text_analysis/classification.html) | — |
| [Случайный лес](https://locusclassicus.github.io/text_analysis/rf.html) | — |
| [Многоклассовая классификация](https://locusclassicus.github.io/text_analysis/multiclass.html) | — |
| [Глубокое обучение](https://locusclassicus.github.io/text_analysis/dnn.html) | — |
| [Приложения Shiny](https://locusclassicus.github.io/text_analysis/shiny.html) | — |
| [LLM в обработке и анализе данных](https://locusclassicus.github.io/text_analysis/llm.html) | [llm-01-start.R](scripts/llm-01-start.R) … [llm-06-errors.R](scripts/llm-06-errors.R), [llm-discuss.txt](scripts/llm-discuss.txt), [llm-ollama.txt](scripts/llm-ollama.txt) |

Шпаргалки запускаются из корня репозитория: пути в них начинаются с `files/` и `data/`.

## Данные

Студенты скачивают данные по ссылкам из глав. Часть ссылок ведет в этот репозиторий, часть — в репозиторий прошлого года [text_analysis_2024](https://github.com/locusclassicus/text_analysis_2024).

Не входят в репозиторий: большие файлы мастер-класса по векторам (`files/embeddings/`) и некоторые промежуточные результаты моделей в `data/`.

## Как готовились данные (helper_scripts)

| Скрипт | Результат | Глава |
|---|---|---|
| [NovelTM.R](helper_scripts/NovelTM.R) | `data/noveltm.Rdata` | plot |
| [prepare_sent_data.R](helper_scripts/prepare_sent_data.R) | `data/liza_tbl.Rdata` | sentiment |
| [lenta.R](helper_scripts/lenta.R) | `data/news.Rdata` | embed |
| [chromote_coins.R](helper_scripts/chromote_coins.R) | `data/byzcoins.Rdata` | leaflet |
| [shchedrin.R](helper_scripts/shchedrin.R) | `tales_text.Rdata` (сказки Салтыкова-Щедрина) | домашнее задание к tokenize |
| [gore_ot_uma.R](helper_scripts/gore_ot_uma.R) | разбор `griboedov.xml` пакетом `{XML}` | xml |
| [linear.R](helper_scripts/linear.R), [logit.R](helper_scripts/logit.R) | `images/linear.png`, `images/logit.png` | regression, multiclass |
| [svm.R](helper_scripts/svm.R), [svr.R](helper_scripts/svr.R) | `images/svm.png`, `images/svr.png` | multiclass, multivar |
| [friends_biplot.R](helper_scripts/friends_biplot.R) | биплот с портретами персонажей «Друзей» | cluster (дополнительное задание) |
